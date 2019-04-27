//
// MIT License
//
// Copyright © 2018
// Native Instruments
//
// For more detailed information, please read the LICENSE in the root directory.
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - >8



//!---------------------------------------------------------------------------------------------------------------------
//!
//!  \file
//!
//!  `ni::match` allows to do pattern matching on polymorphic types. `ni::match` takes one argument, which is the
//!   object that should be matched on and returns a function that takes a variadic list of functions that define
//!   the matching cases. The provided functions (mostly lambdas) must take exactly one argument which is the type
//!   of the specific case to match on.
//!
//!   All provided functions/lambdas must have the same return type. `ni::match` will return the value of the
//!   the lambda that got matched on wrapped inside an optional<>. If no type could be matched the optional will
//!   be empty.
//!
//!   If the result types are void `ni::match` will return a bool indicating a successful match.
//!
//!   It is possible to specify a custom result value for successful matches by adding a lambda without arguments
//!   that returns the value when called. It's also possible to use `ni::otherwise` to define the value. This is
//!   only provided as syntactic sugar. There may be only one default value per match!
//!
//!   Example
//!   ```
//!     struct event { virtual ~event() };
//!     struct mouse_up : event {};
//!     struct mouse_down : event {};
//!     struct mouse_drag : event {};
//!
//!     std::vector<shared_ptr<event>>  events = {…};
//!
//!     for (auto e : events)
//!         ni::match(*e)
//!         (   [this](mouse_up const& e)    { handle_mouse_up(e); }
//!         ,   [this](mouse_down const& e)  { handle_mouse_down(e); }
//!         ,   [this](mouse_drag const& e)  { handle_mouse_drag(e); }
//!         );
//!   ```
//!
//!   It is further possible to build a matcher and invoke it later using `ni::matcher`
//!
//!   Example
//!   ```
//!     auto my_matcher = ni::matcher
//!     (   [this](mouse_up const& e)    { handle_mouse_up(e); }
//!     ,   [this](mouse_down const& e)  { handle_mouse_down(e); }
//!     ,   [this](mouse_drag const& e)  { handle_mouse_drag(e); }
//!     );
//!     boost::for_each(events, my_matcher);
//!
//!   ```
//!
//!---------------------------------------------------------------------------------------------------------------------

#pragma once

#include <ni/functional/signature.h>
#include <ni/meta/fold_or.h>
#include <ni/meta/fold_add.h>
#include <ni/meta/try_catch.h>

#include <boost/optional.hpp>

#include <ni/functional/overload.h>
#include <tuple>
#include <variant>



namespace ni
{

    //  dyn_cast<> is a customization point for (open) sum types,
    //  e.g. polymorphic (virtual) classes, std/boost::any, custom implementation using some tag system, etc.
    //
    //  template <typename TargetType>
    //  TargetType* dyn_cast(CustomType* p) { return b->custom_cast<TargetType>(); }


    // this let's ADL kick in for the client code for choosing the right dyn_cast<>
    template <typename> void dyn_cast();

    struct wildcard
    {
        wildcard() = default;
        template <class T> constexpr wildcard(T&&) {}
    };


    namespace detail
    {
        // ADL can be quite weird. This SFINAE dispatch helps to find the right function if it exists.
        template <typename TargetType> struct target_type {};

        template <typename TargetType, typename SourceType>
        auto matcher_dyn_cast(meta::try_t, target_type<TargetType>, SourceType* p) -> decltype(dyn_cast<TargetType>(p))
        {
            return dyn_cast<TargetType>(p);
        }

        template <typename TargetType, typename SourceType>
        auto matcher_dyn_cast(meta::catch_t, target_type<TargetType>, SourceType* p) -> decltype(dynamic_cast<TargetType*>(p))
        {
            return dynamic_cast<TargetType*>(p);
        }

        // Some helper type trait

        template <typename Function, typename... Functions>
        struct result
        {
            using type = typename ni::signature<Function>::result_type;

            template <typename...> struct type_list {};

            template <typename F>
            using as_type = std::decay_t<type>;

            using all_results = type_list<typename ni::signature<Functions>::result_type...>;
            using expected_results = type_list<as_type<Functions>...>;

            static_assert( std::is_same< all_results, expected_results>::value
                         , "All functions must have the same result type.");

        };


        template <typename F>
        static constexpr bool is_unmatched_case =
            std::is_same<wildcard, typename signature<F>::template argument<0>::type>::value;


        template <typename... Functions>
        struct result_type_info
        {
            static constexpr bool contains_wildcard = meta::fold_or_v<is_unmatched_case<Functions>...>;
            using wrapped_result_t = typename result<Functions...>::type;
            using result_t = std::conditional_t
            <   std::is_same<void,wrapped_result_t>::value
            ,   bool
            ,   std::conditional_t
                <   contains_wildcard
                ,   wrapped_result_t
                ,   boost::optional<wrapped_result_t>
                >
            >;
        };


        template <typename ResultType>
        struct invoker
        {
            template <typename Function, typename Arg>
            static decltype(auto) apply(Function& f, Arg&& a)
            {
                return f(std::forward<Arg>(a));
            }
        };

        template <>
        struct invoker<void>
        {
            template <typename Function, typename Arg>
            static decltype(auto) apply(Function& f, Arg&& a)
            {
                return f(std::forward<Arg>(a)), true;
            }
        };


        // -------------------------------------------------------------------------------
        // Actual dispatcher. Possible optimization for long lists use hash-map for O(1) lookup
        // -------------------------------------------------------------------------------

        template <typename ResultTypeInfo, typename Type, typename Lambda, typename... Lambdas>
        auto match_by_linear_search(Type* x, Lambda& l, Lambdas&... ls)
        -> std::enable_if_t< !is_unmatched_case<Lambda>, typename ResultTypeInfo::result_t>;


        template <typename ResultTypeInfo, typename Type>
        auto match_by_linear_search(Type*) -> typename ResultTypeInfo::result_t
        {
            return {};
        }

        template <typename ResultTypeInfo, typename Type, typename Lambda1, typename Lambda2, typename... Lambdas>
        auto match_by_linear_search(Type* x, Lambda1& l1, Lambda2& l2, Lambdas&... ls)
        -> std::enable_if_t< is_unmatched_case<Lambda1>, typename ResultTypeInfo::result_t>
        {
            return match_by_linear_search<ResultTypeInfo>(x,l2,ls...,l1);
        }

        template <typename ResultTypeInfo, typename Type, typename Lambda, typename... Lambdas>
        auto match_by_linear_search(Type*, Lambda& l)
        -> std::enable_if_t< is_unmatched_case<Lambda>, typename ResultTypeInfo::result_t>
        {
            return invoker<typename ResultTypeInfo::wrapped_result_t>::apply(l, wildcard{});
        }

        template <typename ResultTypeInfo, typename Type, typename Lambda, typename... Lambdas>
        auto match_by_linear_search(Type* x, Lambda& l, Lambdas&... ls)
        -> std::enable_if_t< !is_unmatched_case<Lambda>, typename ResultTypeInfo::result_t>
        {
            using target_t = std::remove_reference_t<typename signature<Lambda>::template argument<0>::type>;
            if (auto* p = matcher_dyn_cast(meta::try_t{}, target_type<target_t>{}, x))
                return invoker<typename ResultTypeInfo::wrapped_result_t>::apply(l,*p);
            else
                return match_by_linear_search<ResultTypeInfo>(x,ls...);
        }


        // Helper to pass overloaded function as object
        template <typename ResultTypeInfo>
        struct match_by_linear_search_fwd
        {
            template <typename... Args>
            decltype(auto) operator()(Args&&... args)
            {
                return match_by_linear_search<ResultTypeInfo>(std::forward<Args>(args)...);
            }
        };


        // -------------------------------------------------------------------------------
        // helpers -- unpack tuple as function arguments
        // -------------------------------------------------------------------------------

        template <class F, class Tuple, std::size_t... I>
        constexpr decltype(auto) apply_tuple_impl(F&& f, Tuple&& t, std::index_sequence<I...>)
        {
            return std::forward<F>(f)( std::get<I>(std::forward<Tuple>(t))... );
        }

        template <class F, class Tuple>
        constexpr decltype(auto) apply_tuple(F&& f, Tuple&& t)
        {
            return apply_tuple_impl(
                std::forward<F>(f), std::forward<Tuple>(t),
                std::make_index_sequence<std::tuple_size_v<std::remove_reference_t<Tuple>>>{}
            );
        }


        template <class F, class X, class Tuple, std::size_t... I>
        constexpr decltype(auto) apply_arg_tuple_impl(F&& f, X&& x, Tuple&& t, std::index_sequence<I...>)
        {
            return std::forward<F>(f)( std::forward<X>(x), std::get<I>(std::forward<Tuple>(t))... );
        }

        template <class F, class X, class Tuple>
        constexpr decltype(auto) apply_arg_tuple(F&& f, X&& x, Tuple&& t)
        {
            return apply_arg_tuple_impl(
                std::forward<F>(f), std::forward<X>(x), std::forward<Tuple>(t),
                std::make_index_sequence<std::tuple_size_v<std::remove_reference_t<Tuple>>>{}
            );
        }


        template <typename... Lambdas>
        struct matcher_impl
        {
            std::tuple<Lambdas...> matches;

            template <typename X>
            decltype(auto) operator()(X&& x)
            {
                static_assert(
                    meta::fold_and_v<(signature<Lambdas>::number_of_arguments == 1)...>,
                    "Can only match on lambdas with one argument."
                );

                constexpr auto num_wildcards = meta::fold_and_v<(is_unmatched_case<Lambdas>?1:0)...>;
                static_assert(
                    num_wildcards <= 1, "There can be only one default value defined per matcher."
                );

                using result_info_t = result_type_info<Lambdas...>;
                return apply_arg_tuple( match_by_linear_search_fwd<result_info_t>{}, &x, matches );
            }


            // std::variant entry point
            template <typename... Ts>
            decltype(auto) operator()(std::variant<Ts...> const& var)
            {
                return std::visit(apply_tuple(overload<Lambdas&...>, matches), var);
            }

            template <typename... Ts>
            decltype(auto) operator()(std::variant<Ts...>& var)
            {
                return std::visit(apply_tuple(overload<Lambdas&...>, matches), var);
            }
        };
    }


    // -----------------------------------------------------------------------------------

    template <typename Value>
    auto otherwise(Value value)
    {
        return [value=std::move(value)](wildcard){ return value; };
    }

    // -----------------------------------------------------------------------------------
 
    template <typename... Lambdas>
    auto matcher(Lambdas&&... lambdas)
    {
        return detail::matcher_impl<std::decay_t<Lambdas>...>{
            { std::forward<Lambdas>(lambdas)... }
        };
    }

    // -----------------------------------------------------------------------------------

    template <typename Type>
    auto match(Type&& x)
    {
        return [x=std::move(x)](auto&&... lambdas) mutable -> decltype(auto)
        {
            return ::ni::matcher(std::forward<decltype(lambdas)>(lambdas)...)(x);
        };
    }

    template <typename Type>
    auto match(Type& x)
    {
        return [&x](auto&&... lambdas) mutable -> decltype(auto)
        {
            return ::ni::matcher(std::forward<decltype(lambdas)>(lambdas)...)(x);
        };
    }

}
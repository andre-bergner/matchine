//
// MIT License
//
// Copyright © 2018
// Native Instruments
//
// For more detailed information, please read the LICENSE in the root directory.
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - >8

#pragma once

#include <ni/functional/signature.h>
#include <ni/meta/fold_or.h>
#include <ni/meta/fold_add.h>
#include <ni/meta/try_catch.h>

#include <boost/optional.hpp>

#include <tuple>


namespace ni
{
    struct wildcard
    {
        wildcard() = default;
        template <class T> constexpr wildcard(T&&) {}
    };

    //  dyn_cast<> is a customization point for (open) sum types,
    //  e.g. polymorphic (virtual) classes, std/boost::any, custom implementation using some tag system, etc.
    //
    //  template <typename TargetType>
    //  TargetType* dyn_cast(CustomType* p) { return b->custom_cast<TargetType>(); }

    // this let's ADL kick in for the client code for choosing the right dyn_cast<>
    template <typename> void dyn_cast();

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
    }


    // -----------------------------------------------------------------------------------
    // match customization points
    // -----------------------------------------------------------------------------------

    template <typename... Lambdas, typename X>
    decltype(auto) apply_match(std::tuple<Lambdas...>& matches, X&& x)
    {
        static_assert(
            meta::fold_and_v<(signature<Lambdas>::number_of_arguments == 1)...>,
            "Can only match on lambdas with one argument."
        );

        using namespace detail;

        constexpr auto num_wildcards = meta::fold_and_v<(is_unmatched_case<Lambdas>?1:0)...>;
        static_assert(
            num_wildcards <= 1, "There can be only one default value defined per matcher."
        );

        using result_info_t = result_type_info<Lambdas...>;
        return apply_arg_tuple( match_by_linear_search_fwd<result_info_t>{}, &x, matches );
    }

}
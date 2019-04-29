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

#include <ni/functional/overload.h>
#include <tuple>
#include <variant>


namespace ni
{
    namespace detail
    {
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
    }

    // -----------------------------------------------------------------------------------
    // match customization points
    // -----------------------------------------------------------------------------------

    // -----------------------------------------------------------------------------------
    // customization point for std::variant

    template <typename... Lambdas, typename... Ts>
    decltype(auto) apply_match(std::tuple<Lambdas...>& matches, std::variant<Ts...> const& var)
    {
        return std::visit(detail::apply_tuple(overload<Lambdas&...>, matches), var);
    }

    template <typename... Lambdas, typename... Ts>
    decltype(auto) apply_match(std::tuple<Lambdas...>& matches, std::variant<Ts...>& var)
    {
        return std::visit(detail::apply_tuple(overload<Lambdas&...>, matches), var);
    }
}

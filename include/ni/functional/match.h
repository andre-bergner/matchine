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

#include "detail/apply_match_by_linear_search.h"
#include "detail/apply_match_std_variant.h"

#include <tuple>


namespace ni
{
    template <typename... Lambdas>
    struct Matcher
    {
        std::tuple<Lambdas...> matches;

        template <typename X>
        decltype(auto) operator()(X&& x)
        {
            return apply_match(matches, std::forward<X>(x));
        }
    };
 
    template <typename... Lambdas>
    auto matcher(Lambdas&&... lambdas)
    {
        return Matcher<std::decay_t<Lambdas>...>{{ std::forward<Lambdas>(lambdas)... }};
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

    // -----------------------------------------------------------------------------------

    template <typename Value>
    auto otherwise(Value value)
    {
        return [value=std::move(value)](wildcard){ return value; };
    }
}

//
// MIT License
//
// Copyright © 2018
// Native Instruments
//
// For more detailed information, please read the LICENSE in the root directory.
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - >8

#include <ni/functional/match.h>

#include <gtest/gtest.h>

#include <variant>


TEST( ni_match, match_variant )
{
    struct TheAnswer {};
    struct NotMatched {};
    std::variant<int, double, TheAnswer, NotMatched> var;

    auto m = ni::matcher
    (   [](int x)     { return int(x) + 1; }
    ,   [](double x)  { return int(x) + 2; }
    ,   [](TheAnswer) { return 42; }
    ,   ni::otherwise(-1)
    );

    var = 1336;
    EXPECT_EQ( 1337, m(var) );

    var = 7355.;
    EXPECT_EQ( 7357, m(var) );

//    auto const& const_var = var;
//    EXPECT_EQ( 7357, m(const_var) );

    var = TheAnswer{};
    EXPECT_EQ( 42, m(var) );

    var = NotMatched{};
    EXPECT_EQ( -1, m(var) );
}


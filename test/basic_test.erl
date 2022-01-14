-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

bf8_test() ->
    Keys = [10, 20, 30, 40, 50],
    HashFun = fun(I) -> erlang:phash2(I) end,
    FType = {binary_fuse, 8},

    {ok, BF8} = xorf:new(FType, Keys, HashFun),
    ?assert(is_reference(BF8)),

    ?assert(xorf:contains({FType, BF8}, HashFun(10))),
    ?assertNot(xorf:contains({FType, BF8}, HashFun(100))),

    {ok, SBF8} = xorf:to_bin({FType, BF8}),
    ?assert(is_binary(SBF8)),

    {ok, DBF8} = xorf:from_bin({FType, SBF8}),
    ?assert(xorf:contains({FType, DBF8}, HashFun(10))),
    ?assertNot(xorf:contains({FType, DBF8}, HashFun(100))),

    ok.

bf16_test() ->
    Keys = [10, 20, 30, 40, 50],
    HashFun = fun(I) -> erlang:phash2(I) end,
    FType = {binary_fuse, 16},

    {ok, BF16} = xorf:new(FType, Keys, HashFun),
    ?assert(is_reference(BF16)),

    ?assert(xorf:contains({FType, BF16}, HashFun(10))),
    ?assertNot(xorf:contains({FType, BF16}, HashFun(100))),

    {ok, SBF16} = xorf:to_bin({FType, BF16}),
    ?assert(is_binary(SBF16)),

    {ok, DBF16} = xorf:from_bin({FType, SBF16}),
    ?assert(xorf:contains({FType, DBF16}, HashFun(10))),
    ?assertNot(xorf:contains({FType, DBF16}, HashFun(100))),

    ok.

bf32_test() ->
    Keys = [10, 20, 30, 40, 50],
    HashFun = fun(I) -> erlang:phash2(I) end,
    FType = {binary_fuse, 32},

    {ok, BF32} = xorf:new(FType, Keys, HashFun),
    ?assert(is_reference(BF32)),

    ?assert(xorf:contains({FType, BF32}, HashFun(10))),
    ?assertNot(xorf:contains({FType, BF32}, HashFun(100))),

    {ok, SBF32} = xorf:to_bin({FType, BF32}),
    ?assert(is_binary(SBF32)),

    {ok, DBF32} = xorf:from_bin({FType, SBF32}),
    ?assert(xorf:contains({FType, DBF32}, HashFun(10))),
    ?assertNot(xorf:contains({FType, DBF32}, HashFun(100))),

    ok.

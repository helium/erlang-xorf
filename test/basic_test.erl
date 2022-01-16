-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

bf_test() ->
    Keys = lists:seq(1, 80),
    HashFun = fun erlang:phash2/1,

    lists:foreach(
        fun(Size) ->
            {ok, Filter} = xorf:new({binary_fuse, Size}, Keys, HashFun),
            ?assert(is_reference(Filter)),
            ?assert(xorf:contains(Filter, 10, HashFun)),
            ?assertNot(xorf:contains(Filter, 100, HashFun)),

            {ok, FilterBin} = xorf:to_bin(Filter),
            ?assert(is_binary(FilterBin)),

            {ok, DFilter} = xorf:from_bin({binary_fuse, Size}, FilterBin),
            ?assert(is_reference(DFilter)),
            ?assert(xorf:contains(DFilter, 10, HashFun)),
            ?assertNot(xorf:contains(DFilter, 100, HashFun))
        end,
        [8, 16, 32]
    ),

    ok.

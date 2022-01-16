-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

identity_test() ->
    Keys = lists:seq(1, 80),
    HashFun = fun erlang:phash2/1,
    FtypesAndSizes = [{F, S} || F <- ['xor', binary_fuse], S <- [8, 16, 32]],

    lists:foreach(
        fun({Ftype, Size}) ->
            {ok, Filter} = xorf:new({Ftype, Size}, Keys, HashFun),
            ?assert(is_reference(Filter)),
            ?assert(xorf:contains(Filter, 10, HashFun)),
            ?assertNot(xorf:contains(Filter, 100, HashFun)),

            {ok, FilterBin} = xorf:to_bin(Filter),
            ?assert(is_binary(FilterBin)),

            {ok, DFilter} = xorf:from_bin({Ftype, Size}, FilterBin),
            ?assert(is_reference(DFilter)),
            ?assert(xorf:contains(DFilter, 10, HashFun)),
            ?assertNot(xorf:contains(DFilter, 100, HashFun))
        end,
        FtypesAndSizes
    ),

    ok.

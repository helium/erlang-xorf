-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

identity_test() ->
    Keys = [rand:uniform(99) || _ <- lists:seq(1, 100000)],
    HashFun = fun erlang:phash2/1,
    FtypesAndSizes = [{F, S} || F <- [exor, binary_fuse], S <- [8, 16, 32]],

    lists:foreach(
        fun({Ftype, Size}) ->
            {ok, Filter} = xorf:new({Ftype, Size}, lists:usort(lists:map(HashFun, Keys))),
            ?assert(is_reference(Filter)),
            ?assert(xorf:contains(Filter, HashFun(10))),
            ?assertNot(xorf:contains(Filter, HashFun(100))),

            {ok, FilterBin} = xorf:to_bin(Filter),
            ?assert(is_binary(FilterBin)),

            {ok, DFilter} = xorf:from_bin({Ftype, Size}, FilterBin),
            ?assert(is_reference(DFilter)),
            ?assert(xorf:contains(DFilter, HashFun(10))),
            ?assertNot(xorf:contains(DFilter, HashFun(100)))
        end,
        FtypesAndSizes
    ),

    ok.

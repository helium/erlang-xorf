-module(xorf).

-type filter_type() :: {binary_fuse, 8 | 16 | 32}.
-type hash_function() :: default_hash | none | fun((any()) -> non_neg_integer()).
-type filter() :: {filter_type(), reference()}.
-type filterbin() :: {filter_type(), binary()}.

-eport_types([filter_type/0, hash_function/0]).

-export([new/3, to_bin/1, from_bin/1, contains/2]).

-spec new(FilterType :: filter_type(),
          Entries :: [non_neg_integer()],
          HashFunction :: hash_function()) -> {ok, reference()} | {error, any()}.
new(FilterType, Entries, HashFunction) ->
    Keys = lists:map(HashFunction, Entries),
    new(FilterType, Keys).

-spec new(FilterType :: filter_type(), Keys :: [non_neg_integer()]) -> {ok, reference()} | {error, any()}.
new({binary_fuse, 8}, Keys) ->
    xorf_nif:bf8_new(Keys);
new({binary_fuse, 16}, Keys) ->
    xorf_nif:bf16_new(Keys);
new({binary_fuse, 32}, Keys) ->
    xorf_nif:bf32_new(Keys).

-spec to_bin(Filter :: filter()) -> {ok, binary()}.
to_bin({{binary_fuse, 8}, FilterRef}) ->
    xorf_nif:bf8_to_bin(FilterRef);
to_bin({{binary_fuse, 16}, FilterRef}) ->
    xorf_nif:bf16_to_bin(FilterRef);
to_bin({{binary_fuse, 32}, FilterRef}) ->
    xorf_nif:bf32_to_bin(FilterRef).

-spec from_bin(FilterBin :: filterbin()) -> {ok, reference()}.
from_bin({{binary_fuse, 8}, Binary}) ->
    xorf_nif:bf8_from_bin(Binary);
from_bin({{binary_fuse, 16}, Binary}) ->
    xorf_nif:bf16_from_bin(Binary);
from_bin({{binary_fuse, 32}, Binary}) ->
    xorf_nif:bf32_from_bin(Binary).

 -spec contains(filter(), any()) -> boolean().
contains({{binary_fuse, 8}, Filter}, Key) ->
    xorf_nif:bf8_contains(Filter, Key);
contains({{binary_fuse, 16}, Filter}, Key) ->
    xorf_nif:bf16_contains(Filter, Key);
contains({{binary_fuse, 32}, Filter}, Key) ->
    xorf_nif:bf32_contains(Filter, Key).

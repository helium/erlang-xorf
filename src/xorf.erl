-module(xorf).

-type filter_size() :: 8 | 16 | 32.
-type filter_entry() :: non_neg_integer().
-type filter_type() :: {binary_fuse, filter_size()}.
-type hash_function() :: default_hash | none | fun((any()) -> filter_entry()).
-type filter() :: {filter_type(), xorf_nif:filter()}.

-eport_types([
    filter_type/0,
    filter_size/0,
    filter_entry/0,
    filter/0,
    hash_function/0
]).

-export([
    new/2, new/3,
    to_bin/1,
    from_bin/2,
    contains/2, contains/3
]).

-spec new(
    FilterType :: filter_type(),
    Entries :: [non_neg_integer()],
    HashFunction :: hash_function()
) -> {ok, reference()} | {error, any()}.
new(FilterType, Keys, HashFunction) ->
    Entries = lists:map(HashFunction, Keys),
    new(FilterType, Entries).

-spec new(FilterType :: filter_type(), Entries :: [filter_entry()]) ->
    {ok, filter()} | {error, any()}.
new({binary_fuse, Size}, Keys) ->
    xorf_nif:bf_new(Size, Keys).

-spec to_bin(Filter :: filter()) -> {ok, binary()} | {error, any()}.
to_bin(Filter) ->
    xorf_nif:bf_to_bin(Filter).

-spec from_bin(Type :: filter_type(), Binary :: binary()) -> {ok, filter()} | {error, any()}.
from_bin({binary_fuse, Size}, Binary) ->
    xorf_nif:bf_from_bin(Size, Binary).

-spec contains(filter(), filter_entry(), hash_function()) -> boolean().
contains(Filter, Key, HashFunction) ->
    xorf_nif:bf_contains(Filter, HashFunction(Key)).

-spec contains(filter(), filter_entry()) -> boolean().
contains(Filter, Key) ->
    xorf_nif:bf_contains(Filter, Key).

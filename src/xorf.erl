-module(xorf).

-type filter_size() :: 8 | 16 | 32.
-type filter_entry() :: non_neg_integer().
-type filter_type() :: {exor | binary_fuse, filter_size()}.
-type filter() :: xorf_nif:filter().

-eport_types([
    filter_type/0,
    filter_size/0,
    filter_entry/0,
    filter/0,
    hash_function/0
]).

-export([
    new/2,
    to_bin/1,
    from_bin/2,
    contains/2
]).

%% @doc Create a new filter given a unique list of 64 bit integers.
%%
%% Constructs an xor or binary fuse fitler with the given number of bits per
%% entry for the list of provided keys. Note that providing a non unique list of
%% integers may result in undefined behavior.
-spec new(FilterType :: filter_type(), Entries :: [filter_entry()]) ->
    {ok, filter()} | {error, any()}.
new({binary_fuse, Size}, Keys) ->
    xorf_nif:bf_new(Size, Keys);
new({exor, Size}, Keys) ->
    xorf_nif:xor_new(Size, Keys).

%% @doc Convert a a filter to it's binary form
-spec to_bin(Filter :: filter()) -> {ok, binary()} | {error, any()}.
to_bin(Filter) ->
    xorf_nif:to_bin(Filter).

%% @doc Create a filter from a binary.
%%
%% The type of filter is not stored as part of the binary
-spec from_bin(Type :: filter_type(), Binary :: binary()) -> {ok, filter()} | {error, any()}.
from_bin({binary_fuse, Size}, Binary) ->
    xorf_nif:bf_from_bin(Size, Binary);
from_bin({exor, Size}, Binary) ->
    xorf_nif:xor_from_bin(Size, Binary).

%% @doc Check membership in a filter
%%
%% Returns true if the given integer entry is a mamber of the filter. Note that
%% false positives are possible in xor filters, but become less likely with the
%% number of bits used per entry during filter creation.
-spec contains(filter(), filter_entry()) -> boolean().
contains(Filter, Key) ->
    xorf_nif:contains(Filter, Key).

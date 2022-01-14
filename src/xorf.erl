-module(xorf).

-type filter_type() :: {exor | binary_fuse, 8 | 16 | 32}.
-type hash_function() :: default_hash | none | fun((any()) -> non_neg_integer()).
-type filter() :: {filter_type(), reference()}.

-eport_types([filter_type/0, hash_function/0]).

-export([new/3]).

-spec new(filter_type(), [any()], hash_function()) -> {ok, filter()} | {error, term()}.
new(FilterType, Entries, HashFunction) ->
    {error, undefined}.

-spec contains(filter(), any(), hash_function()) -> boolean().
contains(Filter, Entry, HashFunction) ->
    false.

-spec to_bin(filter()) -> binary().
to_bin(Filter) ->
    <<>>.

-spec from_bin(binary()) -> {ok, filter()} | {error, term()}.
from_bin(Binary) ->
    {error, undefined}.

-module(xorf_nif).

-export([
    bf_new/2, xor_new/2,
    to_bin/1,
    bf_from_bin/2, xor_from_bin/2,
    contains/2
]).

%% Native library support
-export([load/0]).
-on_load(load/0).

-type filter_size() :: xorf:filter_size().
-type filter_entry() :: xorf:filter_entry().
-type filter() :: reference().

-export_type([filter/0]).

-spec bf_new(Size :: filter_size(), Keys :: [filter_entry]) -> {ok, filter()} | {error, any()}.
bf_new(_Size, _Keys) ->
    not_loaded(?LINE).

-spec xor_new(Size :: filter_size(), Keys :: [filter_entry]) -> {ok, filter()} | {error, any()}.
xor_new(_Size, _Keys) ->
    not_loaded(?LINE).

-spec to_bin(Filter :: filter()) -> {ok, binary()} | {error, any()}.
to_bin(_Filter) ->
    not_loaded(?LINE).

-spec bf_from_bin(Size :: filter_size(), Binary :: binary()) -> {ok, filter()} | {error, any()}.
bf_from_bin(_Size, _Binary) ->
    not_loaded(?LINE).

-spec xor_from_bin(Size :: filter_size(), Binary :: binary()) -> {ok, filter()} | {error, any()}.
xor_from_bin(_Size, _Binary) ->
    not_loaded(?LINE).

-spec contains(Filter :: filter(), Key :: filter_entry()) -> boolean().
contains(_Filter, _Key) ->
    not_loaded(?LINE).

%% @private
load() ->
    erlang:load_nif(filename:join(priv(), "libxorf"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.

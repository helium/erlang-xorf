-module(xorf_nif).

-export(
   [bf8_new/1,
    bf8_to_bin/1,
    bf8_from_bin/1,
    bf8_contains/2,
    bf16_new/1,
    bf16_to_bin/1,
    bf16_from_bin/1,
    bf16_contains/2,
    bf32_new/1,
    bf32_to_bin/1,
    bf32_from_bin/1,
    bf32_contains/2
   ]).

%% Native library support
-export([load/0]).
-on_load(load/0).

-spec bf8_new(Keys :: [non_neg_integer()]) -> {ok, reference()} | {error, any()}.
bf8_new(_Keys) ->
    not_loaded(?LINE).
-spec bf16_new(Keys :: [non_neg_integer()]) -> {ok, reference()} | {error, any()}.
bf16_new(_Keys) ->
    not_loaded(?LINE).
-spec bf32_new(Keys :: [non_neg_integer()]) -> {ok, reference()} | {error, any()}.
bf32_new(_Keys) ->
    not_loaded(?LINE).

-spec bf8_to_bin(FilterRef :: reference()) -> {ok, binary()} | {error, any()}.
bf8_to_bin(_FilterRef) ->
    not_loaded(?LINE).
-spec bf16_to_bin(FilterRef :: reference()) -> {ok, binary()} | {error, any()}.
bf16_to_bin(_FilterRef) ->
    not_loaded(?LINE).
-spec bf32_to_bin(FilterRef :: reference()) -> {ok, binary()} | {error, any()}.
bf32_to_bin(_FilterRef) ->
    not_loaded(?LINE).

-spec bf8_from_bin(Binary :: binary()) -> {ok, reference()} | {error, any()}.
bf8_from_bin(_Binary) ->
    not_loaded(?LINE).
-spec bf16_from_bin(Binary :: binary()) -> {ok, reference()} | {error, any()}.
bf16_from_bin(_Binary) ->
    not_loaded(?LINE).
-spec bf32_from_bin(Binary :: binary()) -> {ok, reference()} | {error, any()}.
bf32_from_bin(_Binary) ->
    not_loaded(?LINE).

-spec bf8_contains(FilterRef :: reference(), Key :: any()) -> boolean() | {error, any()}.
bf8_contains(_FilterRef, _Key) ->
    not_loaded(?LINE).
-spec bf16_contains(FilterRef :: reference(), Key :: any()) -> boolean() | {error, any()}.
bf16_contains(_FilterRef, _Key) ->
    not_loaded(?LINE).
-spec bf32_contains(FilterRef :: reference(), Key :: any()) -> boolean() | {error, any()}.
bf32_contains(_FilterRef, _Key) ->
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

-module(xorf_nif).

-export([example/2]).

%% Native library support
-export([load/0]).
-on_load(load/0).

-spec example(binary(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
example(_Bin, _Hash) ->
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

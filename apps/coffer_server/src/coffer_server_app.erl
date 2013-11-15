-module(coffer_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    coffer_deps:ensure(),
    coffer_util:ensure_deps_started(coffer_server),

    %% start HTTP
    ok = start_http(),

    coffer_server_sup:start_link().

stop(_State) ->
    ok.


start_http() ->
    %% get max of acceptors
    NbAcceptors = coffer_config:get_config(nb_acceptors, 100),

    %% bind address
    Bind = coffer_config:get_config(bind_http, "0.0.0.0:8000"),

    %% bind options
    BindOpts = case coffer_config:parse_address(list_to_binary(Bind)) of
        {any, Port} ->
            [{port, Port}];
        {Ip, Port} ->
            {ok, ParsedIp} = inet_parse:address(Ip),
            [{port, Port}, {ip, ParsedIp}]
    end,

    %% final transport options
    TransOpts = BindOpts ++ coffer_config:ssl_options(),

    Transport = case coffer_config:is_ssl() of
        true -> ranch_ssl;
        false -> ranch_tcp
    end,

    start_listener(NbAcceptors, Transport, TransOpts,
                   coffer_config:http_env(), 5).


%% start a listener
start_listener(_, _, _, _, 0) ->
    cf_lager:info("HTTP API not started, too much tries.~n", []),
    {error, http_not_started};
start_listener(NbAcceptors, Transport, TransOpts, ProtoOpts, Tries) ->
    case ranch:start_listener(coffer_http, NbAcceptors, Transport, TransOpts,
                              cowboy_protocol, ProtoOpts) of
        {ok, _} ->
            cf_lager:info("HTTP API started", []),
            ok;
        {error, _} = Error ->
            cf_lager:error("error starting the HTTP api: ~p~n", [Error]),
            start_listener(NbAcceptors, Transport, TransOpts, ProtoOpts,
                Tries-1);
        _ ->
            %% already started
            ok
    end.

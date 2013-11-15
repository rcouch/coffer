%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%
-module(coffer_config).

-export([get_config/1, get_config/2]).
-export([parse_uri/2]).
-export([storage_scheme/0]).
-export([index_scheme/0]).

%% HTTP options
-export([parse_address/1,
         http_env/0,
         ssl_options/0,
         is_ssl/0]).

-define(DEFAULT_PORT, 7000).

%% @doc return a config value
get_config(Key) ->
    get_config(Key, undefined).

%% @doc return a config value
get_config(Key, Default) ->
    case application:get_env(coffer_server, Key) of
        {ok, Val} -> Val;
        undefined -> Default
    end.

%% @doc parse a storage or index uri
parse_uri(Uri, Type) ->
    case urlsplit(Uri) of
        {"", _, _, _} ->
            {error, {bad_uri, Uri}};
        {Scheme, Path, Query, Fragments} ->
            %% parse params
            Params = parse_qs(Query),
            %% get the backend associated to the scheme
            Backend = case Type of
                storage ->
                    proplists:get_value(Scheme, storage_scheme());
                index ->
                    proplists:get_value(Scheme, index_scheme())
            end,

            %% invalid uri, fail now
            if Backend =:= undefined ->
                    throw({error, {invalid_uri, Uri}});
                true -> ok
            end,

            {Backend, [{path, Path},
                       {params, Params},
                       {tags, Fragments}]}
    end.

%% @doc return all supported storage scheme
storage_scheme() ->
    Defaults = case application:get_env(coffer_blobserver, backends) of
        undefined -> [];
        {ok, Scheme} -> Scheme
    end,
    coffer_util:propmerge(get_config(storage_backends, []), Defaults).

%% @doc return all supported index scheme
index_scheme() ->
    Defaults = case application:get_env(coffer_index, backends) of
        undefined -> [];
        {ok, Scheme} -> Scheme
    end,
    coffer_util:propmerge(get_config(index_backends, []), Defaults).



parse_address(<<"[", Rest/binary>>) ->
    case binary:split(Rest, <<"]">>) of
        [Host, <<>>] ->
            {binary_to_list(Host), ?DEFAULT_PORT};
        [Host, <<":", Port/binary>>] ->
            {binary_to_list(Host),
             list_to_integer(binary_to_list(Port))};
        _ ->
            parse_address(Rest)
    end;
parse_address(Addr) ->
    case binary:split(Addr, <<":">>) of
        [Port] ->
            {any, list_to_integer(binary_to_list(Port))};
        [<<>>, Port] ->
            {any, list_to_integer(binary_to_list(Port))};
        [Host, Port] ->
            {binary_to_list(Host),
             list_to_integer(binary_to_list(Port))}
    end.

http_env() ->
    DispatchRules = coffer_http:dispatch_rules(),
    Dispatch = [{'_', DispatchRules}],
    Dispatch1 = cowboy_router:compile(Dispatch),
    [{env, [{dispatch, Dispatch1}]}].


ssl_options() ->
    CertFile = get_config(certfile, nil),
    KeyFile = get_config(keyfile, nil),
    case CertFile /= nil of
        true ->
            SslOpts0 = [{certfile, CertFile}],

            %% open certfile to get entries.
            {ok, PemBin} = file:read_file(CertFile),
            CertEntries = public_key:pem_decode(PemBin),

            SslOpts = case KeyFile of
                nil ->
                    if length(CertEntries) >= 2 ->
                            SslOpts0;
                        true ->
                            lager:error("SSL Private Key is missing"),
                            throw({error, missing_keyfile})
                    end;
                KeyFile ->
                    SslOpts0 ++ [{keyfile, KeyFile}]
            end,

            %% set password if one is needed for the cert
            SslOpts1 = case get_config(key_password, nil) of
                nil -> SslOpts;
                Password ->
                    SslOpts ++ [{password, Password}]
            end,

            %% check if cacerts are already set in the pem file
            SslOpts2 = case get_config(cacert_file, nil) of
                nil ->
                    case CertEntries of
                        [_P, _Cert| CaCerts] when CaCerts /= [] ->
                            SslOpts1 ++ [{cacerts, CaCerts}];
                        _ ->
                            SslOpts1
                    end;
                CaCertFile ->
                    SslOpts1 ++ [{cacertfile, CaCertFile}]
            end,

            % do we verify certificates ?
            FinalSslOpts = case get_config(verify_ssl_certificates, false) of
                "false" ->
                    SslOpts2 ++ [{verify, verify_none}];
                "true" ->
                    %% get depth
                    Depth = list_to_integer(
                        get_config(ssl_certificate_max_depth, 1)
                    ),
                    %% check if we need a CA.
                    WithCA = SslOpts1 /= SslOpts1,
                    case WithCA of
                        false when Depth >= 1 ->
                            lager:error("Verify SSL certificate "
                                    ++"enabled but file containing "
                                    ++"PEM encoded CA certificates is "
                                    ++"missing"),
                            throw({error, missing_cacerts});
                        _ ->
                            ok
                    end,
                    [{depth, Depth},{verify, verify_peer}]
            end,
            FinalSslOpts;
        false ->
            lager:error("SSL enabled but PEM certificates are missing.", []),
            throw({error, missing_certs})
    end.

is_ssl() ->
    get_config(ssl, false) =:= true.

%% internals
%%



%% @spec parse_qs(string()) -> [{Key, Value}]
%% @doc Parse a query string
parse_qs(String) ->
    parse_qs(String, []).

parse_qs([], Acc) ->
    lists:reverse(Acc);
parse_qs(String, Acc) ->
    {Key, Rest} = parse_qs_key(String),
    {Value, Rest1} = parse_qs_value(Rest),
    parse_qs(Rest1, [{Key, Value} | Acc]).

parse_qs_key(String) ->
    parse_qs_key(String, []).

parse_qs_key([], Acc) ->
    {lists:reverse(Acc), ""};
parse_qs_key([$= | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
parse_qs_key(Rest=[$; | _], Acc) ->
    {lists:reverse(Acc), Rest};
parse_qs_key(Rest=[$& | _], Acc) ->
    {lists:reverse(Acc), Rest};
parse_qs_key([C | Rest], Acc) ->
    parse_qs_key(Rest, [C | Acc]).

parse_qs_value(String) ->
    parse_qs_value(String, []).

parse_qs_value([], Acc) ->
    {lists:reverse(Acc), ""};
parse_qs_value([$; | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
parse_qs_value([$& | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
parse_qs_value([C | Rest], Acc) ->
    parse_qs_value(Rest, [C | Acc]).



%% @spec urlsplit(Url) -> {Scheme, Path, Query, Fragment}
%% @doc Return a 5-tuple, does not expand % escapes. Only supports HTTP style
%%      URLs.
urlsplit(Url) ->
    {Scheme, Url1} = urlsplit_scheme(Url),
    {Path, Query, Fragment} = urlsplit_path(Url1),
    {Scheme, Path, Query, Fragment}.

urlsplit_scheme(Url) ->
    case urlsplit_scheme(Url, []) of
        no_scheme ->
            {"", Url};
        Res ->
            Res
    end.

urlsplit_scheme([C | Rest], Acc) when ((C >= $a andalso C =< $z) orelse
                                       (C >= $A andalso C =< $Z) orelse
                                       (C >= $0 andalso C =< $9) orelse
                                       C =:= $+ orelse C =:= $- orelse
                                       C =:= $.) ->
    urlsplit_scheme(Rest, [C | Acc]);
urlsplit_scheme([$: | Rest], Acc=[_ | _]) ->
    {string:to_lower(lists:reverse(Acc)), Rest};
urlsplit_scheme(_Rest, _Acc) ->
    no_scheme.

urlsplit_path("//" ++ Rest) ->
    urlsplit_path(Rest, []);
urlsplit_path(Path) ->
    {Path, "", ""}.

urlsplit_path("", Acc) ->
    {lists:reverse(Acc), "", ""};
urlsplit_path("?" ++ Rest, Acc) ->
    {Query, Fragment} = urlsplit_query(Rest),
    {lists:reverse(Acc), Query, Fragment};
urlsplit_path("#" ++ Rest, Acc) ->
    {lists:reverse(Acc), "", Rest};
urlsplit_path([C | Rest], Acc) ->
    urlsplit_path(Rest, [C | Acc]).

urlsplit_query(Query) ->
    urlsplit_query(Query, []).

urlsplit_query("", Acc) ->
    {lists:reverse(Acc), ""};
urlsplit_query("#" ++ Rest, Acc) ->
    {lists:reverse(Acc), Rest};
urlsplit_query([C | Rest], Acc) ->
    urlsplit_query(Rest, [C | Acc]).

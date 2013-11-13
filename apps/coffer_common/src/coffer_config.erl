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



%% @doc return a config value
get_config(Key) ->
    get_config(Key, undefined).

%% @doc return a config value
get_config(Key, Default) ->
    case application:get_env(coffer, Key) of
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
            {Backend, [{path, Path},
                       {params, Params},
                       {tags, Fragments}]}
    end.

%% @doc return all supported storage scheme
storage_scheme() ->
    Defaults = application:get_env(coffer_blobserver, backends),
    coffer_util:propmerge(Defaults, get_config(storage_backends, [])).

%% @doc return all supported index scheme
index_scheme() ->
    Defaults = application:get_env(coffer_index, backends),
    coffer_util:propmerge(Defaults, get_config(index_backends, [])).



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

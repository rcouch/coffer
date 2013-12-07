%%% -*- erlang -*-
%%%
%%% This file is part of coffer-server released under the Apache license 2.
%%% See the NOTICE for more information.
%%%
%% @doc stat handler to check if some blobs exists or are actually
%% downloading.

-module(coffer_http_stat).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {StorageName, Req3} = cowboy_req:binding(storage, Req2),
    {ok, Req4} = case coffer_blobserver:get_storage(StorageName) of
        {ok, Storage} ->
             handle_stat(Method, Storage, Req3);
        {error, not_found} ->
            coffer_http_util:not_found(Req3);
        {error, Reason} ->
            coffer_http_util:error(Reason, Req3)
    end,
    {ok, Req4, State}.

handle_stat(<<"GET">>, Storage, Req) ->
    {KVs, Req1} = cowboy_req:qs_vals(Req),
    BlobRefs = [BlobRef || {_, BlobRef} <- KVs],
    stat_response(BlobRefs, Storage, Req1);
handle_stat(<<"POST">>, Storage, Req) ->
    {ok, KVs, Req1} = cowboy_req:body_qs(1024000, Req),
    BlobRefs = [BlobRef || {_, BlobRef} <- KVs],
    stat_response(BlobRefs, Storage, Req1);
handle_stat(_, _, Req) ->
    coffer_http_util:not_allowed(<<"GET, POST">>, Req).

terminate(_Reason, _Req, _State) ->
    ok.

stat_response([], _Storage, Req) ->
    coffer_http_util:error(400, <<"blobrefs_missing">>, Req);
stat_response(BlobRefs, Storage, Req) ->
    JsonObj = stat_object(Storage, BlobRefs),
    {Json, Req1} = coffer_http_util:to_json(JsonObj, Req),
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
                     Json, Req1).

stat_object(Storage, BlobRefs) ->
    %% get stats from the backend
    {ok, {Found, _Missing, Partials}} = coffer_blobserver:stat(Storage,
                                                               BlobRefs),

    %% Get the max upload size from the ref found. if none have been
    %% found set it to 0 (so we make lists:max happy.
    Obj = case Found of
        [] ->
            [{<<"stat">>, []}, {<<"maxUploadSize">>, 0}];
        _ ->
            MaxUploadSize = lists:max([Size || {_, Size} <- Found]),
            [{<<"stat">>, Found}, {<<"maxUploadSize">>, MaxUploadSize}]
    end,
    %% return partially uploaded. for now it's only a way to make sure a
    %% blob is actually downloading
    case Partials of
        [] ->
            Obj;
        _ ->
            Obj + [{<<"alreadyHavePartially">>, Partials}]
    end.

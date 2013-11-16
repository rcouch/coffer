%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%
-module(coffer_http_blob).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-compile([{parse_transform, lager_transform}]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {StorageName, Req3} = cowboy_req:binding(storage, Req2),
    {BlobRef, Req4} = cowboy_req:binding(blob, Req3),

    {ok, Req5} = case coffer_blobserver:get_storage(StorageName) of
        {ok, Storage} ->
            maybe_process(Method, BlobRef, Storage, Req4);
        {error, not_found} ->
            coffer_http_util:not_found(Req4);
        {error, Reason} ->
            coffer_http_util:error(Reason, Req4)
    end,
    {ok, Req5, State}.

maybe_process(<<"HEAD">>, BlobRef, Storage,  Req) ->
    case coffer_blobserver:is_blob(Storage, BlobRef) of
        true ->
            cowboy_req:reply(200, [], [], Req);
        false ->
            cowboy_req:reply(404, [], [], Req)
    end;

maybe_process(<<"GET">>, BlobRef, Storage, Req) ->
    case coffer_blobserver:fetch_blob(Storage, BlobRef) of
        {error, not_found} ->
            lager:error("blob ~p not found.", [BlobRef]),
            coffer_http_util:not_found(Req);
        {error, Reason} ->
            coffer_http_util:error(Reason, Req);
        Reader ->
            BodyFun = fun(ChunkFun) ->
                    send_blob(Reader, ChunkFun)
            end,
            cowboy_req:reply(200, [], {chunked, BodyFun}, Req)
    end;

maybe_process(<<"PUT">>, BlobRef, Storage, Req0) ->
    BodyReaderFun = fun(Req) ->
            case cowboy_req:stream_body(Req) of
                {ok, Bin, Req1} ->
                    {ok, Bin, Req1};
                {done, Req1} ->
                    {ok, eob, Req1};
                Error ->
                    Error
            end
    end,
    BodyReader = {BodyReaderFun, Req0},
    case coffer_blobserver:receive_blob(Storage, BlobRef, BodyReader) of
        {ok, UploadSize, Req2} ->
                StatusMessage = [
                    { <<"received">>, [
                        [
                            {<<"blobref">>, BlobRef},
                            {<<"size">>, UploadSize}]
                        ]
                    }
                ],
                {Json, Req3} = coffer_http_util:to_json(StatusMessage,
                                                        Req2),
                cowboy_req:reply(201, [], Json, Req3);
        {{error, _}=Error, Req2} ->
            lager:error("problem uploading blob id ~p: ~p",
                        [BlobRef, Error]),
            coffer_http_util:error(400, Error, Req2);
        {error, already_exists, Req2} ->
            coffer_http_util:error(409, already_exists, Req2);
        {error, Reason} ->
            coffer_http_util:error(Reason, Req0)
    end;


maybe_process(<<"DELETE">>, BlobRef, Storage, Req) ->
    case coffer_blobserver:delete_blob(Storage, BlobRef) of
        ok ->
            StatusMessage = [
                { <<"deleted">>,
                    [
                        {<<"blobref">>, BlobRef}
                    ]
                }
            ],
            {Json, Req1} =  coffer_http_util:to_json(
                    StatusMessage, Req),
            cowboy_req:reply(202, [], Json, Req1);
        {error, Reason} ->
            coffer_http_util:error(Reason, Req)
    end;

maybe_process(_, _, _, Req) ->
    coffer_http_util:not_allowed([<<"HEAD">>, <<"GET">>, <<"PUT">>,
                                  <<"DELETE">>], Req).

terminate(_Reason, _Req, _State) ->
    ok.

%% ---

send_blob({ReaderFun, St}, ChunkFun) ->
    case ReaderFun(St) of
        eob ->
            ok;
        {error, Error} ->
            Json = jsx:encode([{<<"error">>, Error}]),
            ChunkFun(Json),
            ok;
        {ok, Bin, NewSt} ->
            ChunkFun(Bin),
            send_blob({ReaderFun, NewSt}, ChunkFun)
    end.

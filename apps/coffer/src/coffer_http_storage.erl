%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%
-module(coffer_http_storage).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(LIMIT, 16#10000000).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {StorageName, Req3} = cowboy_req:binding(storage, Req2),
    {ok, Req4} = case coffer_blobserver:get_storage(StorageName) of
        {ok, Storage} ->
             maybe_process(Method, Storage, Req3);
        {error, not_found} ->
            coffer_http_util:not_found(Req3);
        {error, Reason} ->
            coffer_http_util:error(Reason, Req3)
    end,
    {ok, Req4, State}.

maybe_process(<<"HEAD">>, _, Req) ->
    coffer_http_util:ok(Req);
maybe_process(<<"GET">>, Storage, Req) ->
    enumerate(Storage, Req);
maybe_process(<<"POST">>, Storage, Req) ->
    case process_multipart(Storage, Req) of
        {ok, Received, Req1} ->
            {Success, Errors} = lists:foldl(fun
                        ({BlobRef, {ok, UploadSize}}, {SAcc, EAcc}) ->
                            SAcc1 = [[
                                        {<<"blobref">>, BlobRef},
                                        {<<"size">>, UploadSize}]
                                     | SAcc],
                            {SAcc1, EAcc};
                        ({BlobRef, {error, Reason}}, {SAcc, EAcc}) ->
                            EAcc1 = [[
                                        {<<"blobref">>, BlobRef},
                                        {<<"error">>,
                                         coffer_util:to_binary(Reason)}]
                                     | EAcc],
                            {SAcc, EAcc1}
                    end, {[], []}, Received),
            StatusMessage = [{ <<"received">>, Success },
                             { <<"errors">>, Errors }],

            {Json, Req2} = coffer_http_util:to_json(StatusMessage, Req1),
            cowboy_req:reply(201,
                             [{<<"Content-Type">>, <<"application/json">>}],
                             Json, Req2);
        {error, no_part, Req1} ->
            coffer_http_util:ok(Req1)
    end;
maybe_process(_, _, Req) ->
    coffer_http_util:not_allowed(<<"HEAD, GET, POST">>, Req).

terminate(_Reason, _Req, _State) ->
    ok.


%% enumerate blobs
enumerate(Storage, Req) ->
    {Limit, Req2} = case cowboy_req:qs_val(<<"limit">>, Req) of
        {undefined, Req1} -> {?LIMIT, Req1};
        {L, Req1} -> {list_to_integer(binary_to_list(L)), Req1}
    end,

    BodyFun = fun(ChunkFun) ->
            StartBody =  <<"{\"blobs\": [\n" >>,
            ChunkFun(StartBody),
            Reader = coffer_blobserver:enumerate(Storage),
            do_enumerate(Reader, ChunkFun, <<"">>, 0, Limit -1),
            ChunkFun(<< "\n]}" >>),
            ok
    end,
    cowboy_req:reply(200, [{<<"Content-Type">>, <<"application/json">>}],
                        {chunked, BodyFun}, Req2).


do_enumerate(_, _, _, Count, Limit) when Count > Limit ->
    ok;
do_enumerate({ReaderFun, State}, ChunkFun, Pre, Count, Limit) ->
    case ReaderFun(State) of
        {ok, {BlobRef, Size}, NewState} ->
            Json = jsx:encode([{<<"blobref">>, BlobRef},
                                  {<<"size">>, Size}]),

            ChunkFun(iolist_to_binary([Pre, Json])),
            do_enumerate({ReaderFun, NewState}, ChunkFun, <<",\n">>, Count+1,
                         Limit);
        {error, Reason} ->
            Json = jsx:encode([{<<"error">>, Reason}]),
            ChunkFun(iolist_to_binary([Pre, Json])),
            ok;

        done ->
            ok
    end.


%% multipart processing

process_multipart(Storage, Req) ->
    {Reply, Req2} = multipart_data(Req),

    case Reply of
        {headers, _Headers} ->
            get_part(Storage, Reply, Req2, []);
        {eof, Req2} ->
            {error, no_part, Req2};
        {ok, undefined, Req2} ->
            {error, no_part, Req2}
    end.


multipart_data(Req) ->
    case cowboy_req:multipart_data(Req) of
        {headers, Headers, Req2} ->
            {{headers, Headers}, Req2};
        {body, Data, Req2} ->
            {{body, Data}, Req2};
        {end_of_part, Req2} ->
            {end_of_part, Req2};
        {eof, Req2} ->
            {eof, Req2};
        {ok, undefined, Req2} ->
            {undefined, Req2};
        Error ->
            lager:error("Multipart unknown return value: ~p", [Error]),
            Error
    end.


get_part(Storage, {headers, Headers}, Req, Acc) ->
    %% extract the blobref
    DispositionBinary = proplists:get_value(<<"content-disposition">>,
                                            Headers),
    {_, Props} = cowboy_multipart:content_disposition(DispositionBinary),
    BlobRef = proplists:get_value(<<"name">>, Props),
    lager:info("uploading ~p", [BlobRef]),
    %% process the part
    ReaderFun = fun(Req1) ->
            {Reply, Req2} = multipart_data(Req1),
            case Reply of
                {body, Data} ->
                    {ok, Data, Req2};
                end_of_part ->
                    {ok, eob, Req2}
            end
    end,
    Reader = {ReaderFun, Req},
    {Acc2, Req4} = case coffer_blobserver:receive_blob(Storage, BlobRef,
                                                       Reader) of
        {ok, UploadSize, Req3} ->
            {[{BlobRef, {ok, UploadSize}} | Acc], Req3};
        {error, Reason, Req3} ->
            {[{BlobRef, {error, Reason}} | Acc], Req3}
    end,
    %% process other parts or stop
    {Reply, Req5} = multipart_data(Req4),
    case Reply of
        {headers, _} ->
            get_part(Storage, Reply, Req5, Acc2);
        _ ->
            {ok, Acc2, Req5}
    end.

%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%
%%% @doc JSX callback module to handle blobs list and parse them


-module(coffer_client_enumerate).
-export([init/1, handle_event/2]).

-export([wait_blobs/2,
         wait_list/2,
         wait_blob/2,
         wait_blobref/2,
         wait_size/2]).


init(_) ->
    {wait_blobs, {<<>>, -1}, queue:new()}.

handle_event(Event, {Fun, _, _}=St) ->
    ?MODULE:Fun(Event, St).


wait_blobs(start_object, St) ->
    St;
wait_blobs(end_object, St) ->
    St;
wait_blobs({key, <<"blobs">>}, {_, Blob, Q}) ->
    {wait_list, Blob, Q};
wait_blobs({key, _}, St) ->
    St;
wait_blobs(end_json, {_, _, Q}) ->
    Q.

wait_list(start_array, {_, Blob, Q}) ->
    {wait_blob, Blob, Q};

wait_list(end_array, {_, Blob, Q}) ->
    {wait_blobs, Blob, Q}.

wait_blob(start_object, {Fun, _, Q}) ->
    {Fun, {<<>>, -1}, Q};
wait_blob({key, <<"blobref">>}, {_, Blob, Q}) ->
    {wait_blobref, Blob, Q};
wait_blob({key, <<"size">>}, {_, Blob, Q}) ->
    {wait_size, Blob, Q};
wait_blob({key, _}, St) ->
    St;
wait_blob(end_object, {_, Blob, Q}) ->
    Q1 = queue:in(Blob, Q),
    {wait_blob, {<<>>, -1}, Q1};
wait_blob(end_array, {_, _, Q}) ->
    {wait_blobs, {<<>>, -1}, Q};
wait_blob(_, St) ->
    St.

wait_blobref({string, Val}, {_, {_, Size}, Q}) ->
    {wait_blob, {Val, Size}, Q};
wait_blobref(_, {_, Blob, Q}) ->
    %% ignore invalid keys. they will be check on another pass.
    {wait_blob, Blob, Q}.

wait_size({integer, Val}, {_, {Ref, _}, Q}) ->
    {wait_blob, {Ref, Val}, Q};
wait_size(_, {_, Blob, Q}) ->
    %% ignore invalid keys. they will be check on another pass.
    {wait_blob, Blob, Q}.

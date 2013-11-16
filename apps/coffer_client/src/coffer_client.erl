%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%
-module(coffer_client).

-record(coffer_client, {url,
                        pool,
                        opts}).

%% API to connect to a storage
-export([open/1, open/2,
         close/1,
         is_exists/2]).

%% client uril
-export([start/0, stop/0]).


%% CLIENT API

%% @doc open a connection to a storage, the url given is the Url of a storage.
%% A client is dedicated to a storage and has its own pool.
open(Url) ->
    open(Url, []).

%% @doc same as above but with connections options. See the hackney
%% options for it. A special pool_opts property has been added to create
%% the client pool and pass needed options to it.
open(Url, Opts) ->
    %% each client maintain its own pool
    PoolName = list_to_atom(binary_to_list(Url)),
    PoolOpts = proplists:get_value(pool_opts, Opts, [{pool_size, 10}]),
    {ok, Pool} = hackney:start_pool(PoolName, PoolOpts),
    %% set client opts
    ClientOpts = [{pool, Pool} | Opts],
    %% return the client context
    #coffer_client{url=Url,
                   pool = Pool,
                   opts = ClientOpts}.

%% @doc close a connection to a storage and stop the pool
close(#coffer_client{pool=Pool}) ->
    hackney:stop_pool(Pool).

is_exists(#coffer_client{opts=Opts}=Ctx, BlobRef) ->
    case hackney:head(blob_url(BlobRef), [] <<>>, Opts) of
        {ok, 200, _, Client} ->
            hackney:skip_body(Client),
            true;
        {ok, 404, _, CLient} ->
            hackney:skip_body(Client),
            false;
        Error ->
            Error
    end.


%%% CLIENT URIL

%% @doc Start the coffer_client application. Useful when testing using the shell.
start() ->
    hackney_deps:ensure(),
    application:load(coffer_application),
    coffer_util:ensure_deps_started(coffer_client),
    application:start(coffer_client).

%% @doc Stop the coffer_client application. Useful when testing using the shell.
stop() ->
    application:stop(coffer_client).


%% @private
%%
blob_url(Url, {HashType, Hash}) ->
    blob_url(Url, coffer_util:to_binary(HashType),
             coffer_util:to_binary(Hash));
blob_url(Url, BlobRef) when is_list(BlobRef) ->
    blob_url(Url, list_to_binary(BlobRef));
blob_url(#coffer_client{url=Url}, BlobRef) ->
    << Url/binary, "/", BlobRef/binary >>.

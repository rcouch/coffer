%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%

-module(coffer_blobserver).
-behaviour(gen_server).

%% blob api
%%
-export([enumerate/1,
         receive_blob/3,
         fetch_blob/2]).

%% public storage api
-export([all_storages/0,
         get_storage/1,
         update/2,
         attach/3,
         detach/1]).

%% public gen_server api
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("coffer_blobserver.hrl").
-define(SERVER, ?MODULE).


%% list all blobs on the storage
enumerate(#storage{backend=Backend, state=St}) ->
    Backend:enumerate(St).

%% receive a blob on the storage
receive_blob(#storage{backend=Backend, state=St}, BlobRef, Reader) ->
    Backend:receive_blob(BlobRef, Reader, St).

%% fetch a blob
fetch_blob(#storage{backend=Backend, state=St}, BlobRef) ->
    Backend:fetch_blob(BlobRef, St).


%%%
%%% STORAGE API
%%%

%% @doc return the sorted list of storages
all_storages() ->
    gen_server:call(?MODULE, list).

%% @doc return latest state of the storage
get_storage(Name) ->
    gen_server:call(?MODULE, {get, Name}).

%% @doc udpate a storage state with the latest state. could be useful
%% when a state need to advertise a new process or other things.
update(Name, NewState) ->
    gen_server:call(?MODULE, {update, Name, NewState}).

%% attach a new storage
attach(Name, Backend, Opts) ->
    gen_server:call(?MODULE, {attach, Name, Backend, Opts}).

%% detach a storage
detach(Name) ->
    gen_server:call(?MODULE, {detach, Name}).


%% gen_servers to maintain the storage listt

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_) ->
    Storages = init_storages(),
    {ok, Storages}.


handle_call({get, Name}, _From, Storages) ->
    Reply = case dict:find(Name, Storages) of
        {ok, Storage} ->
            {ok, Storage};
        error ->
            {error, not_found}
    end,
    {reply, Reply, Storages};
handle_call(list, _From, Storages) ->
    AllStorages = lists:usort(dict:fetch_keys(Storages)),
    {reply, AllStorages, Storages};
handle_call({attach, Name, Backend, Opts}, _From, Storages) ->
    case dict:is_key(Name, Storages) of
        true ->
            {reply, {error, already_exists}, Storages};
        false ->
            case new_storage(Name, Backend, Opts) of
                {ok, Storage} ->
                    {reply, {ok, Storage}, dict:store(Name, Storage,
                                                      Storages)};
                Error ->
                    {reply, Error, Storages}
            end
    end;
handle_call({detach, Name}, _From, Storages) ->
    case dict:find(Name, Storages) of
        {ok, #storage{backend=Backend, state=State}} ->
            ok = Backend:detach(State),
            {reply, ok, dict:erase(Name, Storages)};
        _ ->
            {reply, {error, not_found}, Storages}
    end.


handle_cast({update, Name, NewState}, Storages) ->
    case dict:find(Name, Storages) of
        {ok, Storage} ->
            NewStorage = Storage#storage{state=NewState},
            {noreply, dict:store(Name, NewStorage, Storages)};
        error ->
            {noreply, Storages}
    end.

handle_info(_Info, Storages) ->
    {noreply, Storages}.

terminate(_Reason, Storages) ->
    lists:foreach(fun(#storage{backend=Backend, state=St}) ->
                Backend:terminate(St)
        end, Storages),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


init_storages() ->
    lists:foldl(fun({Name, StorageUri, _IndexUri}, D) ->
                {Backend, Opts} = coffer_config:parse_uri(StorageUri,
                                                          storage),
                %% store the name as a binary
                Name1 = list_to_binary(Name),

                case new_storage(Name1, Backend, Opts) of
                    {ok, Storage} ->
                        lager:info("Initializing  storage: ~s~n", [Name]),
                        dict:store(Name1, Storage, D);
                    Error ->
                        lager:error("Error initializing  storage: ~p~n",
                                       [Error]),
                        D
                end
        end, dict:new(), coffer_config:get_config(storages, [])).

new_storage(Name, Backend, Opts) ->
    case Backend:init(Name, Opts) of
        {ok, StorageState} ->
            Storage = #storage{name=Name,
                               backend=Backend,
                               state=StorageState},
            {ok, Storage};
        Error ->
            Error
    end.

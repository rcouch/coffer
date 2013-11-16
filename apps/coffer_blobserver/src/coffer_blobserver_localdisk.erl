%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%
-module(coffer_blobserver_localdisk).

-export([init/2,
         enumerate/1,
         receive_blob/3,
         fetch_blob/2, fetch_blob/3,
         is_blob/2,
         delete_blob/2,
         stat/2,
         terminate/1]).


%% private
-export([enumerate_loop/2]).

-include_lib("kernel/include/file.hrl").

-record(ldst, {name,
               root}).

-define(DEFAULT_WINDOW, 8192).

init(Name, Opts) ->
    Path = proplists:get_value(path, Opts),
    ok = filelib:ensure_dir(filename:join(Path, "test")),
    %% delete folder
    ok = filelib:ensure_dir(filename:join([Path, ".delete", "test"])),
    {ok, #ldst{name=Name,
               root=Path}}.


receive_blob(BlobRef, Reader, #ldst{root=Root}) ->
    case coffer_blob:blob_path(Root, BlobRef) of
       error ->
           {error, invalid_blobref};
       BlobPath ->
            case filelib:is_file(BlobPath) of
                true ->
                    {ok, NewState} = skip_blob(Reader),
                    {error, already_exists, NewState};
                _ ->
                    write_blob(BlobRef, BlobPath, Reader)
            end
    end.

enumerate(#ldst{root=Root}) ->
    Self = self(),
    {EnumeratePid, MonRef} = spawn_monitor(?MODULE, enumerate_loop,
                                           [Self, Root]),
    ReaderFun = fun({Pid, MRef}=St) ->
            Pid ! {next, Self},
            receive
                {blob, {BlobRef, Size}, Pid} ->
                    {ok, {BlobRef, Size}, St};
                {done, Pid} ->
                    erlang:demonitor(MRef, [flush]),
                    done;
                {'DOWN', _, process, Pid, Reason} ->
                    erlang:demonitor(MRef, [flush]),
                    {error, {enumerate_worker_died, Reason}}
            end
    end,
    {ReaderFun, {EnumeratePid, MonRef}}.

fetch_blob(BlobRef, State) ->
    fetch_blob(BlobRef, ?DEFAULT_WINDOW, State).


fetch_blob(BlobRef, Window, #ldst{root=Root}) ->
    ReaderFun = fun(Fd) ->
            case file:read(Fd, Window) of
                {ok, Bin} ->
                    {ok, Bin, Fd};
                eof ->
                    file:close(Fd),
                    eob;
                Error ->
                    Error
            end
    end,

    case coffer_blob:blob_path(Root, BlobRef) of
        error ->
            {error, invalid_blobref};
        BlobPath ->
            case filelib:is_file(BlobPath) of
                true ->
                    case file:open(BlobPath, [read]) of
                        {ok, Fd} ->
                            {ReaderFun, Fd};
                        Error ->
                            Error
                    end;
                false ->
                    {error, not_found}
            end
    end.

is_blob(BlobRef, #ldst{root=Root}) ->
    case coffer_blob:blob_path(Root, BlobRef) of
        error ->
            {error, invalid_blobref};
        BlobPath ->
            filelib:is_file(BlobPath)
    end.

%% @todo add a watcher to garbage collect from time to time deleted
%% files that may leak on the fs for any reason.
delete_blob(BlobRef, #ldst{root=Root}) ->
    case coffer_blob:blob_path(Root, BlobRef) of
        error ->
            {error, invalid_blobref};
        BlobPath ->
            case filelib:is_file(BlobPath) of
                true ->
                    %% if the file exist we first delete it,
                    %% deletion will be handled asynchronously to not
                    %% wait more than needed.
                    DelFile = filename:join([Root,".delete",
                                             binary_to_list(uuid:get_v4())]),
                    case file:rename(BlobPath, DelFile) of
                        ok ->
                            spawn(file, delete, [DelFile]),
                            ok;
                        Error ->
                            Error
                    end;
                false ->
                    %% file have already been deleted ignore it
                    ok
            end
    end.

stat([], _St) ->
    {ok, [], [], []};
stat(BlobRefs0, #ldst{root=Root}) ->
    %% before stating anything check the blob refs
    BlobRefs = lists:foldl(fun(BlobRef, Acc) ->
                    case coffer_blob:validate_blobref(BlobRef) of
                        ok ->
                            Acc ++ [BlobRef];
                        error ->
                            Acc
                    end
            end, [], BlobRefs0),

    %% find missing
    {Found, Missing} = lists:foldl(fun(BlobRef, {F, M}) ->
                    BlobPath = coffer_blob:blob_path(Root, BlobRef),
                    case filelib:is_file(BlobPath) of
                        true ->
                            {[{BlobRef, file_size(BlobPath)} | F], M};
                        _ ->
                            {F, [BlobRef | M]}
                    end
            end, {[], []}, BlobRefs),

    {Partials, Missing1} = case Missing of
        [] ->
            {[], Missing};
        _ ->
            ToFind = [{BlobRef, temp_blob(BlobRef)} || BlobRef <- Missing],
            lists:foldl(fun({BlobRef, TmpBlobPath}, {P, M}) ->
                        case filelib:is_file(TmpBlobPath) of
                            true ->
                                {[{BlobRef, file_size(TmpBlobPath)} | P], M};
                            _ ->
                                {P, [BlobRef | M]}
                        end
                end, {[], []}, ToFind)
    end,
    {ok, {lists:reverse(Found), lists:reverse(Missing1),
          lists:reverse(Partials)}}.

terminate(_St) ->
    ok.

%% @private
%%

enumerate_loop(To, Path) ->
    MonRef = erlang:monitor(process, To),
    walk(Path, Path, To),
    To ! {done, self()},
    erlang:demonitor(MonRef, [flush]).

walk(Root, Path, To) ->
    case filelib:is_dir(Path) of
        true ->
            Children = filelib:wildcard(Path ++ "/*"),
            lists:foreach(fun(P) ->
                        walk(Root, P, To)
                end,  Children);
        _ ->
            case filelib:is_file(Path) of
                true ->
                    Size = file_size(Path),
                    BlobRef = coffer_blob:from_path(Root, Path),
                    To ! {blob, {BlobRef, Size}, self()},
                    receive
                        {next, To} ->
                            ok;
                        {'DOWN', _, process, To, _} ->
                            exit(normal)
                    end;
                _ ->
                    ok
            end
    end.
                            %%

write_blob(BlobRef, BlobPath, {ReaderFun, ReaderState}) ->
    TmpBlobPath = temp_blob(BlobRef),
    case file:open(TmpBlobPath, [write, append]) of
        {ok, Fd} ->
            case write_blob1(ReaderFun, ReaderState, Fd) of
                {ok, NewReaderState} ->
                    ok = filelib:ensure_dir(BlobPath),
                    case file:rename(TmpBlobPath, BlobPath) of
                        ok ->
                            Size = file_size(BlobPath),
                            {ok, Size, NewReaderState};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            {Error, ReaderState}
    end.

write_blob1(ReaderFun, ReaderState, Fd) ->
    case ReaderFun(ReaderState) of
        {ok, eob, NewReaderState} ->
            file:close(Fd),
            {ok, NewReaderState};
        {ok, Bin, NewReaderState} ->
            ok = file:write(Fd, Bin),
            file:sync(Fd), %% we sync on each write
            write_blob1(ReaderFun, NewReaderState, Fd);
        Error ->
            file:close(Fd),
            Error
    end.

skip_blob({ReaderFun, ReaderState}) ->
    case ReaderFun(ReaderState) of
        {ok, eob, NewReaderState} ->
            {ok, NewReaderState};
        {ok, _Bin, NewReaderState} ->
            skip_blob({ReaderFun, NewReaderState});
        Error ->
            Error
    end.

temp_blob(BlobRef) ->
    TempName = iolist_to_binary([<<"coffer-">>, BlobRef, <<".tmp">>]),
    filename:join([coffer_util:gettempdir(), TempName]).

file_size(Path) ->
    {ok, FileInfo} = file:read_file_info(Path),
    #file_info{size=Size} = FileInfo,
    Size.

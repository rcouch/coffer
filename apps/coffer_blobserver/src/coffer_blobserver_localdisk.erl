%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%
-module(coffer_blobserver_localdisk).

-export([init/2,
         enumerate/1,
         receive_blob/3,
         fetch_blob/3,
         terminate/1]).


%% private
-export([enumerate_loop/2]).

-include_lib("kernel/include/file.hrl").

-record(ldst, {name,
               root}).

init(Name, Opts) ->
    Path = proplists:get_value(path, Opts),
    ok = filelib:ensure_dir(filename:join(Path, "test")),
    {ok, #ldst{name=Name,
               root=Path}}.


receive_blob(BlobRef, Reader, #ldst{root=Root}) ->
    case coffer_blob:blob_path(Root, BlobRef) of
       error ->
           {error, invalid_blobref};
       BlobPath ->
            case filelib:is_file(BlobPath) of
                true ->
                    {error, already_exists};
                _ ->
                    write_blob(BlobRef, BlobPath, Reader)
            end
    end.

enumerate(#ldst{root=Root}) ->
    Self = self(),
    EnumeratePid = spawn(?MODULE, enumerate_loop, [Self, Root]),
    ReaderFun = fun(Pid) ->
            Pid ! {next, Self},
            receive
                {blob, {BlobRef, Size}, Pid} ->
                    {ok, {BlobRef, Size}, Pid};
                {done, Pid} ->
                    done
            end
    end,
    {ReaderFun, EnumeratePid}.

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
            {ok, NewReaderState} = write_blob1(ReaderFun, ReaderState,
                                               Fd),
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
    end.

write_blob1(ReaderFun, ReaderState, Fd) ->
    case ReaderFun(ReaderState) of
        {ok, eob, NewReaderState} ->
            file:close(Fd),
            {ok, NewReaderState};
        {ok, Bin, NewReaderState} ->
            file:write(Fd, Bin),
            write_blob1(ReaderFun, NewReaderState, Fd)
    end.


temp_blob(BlobRef) ->
    TempName = iolist_to_binary([<<"coffer-">>, BlobRef, <<".tmp">>]),
    filename:join([coffer_util:gettempdir(), TempName]).

file_size(Path) ->
    {ok, FileInfo} = file:read_file_info(Path),
    #file_info{size=Size} = FileInfo,
    Size.

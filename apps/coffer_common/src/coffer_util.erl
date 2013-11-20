%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%
-module(coffer_util).

-export([ensure_deps_started/1]).
-export([require/1]).
-export([propmerge/2, propmerge/3]).
-export([gettempdir/0]).
-export([to_binary/1]).

ensure_deps_started(App) ->
    {ok, Deps} = application:get_key(App, applications),
    true = lists:all(fun ensure_started/1, Deps).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            true;
        {error, {already_started, App}} ->
            true;
        Else ->
            error_logger:error_msg("Couldn't start ~p: ~p", [App, Else]),
            Else
    end.

%% @doc Start the given applications if they were not already started.
-spec require(list(module())) -> ok.
require([]) ->
	ok;
require([App|Rest]) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end,
	require(Rest).

%% @doc merge 2 proplists. All the Key - Value pairs from both proplists
%% are included in the new proplists. If a key occurs in both dictionaries
%% then Fun is called with the key and both values to return a new
%% value. This a wreapper around dict:merge
propmerge(F, L1, L2) ->
        dict:to_list(dict:merge(F, dict:from_list(L1), dict:from_list(L2))).

%% @doc Update a proplist with values of the second. In case the same
%% key is in 2 proplists, the value from the first are kept.
propmerge(L1, L2) ->
    propmerge(fun(_, V1, _) -> V1 end, L1, L2).


%% @spec gettempdir() -> string()
%% @doc Get a usable temporary directory using the first of these that is a directory:
%%      $TMPDIR, $TMP, $TEMP, "/tmp", "/var/tmp", "/usr/tmp", ".".
gettempdir() ->
    gettempdir(gettempdir_checks(), fun normalize_dir/1).

gettempdir_checks() ->
    [{fun os:getenv/1, ["TMPDIR", "TMP", "TEMP"]},
     {fun gettempdir_identity/1, ["/tmp", "/var/tmp", "/usr/tmp"]},
     {fun gettempdir_cwd/1, [cwd]}].

gettempdir_identity(L) ->
    L.

gettempdir_cwd(cwd) ->
    {ok, L} = file:get_cwd(),
    L.

gettempdir([{_F, []} | RestF], Normalize) ->
    gettempdir(RestF, Normalize);
gettempdir([{F, [L | RestL]} | RestF], Normalize) ->
    case Normalize(F(L)) of
        false ->
            gettempdir([{F, RestL} | RestF], Normalize);
        Dir ->
            Dir
    end.

normalize_dir(False) when False =:= false orelse False =:= "" ->
    %% Erlang doesn't have an unsetenv, wtf.
    false;
normalize_dir(L) ->
    Dir = filename:absname(L),
    case filelib:is_dir(Dir) of
        false ->
            false;
        true ->
            Dir
    end.


to_binary(V) when is_list(V) ->
    list_to_binary(V);
to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
to_binary(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V)).

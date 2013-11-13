%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%
-module(coffer_blob).

-export([blob_regexp/0]).
-export([parse_blobref/1]).
-export([validate_blobref/1]).
-export([blob_path/2]).
-export([from_path/2]).


blob_path(Root, BlobRef) ->
    case coffer_blob:parse_blobref(BlobRef) of
        error ->
            {error, invalid_blobref};
        {HashType, Hash} ->
            %% we store the hash in /<root>/<type>/<a>/<b>/<c>/<rest>
            << A:1/binary, B:1/binary, C:1/binary, FName/binary >> = Hash,
            {ok, filename:join([Root, HashType, A, B, C, FName])}
    end.

from_path(Root, Path) ->
    [_, RelPath] = re:split(Path, Root, [{return, list}]),
    [HashType|Rest] = string:tokens(RelPath, "/"),
    iolist_to_binary([HashType, "-", [C || C <- Rest]]).


parse_blobref(BlobRef) ->
    Re = blob_regexp(),
    case re:run(BlobRef, Re, [{capture, all, binary}]) of
        nomatch ->
            error;
        {match, [_, HashType, Hash]} when length(Hash) >= 4 ->
            {HashType, Hash};
        _ ->
            error
    end.

validate_blobref(BlobRef) ->
    Re = blob_regexp(),
    case re:run(BlobRef, Re, [{capture, none}]) of
        nomatch ->
            error;
        {match, [_, _HashType, Hash]} when length(Hash) >= 4 ->
            ok;
        _ ->
            error
    end.

blob_regexp() ->
    %% we cache the regexp so it can be reused in the same process
    case get(blob_regexp) of
        undefined ->
            {ok, RegExp} = re:compile("^([a-z][a-zA-Z0-9^-]*)-([a-zA-Z0-9]*)$"),
            put(blob_regexp, RegExp),
            RegExp;
        RegExp ->
            RegExp
    end.

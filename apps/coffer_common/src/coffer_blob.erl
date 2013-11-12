%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%
-module(coffer_blob).

-export([blob_regexp/0]).
-export([parse_blobref/1]).
-export([validate_blobref/1]).

parse_blobref(BlobRef) ->
    Re = blob_regexp(),
    case re:run(BlobRef, Re, [{capture, all, binary}]) of
        nomatch ->
            error;
        Matches ->
            {match, [_, HashType, Hash]} = Matches,
            {HashType, Hash}
    end.

validate_blobref(BlobRef) ->
    Re = blob_regexp(),
    case re:run(BlobRef, Re, [{capture, none}]) of
        nomatch ->
            error;
        _ ->
            ok
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

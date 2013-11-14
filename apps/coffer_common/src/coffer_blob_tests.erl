%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%

-module(coffer_blob_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

validate_test() ->
    ?assertMatch(ok, coffer_blob:validate_blobref(<<"hashtype-hash">>)),
    ?assertMatch(error, coffer_blob:validate_blobref(<<"hashtype-bad">>)),
    ?assertMatch(error, coffer_blob:validate_blobref(<<"bad">>)),
    ?assertMatch(error, coffer_blob:validate_blobref(<<"-bad">>)).

parse_test() ->
    ?assertMatch({<<"hashtype">>, <<"hash">>},
                 coffer_blob:parse_blobref(<<"hashtype-hash">>)),
    ?assertMatch(error, coffer_blob:parse_blobref(<<"hashtype-bad">>)),
    ?assertMatch(error, coffer_blob:parse_blobref(<<"bad">>)),
    ?assertMatch(error, coffer_blob:parse_blobref(<<"-bad">>)).

blob_path_test() ->
    ?assertMatch(<<"/hash/a/b/c/d">>,
                 coffer_blob:blob_path("/", "hash-abcd")),
    ?assertMatch(<<"/root/hash/a/b/c/d">>,
                 coffer_blob:blob_path("/root", "hash-abcd")),
    ?assertMatch(error, coffer_blob:blob_path("/", "hash-abc")).


from_path_test() ->
    ?assertMatch(<<"hash-abcd">>,
                 coffer_blob:from_path("/", "/hash/a/b/c/d")),
    ?assertMatch(<<"hash-abcd">>,
                 coffer_blob:from_path("/root", "/root/hash/a/b/c/d")).


-endif.

-module(coffer_blob_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

validate_test() ->
    ?assertMatch(ok, coffer_blob:validate_blobref(<<"hashtype-hash">>)),
    ?assertMatch(error, coffer_blob:validate_blobref(<<"bad">>)),
    ?assertMatch(error, coffer_blob:validate_blobref(<<"-bad">>)).

parse_test() ->
    ?assertMatch({<<"hashtype">>, <<"hash">>},
                 coffer_blob:parse_blobref(<<"hashtype-hash">>)),
    ?assertMatch(error, coffer_blob:parse_blobref(<<"bad">>)),
    ?assertMatch(error, coffer_blob:parse_blobref(<<"-bad">>)).

-endif.

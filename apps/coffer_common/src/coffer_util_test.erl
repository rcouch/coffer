%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%

-module(coffer_util_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


propmerge_test() ->
    ?assertMatch([{a, 1}, {b, 2}], coffer_util:propmerge([{a, 1}], [{b,2}])),
    ?assertMatch([{a, 1}, {b, 3}], coffer_util:propmerge([{a, 1}, {b, 2}],
                                                         [{b, 3}])).

-endif.

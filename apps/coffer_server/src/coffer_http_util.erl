%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.

-module(coffer_http_util).

-export([not_allowed/2,
         not_found/1,
         error/2, error/3, error/4,
         ok/1, ok/2]).
-export([to_json/2]).
-export([maybe_prettify_json/2]).

not_allowed(AllowedMethods, Req) ->
    AddCommaFunc = fun(Element, Acc) ->
        case Acc of
            [<<>>] ->
                [Element|Acc];
            _ ->
                [Element,<<",">>|Acc]
        end
	end,
    ReversedAllowedMethodsWithComaList = lists:foldl(
        AddCommaFunc,
        [<<"">>],
        AllowedMethods
    ),
    AllowedMethodsWithComaList = lists:reverse(
            ReversedAllowedMethodsWithComaList
    ),
    AllowedMethodsWithComa = iolist_to_binary(AllowedMethodsWithComaList),
    Extra = [{<<"Allow">>, AllowedMethodsWithComa}],
    error(405, [{<<"error">>, <<"not_allowed">>}], Extra, Req).

not_found(Req) ->
    error(404, [{<<"error">>, <<"not found">>}], Req).

error(Reason, Req) ->
    error(500, Reason, [], Req).


error(Status, Reason, Req) ->
    error(Status, Reason, [], Req).

error(Status, Reason, Extra, Req) ->
    ReasonInBinary = iolist_to_binary(io_lib:format("~p", [Reason])),
    ReturnedData = [{<<"error">>, ReasonInBinary}] ++ Extra,
    {Json, Req1} = to_json(ReturnedData, Req),
    cf_cowboy_req:reply(Status, [{<<"Content-Type">>, <<"application/json">>}],
                     Json, Req1).

ok(Req) ->
    ok(200, Req).

ok(Status, Req) ->
    Json = cf_jsx:encode([{<<"ok">>, true}]),
    cf_cowboy_req:reply(Status, [{<<"Content-Type">>, <<"application/json">>}],
                     Json, Req).

to_json(Json, Req) ->
    maybe_prettify_json(cf_jsx:encode(Json), Req).

maybe_prettify_json(Json, Req) ->
    case cf_cowboy_req:qs_val(<<"pretty">>, Req) of
        {undefined, Req1} ->
            {Json, Req1};
        {<<"true">>, Req1} ->
           {cf_jsx:prettify(Json), Req1}
    end.

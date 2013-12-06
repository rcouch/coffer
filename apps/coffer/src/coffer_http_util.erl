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
    error(405, <<"not_allowed">>, Extra, Req).

not_found(Req) ->
    error(404, <<"not found">>, Req).

error(Reason, Req) ->
    error(500, Reason, [], Req).


error(Status, Reason, Req) ->
    error(Status, Reason, [], Req).

error(Status, Reason, Extra, Req) ->
    ReasonInBinary = case Reason of
        {error, Error} ->
            Error;
        _ when is_atom(Reason) ->
            list_to_binary(atom_to_list(Reason));
        _ when is_binary(Reason) ->
            Reason;
        _ ->
            iolist_to_binary(io_lib:format("~p", [Reason]))
    end,
    ReturnedData = [{<<"error">>, ReasonInBinary}] ++ Extra,
    {Json, Req1} = to_json(ReturnedData, Req),
    cowboy_req:reply(Status, [{<<"Content-Type">>, <<"application/json">>}],
                     Json, Req1).

ok(Req) ->
    ok(200, Req).

ok(Status, Req) ->
    Json = jsx:encode([{<<"ok">>, true}]),
    cowboy_req:reply(Status, [{<<"Content-Type">>, <<"application/json">>}],
                     Json, Req).

to_json(Json, Req) ->
    maybe_prettify_json(jsx:encode(Json), Req).

maybe_prettify_json(Json, Req) ->
    case cowboy_req:qs_val(<<"pretty">>, Req) of
        {undefined, Req1} ->
            {Json, Req1};
        {<<"true">>, Req1} ->
           {jsx:prettify(Json), Req1}
    end.

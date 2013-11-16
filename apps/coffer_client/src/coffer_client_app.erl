%%% -*- erlang -*-
%%%
%%% This file is part of coffer released under the Apache license 2.
%%% See the NOTICE for more information.
%%%
-module(coffer_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    coffer_deps:ensure(),
    coffer_util:ensure_deps_started(coffer),
    coffer_client_sup:start_link().

stop(_State) ->
    ok.

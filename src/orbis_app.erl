%%%
%%% Copyright (c) 2015 Alexander Færøy
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc The Orbis Application
%%%
%%% This is the entry point for the Orbis application, which takes care of
%%% starting the primary supervisor.
%%%
%%% @end
%%% ----------------------------------------------------------------------------
-module(orbis_app).
-behaviour(application).

%% API.
-export([start/0,
         start/2,
         stop/1]).

%% @private
-spec start() -> {ok, [atom()]} | {error, term()}.
start() ->
    application:ensure_all_started(orbis).

%% @private
-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
    orbis_sup:start_link().

%% @private
-spec stop([]) -> ok.
stop(_State) ->
    ok.

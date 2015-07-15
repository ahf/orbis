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
%%% @doc Orbis Worker Behaviour and Utilities
%%% @end
%%% ----------------------------------------------------------------------------
-module(orbis_worker).

%% API.
-export([start_link/4]).

-callback start_link([term()]) -> {ok, pid()} | ignore | {error, term()}.

-spec start_link(Module, Name, Partition, Arguments) -> {ok, pid()} | ignore | {error, term()}
    when
        Module    :: module(),
        Name      :: orbis:pool(),
        Partition :: orbis_chash_bucket:partition(),
        Arguments :: [term()].
start_link(Module, Name, Partition, Arguments) ->
    case Module:start_link([Name, Partition | Arguments]) of
        {ok, Worker} when is_pid(Worker) ->
            true = orbis_worker_manager:monitor_pool_worker(Name, Partition, Worker),
            {ok, Worker};
        Other ->
            Other
    end.

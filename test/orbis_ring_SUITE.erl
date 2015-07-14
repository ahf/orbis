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
%%% @doc Orbis Ring CT Suite
%%% @end
%%% ----------------------------------------------------------------------------
-module(orbis_ring_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, basic_group}].

groups() ->
    [{basic_group, [shuffle], [new, power_of_two, size]}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(orbis),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(orbis),
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

new(_Config) ->
    Ring1A = orbis_ring:new(1),
    Ring1B = orbis_ring:new(1),

    Ring1A =:= Ring1B,

    Ring2A = orbis_ring:new(2),
    Ring2B = orbis_ring:new(2),

    Ring2A =:= Ring2B,

    Ring1A =/= Ring2A.

power_of_two(_Config) ->
    threw = throws(fun () -> orbis_ring:new(0) end, error, badarg),
    threw = throws(fun () -> orbis_ring:new(3) end, error, badarg),
    threw = throws(fun () -> orbis_ring:new(5) end, error, badarg),
    threw = throws(fun () -> orbis_ring:new(127) end, error, badarg),

    ok = throws(fun () -> orbis_ring:new(1) end, error, badarg),
    ok = throws(fun () -> orbis_ring:new(2) end, error, badarg),
    ok = throws(fun () -> orbis_ring:new(4) end, error, badarg),
    ok = throws(fun () -> orbis_ring:new(8) end, error, badarg),
    ok = throws(fun () -> orbis_ring:new(16) end, error, badarg),
    ok = throws(fun () -> orbis_ring:new(32) end, error, badarg),
    ok = throws(fun () -> orbis_ring:new(64) end, error, badarg),
    ok = throws(fun () -> orbis_ring:new(128) end, error, badarg),
    ok = throws(fun () -> orbis_ring:new(256) end, error, badarg),
    ok = throws(fun () -> orbis_ring:new(512) end, error, badarg),
    ok = throws(fun () -> orbis_ring:new(1024) end, error, badarg).

size(_Config) ->
    Ring1 = orbis_ring:new(1),
    1 = orbis_ring:size(Ring1),
    1 = length(orbis_ring:partitions(Ring1)),

    Ring2 = orbis_ring:new(2),
    2 = orbis_ring:size(Ring2),
    2 = length(orbis_ring:partitions(Ring2)),

    Ring4 = orbis_ring:new(4),
    4 = orbis_ring:size(Ring4),
    4 = length(orbis_ring:partitions(Ring4)),

    Ring8 = orbis_ring:new(8),
    8 = orbis_ring:size(Ring8),
    8 = length(orbis_ring:partitions(Ring8)).

throws(Fun, Type, Exception) ->
    try Fun() of
        _ ->
            ok
    catch
        Type:Exception ->
            threw
    end.

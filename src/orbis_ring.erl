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
%%% @doc Orbis Ring API
%%% @end
%%% ----------------------------------------------------------------------------
-module(orbis_ring).

%% API.
-export([new/1,
         partitions/1,
         size/1,
         find_partition/2
        ]).

%% 2^(256 - 1).
-define(RING_MAX_PARTITION, 115792089237316195423570985008687907853269984665640564039457584007913129639936).

%% @doc Create new Ring.
%% @end
-spec new(Size :: integer()) -> orbis:ring().
new(Size) ->
    case is_power_of_two(Size) of
        true ->
            Partitions = lists:seq(0, ?RING_MAX_PARTITION - 1, increment(Size)),
            #{ size => Size, partitions => Partitions };

        false ->
            erlang:error(badarg)
    end.

%% @doc Get partitions of a given ring.
%% @end
-spec partitions(Ring :: orbis:ring()) -> [orbis:partition()].
partitions(#{ partitions := Partitions }) ->
    Partitions.

%% @doc Get size of a given ring.
%% @end
-spec size(Ring :: orbis:ring()) -> pos_integer().
size(#{ size := Size }) ->
    Size.

%% @doc Find a partition on the ring for the given value.
%% @end
-spec find_partition(Index :: orbis:index(), Ring :: orbis:ring()) -> orbis:partition().
find_partition(Index, Ring) ->
    Size = orbis_ring:size(Ring),
    Increment = increment(Size),
    (((Index div Increment) + 1) rem Size) * Increment.

%% @private
-spec increment(Size :: integer()) -> integer().
increment(Size) ->
    ?RING_MAX_PARTITION div Size.

%% @private
-spec is_power_of_two(Integer :: integer()) -> boolean().
is_power_of_two(0) ->
    false;
is_power_of_two(Integer) ->
    Integer band (Integer - 1) =:= 0.

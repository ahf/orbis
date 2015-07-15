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
%%% @doc Orbis Consistent Hashing Bucket API
%%% @end
%%% ----------------------------------------------------------------------------
-module(orbis_chash_bucket).

%% API.
-export([new/1,
         size/1,
         partitions/1,
         find_partition/2]).

-type bucket()    :: orbis_chash:bucket().
-type partition() :: orbis_chash:partition().

%% 2^(256 - 1).
-define(PARTITION_MAX, 115792089237316195423570985008687907853269984665640564039457584007913129639936).

%% @doc Create a new Bucket.
%%
%% This function creates a new Bucket of a given Size. The Size must be a power
%% of two. For example: 1, 2, 4, 8, 16, 32, ...
%%
%% @end
-spec new(Size :: integer()) -> bucket().
new(Size) ->
    case is_power_of_two(Size) of
        true ->
            Partitions = lists:seq(0, ?PARTITION_MAX - 1, ?PARTITION_MAX div Size),
            #{ size => Size, partitions => Partitions };
        false ->
            erlang:error(badarg)
    end.

%% @doc Get the size of a Bucket.
%%
%% This is a utility function that will give you the size of the given Bucket.
%%
%% @end
-spec size(Bucket :: bucket()) -> pos_integer().
size(#{ size := Size }) ->
    Size.

%% @doc Get the member partitions of a Bucket.
%%
%% This is a utility function that will give you the member partitions of the
%% given Bucket.
%%
%% @end
-spec partitions(Bucket :: bucket()) -> [partition()].
partitions(#{ partitions := Partitions }) ->
    Partitions.

%% @doc Find the suitable partition within a bucket for the given integer.
%% @end
-spec find_partition(N :: integer(), Bucket :: bucket()) -> partition().
find_partition(N, #{ size := Size }) ->
    Increment = ?PARTITION_MAX div Size,
    (((N div Increment) + 1) rem Size) * Increment.

%% @private
-spec is_power_of_two(N :: integer()) -> boolean().
is_power_of_two(0) ->
    false;
is_power_of_two(N) ->
    N band (N - 1) =:= 0.

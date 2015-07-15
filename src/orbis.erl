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
%%% @doc Orbis API
%%% @end
%%% ----------------------------------------------------------------------------
-module(orbis).

%% API.
-export([dispatch/3,
         child_spec/3,
         child_spec/4]).

%% Types.
-export_type([pool/0]).

-type pool() :: term().

-spec dispatch(Name, Key, Fun) -> term()
    when
        Name :: pool(),
        Key  :: term(),
        Fun  :: fun((Worker :: pid()) -> term()).
dispatch(Name, Key, Fun) ->
    case orbis_worker_manager:lookup_pool_bucket(Name) of
        {ok, Bucket} ->
            Hash = orbis_chash:hash(Key),
            Partition = orbis_chash_bucket:find_partition(Hash, Bucket),
            case orbis_worker_manager:lookup_pool_worker(Name, Partition) of
                {ok, Worker} when is_pid(Worker) ->
                    Fun(Worker);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec child_spec(Name, Size, Module) -> supervisor:child_spec() | {error, term()}
    when
        Name   :: pool(),
        Size   :: pos_integer(),
        Module :: module().
child_spec(Name, Size, Module) ->
    child_spec(Name, Size, Module, []).

-spec child_spec(Name, Size, Module, Arguments) -> supervisor:child_spec() | {error, term()}
    when
        Name      :: pool(),
        Size      :: pos_integer(),
        Module    :: module(),
        Arguments :: [term()].
child_spec(Name, Size, Module, Arguments) ->
    case orbis_worker_manager:new_pool(Name, Size) of
        true ->
            {ok, Partitions} = orbis_worker_manager:lookup_pool_partitions(Name),
            lists:map(fun (Partition) ->
                          {{Name, Partition}, {orbis_worker, start_link, [Module, Name, Partition, Arguments]}, permanent, 5000, worker, [orbis_worker]}
                      end, Partitions);
        false ->
            {error, pool_already_exists}
    end.

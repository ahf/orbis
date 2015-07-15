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
%%% @doc Orbis Worker Manager
%%% @end
%%% ----------------------------------------------------------------------------
-module(orbis_worker_manager).
-behaviour(gen_server).

%% API.
-export([start_link/0,
         new_pool/2,
         delete_pool/1,
         lookup_pool_bucket/1,
         lookup_pool_size/1,
         lookup_pool_partitions/1,
         lookup_pool_worker/2,
         monitor_pool_worker/3
         ]).

%% Generic Server Callbacks.
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

-define(SERVER, ?MODULE).
-define(TABLE, orbis_ring_metadata).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec new_pool(Name, Size) -> boolean()
    when
      Name :: orbis:pool(),
      Size :: pos_integer().
new_pool(Name, Size) ->
    Bucket = orbis_chash_bucket:new(Size),
    gen_server:call(?SERVER, {new_pool, Name, Bucket}).

-spec delete_pool(Name) -> true
    when
        Name :: orbis:pool().
delete_pool(Name) ->
    gen_server:call(?SERVER, {delete_pool, Name}).

-spec lookup_pool_bucket(Name) -> {ok, Bucket} | {error, term()}
    when
        Name   :: orbis:pool(),
        Bucket :: orbis_chash:bucket().
lookup_pool_bucket(Name) ->
    case ets:lookup(?TABLE, {pool, Name}) of
        [{_, Bucket}] ->
            {ok, Bucket};
        [] ->
            {error, not_found}
    end.

-spec lookup_pool_size(Name) -> {ok, Size} | {error, term()}
    when
        Name :: orbis:pool(),
        Size :: pos_integer().
lookup_pool_size(Name) ->
    case lookup_pool_bucket(Name) of
        {ok, Bucket} ->
            {ok, orbis_chash_bucket:size(Bucket)};
        {error, _} = Error ->
            Error
    end.

-spec lookup_pool_partitions(Name) -> {ok, Partitions} | {error, term()}
    when
        Name       :: orbis:pool(),
        Partitions :: [orbis_chash_bucket:partition()].
lookup_pool_partitions(Name) ->
    case lookup_pool_bucket(Name) of
        {ok, Bucket} ->
            {ok, orbis_chash_bucket:partitions(Bucket)};
        {error, _} = Error ->
            Error
    end.

-spec lookup_pool_worker(Name, Partition) -> {ok, pid()} | {error, term()}
    when
        Name      :: orbis:pool(),
        Partition :: orbis_chash_bucket:partition().
lookup_pool_worker(Name, Partition) ->
    case ets:lookup(?TABLE, {pool_worker, Name, Partition}) of
        [{_, Pid}] when is_pid(Pid) ->
            {ok, Pid};
        [] ->
            {error, worker_not_found}
    end.

-spec monitor_pool_worker(Name, Partition, Worker) -> boolean()
    when
        Name      :: orbis:pool(),
        Partition :: orbis_chash_bucket:partition(),
        Worker    :: pid().
monitor_pool_worker(Name, Partition, Pid) ->
    gen_server:call(?SERVER, {monitor_pool_worker, Name, Partition, Pid}).

%% @private
init([]) ->
    ets:new(?TABLE, [protected,
                     named_table,
                     ordered_set,
                     {read_concurrency, true}]),
    {ok, #state {}}.

%% @private
handle_call({new_pool, Name, Bucket}, _From, State) ->
    Created = ets:insert_new(?TABLE, {{pool, Name}, Bucket}),
    {reply, Created, State};

handle_call({delete_pool, Name}, _From, State) ->
    Deleted = ets:delete(?TABLE, {pool, Name}),
    {reply, Deleted, State};

handle_call({monitor_pool_worker, Name, Partition, Worker}, _From, State) ->
    erlang:monitor(process, Worker),
    Created = ets:insert_new(?TABLE, {{pool_worker, Name, Partition}, Worker}),
    {reply, Created, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(Info, State) ->
    io:format("Info: ~p~n", [Info]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

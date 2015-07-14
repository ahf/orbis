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
%%% @doc Orbis Ring Manager
%%% @end
%%% ----------------------------------------------------------------------------
-module(orbis_ring_manager).
-behaviour(gen_server).

%% API.
-export([start_link/0,
         create/2,
         lookup/1,
         delete/1
        ]).

%% Generic Server Callbacks.
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {}).

-define(SERVER, ?MODULE).
-define(TABLE, orbis_ring_metadata).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec create(Name, Size) -> boolean()
    when
      Name :: orbis:ring_name(),
      Size :: pos_integer().
create(Name, Size) ->
    Ring = orbis_ring:new(Size),
    gen_server:call(?SERVER, {create_named_ring, Name, Ring}).

-spec lookup(Name) -> Ring | not_found
    when
        Name :: orbis:ring_name(),
        Ring :: orbis:ring().
lookup(Name) ->
    case ets:lookup(?TABLE, {ring, Name}) of
        [{_, Ring}] ->
            Ring;
        [] ->
            not_found
    end.

-spec delete(Name) -> true
    when
        Name :: orbis:ring_name().
delete(Name) ->
    gen_server:call(?SERVER, {delete, Name}).

%% @private
init([]) ->
    ets:new(?TABLE, [protected,
                     named_table,
                     ordered_set,
                     {read_concurrency, true}]),
    {ok, #state {}}.

%% @private
handle_call({create_named_ring, Name, Ring}, _From, State) ->
    Created = ets:insert_new(?TABLE, {{ring, Name}, Ring}),
    {reply, Created, State};

handle_call({delete, Name}, _From, State) ->
    Deleted = ets:delete(?TABLE, {ring, Name}),
    {reply, Deleted, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

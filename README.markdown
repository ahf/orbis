Orbis
=====

[![Build Status](https://travis-ci.org/ahf/orbis.svg?branch=develop)](https://travis-ci.org/ahf/orbis)

Orbis is an Erlang library for writing applications that uses consistent hashing
to distribute requests amongst a set of workers in an application. The library
provides no network distribution, but is a lot simpler to use and getting
started with than larger and more complex libraries like [Riak Core](https://github.com/basho/riak_core/).

The API of Orbis is greatly inspired by the fantastic [Poolboy Erlang
library](https://github.com/devinus/poolboy).

Usage
-----

1. Create a named pool using `orbis:child_spec/3` (or `orbis:child_spec/4`), which
   allows you to specify a name of the worker pool, size of the worker pool, which
   must be a number of power of two (1, 2, 4, 8, ...), a worker module and an
   optional set of arguments passed to the worker's `start_link` function. Pass
   the result of the `orbis:child_spec` function to your supervisor.

2. Implement your worker module with the `orbis_worker` behaviour.

3. Use `orbis:dispatch/3` to distribute work amongst your set of workers.

Example Application
-------------------

Here's the interesting files of an example application using Orbis to distribute
requests amongst a set of `gen_server` workers. You can see and download the
full example application [here](https://github.com/ahf/orbis_example/).

### The Orbis Example Application API.

This module implements the API which distributes the events amongst our workers.

```erlang
-module(orbis_example).

%% API.
-export([ping/1, crash/1]).

ping(Key) ->
    dispatch(Key, fun (Worker) ->
                      orbis_example_worker:ping(Worker)
                  end).

crash(Key) ->
    dispatch(Key, fun (Worker) ->
                      orbis_example_worker:crash(Worker)
                  end).

dispatch(Key, Fun) ->
    orbis:dispatch(orbis_example_pool, Key, Fun).
```

### The Orbis Worker

This module implements the `gen_server` which will handle our requests. It
exposes two API functions: `ping/1`, which will return a `pong` together with
some information about the server which handled the given request, and a
`crash/1` which will simply crash the server handling the request.

```erlang
-module(orbis_example_worker).
-behaviour(orbis_worker).
-behaviour(gen_server).

%% API.
-export([ping/1,
         crash/1]).

%% Orbis Worker.
-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    name      :: orbis:pool(),
    partition :: orbis_chash_bucket:partition()
}).

-define(SERVER, ?MODULE).

ping(Worker) ->
    gen_server:call(Worker, ping).

crash(Worker) ->
    gen_server:cast(Worker, crash).

start_link(Arguments) ->
    gen_server:start_link(?SERVER, Arguments, []).

init([Name, Partition | _Arguments]) ->
    {ok, #state { name = Name, partition = Partition }}.

handle_call(ping, _From, #state { partition = Partition } = State) ->
    {reply, {pong, #{ partition => Partition, worker => self() }}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(crash, State) ->
    erlang:error(crash),
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
```

### The Application Supervisor

This module implements the supervisor which managers the workers. The call to
`orbis:child_spec/3` (or `orbis:child_spec/4`) will initialize the state needed
for the Orbis pool and will return a child spec which we pass to the supervisor,
which will take care of starting and restarting our workers.

```erlang
-module(orbis_example_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Start a pool named orbis_example_pool with 64 workers and use
    %% orbis_example_worker as worker.
    Procs = orbis:child_spec(orbis_example_pool, 64, orbis_example_worker),
    {ok, {{one_for_one, 10, 60}, Procs}}.
```

Authors
-------

- [Alexander Færøy](https://twitter.com/ahfaeroey).

Licence
-------

    Copyright (c) 2015 Alexander Færøy

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

Related Projects
----------------

1. [Basho's Riak Core](https://github.com/basho/riak_core).

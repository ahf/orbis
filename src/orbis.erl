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
-export([dispatch/3]).

%% Types.
-export_type([ring/0,
              ring_name/0,
              partition/0,
              index/0
             ]).

-type ring() :: #{
    size       => pos_integer(),
    partitions => [partition()]
}.

-type ring_name() :: term().
-type partition() :: integer().
-type index()     :: integer().

-spec dispatch(Name, Key, Fun) -> term()
    when
        Name :: ring_name(),
        Key  :: term(),
        Fun  :: fun((Worker :: pid()) -> term()).
dispatch(_Name, _Key, _Fun) ->
    ok.

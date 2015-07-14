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
-export([hash/1]).

%% Types.
-export_type([ring/0,
              partition/0,
              index/0
             ]).

-type ring() :: #{
    size       => pos_integer(),
    partitions => [partition()]
}.

-type partition() :: integer().
-type index()     :: integer().

-spec hash(Data :: term()) -> integer().
hash(Data) when is_binary(Data) ->
    binary:decode_unsigned(crypto:hash(sha256, Data));
hash(Data) ->
    hash(term_to_binary(Data)).

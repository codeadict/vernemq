%% Copyright 2017 Erlio GmbH Basel Switzerland (http://erl.io)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(vmq_db_engine).

-include("vmq_server.hrl").

-callback write() ->
  ok | {error, Reason::atom()} | any().

-callback read() ->
  ok | {error, Reason::atom()} | any().

% This is called by VerneMQ to delete a database record.
-callback delete() ->
  ok | {error, Reason::atom()} | any().

-callback fold() ->
  ok | {error, Reason::atom()} | any().

-callback read_changes_stream() ->
  ok | {error, Reason::atom()} | any().


init(Engine, Options) ->
  case Engine:init(Options) of
    {ok, EngineState} ->
      {ok, {Engine, EngineState}};
    Error ->
      throw(Error)
  end.

write(Engine, DB, Record) ->
  Engine:write(DB, Record).

delete(Engine, DB, RecordId) ->
  Engine:delete(DB, RecordId).

read_changes_stream(Engine, DB) ->
  Engine:read_changes_stream(DB).






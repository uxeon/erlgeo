%%
%% %CopyrightBegin%
%%
%% UXEON Sp. z o.o. 2016. All Rights Reserved.
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
%%
%% %CopyrightEnd%
%%
%%%-----------------------------------------------------------------------------
%%% @author Bartosz Kołodziej <bartosz.kolodziej@uxeon.com>
%%% @doc erlgeo application
%%% @end
%%%
%%%-----------------------------------------------------------------------------
-module(erlgeo).
-author("Bartosz Kołodziej <bartosz.kolodziej@uxeon.com>").
-behaviour(application).

%%------------------------------------------------------------------------------
%% Application behaviour exports
%%------------------------------------------------------------------------------
-export([
    start/0,
    start/2,
    shutdown/0,
    stop/1
]).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([
    reverse/2,
    reverse/3,
    forward/1,
    forward/2
]).

reverse(Lon,Lat) -> erlgeo_opencage:reverse(Lon,Lat).
reverse(Lon,Lat,Options) -> erlgeo_opencage:reverse(Lon,Lat,Options).

forward(Address) -> erlgeo_opencage:forward(Address).
forward(Address,Options) -> erlgeo_opencage:reverse(Address,Options).


%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------


%%==============================================================================
%% External functions
%%==============================================================================
%%------------------------------------------------------------------------------
%% @doc The starting point for an erlang application.
%% @spec start(Type, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
start(_Type,_StartArgs) ->
    case erlgeo_sup:start_link() of
        {ok,Pid} -> {ok,Pid};
        Error    -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc The starting point for an erlang application,
%%      calls start/2 with args: Type=normal, StartArgs=[]
%% @spec start() -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% @end
%%------------------------------------------------------------------------------
start() ->
    start(normal,[]).

%%------------------------------------------------------------------------------
%% @doc Called to shudown the application.
%% @spec shutdown() -> ok
%% @end
%%------------------------------------------------------------------------------
shutdown() ->
    application:stop(erlgeo).

%%==============================================================================
%% Internal functions
%%==============================================================================
%%------------------------------------------------------------------------------
%% Called upon the termination of an application.
%%------------------------------------------------------------------------------
stop(_State) ->
    ok.

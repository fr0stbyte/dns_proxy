%%---------------------------------------------------------------------
%%   Copyright [2011] [Radu Brumariu (radubrumariu@gmail.com)]
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%---------------------------------------------------------------------

-module(dns_udp_sup).

-behaviour(supervisor).

-include_lib("include/dns.hrl").
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Params), {I, {I, start_link, [Params]}, permanent, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [dns_utils:get_env_key(dns_udp,listen_port,6666)]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Params]) ->
    Children = [ ?CHILD(dns_udp_srv, worker, Params) ],
    {ok, { {one_for_one, 5, 10}, Children} }.


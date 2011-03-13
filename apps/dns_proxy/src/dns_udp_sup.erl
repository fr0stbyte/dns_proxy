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


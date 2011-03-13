
-module(dns_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD_SUP(I), {I, {I, start_link, []}, permanent, infinity, supervisor, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    DNS_UDP = ?CHILD_SUP(dns_udp_sup),
    DNS_MNESIA = ?CHILD_SUP(dns_mnesia_sup),
    DNS_WORKER = ?CHILD_SUP(dns_worker_sup),
    {ok, { {one_for_one, 5, 10}, [DNS_UDP, DNS_MNESIA, DNS_WORKER]} }.


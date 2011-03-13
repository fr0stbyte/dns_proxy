-module(dns_proxy_app).

-behaviour(application).

-include_lib("include/dns.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    init_mnesia(),
    dns_proxy_sup:start_link().

stop(_State) ->
    ok.

%% ====================================================================
%% Internal initialization
%% ====================================================================

init_mnesia() ->
    mnesia:create_table(?DNS_QUERY_TABLE,
        [ {ram_copies,[node()]},
          {attributes, record_info(fields, ?DNS_QUERY_TABLE)} ]),
    mnesia:create_table(?DNS_RR_TABLE,
        [ {ram_copies, [node()]},
          {index, [type, class, ttl, data]},
          {attributes, record_info(fields, ?DNS_RR_TABLE)},
          {type,bag}
        ]),
    ok = mnesia:wait_for_tables([?DNS_QUERY_TABLE, ?DNS_RR_TABLE],200).

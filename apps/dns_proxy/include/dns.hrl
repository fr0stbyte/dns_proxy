-define(LISTEN_PORT, 6666).
-define(DNS_QUERY_TABLE,dns_queries).
-define(DNS_RR_TABLE, dns_rr).

-include_lib("kernel/src/inet_dns.hrl").

-record(client_state,{ type, socket, ip, port }).
-record(test_state, {cs, packet}).



-record(dns_queries, {
	  q=#dns_query{},
	  ar=[],
	  ns=[],
	  an=[]
	 }).

-record(dns_response, {
	  ar=[],
	  ns=[],
	  an=[]
	 }).

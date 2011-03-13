-module(dns_mnesia_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% included headers
%% ------------------------------------------------------------------

%% -include_lib("dns_resource.hrl").

-include_lib("include/dns.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,start/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link(?MODULE, [], []).

start() ->
    supervisor:start_child(dns_mnesia_sup,[]). 

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  {ok, Args}.

handle_call({lookup,Query}, _From, State) ->
%%  error_logger:error_msg("[~p:~p]MnesiaCheckCache~n",[?MODULE,dns_utils:get_timestamp_micro()]),
  Result = lookup(Query),
  {stop,normal, Result, State}.

handle_cast({update, Packet}, State) ->
  error_logger:error_msg("Packet to update/delete : ~p~n",[Packet]),
  delete(Packet),
  update(Packet),
  {stop,normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

lookup(Query) ->
    QR = mnesia:dirty_read({?DNS_QUERY_TABLE,Query}),
    lookup_response(QR).
    
%% P is the decoded packet
delete(P) ->
    case P#dns_rec.qdlist of 
	[Q] -> 
	    MatchHead = #dns_rr{domain=Q#dns_query.domain,type=Q#dns_query.type,class=Q#dns_query.class,_='_'},
	    Result='$_',
	    R = mnesia:dirty_select(?DNS_RR_TABLE,[{MatchHead,[],[Result]}]),
	    [ mnesia:dirty_delete_object(X) || X <- R ];
	_ ->
	    ok
    end.

%% P is the decoded packet
update(P) ->
    %% need to write query,
    [Q1] = P#dns_rec.qdlist,
    AN = write_records(P#dns_rec.anlist),
    NS = write_records(P#dns_rec.nslist),
    AR = write_records(P#dns_rec.arlist),
    Q = #dns_queries{q=Q1,ar = AR, ns = NS, an = AN},
    F = fun() ->
		mnesia:write(Q)
	end,
    mnesia:transaction(F).

write_records(A) ->
    TS = dns_utils:get_timestamp(),
    Data = lists:foldr(fun(X,Acc) ->
			       X1 = X#dns_rr{ttl = TS + X#dns_rr.ttl},
			       F = fun() ->
					   mnesia:write(X1)
				   end,
			       mnesia:transaction(F),
			       [{X#dns_rr.domain, X#dns_rr.type, X#dns_rr.class}|Acc]   %% watch out for duplicates
		       end,
		       [],
		       A),
    dns_utils:remove_list_duplicates(Data).

read_records(A) ->
    TS=dns_utils:get_timestamp(),
    D = dns_utils:get_env_key(dns_mnesia, stale_ttl, 30),
    lists:foldr(fun(X,Acc) ->
			MatchHead = #dns_rr{domain=element(1,X),type=element(2,X),class=element(3,X),ttl='$1',_='_'},
			Guard = {'>','$1',TS-D},
			Result = '$_',
			R = mnesia:dirty_select(?DNS_RR_TABLE,[{MatchHead,[Guard],[Result]}]),

			Res = validate_responses(R,#dns_query{domain=element(1,X),type=element(2,X),class=element(3,X)},TS),
%%			error_logger:error_msg("[~p:~p]Results = ~p~n",[?MODULE,dns_utils:get_timestamp(),Res]),
			[Res|Acc]
		end,
		[],
		A).
 
validate_responses([],Q,_TS)->
    {ok,WRef} = dns_worker_srv:start(),
    gen_server:call(WRef,{lookup,Q});
validate_responses(R,_Q,TS) ->
    [ X#dns_rr{ttl=abs(TS-X#dns_rr.ttl)} || X <- R].
%%    lists:map(fun(X)->
%%		      D = TS-X#dns_rr.ttl,
%%		      X#dns_rr{ttl=abs(D)}
%%	      end,
%%	      R).
    

lookup_response([]) ->
    [];
lookup_response(Queries) ->
    lists:foldr(fun(X,Ac) ->
			[ #dns_response{ar=lists:flatten(read_records(X#dns_queries.ar)),
			  ns = lists:flatten(read_records(X#dns_queries.ns)),
			  an = lists:flatten(read_records(X#dns_queries.an))}
			 |Ac] end,
		[],
		Queries).

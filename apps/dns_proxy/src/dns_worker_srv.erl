-module(dns_worker_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state,{}).
-include_lib("include/dns.hrl").


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  Id = crypto:rand_uniform(1,16#FFFF),
  gen_server:start_link({local, erlang:list_to_atom(atom_to_list(?SERVER)++integer_to_list(Id))}, ?MODULE, [], []).


start() ->
    supervisor:start_child(dns_worker_sup,[]).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    error_logger:error_msg("[~p:~p]Server Starting~n",[?MODULE,dns_utils:get_timestamp_micro()]),
    {ok, #state{}}.

handle_call({lookup,Query}, _From, _State) ->
    R = lookup(Query),
    {Time, {ok,MRef}} = timer:tc(dns_mnesia_srv,start,[]),
    %% {ok,MRef} = dns_mnesia_srv:start(),
    error_logger:error_msg("[~p:~p]Time to spawn dns_mnesia_srv : ~f~n",[?MODULE,dns_utils:get_timestamp_micro(),Time]),
    gen_server:cast(MRef, {update,R}),
    {stop,normal, R#dns_rec.anlist, #state{}}.
  

handle_cast({update,P},_State) ->
    process_packet({update_cache,P}, #state{}),
    {stop, normal, #state{}};

handle_cast({stale_while_revalidate,#dns_rr{domain=D,class=C,type=T}},_State) ->
    R = lookup(#dns_query{domain=D,class=C,type=T}),
    process_packet({update_cache,R},#state{}),
    {stop, normal, #state{}};

handle_cast({CState,Packet}, _State) ->
    {ok,P} = inet_dns:decode(Packet),
    process_packet({init,P},CState),
    {stop, normal, #state{}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


%% Returns the encoded binary packet to send back to the caller
construct_nxdomain(Packet=#dns_rec{header = Header}) ->
    NXHdr = Header#dns_header{rcode = ?NXDOMAIN, qr=1},
    Response = Packet#dns_rec{header = NXHdr},
    %% error_logger:error_msg("[~p:~p]Constructing nxdomain response~n",[?MODULE,dns_utils:get_timestamp_micro()]),
    inet_dns:encode(Response).

analyze(P) ->
    error_logger:error_msg("[~p:~p]Header : ~p~n",[?MODULE,dns_utils:get_timestamp_micro(),P#dns_rec.header]),
    error_logger:error_msg("[~p:~p]QDList : ~p~n",[?MODULE,dns_utils:get_timestamp_micro(),P#dns_rec.qdlist]),
    error_logger:error_msg("[~p:~p]ANList : ~p~n",[?MODULE,dns_utils:get_timestamp_micro(),P#dns_rec.anlist]),
    error_logger:error_msg("[~p:~p]NSList : ~p~n",[?MODULE,dns_utils:get_timestamp_micro(),P#dns_rec.nslist]),
    error_logger:error_msg("[~p:~p]ARList : ~p~n",[?MODULE,dns_utils:get_timestamp_micro(),P#dns_rec.arlist]).
    

%% decoded packet
process_packet({init,Packet},State) ->
%%    analyze(Packet),
    process_packet({check_cache, Packet}, State);

%% need to use list comprehension for more than 1 query
process_packet({check_cache,Packet},State) ->
%%    error_logger:error_msg("[~p:~p]CheckCache:~n",[?MODULE,dns_utils:get_timestamp_micro()]),
    [Q] = Packet#dns_rec.qdlist,
    {ok, MRef} = dns_mnesia_srv:start(),
%%     TS1 = dns_utils:get_timestamp_micro(),
    case gen_server:call(MRef,{lookup,Q}) of
	[] ->
%%	    TS2 = dns_utils:get_timestamp_micro(),
%%	    error_logger:error_msg("[~p:~p]process_packet check_cache : ~f~n",[?MODULE,dns_utils:get_timestamp_micro(),TS2-TS1]),
	    process_packet({lookup, Packet}, State);
	[R] ->
%%	    TS2 = dns_utils:get_timestamp_micro(),
%%	    error_logger:error_msg("[~p:~p]process_packet check_cache : ~f~n",[?MODULE,dns_utils:get_timestamp_micro(),TS2-TS1]),
	    respond_with_data(R,Packet,State)
    end;

process_packet({lookup, Packet}, State) ->
%%    error_logger:error_msg("[~p:~p]Lookup ~p~n",[?MODULE,dns_utils:get_timestamp_micro(),Packet]),
%%    TS = dns_utils:get_timestamp_micro(),
    {ok,Socket} = gen_udp:open(0,[binary, {active, false}]),
    Bin = inet_dns:encode(Packet),
    ok = gen_udp:send(Socket, {10,2,0,61}, 53, Bin),
    {ok, {{10,2,0,61}, 53, Reply}} = gen_udp:recv(Socket, 65535),
    gen_udp:close(Socket),
%%    TS1 = dns_utils:get_timestamp_micro(),
%%    error_logger:error_msg("[~p:~p]process_packet lookup : ~f",[?MODULE,TS1,TS1-TS]),
    process_packet({reply, Reply, Packet}, State);

%% at this point, reply is binary
process_packet({reply, Reply, _Packet}, #client_state{type=_Type,socket=Socket,ip=Address,port=Port}) ->
    {ok, P} = inet_dns:decode(Reply),
%%    error_logger:error_msg("[~p:~p]Reply:~p~n",[?MODULE,dns_utils:get_timestamp_micro(),P]),
%%    TS = dns_utils:get_timestamp_micro(),
    gen_udp:send(Socket,Address,Port,Reply),
%%    TS1 = dns_utils:get_timestamp_micro(),
%%    error_logger:error_msg("[~p:~p]process_packet reply ~f~n",[?MODULE,TS1,TS1-TS]),
%%    analyze(P),
    process_packet({update, P},#state{});

process_packet({update,Packet},_State) -> 
%%    error_logger:error_msg("[~p:~p]CacheUpdate: ~p",[?MODULE,dns_utils:get_timestamp_micro(),Packet]),
%%    TS = dns_utils:get_timestamp_micro(),
    {ok,MRef} = dns_mnesia_srv:start(),
    gen_server:cast(MRef,{update,Packet}).
%%    TS1 = dns_utils:get_timestamp_micro(),
%%    error_logger:error_msg("[~p:~p] process_packet update ~f",[?MODULE,TS1,TS1-TS]).



%% Reply is a #dns_response ( anlist, nslist and arlist )
respond_with_data(Reply, Packet, State) ->
%%    error_logger:error_msg("[~p:~p]respond_with_data:~p~n",[?MODULE,dns_utils:get_timestamp_micro(),Reply]),
    TS = dns_utils:get_timestamp_micro(),
    Hdr = (Packet#dns_rec.header)#dns_header{qr=1},
    Response = Packet#dns_rec{header = Hdr, anlist = Reply#dns_response.an, nslist = Reply#dns_response.ns, arlist = Reply#dns_response.ar},
    BinPack = inet_dns:encode(Response),
    ok=gen_udp:send(State#client_state.socket, State#client_state.ip, State#client_state.port, BinPack),
    TS1 = dns_utils:get_timestamp_micro(),
    error_logger:error_msg("[~p:~p]respond_with_data:~f~n",[?MODULE,dns_utils:get_timestamp_micro(),TS1-TS]).


lookup(Query)->
    {ok,Socket} = gen_udp:open(0,[binary,{active,false}]),
    P = #dns_rec{
      header = #dns_header{id = crypto:rand_uniform(1,16#FFFF),
		      opcode = 'query',
		      rd=true},
      qdlist = [ Query ]
     },
    %% error_logger:error_msg("[~p:~p]Packet ~p~n",[?MODULE,dns_utils:get_timestamp_micro(),P]),
    ok = gen_udp:send(Socket, {68,87,71,230}, 53, inet_dns:encode(P)),
    {ok, {{68,87,71,230}, 53, Reply}} = gen_udp:recv(Socket, 65535),
    {ok, Response} = inet_dns:decode(Reply),
    Response.

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
    {ok,MRef} = dns_mnesia_srv:start(),
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
    case Packet#dns_rec.qdlist of 
	[_Q] ->
	    process_packet({check_cache, Packet}, State);
	_ ->
	    Bin = construct_nxdomain(Packet),
	    send_data(Bin,State)
    end;	    

%% need to use list comprehension for more than 1 query
process_packet({check_cache,Packet},State) ->
    [Q] = Packet#dns_rec.qdlist,
    {ok, MRef} = dns_mnesia_srv:start(),
    case gen_server:call(MRef,{lookup,Q}) of
	[] ->
	    process_packet({lookup, Packet}, State);
	[R] ->
	    respond_with_data(R,Packet,State)
    end;

process_packet({lookup, Packet}, State) ->
    [Q] = Packet#dns_rec.qdlist,
    Reply = lookup(Q),
    process_packet({reply, Reply, Packet}, State);

%% at this point, reply is binary
process_packet({reply, Reply, Packet}, State) ->
    Hdr = Reply#dns_rec.header,
    Id = (Packet#dns_rec.header)#dns_header.id,
    NewHdr = Hdr#dns_header{id=Id},
    NewReply = Reply#dns_rec{ header = NewHdr },
    Bin = inet_dns:encode(NewReply),
    send_data(Bin,State),
%%    analyze(P),
    process_packet({update, NewReply},#state{});

process_packet({update,Packet},_State) -> 
    {ok,MRef} = dns_mnesia_srv:start(),
    gen_server:cast(MRef,{update,Packet}).

%% Reply is a #dns_response ( anlist, nslist and arlist )
respond_with_data(Reply, Packet, State) ->
    Hdr = (Packet#dns_rec.header)#dns_header{qr=1},
    Response = Packet#dns_rec{header = Hdr, anlist = Reply#dns_response.an, nslist = Reply#dns_response.ns, arlist = Reply#dns_response.ar},
    BinPack = inet_dns:encode(Response),
    send_data(BinPack, State).

lookup(Query) when is_record(Query,dns_query)->
    {ok, Response} = inet_res:resolve(Query#dns_query.domain, Query#dns_query.class, Query#dns_query.type),
    Response.

send_data(BinPacket, State) ->
    ok=gen_udp:send(State#client_state.socket, State#client_state.ip, State#client_state.port, BinPacket).

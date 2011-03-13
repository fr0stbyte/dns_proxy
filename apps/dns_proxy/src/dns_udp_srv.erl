-module(dns_udp_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("include/dns.hrl").

-record(state,{id,socket}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([accept_loop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Port]) ->
    Opts = [binary,{active,false}],
    case gen_udp:open(Port, Opts) of
	{ok, LSocket} -> 
	    error_logger:error_msg("[~p:~p]Started server socket : ~p~n",[?MODULE,dns_utils:get_timestamp_micro(),LSocket]),
	    {ok,accept(#state{id=1,socket = LSocket})};
	{error, Reason} ->
	    {stop,Reason}
    end.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast({"accepted",NewState}, _State) ->
    error_logger:error_msg("[~p:~p]Handle cast - accepted state~n",[?MODULE,dns_utils:get_timestamp_micro()]),
    {noreply, accept(NewState)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, _State) ->
  {noreply, #state{}}.

%% handle_info(_Info, State) ->
%%  {noreply, State}.

terminate(completed, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

accept(State) ->
    error_logger:error_msg("[~p:~p] In accept state~n",[?MODULE,dns_utils:get_timestamp_micro()]),
    proc_lib:spawn_link(?MODULE, accept_loop, [{self(),State}]),
    State.

accept_loop({Server, State=#state{id=Id, socket=Socket}}) ->
    %% execute recv on the socket
    {ok, {ClientAddress, Port, Packet} } = gen_udp:recv(Socket, 65536),
    NewState = State#state{id=Id+1},
    gen_server:cast(Server,{"accepted", NewState}),
    %% spawn process to handle the request
    {ok,WRef}=dns_worker_srv:start(),
    gen_server:cast(WRef,{#client_state{type=udp, socket = Socket, ip = ClientAddress, port = Port},Packet}).
    

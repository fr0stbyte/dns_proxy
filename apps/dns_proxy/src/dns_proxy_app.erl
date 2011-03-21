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

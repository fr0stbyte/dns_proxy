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

%% Utilitary functions that need to be shared across all the other modules
-module(dns_utils).

-export([get_timestamp/0, get_timestamp_micro/0, remove_list_duplicates/1,get_env_key/3]).
-ifdef(TEST).
-compile(export_all).
-endif.

%% Generate timestamp
get_timestamp() ->
    {M,S,_} = now(),
    M*1000000+S.

get_timestamp_micro()->
    {M,S,Mi}=now(),
    M*1000000+S+Mi/1000000.

%% Remove successive duplicates in a list
%% [1,2,2,3,4,5] -> [1,2,3,4,5] 
remove_list_duplicates(List)->
    remove_list_duplicates(List,[]).

remove_list_duplicates([X|Tail],[X|T])->
    remove_list_duplicates(Tail,[X|T]);
remove_list_duplicates([X|Tail],Acc) ->
    remove_list_duplicates(Tail,[X|Acc]);
remove_list_duplicates([],Acc) ->
    lists:reverse(lists:flatten(Acc)).

%% get environment key-value pairs
get_env_key(App,Key,Default) ->
    case application:get_env(App,Key) of
	{ok,Val} ->
	    Val;
	_ ->
	    Default
    end.

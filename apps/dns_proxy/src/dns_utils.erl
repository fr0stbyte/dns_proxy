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
    

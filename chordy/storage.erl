-module(storage).
-export([create/0, add/3, lookup/2, merge/2, split/3]).

% create a storage container
create() ->
	[].

% add a (key, value) to a datastore
add(Key, Value, Store) ->
	case lists:keyfind(Key, 1, Store) of
		{Key, _} ->
			io:format("[Store:Add] Key ~w already exists!~n", [Key]),
			Store;
		false ->
			[{Key, Value}|Store]
	end.

% search and retrieve a value from the datastore
lookup(Key, Store) ->
	case lists:keyfind(Key, 1, Store) of
		{Key, Value} ->
			Value;
		false ->
			io:format("[Store:Lookup] Key ~w does not exist!~n", [Key]),
			-1
end.

% merge the contents of two datastores
merge(S1, S2) ->
	lists:keymerge(1, S1, S2).

% split a datastore - retrieve all contents between MyKey and a certain Key
split(MyKey, Key, Store) ->
	lists:partition(fun({K,_}) -> key:between(K, Key, MyKey) end, Store).

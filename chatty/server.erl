-module(server).

%% Exported Functions
-export([start/0, process_requests/1]).

%% API Functions
start() ->
	ServerPid = spawn(server, process_requests, [[]]),
	register(myserver, ServerPid).

process_requests(Clients) ->
	receive
		% A new client joins
		{client_join_req, Name, From} ->
			io:format("[JOIN] ~s~n", [Name]),
			NewClients = [From|Clients],               % Add new client to the clients list
			broadcast(Clients, {join, Name}),          % broadcast join to all other clients
			process_requests(NewClients);              % process requests from new client list
		% A client leaves
		{client_leave_req, Name, From} ->
			io:format("[EXIT] ~s~n", [Name]),
			NewClients = lists:delete(From, Clients),  % delete the client from the clients list
			broadcast(NewClients, {leave, Name}),      % broadcast the leave to all other clients
			From ! exit,                               % send acknowledge to the client leaving
			process_requests(NewClients);              % process requests from the new client list
		% A client sends a message
		{send, Name, Text} ->
			io:format("~p: ~s", [Name, Text]),
			broadcast(Clients, {message, Name, Text}), % broadcast message to every client
			process_requests(Clients);                 % process requests from clients
		% Stop the server
		disconnect ->
			unregister(myserver)                       % unregister server and stop receiving requests
	end.

%% Local Functions
broadcast(PeerList, Message) ->
	Fun = fun(Peer) -> Peer ! Message end,
	lists:map(Fun, PeerList).



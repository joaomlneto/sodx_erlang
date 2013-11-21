-module(server2).

%% Exported Functions
-export([start/0, start/1, init_server/0, init_server/1, process_requests/2]).

%% API Functions
start() ->
	ServerPid = spawn(server2, init_server, []),
	register(myserver, ServerPid).

start(BootServer) ->
	ServerPid = spawn(server2, init_server, [BootServer]),
	register(myserver, ServerPid).

init_server() ->
	process_requests([], [self()]).

init_server(BootServer) ->
	BootServer ! {server_join_req, self()},
	process_requests([], []).

process_requests(Clients, Servers) ->
	receive
		%% Messages between client and server
		% A new client joins
		{client_join_req, Name, From} ->
			io:format("[JOIN] ~s~n", [Name]),
			NewClients = [From|Clients],
			broadcast(Servers, {join, Name}),
			process_requests(NewClients, Servers);
		% A client leaves
		{client_leave_req, Name, From} ->
			io:format("[EXIT] ~s~n", [Name]),
			NewClients = lists:delete(From, Clients),
			broadcast(Servers, {leave, Name}),
			From ! exit,
			process_requests(NewClients, Servers);
		% A client sends a message
		{send, Name, Text} ->
			io:format("~p: ~s", [Name, Text]),
			broadcast(Servers, {message, Name, Text}),
			process_requests(Clients, Servers);

		%% Messages between servers
		% Stop the server
		disconnect ->
			io:format("Bye!~n", []),
			NewServers = lists:delete(self(), Servers),
			broadcast(NewServers, {update_servers, NewServers}),
			unregister(myserver);
		% Server joins
		{server_join_req, From} ->
			io:format("New server joins!~n", []),
			NewServers = [From|Servers],
			broadcast(NewServers, {update_servers, NewServers}),
			process_requests(Clients, NewServers);
		% Server list update
		{update_servers, NewServers} ->
			io:format("Server list update! ~w~n", [NewServers]),
			process_requests(Clients, NewServers);
		% Other messages are relayed to clients
		RelayMessage ->
			io:format("Relaying message...~n", []),
			broadcast(Clients, RelayMessage),
			process_requests(Clients, Servers)

	end.

%% Local Functions
broadcast(PeerList, Message) ->
	Fun = fun(Peer) -> Peer ! Message end,
	lists:map(Fun, PeerList).



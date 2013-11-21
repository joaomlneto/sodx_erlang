-module(client).

%% Exported Functions
-export([start/2, init_client/2]).

%% API Functions
start(ServerPid, MyName) ->
	ClientPid = spawn(client, init_client, [ServerPid, MyName]),
	process_commands(ServerPid, MyName, ClientPid).

init_client(ServerPid, MyName) ->
	ServerPid ! {client_join_req, MyName, self()},
	process_requests().

%% Local Functions
%% This is the background task logic
process_requests() ->
	receive
		{join, Name} ->
			io:format("[JOIN] ~s joined~n", [Name]),
			process_requests();
		{leave, Name} ->
			io:format("[EXIT] ~s left~n", [Name]),
			process_requests();
		{message, Name, Text} ->
			io:format("[~s] ~s", [Name, Text]),
			process_requests();
		exit ->
			ok
	end.

%% This is the main task logic
process_commands(ServerPid, MyName, ClientPid) ->
	%% Read from the standard input and send to server
	Text = io:get_line("-> "),
	if
		Text == "exit\n" ->
			ServerPid ! {client_leave_req, MyName, ClientPid},
			ok;
		true ->
			ServerPid ! {send, MyName, Text},
			process_commands(ServerPid, MyName, ClientPid)
	end.

			

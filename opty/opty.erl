-module(opty).
-export([start/1, start/4, stop/1, startServer/1, stopServer/0, startRemoteClients/4]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Updates: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)
start([StrClients, StrEntries, StrUpdates, StrTime]) ->
	{Clients, _} = string:to_integer(StrClients),
	{Entries, _} = string:to_integer(StrEntries),
	{Updates, _} = string:to_integer(StrUpdates),
	{Time, _} = string:to_integer(StrTime),
	start(Clients, Entries, Updates, Time).

start(Clients, Entries, Updates, Time) ->
	register(s, server:start(Entries)),
	L = startClients(Clients, [], Entries, Updates, s),
	io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION,
	DURATION ~w s ~n", [Clients, Entries, Updates, Time]),
	timer:sleep(Time*1000),
	stop(L).

startServer(Entries) ->
	register(opty, server:start(Entries)).

stopServer() ->
	opty ! stop.

startRemoteClients(Clients, Entries, Updates, Time) ->
	L = startClients(Clients, [], Entries, Updates, {opty, 'opty@127.0.0.1'}),
	io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION,
	DURATION ~w s ~n", [Clients, Entries, Updates, Time]),
	timer:sleep(Time*1000),
	io:format("Stopping...~n"),
	stopClients(L).

stop(L) ->
	io:format("Stopping...~n"),
	stopClients(L),
	s ! stop.

startClients(0,L,_,_,_) -> L;
startClients(Clients, L, Entries, Updates, Server) ->
	Pid = client:start(Clients, Entries, Updates, Server),
	startClients(Clients-1, [Pid|L], Entries, Updates, Server).

stopClients([]) -> ok;
stopClients([Pid|L]) ->
	Pid ! {stop, self()},
	receive
		{done, Pid} -> ok
	end,
	stopClients(L).

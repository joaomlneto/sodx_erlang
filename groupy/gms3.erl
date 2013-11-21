-module(gms3).
-export([start/1, start/2]).

-define(arghh, 100).

%%%%%%%%%%%%%%%%
% Start leader %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start node as leader
% Id: the node identifier
start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id, Rnd, Self) end).

% Initialize node as leader (all alone)
% Id: the node identifier
% Rnd: a seed for the random number generator
% Master: this gms' worker
init(Id, Rnd, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	leader(Id, Master, 1, []).

%%%%%%%%%%%%%%%
% Start Slave %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start node as slave
% Id: the node identifier
% Grp: the address of a node in the group to connect to
start(Id, Grp) ->
	Rnd = random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id, Rnd, Grp, Self) end).

% Initialize node as slave
% Master: this gms' worker
init(Id, Rnd, Grp, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	Self = self(),
	Grp ! {join, Self},
	receive
	{view, N, State, Leader, Peers} ->
		Master ! {ok, State},
		erlang:monitor(process, Leader),
		slave(Id, Master, Leader, N, 0, Peers)
	after 1000 ->
		Master ! {error, "no reply from leader"}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%
% Groupy - Leader state %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This procedure represents a node in the 'leader' state
% Id: the node identifier
% Master: the gms' worker
% N: next sequence number
% Peers: a global-synchronized sequence of peers (from oldest to newest)
leader(Id, Master, N, Peers) ->
	receive
		% Receive a multicast request from a slave
		{mcast, Msg} ->
			bcast(Id, {msg, N, Msg}, Peers), 
			Master ! {deliver, Msg},
			leader(Id, Master, N+1, Peers);
		% Receive a join request from a new member
		{join, Peer} ->
			Master ! request,
			joining(Id, Master, N, Peer, Peers); 
		% Stop the show
		stop ->
			ok;
		% Catch all other kinds of messages
		Error ->
			io:format("leader ~w: strange message ~w~n", [Id, Error])
	end.

% Broadcasts a message to all nodes (and may crash after sending a message!)
% Id: the node identifier
% Msg: message to be broadcasted
% Nodes: the list of nodes to broadcast to
bcast(Id, Msg, Nodes) ->
			lists:foreach(fun(Node) ->
				Node ! Msg,
				crash(Id)
			end,
			Nodes).

% Randomly determines if the current node should crash
crash(Id) ->
		case random:uniform(?arghh) of
			?arghh ->
					io:format("leader ~w: crash~n", [Id]),
					exit(no_luck);
			_ ->
					ok
		end.

% Handles a join of a node to the group
% N: next sequence number
% Peer: the peer joining the group
joining(Id, Master, N, Peer, Peers) ->
	receive
		{ok, State} ->
			Peers2 = lists:append(Peers, [Peer]),
			bcast(Id, {view, N, State, self(), Peers2}, Peers2),
			leader(Id, Master, N+1, Peers2);
		stop ->
			ok
end.

%%%%%%%%%%%%%%%%%%%%%%%%
% Groupy - Slave state %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This procedure represents a node in the 'slave' state
% Id: the node identifier
% Master: the gms' worker
% Leader: The current group leader
% N: expected next sequence number
% Last: the last global state-related message from the leader
% Peers: a global-synchronized sequence of peers (from oldest to newest)
slave(Id, Master, Leader, N, Last, Peers) ->
	receive
		% Receive a multicast request from the master - forward to group leader
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, N, Last, Peers);
		% Receive a join request from a new peer - forward to group leader
		{join, Peer} ->
			Leader ! {join, Peer},
			slave(Id, Master, Leader, N, Last, Peers);
		% Receive a duplicate/old message from the leader - discard
		{msg, I, _} when I < N ->
			slave(Id, Master, Leader, N, Last, Peers);
		% Receive a new message from the leader - deliver to the master
		{msg, I, Msg} ->			
			Master ! {deliver, Msg},
			slave(Id, Master, Leader, I+1, {msg, I, Msg}, Peers);
		% Receive a new group view - update current view
		{view, I, State, Leader, View} ->
			slave(Id, Master, Leader, I+1, {view, I, State, Leader, View}, View);
		% Receive notification from the monitor that the leader is down
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, N, Last, Peers);
		% Stop the show
		stop ->
			ok;
		% Catch all other kinds of messages
		Error ->
			io:format("slave ~w: strange message ~w~n", [Id, Error])
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Groupy - Election state %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This procedure represents a node in the 'election' state
% Id: the node identifier
% Master: the gms' worker
% N: the next sequence number
% Last: the last global state-related message from the leader
% [Leader|Rest]: the global-synchronized sequence of peers
election(Id, Master, N, Last,[Leader|Rest]) ->
	if
		Leader == self() ->
			bcast(Id, Last, Rest), 
			leader(Id, Master, N, Rest);
		true ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N, Last, Rest)
	end.



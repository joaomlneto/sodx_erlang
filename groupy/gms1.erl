-module(gms1).

-export([start/1, start/2]).

%%%%%%%%%%%%%%%%
% Start leader %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start node as leader
% Id: the node identifier
start(Id) ->
	Self = self(),
	spawn_link(fun()-> init(Id, Self) end).

% Initialize node as leader (all alone)
% Id: the node identifier
% Master: this gms' worker
init(Id, Master) ->
	leader(Id, Master, []).

%%%%%%%%%%%%%%%
% Start Slave %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start node as slave
% Id: the node identifier
% Grp: the address of a node in the group to connect to
start(Id, Grp) ->
	Self = self(),
	spawn_link(fun()-> init(Id, Grp, Self) end).

% Initialize node as slave
% Master: this gms' worker
init(Id, Grp, Master) ->
	Self = self(),
	Grp ! {join, Self},
	receive
		{view, State, Leader, Peers} ->
			Master ! {ok, State},
			slave(Id, Master, Leader, Peers)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%
% Groupy - Leader state %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This procedure represents a node in the 'leader' state
% Id: the node identifier
% Master: the gms' worker
% Peers: a global-synchronized sequence of peers (from oldest to newest)
leader(Id, Master, Peers) ->
	receive
		% Receive a multicast request from a slave
		{mcast, Msg} ->
			bcast(Id, {msg, Msg}, Peers), 
			Master ! {deliver, Msg},
			leader(Id, Master, Peers);
		% Receive a join request from a new member
		{join, Peer} ->
			Master ! request,
			joining(Id, Master, Peer, Peers);
		% Stop the show
		stop ->
			ok;
		% Catch all other kinds of messages
		Error ->
			io:format("leader ~w: strange message ~w~n", [Id, Error])
	end.

% Broadcasts a message to all nodes
% Msg: message to be broadcasted
% Nodes: the list of nodes to broadcast to
bcast(_, Msg, Nodes) ->
		lists:foreach(fun(Node) -> Node ! Msg end, Nodes).

% Handles a join of a node to the group
% Peer: the peer joining the group
joining(Id, Master, Peer, Peers) ->
	receive
		{ok, State} ->
		Peers2 = lists:append(Peers, [Peer]),
		bcast(Id, {view, State, self(), Peers2}, Peers2),
		leader(Id, Master, Peers2);
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
% Peers: a global-synchronized sequence of peers (from oldest to newest)
slave(Id, Master, Leader, Peers) ->
	receive
		% Receive a multicast request from the master - forward to group leader
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, Peers);
		% Receive a join request from a new peer - forward to group leader
		{join, Peer} ->
			Leader ! {join, Peer},
			slave(Id, Master, Leader, Peers);
		% Receive a new message from the leader - deliver to the master
		{msg, Msg} ->
			Master ! {deliver, Msg},
			slave(Id, Master, Leader, Peers);
		% Receive a new group view - update current view
		{view, _, _, View} ->
			slave(Id, Master, Leader, View);
		% Stop the show
		stop ->
			ok;
		% Catch all other kinds of messages
		Error ->
			io:format("slave ~w: strange message ~w~n", [Id, Error])
	end.



-module(node3).
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 5000).

% Start a node with a certain key on a new DHT
start(MyKey) ->
	start(MyKey, nil).

% Start a node with a certain key and join a DHT
start(MyKey, PeerPid) ->
	timer:start(),
	spawn(fun() -> init(MyKey, PeerPid) end).

% Initialize a node
% Responsible for notifying the remote node in the target DHT
% and for scheduling the periodic stabilization
init(MyKey, PeerPid) ->
	% start with no predecessor defined
	Predecessor = nil,
	% start with no next node defined
	Next = nil,
	% connect to a peer in a DHT
	{ok, Successor} = connect(MyKey, PeerPid),
	% schedule periodic stabilization
	schedule_stabilize(),
	% create an empty datastore for ourselves and one for replication
	Store = storage:create(),
	Replica = storage:create(),
	% enter main loop
	node(MyKey, Predecessor, Successor, Next, Store, Replica).

% Connect to a node in a DHT...
% Except you don't!
% Just start a DHT with ourselves - forever alone
connect(MyKey, nil) ->
	% We are the first ones! We are our own successor
	{ok, {MyKey, nil, self()}};

% Connect to a node in a DHT
% This time for real!
connect(_, PeerPid) ->
	Qref = make_ref(),
	% Request the PeerPid's key
	PeerPid ! {key, Qref, self()},
	receive
		% the Peer replies with the key!
		{Qref, Skey} ->
			Sref = monit(PeerPid),
			{ok, {Skey, Sref, PeerPid}}
		% something went wrong and we got no response
		after ?Timeout ->
			io:format("Timeout: no response from ~w~n", [PeerPid])
	end.


% the node main loop - waiting for messages and stuff...
node(MyKey, Predecessor, Successor, Next, Store, Replica) ->
	receive
		% a peer node wants to know our key
		{key, Qref, PeerPid} ->
			PeerPid ! {Qref, MyKey},
			node(MyKey, Predecessor, Successor, Next, Store, Replica);
		% a new node informs us of its existence
		{notify, New} ->
			{Pred, Keep} = notify(New, MyKey, Predecessor, Store),
			node(MyKey, Pred, Successor, Next, Keep, Replica);
		% a potential predecessor needs to know our predecessor
		{request, Peer} ->
			request(Peer, Predecessor, Successor),
			node(MyKey, Predecessor, Successor, Next, Store, Replica);
		% our successor informs us about his current predecessor and successor
		{status, Pred, Nx} ->
			{Succ, Nxt} = stabilize(Pred, Nx, MyKey, Successor),
			node(MyKey, Predecessor, Succ, Nxt, Store, Replica);
		% stabilize ourselves periodically
		stabilize ->
			stabilize(Successor),
			node(MyKey, Predecessor, Successor, Next, Store, Replica);
		% received a probe from a client - make it go around!
		probe ->
			create_probe(MyKey, Successor, Store, Next),
			node(MyKey, Predecessor, Successor, Next, Store, Replica);
		% received the probe i sent back - it went around!
		{probe, MyKey, Nodes, T} ->
			remove_probe(MyKey, Nodes, T),
			node(MyKey, Predecessor, Successor, Next, Store, Replica);
		% receive a probe originating from someone else
		{probe, RefKey, Nodes, T} ->
			forward_probe(MyKey, RefKey, [{MyKey, Store, Next}|Nodes], T, Successor),
			node(MyKey, Predecessor, Successor, Next, Store, Replica);
		% receive request to add a (key,value) to the datastore
		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client, MyKey, Predecessor, Successor, Store),
			node(MyKey, Predecessor, Successor, Next, Added, Replica);
		% lookup a value by key in the datastore
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
			node(MyKey, Predecessor, Successor, Next, Store, Replica);
		% receive part of someone's datastore to add to our own
		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			{_, _, Spid} = Successor,
			Spid ! {pushreplica, Merged}, % we could only send the new elements
			node(MyKey, Predecessor, Successor, Next, Merged, Replica);
		% receive entry request to replicate
		{replicate, Key, Value} ->
			NewReplica = storage:add(Key, Value, Replica),
			node(MyKey, Predecessor, Successor, Next, Store, NewReplica);
		% message to stop a node
		stop ->
			bye;
		% failure detector detects succ/pred is down
		{'DOWN', Ref, process, _, _} ->
			{Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
			node(MyKey, Pred, Succ, Nxt, Store, Replica)
	end.

% stabilize
% Our successor informs us about his current predecessor
% Pred: our successor's current predecessor (RECEIVED REMOTELY)
% Nx: our successor's current successor (RECEIVED REMOTELY)
% MyKey: our key
% Successor: our successor's PID
% Returns: the pair {Successor, Next}
stabilize(Pred, Nx, MyKey, Successor) ->
	{Skey, Sref, Spid} = Successor,
	case Pred of
		% our successor has no predecessor
		% we should notify it about our existence
		% Next node is our successor's successor (Nx)
		nil ->
			Spid ! {notify, {MyKey, self()}},
			{Successor, Nx};
		% our successor's predecessor is us... stable situation!
		% Next node is our successor's successor (Nx)
		{MyKey, _} ->
			{Successor, Nx};
		% our successor's predecessor is himself! only one node in the DHT
		% we should notify it about our existence
		% Next node is ourselves (now there are two nodes in the DHT)
		{Skey, _} ->
			Spid ! {notify, {MyKey, self()}},
			{Successor, Nx};
		% our successor's predecessor is someone else
		% we must check who is wrong - us or him
		{Xkey, Xpid} ->
			case key:between(Xkey, MyKey, Skey) of
				% the new guy (X) is between us and our successor
				% we are wrong - fix ourselves!
				% Next node is our old successor for the time being
				% Schedule a stabilize, because there can be more nodes in between
				% stop monitoring the old successor and start monitoring X
				true ->
					self() ! stabilize,
					demonit(Sref),
					Xref = monit(Xpid),
					{{Xkey, Xref, Xpid}, Successor};
				% our successor is wrong! - notify him we exist
				% we should be his predecessor!
				% Next node is our successor's successor (Nx), or at least we assume for now
				false ->
					Spid ! {notify, {MyKey, self()}},
					{Successor, Nx}
			end
end.

% request
% A potential predecessor needs to know our successor
% Peer: the PID of the peer requesting the information
% Predecessor: our current predecessor
request(Peer, Predecessor, {Skey, _, Spid}) ->
	case Predecessor of
		% we dont have one :( help us!
		nil ->
			Peer ! {status, nil, {Skey, Spid}};
		% just send it to them
		{Pkey, _, Ppid} ->
			Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
end.

% schedule_stabilize
% A procedure to schedule the stabilization some time in the future
schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).

% stabilize
% Stabilization
stabilize({_, _, Spid}) ->
	% send request message to successor
	Spid ! {request, self()}.

% notify
% Someone notifies us that we are his successor (he is our predecessor)
% We have to do some investigation, though...
% Returns: {key, pid} of our predecessor and the part of datastore to keep
%          format: {{PredecessorKey, PredecessorPid}, Datastore}
notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
	case Predecessor of
		% we dont have a predecessor, we'll have to believe him
		nil ->
			% send him his share of the data and let him
			% become our new predecessor
			% dont forget to start monitoring him
			Keep = handover(Store, MyKey, Nkey, Npid),
			Nref = monit(Npid),
			{{Nkey, Nref, Npid}, Keep};
		% we have a predecessor, lets check both keys
		{Pkey, Pref, _} ->
			case key:between(Nkey, Pkey, MyKey) of
				% the new guy is closer than our predecessor - accept him
				% dont forget to start monitoring him and stop monitoring the old one
				true ->
					Keep = handover(Store, MyKey, Nkey, Npid),
					demonit(Pref),
					Nref = monit(Npid),
					{{Nkey, Nref, Npid}, Keep};
				% the new guy is not our predecessor - ignore him
				false ->
					{Predecessor, Store}
			end
	end.

% handover
% Split datastore into two
% send one part to remote node and return the other part
handover(Store, MyKey, Nkey, Npid) ->
	{Keep, Leave} = storage:split(MyKey, Nkey, Store),
	Npid ! {handover, Leave},
	Keep.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Probing-related functionality %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_probe
% we received a request for probing a DHT from a client
% create a probe and make it go around the DHT - send to successor
create_probe(MyKey, {Skey, _, Spid}, Store, Next) ->
	Spid ! {probe, MyKey, [{MyKey, Store, Next}], erlang:now()},
	io:format("[~w] Create probe ~w! Passing to ~w~n", [MyKey, MyKey, Skey]).

% remove_probe
% the probe we created went around! remove it from the DHT
remove_probe(MyKey, Nodes, T) ->
	Time = timer:now_diff(erlang:now(), T),
	io:format("[~w] Received probe ~w in ~w ms Ring: ~w~n", [MyKey, MyKey, Time, Nodes]).

% forward_probe
% forward a probe originated in some other node along the DHT
% Note: we have already been added to the Nodes list in the main loop (node/3)
forward_probe(MyKey, RefKey, Nodes, T, {Skey, _, Spid}) ->
	Spid ! {probe, RefKey, Nodes, T},
	io:format("[~w] Forward probe ~w to ~w!~n", [MyKey, RefKey, Skey]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Datastore-related functionality %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% add
% add a (key, value) to the DHT datastore
add(Key, Value, Qref, Client, MyKey, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, MyKey) of
		% it is on our datastore domain
		true ->
			% update our own datastore
			Added = storage:add(Key, Value, Store),
			Spid ! {replicate, Key, Value},
			Client ! {Qref, ok},
			Added;
		% it is not on our datastore domain
		false ->
			% forward to the right node or something...
			Spid ! {add, Key, Value, Qref, Client},
			Store
	end.

% lookup
% lookup a value by key in the DHT datastore
lookup(Key, Qref, Client, MyKey, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, MyKey) of
		% it is on our datastore domain
		true ->
			% retrieve it from our local datastore and send it to client
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		% it is not on our datastore domain
		false ->
			% forward to the right node or something...
			Spid ! {lookup, Key, Qref, Client}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Failure detection functionality %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% monit
% start monitoring a process
% Return: a monitoring reference
monit(Pid) ->
	erlang:monitor(process, Pid).

% demonit
% stop monitoring a process via monitor reference
demonit(nil) ->
	ok;
demonit(MonitorRef) ->
	erlang:demonitor(MonitorRef, [flush]).

% down
% detect a node as down
down(Ref, {_, Ref, _}, Successor, Next) ->
	{nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
	self() ! stabilize,
	Nref = monit(Npid),
	{Predecessor, {Nkey, Nref, Npid}, nil}.

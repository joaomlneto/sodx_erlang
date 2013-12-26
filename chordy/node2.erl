-module(node2).
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
	% connect to a peer in a DHT
	{ok, Successor} = connect(MyKey, PeerPid),
	% schedule periodic stabilization
	schedule_stabilize(),
	% create an empty datastore
	Store = storage:create(),
	% enter main loop
	node(MyKey, Predecessor, Successor, Store).

% Connect to a node in a DHT...
% Except you don't!
% Just start a DHT with ourselves - forever alone
connect(MyKey, nil) ->
	% We are the first ones! We are our own successor
	{ok, {MyKey, self()}};

% Connect to a node in a DHT
% This time for real!
connect(_, PeerPid) ->
	Qref = make_ref(),
	% Request the PeerPid's key
	PeerPid ! {key, Qref, self()},
	receive
		% the Peer replies with the key!
		{Qref, Skey} ->
			{ok, {Skey, PeerPid}}
		% something went wrong and we got no response
		after ?Timeout ->
			io:format("Timeout: no response from ~w~n", [PeerPid])
	end.


% the node main loop - waiting for messages and stuff...
node(MyKey, Predecessor, Successor, Store) ->
	receive
		% a peer node wants to know our key
		{key, Qref, PeerPid} ->
			PeerPid ! {Qref, MyKey},
			node(MyKey, Predecessor, Successor, Store);
		% a new node informs us of its existence
		{notify, New} ->
			{Pred, Keep} = notify(New, MyKey, Predecessor, Store),
			node(MyKey, Pred, Successor, Keep);
		% a potential predecessor needs to know our predecessor
		{request, Peer} ->
			request(Peer, Predecessor),
			node(MyKey, Predecessor, Successor, Store);
		% our successor informs us about his current predecessor
		{status, Pred} ->
			Succ = stabilize(Pred, MyKey, Successor),
			node(MyKey, Predecessor, Succ, Store);
		% stabilize ourselves periodically
		stabilize ->
			stabilize(Successor),
			node(MyKey, Predecessor, Successor, Store);
		% received a probe from a client - make it go around!
		probe ->
			create_probe(MyKey, Successor, Store),
			node(MyKey, Predecessor, Successor, Store);
		% received the probe i sent back - it went around!
		{probe, MyKey, Nodes, T} ->
			remove_probe(MyKey, Nodes, T),
			node(MyKey, Predecessor, Successor, Store);
		% receive a probe originating from someone else
		{probe, RefKey, Nodes, T} ->
			forward_probe(MyKey, RefKey, [{MyKey, Store}|Nodes], T, Successor),
			node(MyKey, Predecessor, Successor, Store);
		% receive request to add a (key,value) to the datastore
		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client, MyKey, Predecessor, Successor, Store),
			node(MyKey, Predecessor, Successor, Added);
		% lookup a value by key in the datastore
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
			node(MyKey, Predecessor, Successor, Store);
		% receive datastore handover from a node
		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			node(MyKey, Predecessor, Successor, Merged)
	end.

% Our successor informs us about his current predecessor
% Pred: our successor's current predecessor
% MyKey: our key
% Successor: our successor's PID
stabilize(Pred, MyKey, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		% our successor has no predecessor
		% we should notify it about our existence
		nil ->
			Spid ! {notify, {MyKey, self()}},
			Successor;
		% our successor's predecessor is us... stable situation!
		{MyKey, _} ->
			Successor;
		% our successor's predecessor is himself!
		% we should notify it about our existence
		{Skey, _} ->
			Spid ! {notify, {MyKey, self()}},
			Successor;
		% our successor's predecessor is someone else
		% we must check who is wrong - us or him
		{Xkey, Xpid} ->
			case key:between(Xkey, MyKey, Skey) of
				% the new guy is between us and our successor
				% we are wrong - fix ourselves!
				true ->
					self() ! stabilize,
					{Xkey, Xpid};
				% our successor is wrong! - notify him we exist
				false ->
					Spid ! {notify, {MyKey, self()}},
					Successor
			end
end.

% A potential predecessor needs to know our successor
% Peer: the PID of the peer requesting the information
% Predecessor: our current predecessor
request(Peer, Predecessor) ->
	case Predecessor of
		% we dont have one :( help us!
		nil ->
			Peer ! {status, nil};
		% just send it to them
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
end.

% A procedure to schedule the stabilization some time in the future
schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).

% Stabilization
stabilize({_, Spid}) ->
	% send request message to successor
	Spid ! {request, self()}.

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
			Keep = handover(Store, MyKey, Nkey, Npid),
			{{Nkey, Npid}, Keep};
		% we have a predecessor, lets check both keys
		{Pkey, _} ->
			case key:between(Nkey, Pkey, MyKey) of
				% the new guy is closer than our predecessor - accept him
				true ->
					Keep = handover(Store, MyKey, Nkey, Npid),
					{{Nkey, Npid}, Keep};
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
create_probe(MyKey, {Skey, Spid}, Store) ->
	Spid ! {probe, MyKey, [{MyKey, Store}], erlang:now()},
	io:format("[~w] Create probe ~w! Passing to ~w~n", [MyKey, MyKey, Skey]).

% remove_probe
% the probe we created went around! remove it from the DHT
remove_probe(MyKey, Nodes, T) ->
	Time = timer:now_diff(erlang:now(), T),
	io:format("[~w] Received probe ~w in ~w ms Ring: ~w~n", [MyKey, MyKey, Time, Nodes]).

% forward_probe
% forward a probe originated in some other node along the DHT
% Note: we have already been added to the Nodes list in the main loop (node/3)
forward_probe(MyKey, RefKey, Nodes, T, {Skey, Spid}) ->
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


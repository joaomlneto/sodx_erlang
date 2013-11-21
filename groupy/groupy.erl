-module(groupy).
-export([start/2, stop/0, start_leader/3, start_slave/3, start_slave/4]).

-define(leaderaddr, {groupy, 'leader@127.0.0.1'}).

%%%%%%%%%%%%%%%%
% Local GROUPY %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start groupy experiment with 5 nodes
% Module: the group membership protocol module
% Sleep: time workers should wait before sending next message (miliseconds)
start(Module, Sleep) ->
	Leader = worker:start("1", Module, 1, Sleep),
	register(a, Leader),
	register(b, worker:start("2", Module, 2, Leader, Sleep)),
	register(c, worker:start("3", Module, 3, Leader, Sleep)),
	register(d, worker:start("4", Module, 4, Leader, Sleep)),
	register(e, worker:start("5", Module, 5, Leader, Sleep)).

% Stop the local groupy nodes
stop() ->
	a ! stop,
	b ! stop,
	c ! stop,
	d ! stop,
	e ! stop.

%%%%%%%%%%%%%%%%%%%%%%
% Distributed GROUPY %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start a groupy node as the leader (create a new groupy group)
% Module: the group membership protocol module
% Sleep: time workers should wait before sending the next message (miliseconds)
% Id: the node identifier
start_leader(Module, Sleep, Id) ->
	Rnd = random:uniform(1000),
	worker:start(Id, Module, Rnd, Sleep).

% Start a groupy node as a slave (join another groupy group)
% By default it will connect to the node 'leader@127.0.0.1'
start_slave(Module, Sleep, Id) ->
	Rnd = random:uniform(1000),
	worker:start(Id, Module, Rnd, ?leaderaddr, Sleep).

% The same as before - but now you can specify where is the leader node in the
% LeaderAddress.
start_slave(Module, Sleep, Id, LeaderAddress) ->
	Rnd = random:uniform(1000),
	worker:start(Id, Module, Rnd, LeaderAddress, Sleep).


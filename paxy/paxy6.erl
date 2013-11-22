% experiment 5: distributed version

-module(paxy6).
-export([dstart_acceptors/1, dstart_proposers/1, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

-define(APP, paxy).

%start(Seed) ->
%	AcceptorNames = ["Acceptor 1", "Acceptor 2", "Acceptor 3", "Acceptor 4", "Acceptor 5"],
%	AccRegister = [a, b, c, d, e],
%	ProposerNames = ["Proposer 1", "Proposer 2", "Proposer 3"],
%	PropInfo = [{kurtz, ?RED, 10}, {kilgore, ?GREEN, 2}, {willard, ?BLUE, 3}],
%	% computing panel heights
%
%	AccPanelHeight = length(AcceptorNames)*50 + 20, %plus the spacer value
%	PropPanelHeight = length(ProposerNames)*50 + 20,
%	register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames, AccPanelHeight, PropPanelHeight) end)),
%	gui ! {reqState, self()},
%	receive
%		{reqState, State} ->
%			{AccIds, PropIds} = State,
%			start_acceptors(AccIds, AccRegister, Seed),
%			start_proposers(PropIds, PropInfo, AccRegister, Seed+1)
%	end,
%	true.

dstart_acceptors(Seed) ->
	AcceptorNames = ["Acceptor 1", "Acceptor 2", "Acceptor 3", "Acceptor 4", "Acceptor 5"],
	AccRegister = [a, b, c, d, e],
	ProposerNames = ["Proposer 1", "Proposer 2", "Proposer 3"],
	AccPanelHeight = length(AcceptorNames)*50 + 20, %plus the spacer value
	PropPanelHeight = length(ProposerNames)*50 + 20,
	register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames, AccPanelHeight, PropPanelHeight) end)),
	gui ! {reqState, self()},
	receive
		{reqState, State} ->
			{AccIds, _} = State,
			start_acceptors(AccIds, AccRegister, Seed)
	end,
	true.

dstart_proposers(Seed) ->
	AccRegister = [{a, 'acceptors@127.0.0.1'}, {b, 'acceptors@127.0.0.1'}, {c, 'acceptors@127.0.0.1'}, {d, 'acceptors@127.0.0.1'}, {e, 'acceptors@127.0.0.1'}],
	PropInfo = [{kurtz, ?RED, 10}, {kilgore, ?GREEN, 2}, {willard, ?BLUE, 3}],
	{gui, 'acceptors@127.0.0.1'} ! {reqState, self()},
	receive
		{reqState, State} ->
			{_, PropIds} = State,
			start_proposers(PropIds, PropInfo, AccRegister, Seed+1)
	end,
	true.

start_acceptors(AccIds, AccReg, Seed) ->
	case AccIds of
		[] ->
			ok;
		[AccId|Rest] ->
			[RegName|RegNameRest] = AccReg,
			register(RegName, acceptor6:start(RegName, Seed, AccId)),
			start_acceptors(Rest, RegNameRest, Seed+1)
	end.

start_proposers(PropIds, PropInfo, Acceptors, Seed) ->
	case PropIds of
		[] ->
			ok;
		[PropId|Rest] ->
			[{RegName, Colour, Inc}|RestInfo] = PropInfo,
			proposer:start(RegName, Colour, Acceptors, Seed+Inc, PropId),
			start_proposers(Rest, RestInfo, Acceptors, Seed)
		end.

stop() ->
	stop(gui),
	stop(a),
	stop(b),
	stop(c),
	stop(d),
	stop(e).

stop(Name) ->
	case whereis(Name) of
		undefined ->
			ok;
		Pid ->
			Pid ! stop
	end.


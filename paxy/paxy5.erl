% experiment 4: previous experiment with more proposers and acceptors

-module(paxy5).
-export([start/1, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(RG, {255,255,0}).
-define(GB, {0,255,255}).
-define(RB, {255,0,255}).

start(Seed) ->
    AcceptorNames = ["Acceptor 1", "Acceptor 2", "Acceptor 3", "Acceptor 4", "Acceptor 5", "Acceptor 6", "Acceptor 7", "Acceptor 8", "Acceptor 9", "Acceptor 10"],
    AccRegister = [a, b, c, d, e, f, g, h, i, j],
    ProposerNames = ["Proposer 1", "Proposer 2", "Proposer 3", "Proposer 4", "Proposer 5", "Proposer 6"],
    PropInfo = [{kurtz, ?RED, 10}, {kilgore, ?GREEN, 2}, {willard, ?BLUE, 3}, {p4, ?RG, 4}, {p5, ?GB, 5}, {p6, ?RB, 6}],
    % computing panel heights

    AccPanelHeight = length(AcceptorNames)*50 + 20, %plus the spacer value
    PropPanelHeight = length(ProposerNames)*50 + 20,
    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames, AccPanelHeight, PropPanelHeight) end)),
    gui ! {reqState, self()},
    receive
        {reqState, State} ->
            {AccIds, PropIds} = State,
            start_acceptors(AccIds, AccRegister, Seed),
            start_proposers(PropIds, PropInfo, AccRegister, Seed+1)
    end,
    true.

start_acceptors(AccIds, AccReg, Seed) ->
    case AccIds of
        [] ->
            ok;
        [AccId|Rest] ->
            [RegName|RegNameRest] = AccReg,
            register(RegName, acceptor4:start(RegName, Seed, AccId)),
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


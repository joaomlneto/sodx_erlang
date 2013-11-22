-module(proposer8).
-export([start/5]).

-define(timeout, 5000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Seed, PanelId) ->
	spawn(fun() -> init(Name, Proposal, Acceptors, Seed, PanelId) end).

init(Name, Proposal, Acceptors, Seed, PanelId) ->
	random:seed(Seed, Seed, Seed),
	Round = order:null(Name),
	round(Name, ?backoff, Round, Proposal, Acceptors, PanelId).

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
	% Update gui
	io:format("[Proposer ~w] set gui: Round ~w Proposal ~w~n",
	[Name, Round, Proposal]),
	PanelId ! {updateProp, "Round: "
	++ lists:flatten(io_lib:format("~p", [Round])), "Proposal: "
	++ lists:flatten(io_lib:format("~p", [Proposal])), Proposal},
	case ballot(Name, Round, Proposal, Acceptors, PanelId) of
		{ok, Decision} ->
			io:format("[Proposer ~w] ~w decided ~w in round ~w~n",
			[Name, Acceptors, Decision, Round]),
			{ok, Decision};
		abort ->
			 timer:sleep(random:uniform(Backoff)),
			 Next = order:inc(Round),
			 round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
	prepare(Round, Acceptors),
	Quorum = (length(Acceptors) div 2) + 1,
	Max = order:null(),
	case collect(Quorum, Round, Max, Proposal) of
		{accepted, Value} ->
			% update gui
			io:format("[Proposer ~w] set gui: Round ~w Proposal ~w~n",
			[Name, Round, Value]),
			PanelId ! {updateProp, "Round: "
			++ lists:flatten(io_lib:format("~p", [Round])), "Proposal: "
			++ lists:flatten(io_lib:format("~p", [Value])), Value},
			accept(Round, Value, Acceptors),
			case vote(Quorum, Round) of
				ok ->
					{ok, Value};
				abort ->
					abort
			end;
		abort ->
			abort
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collect(N, Round, Max, Proposal) ->
	collect(N, Round, Max, Proposal, N).

collect(0, _, _, Proposal, _) ->
	{accepted, Proposal};
collect(_, _, _, _, 0) ->
	abort;
collect(N, Round, Max, Proposal, SorryCounter) ->
	receive
		{promise, Round, _, na} ->
			collect(N-1, Round, Max, Proposal, SorryCounter);
		{promise, Round, Voted, Value} ->
			case order:gr(Voted, Max) of
				true ->
					collect(N-1, Round, Voted, Value, SorryCounter);
				false ->
					collect(N-1, Round, Max, Proposal, SorryCounter)
			end;
		{promise, _, _, _} ->
			collect(N, Round, Max, Proposal, SorryCounter - 1);
		{sorry, {prepare, Round}} ->
			collect(N, Round, Max, Proposal, SorryCounter - 1);
		{sorry, _} ->
			collect(N, Round, Max, Proposal, SorryCounter - 1)
	after ?timeout ->
			abort
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vote(N, Round) -> vote(N, Round, N).

vote(0, _, _) ->
	ok;
vote(_, _, 0) ->
	io:format("We've got a sorry quorum!~n", []),
	abort;
vote(N, Round, SorryCounter) ->
	io:format("test test~n"),
	receive
		{vote, Round} ->
			vote(N-1, Round, SorryCounter);
		{vote, _} ->
			vote(N, Round, SorryCounter);
		{sorry, {accept, Round}} ->
			vote(N, Round, SorryCounter - 1);
		{sorry, _} ->
			vote(N, Round, SorryCounter - 1)
	after ?timeout ->
			abort
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare(Round, Acceptors) ->
	Fun = fun(Acceptor) ->
		send(Acceptor, {prepare, self(), Round})
	end,
	lists:map(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
	Fun = fun(Acceptor) ->
		send(Acceptor, {accept, self(), Round, Proposal})
	end,
	lists:map(Fun, Acceptors).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Name, Message) ->
	case whereis(Name) of
		undefined ->
			down;
		Pid ->
			Pid ! Message
	end.


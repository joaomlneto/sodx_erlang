-module(client).
-export([start/4]).

-define(rp, 4).
-define(wp, 5).

-define(num_entries, 5).

generateSubset(Entries) ->
	EntriesList = lists:seq(1, Entries),
	MixedEntriesList = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- EntriesList])],
	lists:sublist(MixedEntriesList, ?num_entries).

start(Name, Entries, Updates, Server) ->
	spawn(fun() -> open(Name, Entries, Updates, Server, 0, 0) end).
open(Name, Entries, Updates, Server, Total, Ok) ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	Server ! {open, self()},
	receive
		{stop, From} ->
			io:format("~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n",
			[Name, Total, Ok, 100*Ok/Total]),
			From ! {done, self()},
			ok;
		{transaction, Validator, Store} ->
			Handler = handler:start(self(), Validator, Store),
			do_transactions(Name, Entries, Updates, Server, Handler,
							Total, Ok, Updates, generateSubset(Entries))
end.
% Commit transaction
do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, 0, _) ->
	%io:format("~w: Commit: TOTAL ~w, OK ~w~n", [Name, Total, Ok]),
	%timer:sleep(100),
	Ref = make_ref(),
	Handler ! {commit, Ref},
Result = receiveCommitValue(Ref),
	if
		Result == ok ->
			open(Name, Entries, Updates, Server, Total+1, Ok+1);
		true ->
			open(Name, Entries, Updates, Server, Total+1, Ok)
end;

% Reads and Writes
do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, N, EntriesSubset) ->
	%io:format("~w: R/W: TOTAL ~w, OK ~w, N ~w~n", [Name, Total, Ok, N]),
	Ref = make_ref(),
	%Num = random:uniform(Entries),
	NumPos = random:uniform(?num_entries),
	Num = lists:nth(NumPos, EntriesSubset),
	case random:uniform(?rp) of
		?rp ->
			Handler ! {read, Ref, Num},
			Value = receiveValue(Ref);
		_ ->
			Value = random:uniform(1000)
	end,
	case random:uniform(?wp) of
		?wp ->
			Handler ! {write, Num, Value+1};
		_ ->
			ok
	end,
	do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, N-1, EntriesSubset).

receiveCommitValue(Ref) ->
	receive
		{Ref,Value} -> Value
	end.
receiveValue(Ref) ->
	receive
		{value,Ref,Value} -> Value
	end.


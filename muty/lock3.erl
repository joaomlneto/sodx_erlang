-module(lock3).
-export([init/2]).

init(Id, Nodes) ->
	open(Id, 0, Nodes).

open(Id, Time, Nodes) ->
	receive
		{take, Master} ->
			TimeUpd = Time+1,
			Refs = requests(Id, TimeUpd, Nodes),
			wait(Id, TimeUpd, TimeUpd, Nodes, Master, Refs, []);
		{request, From, Ref, RemTime, _} ->
			TimeUpd = max(Time, RemTime)+1, 
			From ! {ok, Ref, TimeUpd},
			open(Id, TimeUpd, Nodes);
		stop ->
			ok
	end.

requests(Id, Time, Nodes) ->
	NotMe = lists:delete(self(), Nodes),
	lists:map(
	  fun(P) ->
		R = make_ref(),
		P ! {request, self(), R, Time, Id},
		R
	  end,
	  NotMe).

wait(Id, Time, _, Nodes, Master, [], Waiting) ->
	Master ! taken,
	held(Id, Time, Nodes, Waiting);
wait(Id, Time, ReqTime, Nodes, Master, Refs, Waiting) ->
	receive
		{request, From, Ref, RemTime, RemId} ->
			TimeUpd = max(Time, RemTime) + 1,
			if
				(RemTime < ReqTime) or ((RemTime == ReqTime) and (RemId > Id)) ->
					R2 = make_ref(),
					NewRefs = [R2 | Refs],
					From ! {ok, Ref, TimeUpd + 1},
					From ! {request, self(), R2, ReqTime, Id},
					wait(Id, TimeUpd + 1, ReqTime, Nodes, Master, NewRefs, Waiting);
				true ->
					wait(Id, TimeUpd, ReqTime, Nodes, Master, Refs, [{From, Ref}|Waiting])
			end;
		{ok, Ref, RemTime} ->
			TimeUpd = max(Time, RemTime) + 1,
			Refs2 = lists:delete(Ref, Refs),
			wait(Id, TimeUpd, ReqTime, Nodes, Master, Refs2, Waiting);
		release ->
			TimeUpd = Time + 1,
			ok(TimeUpd, Waiting),
			open(Id, TimeUpd, Nodes)
	end.

ok(Time, Waiting) ->
	lists:map(
	  fun({F,R}) ->
		F ! {ok, R, Time}
	  end,
	  Waiting).

held(Id, Time, Nodes, Waiting) ->
	receive
		{request, From, Ref, RemTime, _} ->
			TimeUpd = max(Time, RemTime) + 1,
			held(Id, TimeUpd, Nodes, [{From, Ref}|Waiting]);
	release ->
			TimeUpd = Time + 1,
			ok(TimeUpd, Waiting),
			open(Id, TimeUpd, Nodes)
	end.


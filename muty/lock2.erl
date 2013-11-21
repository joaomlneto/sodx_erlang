-module(lock2).
-export([init/2]).

init(Priority, Nodes) ->
    open(Priority, Nodes).
open(Priority, Nodes) ->
    receive
        {take, Master} ->
            Refs = requests(Priority, Nodes),
            wait(Priority, Nodes, Master, Refs, []);
        {request, From, Ref, _} ->
            From ! {ok, Ref},
            open(Priority, Nodes);
        stop ->
            ok
    end.
requests(Priority, Nodes) ->
    lists:map(
      fun(P) ->
        R = make_ref(),
        P ! {request, self(), R, Priority},
        R
      end,
      Nodes).

wait(Priority, Nodes, Master, [], Waiting) ->
    Master ! taken,
    held(Priority, Nodes, Waiting);
wait(Priority, Nodes, Master, Refs, Waiting) ->
    receive
        {request, From, Ref, Prio} ->
		if
			Prio > Priority ->
				R2 = make_ref(),
				NewRefs = [R2 | Refs],
				From ! {ok, Ref},
				From ! {request, self(), R2, Priority},
				wait(Priority, Nodes, Master, NewRefs, Waiting);
			true ->
            	wait(Priority, Nodes, Master, Refs, [{From, Ref}|Waiting])
		end;
        {ok, Ref} ->
            Refs2 = lists:delete(Ref, Refs),
            wait(Priority, Nodes, Master, Refs2, Waiting);
        release ->
            ok(Waiting),
            open(Priority, Nodes)
    end.
ok(Waiting) ->
    lists:map(
      fun({F,R}) ->
        F ! {ok, R}
      end,
      Waiting).

held(Priority, Nodes, Waiting) ->
    receive
        {request, From, Ref, _} ->
            held(Priority, Nodes, [{From, Ref}|Waiting]);
        release ->
            ok(Waiting),
            open(Priority, Nodes)
    end.


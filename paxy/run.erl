-module(run).
-export([run/0]).

-define(delay, 5000).

delay() ->
	timer:sleep(random:uniform(?delay)).

run() ->
	compile:file(paxy7),
	compile:file(acceptor7),
	compile:file(proposer7),
	paxy7:start(0),
	timer:sleep(1000),
	startcrashing([a, b, c, d, e]).

startcrashing([H|R]) ->
	delay(),
	paxy7:crash(H, 0),
	startcrashing(R++[H]).

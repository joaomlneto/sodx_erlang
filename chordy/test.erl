-module(test).
-export([start/1, join/1, t1/0, t2/0, t3/0, t4/0]).

start(Key) ->
	register(chordy, node3:start(Key)).

join(Key) ->
	register(chordy, node3:start(Key, {chordy, 'N1@127.0.0.1'})).

t1() ->
	chordy:test({chordy, 'N1@127.0.0.1'}, 10000, 1).

t2() ->
	chordy:test({chordy, 'N2@127.0.0.1'}, 10000, 2).

t3() ->
	chordy:test({chordy, 'N3@127.0.0.1'}, 10000, 3).

t4() ->
	chordy:test({chordy, 'N4@127.0.0.1'}, 10000, 4).


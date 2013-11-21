-module(muty).
-export([start/3, stop/0, start1/3, start2/3, start3/3, start4/3, stopme/0]).

start(Lock, Sleep, Work) ->
    register(l1, spawn(Lock, init,[1, [l2,l3,l4]])),
    register(l2, spawn(Lock, init,[2, [l1,l3,l4]])),
    register(l3, spawn(Lock, init,[3, [l1,l2,l4]])),
    register(l4, spawn(Lock, init,[4, [l1,l2,l3]])),
    register(john,   spawn(worker, init, ["John", l1,34,Sleep,Work])),
    register(ringo,  spawn(worker, init, ["Ringo", l2,37,Sleep,Work])),
    register(paul,   spawn(worker, init, ["Paul", l3,43,Sleep,Work])),
    register(george, spawn(worker, init, ["George",l4,72,Sleep,Work])),
    ok.

start1(Lock, Sleep, Work) ->
	Remotes = [{lock, 'b@127.0.0.1'}, {lock, 'c@127.0.0.1'}, {lock, 'd@127.0.0.1'}],
	register(lock, spawn(Lock, init, [1, Remotes])),
	register(worker, spawn(worker, init, ["John", lock, 34, Sleep, Work])).
start2(Lock, Sleep, Work) ->
	Remotes = [{lock, 'a@127.0.0.1'}, {lock, 'c@127.0.0.1'}, {lock, 'd@127.0.0.1'}],
	register(lock, spawn(Lock, init, [2, Remotes])),
	register(worker, spawn(worker, init, ["Ringo", lock, 37, Sleep, Work])).
start3(Lock, Sleep, Work) ->
	Remotes = [{lock, 'a@127.0.0.1'}, {lock, 'b@127.0.0.1'}, {lock, 'd@127.0.0.1'}],
	register(lock, spawn(Lock, init, [3, Remotes])),
	register(worker, spawn(worker, init, ["Paul", lock, 43, Sleep, Work])).
start4(Lock, Sleep, Work) ->
	Remotes = [{lock, 'a@127.0.0.1'}, {lock, 'b@127.0.0.1'}, {lock, 'c@127.0.0.1'}],
	register(lock, spawn(Lock, init, [4, Remotes])),
	register(worker, spawn(worker, init, ["George", lock, 72, Sleep, Work])).

stopme() ->
	john ! stop,
	lock ! stop.

stop() ->
    john ! stop,
    ringo ! stop,
    paul ! stop,
    george ! stop,
    l1 ! stop,
    l2 ! stop,
    l3 ! stop,
    l4 ! stop.


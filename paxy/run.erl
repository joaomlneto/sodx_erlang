-module(run).
-export([run/0]).

run() ->
	compile:file(paxy7),
	compile:file(acceptor7),
	compile:file(proposer7),
	paxy7:start(0).
	

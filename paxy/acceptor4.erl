% experiment 3: drop some promise/vote messages

-module(acceptor4).

-export([start/3]).

-define(delay, 500).
-define(drop, 10).

start(Name, Seed, PanelId) ->
    spawn(fun() -> init(Name, Seed, PanelId) end).
init(Name, Seed, PanelId) ->
    random:seed(Seed, Seed, Seed),
    Promise = order:null(),
    Voted = order:null(),
    Accepted = na,
    acceptor(Name, Promise, Voted, Accepted, PanelId).

maybe_send(Name, Destination, Message) ->
	case random:uniform(?drop) of
		?drop ->
			io:format("[Acceptor ~w] Message dropped", [Name]);
		_ ->
			Destination ! Message
	end.

acceptor(Name, Promise, Voted, Accepted, PanelId) ->
  receive
   {prepare, Proposer, Round} ->
       R = random:uniform(?delay),
       timer:sleep(R),
       case order:gr(Round, Promise) of
           true ->
					maybe_send(Name, Proposer, {promise, Round, Voted, Accepted}),
               % Update gui
               if
                   Accepted == na ->
                       Colour = {0,0,0};
                   true ->
                       Colour = Accepted
               end,
   io:format("[Acceptor ~w] set gui: voted ~w promise ~w colour ~w~n",
                       [Name, Voted, Round, Accepted]),
                       PanelId ! {updateAcc, "Round voted: "
   ++ lists:flatten(io_lib:format("~p", [Voted])), "Cur. Promise: "
   ++ lists:flatten(io_lib:format("~p", [Round])), Colour},
               acceptor(Name, Round, Voted, Accepted, PanelId);
           false ->
               Proposer ! {sorry, {prepare, Round}},
               acceptor(Name, Promise, Voted, Accepted, PanelId)
       end;

	 {accept, Proposer, Round, Proposal} ->
       R = random:uniform(?delay),
       timer:sleep(R),
		 case order:goe(Round, Promise) of
		     true ->
					maybe_send(Name, Proposer, {vote, Round}),
		         case order:goe(Round, Voted) of
		             true ->
		              % Update gui
					io:format("[Acceptor ~w] set gui: voted ~w promise ~w colour ~w~n",
		                 [Name, Round, Promise, Proposal]),
		                 PanelId ! {updateAcc, "Round voted: "
							++ lists:flatten(io_lib:format("~p", [Round])), "Cur. Promise: "
							++ lists:flatten(io_lib:format("~p", [Promise])), Proposal},
		                 acceptor(Name, Promise, Round, Proposal, PanelId);
		             false ->
		                 % Update gui
							io:format("[Acceptor ~w] set gui: voted ~w promise ~w colour ~w~n",
												 [Name, Voted, Promise, Accepted]),
												 PanelId ! {updateAcc, "Round voted: "
							++ lists:flatten(io_lib:format("~p", [Voted])), "Cur. Promise: "
							++ lists:flatten(io_lib:format("~p", [Promise])), Accepted},
												 acceptor(Name, Promise, Voted, Accepted, PanelId)
		         	end;
		     false ->
		         Proposer ! {sorry, {accept, Round}},
		         acceptor(Name, Promise, Voted, Accepted, PanelId)
		 end;


  stop ->
     ok
end.


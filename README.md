# SODX Assignments

This repository contains our solutions to the Erlang programming assignments of the [Distributed and Networked Operating Systems (SODX)](https://www.fib.upc.edu/en/masters/masters-2006/mti/assignatures/SODX.html) course at [UPC](http://upc.edu) during the 2013-2014 academic year.

## Chatty: a simple chat service
Implementation of a centralized chat server and a federated chat server, as well as a client to connect to them.

## Muty: a distributed mutual exclusion lock
- `worker.erl` is a simple worker to stress-test the system.

- `lock1.erl` is a faulty lock implementation

- `lock2.erl` resolves deadlocks from previous implementation

- `lock3.erl` uses lamport clocks to improve over previous implementation

## Groupy: a group membership service
Group membership service providing atomic multicast, allowing several processes to have a coordinated state (i.e. performing the same sequence of changes), taking into consideration node churn (i.e. nodes joining/leaving).

## Paxy: the Paxos protocol
Implementation of the Paxos protocol in Erlang, split into Proposers and Acceptors.

Several implementations to test the reliability of the protocol when it comes to resistance to delays and dropped messages, as well as a few optimizations.

## Opty: optimistic concurrency control
A transaction server with optimistic concurrency control. Implementation of a *transaction server*, *store* and *validator*.

## Chordy: a distributed hash table
Implementation of a DHT following the Chord protocol.

First implementation maintains a ring structure: nodes can be added in the ring but elements cannot be added into the store.

Second implementation uses a store where key-value pairs can be added.

Third implementation adds failure detection and simple ring-repair procedures.

# Acknowledgements

[Jordi Guitart](http://personals.ac.upc.edu/jguitart/), adapted with permission from [Johan Montelius](https://www.kth.se/profile/johanmon) & [Xavier León](http://people.ac.upc.edu/xleon/)

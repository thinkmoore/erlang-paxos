To run, start a number of erlang nodes with unique names in the code directory:
```
cd path/to/erlang-paxos
erl -sname n1@localhost
erl -sname n2@localhost
...
```

Pick one of your nodes to be the master, say ```n1@localhost```.
On the other nodes, run ```net_adm:ping(n1@localhost)``` to make
```n1``` aware of the nodes presence.

Then on ```n1``` run ```paxos:test(Rounds)``` where ```Rounds``` is
the number of decisions you want to make. The test will run an acceptor
on every node including the proposer and calculate the appropriate F.

Each node will log its persistent state to a log file in the current
working directory. On startup, the proposer and acceptors will check
for a log file to resume from. These logs have unbounded growth, so
you'll want to clean them up manually...

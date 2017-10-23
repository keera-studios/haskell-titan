# Haskell Titan - Testing Infrastructure for Temporal AbstractioNs

Haskell Titan is a testing and debugging system for reactive, time-varying and
interactive software.

It is built on the principles of Functional Reactive Programming, although it's
ideas can be applied to other time-based abstractions.

<div style="text-align:center"><img src ="https://raw.githubusercontent.com/keera-studios/haskell-titan/develop/docs/debugger.gif" /></div>

# Structure of Haskell Titan

Haskell Titan is composed of two parts: testing facilities and debugging
facilities.

The testing facilities allow you to: 1) describe and test temporal unit
tests, and 2) test FRP programs using QuickCheck (both using real traces or
unit tests)

The debugging facilities allow you to: 1) record and replay FRP programs in a
referentially transparent manner and 2) debug programs as you run them.

Part of the testing facilities have been introduced in Yampa's repository
directly, and you can find them at:

https://github.com/ivanperez-keera/Yampa

In this repo you will find:

- An extension of Yampa to run programs recording their input and debugging
  them.

- An interactive debugging GUI to connect to a running Yampa program and
  control it remotely.

## Getting started

### Debugging

It's easier to get started with an example:

```
$ git clone https://github.com/keera-studios/haskell-titan
$ cd haskel-titan
$ git clone https://github.com/ivanperez-keera/Yampa
$ cabal sandbox init
$ cabal install Yampa/ -fexpose-core
$ cabal install -fexamples titan-yampa-debugger/
$ cabal install titan-gui/
$ ./.cabal-sandbox/bin/titan-gui &
$ ./.cabal-sandbox/bin/titan-yampa-debugger-example-bouncing-ball
```

You'll need GTK with glade installed. On Ubuntu you can:
```
$ apt-get install libglade2-dev
```

## Related Papers

- [Testing and debugging functional reactive programming](https://dl.acm.org/citation.cfm?id=3110246)

# Collaborations

Please, send pull requests and file bugs.

If you are considering to do something similar for a different FRP
implementation, please consider adding a new backend for this project so that
we can join efforts. That way our efforts will be more likely to help you, and
yours will also help us. There's always some extra effort from trying to
collaborate with others, but it's totally worth it :)

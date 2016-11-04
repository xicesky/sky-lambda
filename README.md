# sky-lambda
Untyped lambda calculus in Haskell

This project wants to be a simple, but flexible implementation of untyped lambda calculus.
It will also support "modules", which should make it much easier do write bigger programs.

## Caveats
* This is intended as a research project, not a productive programming language!
* As this is completely untyped, you have to be VERY careful when trying to write a meaningful program!
* It is very easy to write programs that don't terminate: Evaluation only terminates when your program is in "weak head normal form".

## Notes
* It will support step-by-step execution, trying to make the intermediate steps "readable".
* It will (of course) use a lazy evaluation scheme
* It will not try to do any fancy and fast evaluation!

## TODO List
* Support for modules while keeping things printable

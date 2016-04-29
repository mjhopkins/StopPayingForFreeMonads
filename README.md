# Stop paying for free monads
Haskell code for YOW LambdaJam 2016 workshop

We'll build up an implementation of a free monad over a coproduct of functors, following Wouter Swierstra's "Datatypes à la Carte".

We'll create a couple of languages and interpreters, and run programs using these languages.

We then do the same in the "final" style advocated by Oleg Kiselyov in "Typed Tagless Final Interpreters".


## Prerequisites

[Stack](http://haskellstack.org)

## Instructions

Compile with

```
stack build --file-watch --fast
```

Run the repl with

```
stack ghci
```

Go through the source files, replacing `undefined` with appropriate implementations.
Some exercises are more in the nature of an exploration.

Suggested order to proceed:

* ALaCarte.hs
* ALaCarteDSL.hs
* ALaCarteImpl.hs
* Tagless.hs


## Supplementary exercises

Now we're going to just deal with expressions instead of "statements"

i.e. drop the monad requirement.

We'll build towards developing a library for fast extended regular expressions.

* Expressions.hs
* Regex.hs
* RegexFinal.hs
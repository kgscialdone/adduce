# Adduce

Adduce is an interpreted, concatenative, stack-based programming language that reads like plain English.

```
; Fizzbuzz in Adduce
For each number in a Range from 1 to 101 do (
  Let N be the number.
  Do If $ $ N `% 15 `== 0 then (Print "FizzBuzz") else
     If $ $ N `% 3  `== 0 then (Print "Fizz")     else
     If $ $ N `% 5  `== 0 then (Print "Buzz")     else
     (Print N).
).
```

Adduce was heavily inspired by [Cognate](https://github.com/cognate-lang/cognate), and borrows much of its syntax and semantics from it. The
two differ mainly in implementation and goals; Cognate is a compiled language, while Adduce is interpreted. However, compatibility with Cognate
programs is not a goal for Adduce, nor vice versa; the two are different languages, and will likely diverge over time rather than converge.

## Installation

Notice: Adduce is currently heavily WIP, so use at your own risk!

To build Adduce from source, you'll need Haskell and the Cabal package manager installed.

1. Clone this repository.
2. Run the `cabal install` command.

Adduce will be installed, and can be run with the `adduce` command.


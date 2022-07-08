# Adduce

Adduce is an interpreted, concatenative, stack-based programming language that reads like plain English.

Rather than use a complicated set of execution rules to try to mimic natural language, Adduce uses one simple rule:
words starting with lowercase letters are ignored. This allows developers to write inline comments with no added
boilerplate, turning their code into readable sentences. Once lowercase words have been stripped out, Adduce programs
execute in sentences, separated by periods and each from right to left.

```lisp
; Fizzbuzz in Adduce
For each number in a Range from 1 to 101 do (
  Let N be the number.
  Do If $ $ N `% 15 `== 0 then (Print "FizzBuzz") else
     If $ $ N `% 3  `== 0 then (Print "Fizz")     else
     If $ $ N `% 5  `== 0 then (Print "Buzz")     else
     (Print N).
).
```

Documentation is WIP. For more examples of Adduce's syntax, see [`src/prelude.adc`](src/prelude.adc) or the [`tests`](tests) folder.

Adduce was heavily inspired by [Cognate](https://github.com/cognate-lang/cognate), and borrows much of its syntax and
semantics from it. The two differ mainly in implementation and goals; for example, Cognate is a compiled language, while
Adduce is interpreted. However, compatibility with Cognate programs is not a goal for Adduce, nor vice versa; the two are
different languages, and will likely diverge over time rather than converge.

Adduce is implemented in Haskell, and strives for a minimal set of core functions and keywords. The majority of the language's
default functionality is defined in a [prelude file](src/prelude.adc) written in Adduce itself.

## Installation

Notice: Adduce is currently heavily WIP, so use at your own risk!

To build Adduce from source, you'll need Haskell and the Cabal package manager installed.

1. Clone this repository.
2. Run the `cabal install` command.

Adduce will be installed, and can be run with the `adduce` command.


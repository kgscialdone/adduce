# Adduce

Adduce is an interpreted, concatenative, stack-based programming language that reads like plain English.

Rather than use a complicated set of execution rules to try to mimic natural language, Adduce uses one simple rule:
words starting with lowercase letters are ignored. This allows developers to write inline comments with no added
boilerplate, turning their code into readable sentences. Once lowercase words have been stripped out, Adduce programs
execute in sentences, separated by periods and each from right to left.

Adduce also implements automatic desuffixing, which allows for natural conjugation of identifiers; if no more specific
binding exists, it will remove a number of different common suffixes from the identifier, attempting to resolve the
modified identifiers in a "least modification first" pattern. This means, for example, that `Mapping`, `Mapped`, `Maps`,
and `Mapper` are all valid and automatic aliases for `Map`.

```lisp
; 99 Bottles
Def Plural      (Let N. If $ N `== 1 then ''    else 's').
Def Punctuation (Let N. If $ N `== 0 then '...' else '!').

For each of a Reversed Range from 1 to 100 do (
  Let a Number be from the range.
  Print a Joined List of (Number, ' bottle', Plural for Number, ' of beer on the wall,') with the delimiter ''.
  Print a Joined List of (Number, ' bottle', Plural for Number, ' of beer,') with the delimiter ''.
  Print 'Take one down, pass it around,'.
  Let the Number be the $ Number, `Decremented.
  Print a Joined List of ($ the Number `Or 'No more' if it's zero, ' bottle', the Plural for Number,
    ' of beer on the wall', the Punctuation for Number, '\n') with the delimiter ''.
).
```

Documentation is WIP. For more examples of Adduce's syntax, see [`src/language/prelude.adc`](src/language/prelude.adc) or the [`src/test/scripts`](src/test/scripts) folder.

Adduce was heavily inspired by [Cognate](https://github.com/cognate-lang/cognate), and borrows much of its syntax and
semantics from it. The two differ mainly in implementation and goals; for example, Cognate is a compiled language, while
Adduce is interpreted. However, compatibility with Cognate programs is not a goal for Adduce, nor vice versa; the two are
different languages, and will likely diverge over time rather than converge.

Adduce is implemented in Haskell, and strives for a minimal set of core functions and keywords. The majority of the language's
default functionality is defined in a [prelude file](src/language/prelude.adc) written in Adduce itself.

## Installation

Notice: Adduce is currently heavily WIP, so use at your own risk!

To build Adduce from source, you'll need Haskell and the Cabal package manager installed.

1. Clone this repository.
2. Run the `cabal install` command.

Adduce will be installed, and can be run with the `adduce` command.


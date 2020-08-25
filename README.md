# LambdaF

Have you ever wanted to leverage the amazing Brainfuck tooling out there, from
[BrainfuckIDE](https://github.com/wmww/BrainfuckIDE) to
[bfc](https://github.com/Wilfred/bfc) to
[ServerFuck](https://github.com/K4rakara/ServerFuck), but gotten annoyed at the
lack of expressivity in the language itself?

Look no further than LambdaF: a small, purely functional language that compiles
down to -- you guessed it -- Brainfuck.


## About

### Brainfuck

[Brainfuck](https://esolangs.org/wiki/Brainfuck) is an infamous esoteric
language composed of 8 single-character instructions (`+`, `-`, `<`, `>`, `[`,
`]`, `,`, `.`). Although quite difficult to write programs in due to its very
small instruction set, it is known to be [Turing
complete](https://en.wikipedia.org/wiki/Turing_completeness), which means it can
do anything (algorithmically-speaking) that any mainstream / real languages can.


### λ-calculus

[λ-calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is a computational
model that forms the basis of functional programming. It is also Turing
complete, so it has exactly the same capabilities as Brainfuck: anything
λ-calculus can compute, Brainfuck can compute as well. In other words, any
Brainfuck program can be re-written as a λ-expression and vice-versa.


### LambdaF

LambdaF is a variant of simply-typed λ-calculus with primitive operators to
support basic arithmetic and I/O.

#### Basic syntax

| Syntax         | Name              | Description                                                                                          |
| :------------- | :---------------- | :--------------------------------------------------------------------------------------------------- |
| `x`            | Variable          | Alphanumeric string refering to a previously-defined variable                                        |
| `n`            | Constant          | Integer literal                                                                                      |
| `λx. M`        | Abstraction       | Creates an annonymous function binding a variable `x` in an expression `M`                           |
| `M N`          | Application       | Applies a function `M` to an argument `N`                                                            |
| `letrec x = M` | Recursive binding | Binds a variable `x` to the expression `M` in `M` (only meaningful when `M` is a lambda abstraction) |
| `cond C M N`   | Conditional       | `M` if `C` evaluates to non-0, `N` otherwise                                                         |

(Note that `\` can be used in place of `λ` for better keyboard-friendliness)

#### Primitive functions

On top of the basic syntax, there are some primitive functions for arithmetic
and working with pairs.

| Fucntion   | Description                           |
| :--------- | :------------------------------------ |
| `add M N`  | Integer addition of `M` and `N`       |
| `sub M N`  | Integer subtraction of `M` and `N`    |
| `pair M N` | Creates an ordered pair `(M, N)`      |
| `fst P`    | Gets the first element of a pair `P`  |
| `snd P`    | Gets the second element of a pair `P` |

#### I/O primitives

Finally, there are two more primitive functions for I/O: `getchar` and
`putchar`, which merit a discussion of their own.

From its λ-calculus roots, LambdaF has a property which makes I/O slightly more
difficult than it is in imperative languages: referential transparency. This
means that if an expression `M` evaluates to a value `v`, all occurances of `M`
can be replaced by `v`. When working with plain numbers and tuples, this is
perfectly fine; `1 + 1` is the same as `2` in all scenarios. If LambdaF were to
have a function `putchar c` which output a character, however, simply replacing
it with it's value (which could be an empty/dummy value) won't work: multiple
invocations of `putchar` need to be executed separately, and in order, to give
the correct output. A primitive `getchar` which takes no arguments and evaluates
to a character read from standard input also violates referential transparency:
multiple instances of `getchar` do not evaluate to the same value.

One way to fit input and output into a referentially-transparent framework is to
have I/O functions take an extra parameter representing the I/O state of the
program and return (in addition to their normal return value) a new I/O state
after performing their input or output. A full program in LambdaF would then be
an I/O action which takes in an initial I/O state and returns the final I/O
state of the program after it has executed. Using this concept, `getchar` and
`putchar` can have the types:

```
getchar : IO -> (Char, IO)

putchar : Char -> IO -> IO
```

where `IO` is this magical "I/O state" type.

Now, actually storing the I/O state would require keeping track of all I/O
actions that the program has done, which would take up a lot of memory and
computation. Luckily, the I/O state is opaque to everything except I/O
primitives, which means it can be stored internally as an empty/dummy value.
This removes the extra overhead and lets I/O primitives internally just perform
their repsective I/O operations. The I/O state type is only a theoretical entity
used to control dependencies between I/O operations.

See [this page](https://wiki.haskell.org/IO_inside) on the Haskell wiki for a
better, more in-depth description of this idea (Haskell's `RealWorld` is
equivalent to LambdaF's `IO`).


## Building

This project is built with
[Stack](https://docs.haskellstack.org/en/stable/README/).

```
$ stack build # to build

$ stack install # installs the `lambdaf` compiler locally
```


## Running

Once built and installed, the LambdaF compiler can be run by passing it a source
file and optionally an output file:

```
$ lambdaf program.lf # generates Brainfuck in program.bf

$ lambdaf program.lf -o output.bf # generates Brainfuck in output.bf
```


## Resources / inspiration

- P. Fradet, D. Le Métayer, "Compilation of lambda-calculus into functional
  machine code", https://link.springer.com/chapter/10.1007%2F3-540-50940-2_34

  LambdaF's semantics, the stack-based IR, and the compilation from LambdaF into
  the stack IR are all based on the results of this paper.

- Thomas Lively, Victor Domene, Gabriel Guimaraes, "BrainCoqulus: A Formally
  Verified Compiler of Untyped Lambda Calculusto Brainfuck",
  http://www.read.seas.harvard.edu/~kohler/class/cs260r-17/projects/braincoqulus.pdf

  Tuple and stack representations and the usage of tuples to implement closures
  come from here.

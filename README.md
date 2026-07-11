# sml-packrat

[![CI](https://github.com/sjqtentacles/sml-packrat/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-packrat/actions/workflows/ci.yml)

A memoizing PEG (Parsing Expression Grammar) parser for Standard ML.

`sml-packrat` builds parsers from PEG combinators with **ordered choice** and
**syntactic predicates** (`&` and `!`), and memoizes each named rule's result at
each input position. This packrat memoization makes parses run in time linear in
the input length, even for grammars whose naive backtracking would be
exponential.

## PEG operators

| Combinator        | PEG        | Meaning                                   |
| ----------------- | ---------- | ----------------------------------------- |
| `seq p q`         | `p q`      | sequence                                  |
| `alt p q`         | `p / q`    | ordered choice (try `p`, else `q`)        |
| `many p`          | `p*`       | greedy zero-or-more                       |
| `many1 p`         | `p+`       | greedy one-or-more                        |
| `opt p`           | `p?`       | optional                                  |
| `andP p`          | `&p`       | and-predicate: match, consume nothing     |
| `notP p`          | `!p`       | not-predicate: succeed iff `p` fails      |
| `rule name thunk` | `Name <-`  | a named, memoized nonterminal             |

## How memoization works

Each `rule` owns its **own** memo table indexed by input position. SML has no
existential types, so a single heterogeneous `(rule, pos) -> result` table is
not expressible; giving every rule a monomorphic array of its own result type is
the portable design. Tables are reset automatically at the start of every
`parse`, and the same per-position bookkeeping detects **direct left recursion**
(re-entering a rule at a position before it has produced a result raises
`LeftRecursion`) instead of looping forever.

`memoHits ()` returns the number of cache hits served during the most recent
`parse`, so you can confirm memoization is doing its job.

## Portability

Pure Standard ML using only the Basis library. Verified on:

- **MLton**
- **Poly/ML**

## Building and testing

```sh
make test        # build + run the suite under MLton (default)
make test-poly   # run the suite under Poly/ML
make all-tests   # run under both
make clean
```

## Installing with smlpkg

`sml-packrat` follows the conventions of the
[`smlpkg`](https://github.com/diku-dk/smlpkg) package manager:

```sh
smlpkg add github.com/sjqtentacles/sml-packrat
smlpkg sync
```

This downloads the library into `lib/github.com/sjqtentacles/sml-packrat/`.
Reference it from your own `.mlb` with a relative path to `packrat.mlb`:

```
lib/github.com/sjqtentacles/sml-packrat/packrat.mlb
```

For Poly/ML, `use` the sources in order:

```sml
use "lib/github.com/sjqtentacles/sml-packrat/packrat.sig";
use "lib/github.com/sjqtentacles/sml-packrat/packrat.sml";
```

## Usage

A precedence-correct arithmetic grammar. Recursive references go through a `ref`
and `delay`, because the `peg` type is abstract:

```sml
structure P = Packrat
val digit  = P.litc Char.isDigit
val number = P.map (fn ds => valOf (Int.fromString (implode ds))) (P.many1 digit)
val addop  = P.alt (P.map (fn _ => op +) (P.chr #"+"))
                   (P.map (fn _ => op -) (P.chr #"-"))
val mulop  = P.alt (P.map (fn _ => op * ) (P.chr #"*"))
                   (P.map (fn _ => op div) (P.chr #"/"))

val exprRef : int P.peg ref = ref (P.map (fn _ => 0) (P.chr #"\000"))
val factor = P.rule "factor" (fn () =>
  P.alt number (P.seqR (P.chr #"(") (P.seqL (P.delay (fn () => !exprRef)) (P.chr #")"))))
val term = P.rule "term" (fn () =>
  P.map (fn (x, rest) => List.foldl (fn ((f,y),acc) => f (acc,y)) x rest)
        (P.seq factor (P.many (P.seq mulop factor))))
val expr = P.rule "expr" (fn () =>
  P.map (fn (x, rest) => List.foldl (fn ((f,y),acc) => f (acc,y)) x rest)
        (P.seq term (P.many (P.seq addop term))))
val () = exprRef := expr

val P.Ok (n, _) = P.parse (P.seqL expr (P.notP P.anyc)) "2*(3+4)-1"   (* n = 13 *)
```

## Example

`make example` builds and runs [`examples/demo.sml`](examples/demo.sml), which
parses fixed literal arithmetic expressions with the precedence-correct PEG
above, exercises terminals/ordered choice, memoization, and left-recursion
detection (output is byte-identical under MLton and Poly/ML):

```
=== sml-packrat demo ===

Arithmetic PEG (expr <- term (('+'/'-') term)*, with precedence):
  2+3*4  =  14
  (2+3)*4  =  20
  10-3-2  =  5
  2*(3+4*(5-1))  =  38

Terminals and ordered choice:
  chr #"a" on "abc"   -> a @1
  str "let" on "lex"  -> err @0

Memoization (shared-prefix ordered choice):
  S <- A 'c' / A 'd'  on "aaad"  -> ok @4
  memo hits so far    = 1

Left recursion is detected, not looped forever:
  L <- L 'a' / 'a'  on "aaa"  -> LeftRecursion "L"
```

## Project layout

```
sml.pkg                                          smlpkg manifest
Makefile                                         build + test
lib/github.com/sjqtentacles/sml-packrat/
  packrat.sig                                    the PACKRAT signature
  packrat.sml                                    memoized PEG implementation
  packrat.mlb                                    MLB for consumers
test/
  test.mlb                                       test basis (MLton)
  test.sml                                       assertion suite
.github/workflows/ci.yml                         CI (MLton + Poly/ML)
```

## License

MIT. See [LICENSE](LICENSE).

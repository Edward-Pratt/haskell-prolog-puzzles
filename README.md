# Haskell and Prolog Puzzle Solvers

Solutions to four *Sunday Times* Teaser puzzles, written as generate-and-select programs
in Haskell (functional) and Prolog (logic). Each solver lists the candidate answers, then
filters them with a selector that encodes the puzzle's constraints.

> Built for the **ECM2418 Computer Languages and Representations** module at the
> University of Exeter (individual coursework, 2024).

| Puzzle | Teaser | Language | File |
|---|---|---|---|
| Gill's Primes | 3154 | Haskell | [`Gill.hs`](Gill.hs) |
| Common Names | 3137 | Haskell | [`Common.hs`](Common.hs) |
| Bilateral Symmetry | 3132 | Prolog | [`Bilateral.pl`](Bilateral.pl) |
| In His Prime | 3231 | Prolog | [`Prime.pl`](Prime.pl) |

## Running

Haskell (GHC):

```bash
runghc Gill.hs
runghc Common.hs
```

Prolog (SWI-Prolog). Load a file, then query its generator and selector:

```bash
swipl Bilateral.pl
?- generator3(X), selector3(X).
```

The GenAI use declaration the module required is in [`Declaration.txt`](Declaration.txt).

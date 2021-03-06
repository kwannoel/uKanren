# Description

Many thanks to [William Byrd](https://github.com/webyrd) and [Jason Hemann](https://github.com/jasonhemann) for reviewing this implementation and providing links to useful resources.

Thanks to [E-liang](https://github.com/taneliang) and [Mayank](https://github.com/mkeoliya) for providing valuable feedback and the presentation as well.

---

A minimal implementation of `microKanren`

- To run tests:

  [Install haskell toolchain](https://www.haskell.org/platform/)

  ``` shell
  make tests
  ```

- To generate slides:

  [Install pandoc](https://pandoc.org/installing.html)

  ``` shell
  make slides
  ```

# References

http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

https://github.com/jasonhemann/microKanren

https://gist.github.com/msullivan/4223fd47991acbe045ec#file-microkanren-hs

https://github.com/seantalts/hasktrip/blob/master/src/MicroKanren.lhs

# Key concepts & definitions

## Core functionality

Inputs: Application of a [`goal`](#goal) to a [`state`](#state).
Output: `Stream` of [`state`s](#state) (zero or more, can be infinite).

## Goal
A goal can either `succeed` or `fail`.
If the `goal` succeeds, it may result in a `Stream` of *enlarged states*.

### Primitive goal constructors

- `===` 

  `goals` constructed from this succeed when its arguments `unify`. It can then cause the `state` to grow.
  
  This operator permits circular substitution, e.g. `x = x`.

- `fresh` / `call`
  
  Takes in a *unary function*, `\x -> <body>`.
  
  The binding variable, `x` is a fresh logic variable (unbound to any values).
  
  The body is an expression over which the fresh variable's binding is scoped.
  
  The function when applied, evaluates to a `goal`.
  
- `disj` (disjunction / OR)

  Takes in 2 goals.
  If either of its arguments succeed, `disj` succeeds. Returns a *non-empty stream* if it succeeds.
  
- `conj` (conjunction / AND)

  Takes in 2 goals.
  Both have to succeed for `conj` to succeed. Returns a *non-empty stream* if it succeeds.
  
## State

A `pair` of a [`subtitution`](#substitution) and a [`variable counter`](#variable-counter).

## Substitution

Represented as an association list.

E.g. `[(x, 1), (y, 2), ...]` where `x` is associated with `1`, `y` associated with `2` and so on.

## Variable counter

A non-negative integer. It initializes as `0`.

## Extras

### Disjunction

TODO

### Problem

TODO

### Solutions

#### All combinators should be wrapped with delay

TODO

## Other things todo

nix microKanren?

microKanren on Haskell's typelevel

Use a Free DSL instead: e.g. `Conj` vs `conj`. 
We can include side effects such as `delay` in our interpreter.

## Further reading

### Understanding program synthesis

http://webyrd.net/quines/quines.pdf

https://github.com/webyrd/2012-scheme-workshop-quines-paper-code

https://dl.acm.org/doi/10.1145/3110252

http://io.livecode.ch/learn/gregr/icfp2017-artifact-auas7pp

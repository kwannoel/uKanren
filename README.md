# Description

A minimal implementation of `microKanren`

To run tests:

``` haskell
runghc microKanren.hs
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

### Problem

TODO

### Solutions

#### Allow users to annotate goals

We can explicitly annotate `goals` which recurse indefinitely as `Delayed`.

We then greedily get the `goals` we can get, and come back to the `Delayed` goals after.

However, this requires the `user` to do so, if they incorrectly annotate their expressions, it will still recurse indefinitely.

This is a relatively simple solution to implement however.

#### Check during compile time 

Pending investigation, maybe we could introduce a typelevel parameter for variables, to include `Nat` in their parameters.

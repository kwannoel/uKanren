# Description

A minimal implementation of `microKanren`

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

If we allow cyclic substutions, for instance `[(0, Var 1), (1, Var 0), (2, Atom "valid")]`, when we try to unify either `Var 1` or `Var 2`, we will never terminate.

The following `goal` should still succeed however:

``` haskell
g = disj (fresh (=== Atom "7")) g
```

Since `(fresh (=== Atom "7"))` should succeed.

Let's see why it doesn't if we use `lists` rather than `delayed streams`.

``` haskell
g = disj (fresh (=== Atom "7")) g
  -- Definition of disj
  = \st -> (fresh (=== Atom "7") st) `mplus` g st
  = \st@(s, vc) -> (fresh (=== Atom "7") st) `mplus` g st
  = \st@(s, vc) -> ((\(s', vc') -> (Var c === Atom "7") (s', vc' + 1)) (s, vc)) `mplus` g st
  = \st@(s, vc) -> ((Var c === Atom "7") (s, vc + 1)) `mplus` g st
  = \st@(s, vc) -> ([[((c, Atom "7") : s, vc + 1)]]) `mplus` g st
  = \st@(s, vc) -> ([[((c, Atom "7") : s, vc + 1)]]) `mplus` disj (fresh (=== Atom "7")) g st
  = \st@(s, vc) -> ([[((c, Atom "7") : s, vc + 1)]]) 
        `mplus` ((fresh (=== Atom "7") st) `mplus` g st)
  = \st@(s, vc) -> ([[((c, Atom "7") : s, vc + 1)]]) 
        `mplus` ((fresh (=== Atom "7") st) 
            `mplus` ((fresh (=== Atom "7") st) `mplus` g st))
```

We also know the above matches the following invocation of `mplus`
``` haskell
mplus (x:xs) snd@(y:ys)
    where
        x = [[((c, Atom "7") : s, vc + 1)]]
        xs = []
        snd = (fresh (=== Atom "7") st) `mplus` ((fresh (=== Atom "7") st) `mplus` g st)
        -- More explicitly
        snd = mplus (fresh (=== Atom "7") st) ((fresh (=== Atom "7") st) `mplus` g st)
```

To get `y` out of the above, we have the first evaluate the `snd` arguement so we can pattern match on it.

We can also swap around the original expression:

``` haskell
g = disj (fresh (=== Atom "7")) g
```

This then shows how we face the problem as well, this time for the first argument to `mplus`, `(x:xs)`.

What are our solutions?

Well, we can explicitly annotate `goals` which recurse indefinitely as `Immature`.

We then greedily get the `goals` we can get, and come back to the `Immature` goals after.

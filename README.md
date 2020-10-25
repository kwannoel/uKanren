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

### Disjunction

When we try to perform a disjunction with a non-terminal goal and a terminal goal, we should still get results.

``` haskell
nonTermG = nonTermG `disj` (fresh (=== Atom "7"))
```

In principle, the above should be valid. Although `nonTermG` never terminates, the second goal succeeds, so `disj` should succeed by definition.

If we look into implementation however, the above evaluates to this when applied to a state, `s`:

``` haskell
nonTermG = nonTermG `disj` (fresh (=== Atom "7"))

nonTermG s@(subst, vc) = (concat . transpose) [nonTermG s, (Var vc === sc) (subst, vc + 1)] -- (1)
```

To deconstruct `nonTermG`, we have to try to expand it to the first term at least in the output state.

Deconstruction recurses indefinitely however, continuing from (1):

``` haskell
nonTermG s@(subst, vc) = (concat . transpose) [nonTermG s, (Var vc === sc) (subst, vc + 1)] -- (1)
-- Hiding non important parts to make this apparent, just showing the shape of the recursion
nonTermG s = f [nonTermG s, term]
nonTermG s = f [f [nonTermG s, term], term]
nonTermG s = f [f [f [nonTermG s, term], term], term]
-- ...
```

Hence this never gives us a result.

If we reversed it however, we would still be able to get results:

``` haskell
nonTermG = (fresh (=== Atom "7")) `disj` nonTermG 
nonTermG s@(subst, vc) = (concat . transpose) [(Var vc === sc) (subst, vc + 1), nonTermG s]
-- Hiding non important parts to make this apparent
nonTermG s = f [terminal, nonTermG s]
nonTermG s = f [terminal, f [terminal, nonTermG s]]
nonTermG s = f [terminal, f [terminal, nonTermG s]]
nonTermG s = (terminal: head nonTermG s) ++ f [[], tail (nonTermG s)]
nonTermG s = (terminal: [head (nonTermG s)]) ++ f [[], tail (nonTermG s)]
nonTermG s = [terminal, terminal] ++ f [[], tail (nonTermG s)]

-- For reference, definition of transpose
tranpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h: _) <- xss] ): transpose (xs : [ t | (_:t) <- xss])
```

What are our solutions?

### Solutions

#### Allow users to annotate goals

We can explicitly annotate `goals` which recurse indefinitely as `Immature`.

We then greedily get the `goals` we can get, and come back to the `Immature` goals after.

However, this requires the `user` to do so, if they incorrectly annotate their expressions, it will still recurse indefinitely.

This is a relatively simple solution to implement however.

#### Check during compile time 

Pending investigation, maybe we could introduce a typelevel parameter for variables, to include `Nat` in their parameters.

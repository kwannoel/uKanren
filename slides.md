% MicroKanren

## MicroKanren
Parsing & Implementation

by Noel, E-liang and Mayank

## Based on The original keynote

[William E Byrd - Relational Interpreters, Program Synthesis, Barliman](https://www.youtube.com/watch?v=RVDCRlW1f1Y)

## Intro

- Declarative programming

  - Only talk about the properties of our programs

  - We get the exact properties we ask for
  
  - Common examples: Spreadsheet programming, list comprehensions

- Program synthesis

  - Generating functions by just talking about inputs and outputs
  
  - Functionality is implicitly produced from specifications


## Common pitfalls in declarative programming
- Easy to ideate, hard to reason about "procedure".
  - often resort to procedural "side-effects"

- E.g. difficult to ideate variable bindings

- Reinventing the wheel for simple functions

## MiniKanren!
- Declarative, relational (Japanese - 関連 - "relation") "family" of languages
        - canonically written in Scheme

- Small core (desugared): MicroKanren (today's topic)

- Built to be idiomatic to the "host" language and relational programming.

## Benefits of miniKanren family of languages

1. Easy to extend and modify.
    - extended to logic programming (constraint, nominal and probabilistic), and tabling
    - reason: the core takes just 2 A4 pages to implement (which we did!)

2. Make writing declarative programs easy, imperative programs hard.

3. You can pick your poison!
    - miniKaren has been implemented in multiple host languages - Scheme, Haskell, JavaScript, OCaml, Ruby, and our favourite, PHP.

## Differences with Prolog

- Interleaving complete search VS DFS

- Handling "procedural" or "effectual" operators

## Interleaving vs DFS

- Or how to avoid non-terminating programs as much as possible

- Consider this:
```
non_terminating_rule(X) :- non_terminating_rule(Y) ; terminating_fact(Y)
```

- Expected: termination.

- Prolog (DFS-implementation): goes into an infinite recursion, doesn't terminate. 

- MiniKanren (Interleaving search) : does terminate, by incrementing both branches at each stage (aka BFS). 

## Procedural Side-Effects

- a culprit: Cut (!) operator in Prolog 
   - interferes with the search for the results

- E.g. `self(X, Y): X \= Y, !, …`
   - This prevents backtracking to A

- Why is it considered a side effect?
   - Instead operating as a logical operator, it interferes with the solver itself by changing the way we unify our results.

- another culprit: `retract`
   - which allows you to remove a fact / rule in prolog db, during execution.

## Implementation

## Minimal language

- MicroKanren's API has 4 core operators
```haskell
fresh :: Goal -- ^ Constructs a new variable binding
conj  :: Goal -- ^ Logical operator - Conjunction
disj  :: Goal -- ^ Logical operator - Disjunction
(===) :: Goal -- ^ Logical operator - Equality
```

## How do we use a miniKanren program?

## We construct a goal we want to satisfy

``` haskell
goal :: Goal
goal = fresh $ 
    \x -> fresh $ 
        \y -> ((x === Atom "1") `disj` (x === Atom "2")) 
            `conj` ((y === Atom "a") `disj` (y === Atom "b"))
```

## And execute it against our initial state

```haskell
initialState :: State
initialState = ([], 0)

results :: Stream State
results = goal initialState

displayResults :: IO ()
displayResults = putStrLn $ prettyPrintResults results
```

## Output

```haskell
Var 1 := Atom "a"
Var 0 := Atom "1"

Var 1 := Atom "a"
Var 0 := Atom "2"

Var 1 := Atom "b"
Var 0 := Atom "1"

Var 1 := Atom "b"
Var 0 := Atom "2"
```

As you can see, we will get all associations which satisfy the stated constraints in the `goal`.

## How do we bind new variables?

`fresh` does this implicitly for us, such that we do not need to **name our bindings**.

## Let's see how this is done...

## By looking in State

Inside our `State`, we have an implicit parameter `VariableCounter`:
```haskell
type State = (Subst, VariableCounter)
```

Each time `fresh` is applied to some `State`, we increment the `VariableCounter` and use it to generate
unique identifiers for variables, e.g. `Var 0, Var 1, ...`.

## Example

```haskell
initialState = ([], 0)
```

Gets updated to...

```haskell
updatedState = ([(Var 0, Atom "1")], 1)
```
 

## Why represent results as a Stream of States?

There can be many possible combinations of results which satisfy the constraints.

## Example

Given the following constraint for `X`:

``` haskell
X = 1 ∨ X = 2
```

We can have:

```haskell
X := 1

---- OR

X := 2
```

Hence we represent these possibilities in a `Stream`.

## Laziness of Haskell

In the event answers continue indefinitely: `[Ans1, Ans2, ..]`,
it doesn't matter, since we can just take as many items as we require and leave the rest unevaluated.

In that case why didn't we just use lists (`[a]`)?

## Defining Streams

Looking at our `Stream` definition, we realize it is essentially the same as a `list`,
with an additional possibility: 

`Delay` which indicates the `Stream` should be `Delayed` for evaluation at a future time.

## Stream definition

```haskell
data Stream a = Nil
              | Cons a (Stream a)
              | Delayed (Stream a) deriving (Eq, Show)
```

`Delay` can be used to force switching between 2 streams.

To understand why this is needed, let's talk about `disj`.

## Usage of disjunction

First we need to understand what `disj` does.

`disj` allows us to perform fair complete search of our results.

## Example

if we had 2 result streams:

```haskell
s0 = [a, b, c]
s1 = [x, y, z]
```

`disj` would merge their results together like so:

```haskell
s_merged = [a, x, b, y, c, z]
```

## Under the hood

Under the hood it does something like this:

```haskell
interleave :: [a] -> [a] -> [a]
interleave [] l = l
interleave l [] = l
interleave (x:xs) ys = x : interleave ys xs

> interleave [1..3] [2..4]
> [1,2,2,3,3,4]
```

## Observation

**We can see disj requires us to deconstruct at least one of its arguments**, e.g. `x` in `(x:xs)`.

We can then ignore everything else, until we force further evaluation:

```haskell
*MicroKanren> x :: [Int]; x = interleave [1..] [2,4..]
*MicroKanren> :sprint x
x = _
*MicroKanren> head x
1
*MicroKanren> :sprint x
x = 1 : _
```

## Recursive goals

So far so good, until we recursively define a goal

```haskell
-- |
-- "goalR" refers to recursive goal
-- "goalT" refers to terminal goal which always succeeds

goalR :: Goal
goalR = goalR `disj` goalT

goalT :: Goal
goalT = return
```

Inutitively, this should work fine for us, as `disj` should behave like so...

## Expected behaviors of disj

## Terminal V NonTerminal goals 

If one of its goals never terminates, and the other one does, 
  it should return the results from the terminal goal.
  
  This is because `disj` works like disjunction, only one of the goals need to succeed.
  
  Since `goalT` terminates, we should just return the result of `goalT`.
  
## Ordering of arguments

`disj` should not care about the order of its arguments, relations are commutative.

## In practice

In practice however, the following occurs:
  
```haskell
goalR s = (goalR `disj`  goalT) s
        = goalR s `mplus`  goalT s 
        -- mplus behavior is the same as interleave
        -- We evaluate the first argument, in order to deconstruct it
        = (goalR `disj` goalT) s `mplus` goalT s
        = (goalR s `mplus` goalT s) `mplus` goalT s
        = ((goalR s `mplus` goalT s) 
            `mplus` goalT s) 
                `mplus` goalT s
        = ...
```

As we can see, we are never able to extract the head of `goalR s` due to its recursive definition.

## Swapping arguments

However, if we swapped the arguments around:

```haskell
goalR s = (goalT `disj`  goalR) s
        = goalT s `mplus`  goalR s 
```

Since `goalT s` terminates, we would be able ignore the recursive part, until forced to yield more results.

## User intervention!

This requires the user to manually push the recursive parts to the rightmost argument, which is error-prone.

By changing the data structure to include a `Delay`, we can force switching.

## Using Delay to force switching 

```haskell
goalR s = Delayed (goalR s) `mplus` goalT s
```

`mplus` can pattern match on the `Delayed` data constructor of our `Stream` and perform a switch

```haskell
goalR s = Delayed (goalT s `mplus` goalR s)
```

## Evaluation

This can then be evaluated since `goalT s` is terminal, and `mplus` forces on its first argument.

## User intervention!

This approach is still error prone however, the responsibility still lies with users to wrap the recursive parts in a `Delayed` data constructor.

If we extend the idea to make `Delayed` an inherent / implicit part of the recursive definition of the recursive goal, `goalR` we can avoid this.

## Encoding Delay into microKanren

We wrap all goal constructors, `disj`, `conj`, `fresh`, `(===)` in `Delayed`.

That way, whenever a goal is recursively defined, it always evaluates to a delayed stream.

To make that explicit, let's see the following expansion:

## Expansion

```haskell
goalR s = Delayed (goalR s `mplus` goalT s)
        = Delayed ((Delayed (goalR s `mplus` goalT s)) `mplus` goalT s)
        = Delayed (Delayed (goalT s `mplus` goalR s `mplus` goalT s))
```

## What does it mean?

The essence of it is that goalT s is not recursively defined, so we will always be able to deconstruct it.

More generally, as long as one of the goals are not recursively defined, we will always be able to evaluate `disj` to give us results.


## Using microKanren

- SAT solver

- Program synthesis

- Quines

## Extensions

[MiniKanren](http://minikanren.org/)

- extending to support miniKanren

**TODO** provide examples for the following:

- Disequality operator

- Temporal logic programming
- Constraint logic programming
- Probabilistic logic programming
- Nominal logic programming

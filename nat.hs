import           MicroKanren
import           MiniKanren  (runEval)

nil :: Term
nil = Atom mempty

suc :: Term -> Term
suc = Pair (Atom mempty)

-- | Constraint a variable to be a natural number
nat :: Term -> Goal
nat x = disj
    (x === nil)
    (fresh (\d ->
        conj (x === suc d)
             (nat d)))

-- -- Unification fails for the following
-- -- We should just get a single result
f = \x -> nat x `conj` (x === suc (suc (suc nil)))

{- | problem:
nat x `conj` (x === suc (suc (suc nil)))

when applied to some state, ignores all invalid results from 0 - 2 since their results collapse to Nil.

@
    Nil `mplus` xs = xs
@

Then, we hit our first result:

@
    suc (suc (suc nil)) `mplus` ...
@

After which, we keep getting results that don't match

4 >>= 3

5 >>= 3

...

And the program doesn't know to shortcircuit

since we try to check all numbers from 4 to infinity.

Is there a solution? :x

We need some way to show that nat 4 and up is not equivalent to 3.

If we looked at it naively,

We have an infinite Stream, and we are saying that a value does not exist in this infinite stream.

But how do we know, if we haven't checked the entire stream?

As such we need to encode some property inside natural numbers to assert the uniqueness of a number.

Such that the first time we unify, the rest of it is dumped.

explore this some other time.

-}

main :: IO ()
main = putStrLn $ prettyPrintResults $ takeS 1 $ fresh f initialState

{-|
=========
== WIP ==
=========
-}

module MiniKanren where

import           MicroKanren (Goal, State, Stream (..), Subst, Term (..), Var,
                              VariableCounter, conj, delay, disj, extS, fresh,
                              unify, walk, (===))

-- | Disj' provides us a way to safely disj goals which are recursive
disj' :: Goal -> Goal -> Goal
disj' = delay disj

-- | Conj' provides us a way to safely conj goals which are recursive
conj' :: Goal -> Goal -> Goal
conj' = delay conj

-- |
conde :: [Goal] -> Goal
conde = undefined

main :: IO ()
main = do
    putStrLn "Hello World!"

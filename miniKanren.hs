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
disj' g1 g2 = delay $ disj g1 g2

-- | Conj' provides us a way to safely conj goals which are recursive
conj' :: Goal -> Goal -> Goal
conj' g1 g2 = delay $ conj g1 g2

-- | Conde - satisfy list of goals
conde :: [Goal] -> Goal
conde = foldr conj' $ const Nil

-- | Fresh'

main :: IO ()
main = do
    putStrLn "Hello World!"

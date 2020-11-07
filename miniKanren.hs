{-# LANGUAGE FlexibleInstances #-}

module MiniKanren where

import           MicroKanren        (Goal, State, Stream (..), Subst, Term (..),
                                     Var, VariableCounter, conj, delay, disj,
                                     extS, fresh, unify, walk, (===))

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

-------------- Safe + ergonomic versions of logical operators

-- | Safe disj of one or more goals
disjN :: NonEmpty Goal -> Goal
disjN = delay . foldr1 disj

-- | Safe conj of one or more goals
conjN :: NonEmpty Goal -> Goal
conjN = delay . foldr1 conj

-- | Conde -> disjunction of a series of conjunctions
-- disjN [ conjN [g1, g2, ...]
--       , conjN [h1, h2, ...]
--       ]
conde :: NonEmpty (NonEmpty Goal) -> Goal
conde = delay . disjN . fmap conjN

-- | freshN -> takes in a function which performs binding of n variables to a goal
freshN :: MultiParamFunction f => f -> Goal
freshN f = \(s, c) -> let (c', g) = apply f c
                      in g (s, c')

-- | Auxiliary class to provide indirection
class MultiParamFunction f where
    apply :: f -> VariableCounter -> (VariableCounter, Goal)

-- | Inductive case
instance MultiParamFunction f => MultiParamFunction (Term -> f) where
    apply f c = apply f' c'
        where f' = f (Var c)
              c' = c + 1

-- | Base case
instance MultiParamFunction (Term -> Goal) where
    apply f c = (c + 1, f (Var c))

main :: IO ()
main = do
    putStrLn "Hello World!"

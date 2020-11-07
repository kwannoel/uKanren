{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-|
=========
== WIP ==
=========
-}

module MiniKanren where

import           MicroKanren        (Goal, State, Stream (..), Subst, Term (..),
                                     Var, VariableCounter, conj, delay, disj,
                                     extS, fresh, unify, walk, (===))

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

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
freshN :: MultiParamFunction i r => (Term -> i -> r) -> Goal
freshN f = \(s, c) -> let (c', g) = apply f c
                      in g (s, c')

class MultiParamFunction i r where
    apply :: (Term -> i -> r) -> VariableCounter -> (VariableCounter, Goal)

instance MultiParamFunction i r => MultiParamFunction Term (i -> r) where
    apply f c = apply f' c'
        where f' = f (Var c)
              c' = c + 1

-- | Base case
instance MultiParamFunction State (Stream State) where
    apply f c = (c + 1, f (Var c))

main :: IO ()
main = do
    putStrLn "Hello World!"

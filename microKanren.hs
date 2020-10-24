{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
module Main where

import           Control.Applicative (Alternative)
import           Control.Monad       (MonadPlus (..))

type Goal = State -> Stream State
type State = (Subst, VariableCounter)
type Subst = [(Var, Term)]
type Var = Integer
type VariableCounter = Integer

data Term = Atom String
          | Var Var
          | Pair Term Term
          deriving Show

data Stream a = Nil
              | Cons a (Stream a)
              | Immature (Stream a)
              deriving (Alternative, Applicative, Functor, Show)

instance Monad Stream where
    return x = Cons x Nil
    Nil >>= f         = mzero
    x `Cons` xs >>= f = f x `mplus` (xs >>= f)
    Immature xs >>= f = Immature (xs >>= f)

-- Interleaving rather than appending 2 streams
instance MonadPlus Stream where
    mzero = Nil
    Nil `mplus` xs           = xs
    (x `Cons` xs) `mplus` ys = x `Cons` (ys `mplus` xs)
    Immature xs `mplus` ys   = Immature (ys `mplus` xs) -- Prioritize snd stream

walk :: Term -> Subst -> Term
walk (Var v) s = case lookup v s of Nothing -> Var v
                                    Just a  -> walk a s
walk t _ = t

extS :: Var -> Term -> Subst -> Subst
extS v t s = (v, t) : s

(===) :: Term -> Term -> Goal
(===) t1 t2 = \(s, vc) -> case unify t1 t2 s of
    [] -> mzero
    s' -> return (s', vc)

unify :: Term -> Term -> Subst -> Subst
unify t1 t2 s = go (walk t1 s) (walk t2 s)
    where
        go (Atom a1) (Atom a2) | a1 == a2 = s
        go (Var v1) (Var v2) | v1 == v2 = s
        go (Var v1) t2' = extS v1 t2' s
        go t1' (Var v2) = extS v2 t1' s
        go (Pair u1 u2) (Pair v1 v2) = unify u2 v2 (unify u1 v1 s)
        go _ _ = mzero

fresh :: (Term -> Goal) -> Goal
fresh f = \(s, c) -> f (Var c) (s, c + 1)

disj :: Goal -> Goal -> Goal
disj g1 g2 = \s -> g1 s `mplus` g2 s

conj :: Goal -> Goal -> Goal
conj g1 g2 = \s -> g1 s >>= g2

--------- Tests
initialState :: State
initialState = ([], 0)

tests :: [Goal]
tests = [ atomTest
        , conjTest1
        , conjTest2
        , disjTest1
        , disjTest2
        , fail1
        ]
    where
        atomTest = fresh (\x -> x === Atom "5")
        conjTest1 = conj (fresh (=== Atom "7"))
                        (fresh (=== Atom "8"))
        conjTest2 = conj (fresh (=== Atom "7"))
                        (fresh (=== Atom "8"))
        disjTest1 = disj (fresh (=== Atom "7"))
                        (fresh (=== Atom "8"))
        disjTest2 = disj (fresh (=== Atom "8"))
                        (fresh (=== Atom "8"))
        fail1 = fresh (\a -> (a === Atom "8") `conj` (a === Atom "9"))

cyclicState :: State
cyclicState = ([ (0, Var 1), (1, Var 0) -- Our infinite cycle
               , (2, Atom "1")
               ], 3)

cyclicTests :: [Goal]
cyclicTests = [ canTest1
              , takeNS 2 <$> recurseSndTest -- This test terminates because we are able to deconstruct the first arg
              -- , takeNS 2 <$> recurseFstTest -- This test never terminates
              , takeNS 2 <$> recurseFstImmTest
              ]
    where
        canTest1 = Var 2 === Atom "1"
        recurseFstImmTest = disj (Immature <$> recurseFstImmTest) (fresh (=== Atom "7"))
        recurseSndTest = disj (fresh (=== Atom "7")) recurseSndTest
        recurseFstTest = disj recurseFstTest (fresh (=== Atom "7"))

        takeNS :: Int -> Stream a -> Stream a
        takeNS i _ | i < 1 = Nil
        takeNS _ Nil          = Nil
        takeNS i (Immature s) = takeNS i s
        takeNS i (x `Cons` xs) = x `Cons` takeNS (i - 1) xs

runTests :: State -> [Goal] -> IO ()
runTests _ [] = return ()
runTests s (g:gs) = do
    print $ g s
    runTests s gs

main :: IO ()
main = do
    runTests initialState tests
    runTests cyclicState tests
    runTests cyclicState cyclicTests

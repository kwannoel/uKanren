{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}

module Main where

import           Control.Applicative (Alternative (..))
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


walk :: Term -> Subst -> Term
walk (Var v) s = case lookup v s of Nothing -> Var v
                                    Just a  -> walk a s
walk t _ = t

extS :: Var -> Term -> Subst -> Subst
extS v t s = (v, t) : s

(===) :: Term -> Term -> Goal
(===) t1 t2 = \(s, vc) -> case unify t1 t2 s of
    Nothing -> Nil
    Just s' -> return (s', vc)

unify :: Term -> Term -> Subst -> Maybe Subst
unify t1 t2 s = go (walk t1 s) (walk t2 s)
    where
        go (Atom a1) (Atom a2) | a1 == a2 = Just s
        go (Var v1) (Var v2) | v1 == v2 = Just s
        go (Var v1) t2' = Just $ extS v1 t2' s
        go t1' (Var v2) = Just $ extS v2 t1' s
        go (Pair u1 u2) (Pair v1 v2) = unify u1 v1 s >>= unify u2 v2
        go _ _ = Nothing -- Short circuit if we fail to unify

fresh :: (Term -> Goal) -> Goal
fresh f = \(s, c) -> f (Var c) (s, c + 1)

disj :: Goal -> Goal -> Goal
disj g1 g2 = \s -> g1 s `mplus` g2 s

conj :: Goal -> Goal -> Goal
conj g1 g2 = \s -> g1 s >>= g2

--------- Auxiliary functions and types

data Stream a = Nil
              | Cons a (Stream a)
              | Delayed (Stream a) deriving (Eq, Show)

instance Monad Stream where
    return = pure
    Nil >>= _         = Nil
    x `Cons` xs >>= f = f x `mplus` (xs >>= f)
    Delayed s >>= f   = Delayed (s >>= f)

instance MonadPlus Stream where
    mzero = empty
    mplus = (<|>)

-- Interleaving rather than appending 2 streams
instance Alternative Stream where
    empty = Nil
    Nil <|> xs           = xs
    (x `Cons` xs) <|> ys = x `Cons` (ys <|> xs)
    Delayed xs <|> ys    = Delayed (ys <|> xs)

-- Unused, just to satisfy class constraints for Monad
instance Functor Stream where
    fmap _ Nil          = Nil
    fmap f (a `Cons` s) = f a `Cons` fmap f s
    fmap f (Delayed s)  = Delayed (fmap f s)

-- Unused, just to satisfy class constraints for Monad
instance Applicative Stream where
    pure a = a `Cons` empty
    Nil <*> _            = Nil
    _ <*> Nil            = Nil
    (f `Cons` fs) <*> as = fmap f as <|> (fs <*> as)
    Delayed fs <*> as    = Delayed (fs <*> as)


-- Annotate recursive goals with this
delay :: Goal -> Goal
delay = fmap Delayed

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
              , takeS 2 <$> recurseDelayedFstTest -- This test terminates since recursive parts are delayed
              , takeS 2 <$> recurseDelayedSndTest -- This test terminates since recursive parts are delayed
              -- , takeS 2 <$> recurseFstTest -- This test never terminates
              -- , takeS 2 <$> recurseSndTest -- This test never terminates
              ]
    where
        canTest1 = Var 2 === Atom "1"
        recurseFstTest = disj recurseFstTest (fresh (=== Atom "7"))
        recurseSndTest = disj (fresh (=== Atom "7")) recurseSndTest
        recurseDelayedFstTest = disj (delay recurseDelayedFstTest) (fresh (=== Atom "7"))
        recurseDelayedSndTest = disj (fresh (=== Atom "7")) (delay recurseDelayedSndTest)

takeS :: Int -> Stream a -> Stream a
takeS 0 _             = Nil
takeS n Nil           = Nil
takeS n (Delayed s)   = Delayed (takeS n s)
takeS n (a `Cons` as) = a `Cons` takeS (n - 1) as

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

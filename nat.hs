import           MicroKanren
import           MiniKanren  (run)

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


main :: IO ()
main = putStrLn $ prettyPrintResults $ takeS 4 $ run nat initialState

-- f = \x -> nat x `conj` (x === suc (suc (suc nil))) -- does not evaluate, a bug??

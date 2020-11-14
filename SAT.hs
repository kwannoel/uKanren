-- | Example of a SAT solver
-- stack runghc SAT.hs ./microKanren.hs

module SAT where

import           MicroKanren
import           Prelude     hiding (and, not, or)

true :: Term
true = Atom "True"

false :: Term
false = Atom "False"

and :: Term -> Term -> Term
and (Atom "True") (Atom "True") = true
and _ _                         = false

or :: Term -> Term -> Term
or (Atom "False") (Atom "False") = false
or _ _                           = true

not :: Term -> Term
not (Atom "true") = Atom "False"
not _             = Atom "true"

satSolver :: Goal
satSolver =
  fresh $ \q ->
    fresh $ \x ->
      fresh $ \y ->
          let twoSat = (x `or` y) `and` (x `or` y)
--              twoSat = (x `and` y) `or` (x `or` y)
--              twoSat = (x `and` y) `and` (not x `and` not y)
              bindings = (((x === true) `disj` (x === false))
                           `conj`
                             ((y === true) `disj` (y === false)))
              -- | Our expression we want to solve has to evaluate to true
          in (twoSat === q) `conj` (q === true)
               `conj`
                 bindings
                -- | Bind True, False to x and y

printRes :: Stream State -> String
printRes Nil = "False"
printRes _   = "True"

main :: IO ()
main = putStrLn $ printRes $ satSolver initialState

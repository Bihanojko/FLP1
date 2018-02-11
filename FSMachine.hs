-- dka-2-mka
-- Funkcionalni a logicke programovani
-- Nikola Valesova, xvales02

module FSMachine where

-- data type of states
type TState = String
-- data type of symbols
type TSymbol = Char


-- data type of transitions
-- delta(q, a) = p -> fromState = q, withSymbol = a, toState = p
data Transition = Trans
    { fromState :: TState,
      withSymbol :: TSymbol,
      toState :: TState
    } deriving (Eq)


-- data type of a FSM
-- (Q, Sigma, delta, q, F) == (states, alphabet, transitions, startState, endStates)
data FSMachine = FSM
    { states :: [TState],
      alphabet :: [TSymbol],
      transitions :: [Transition],
      startState :: TState,
      endStates :: [TState]
    } deriving (Eq)

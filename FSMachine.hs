module FSMachine where

type TInput = String
type TState = String
type TSymbol = Char


data Transition = Trans
    { fromState :: TState
    , withSymbol :: TSymbol
    , toState :: TState
    } deriving (Eq)


data FSMachine = FSM
    { states :: [TState]
    , alphabet :: [TSymbol]
    , transitions :: [Transition]
    , startState :: TState
    , endStates :: [TState]
    } deriving (Eq)

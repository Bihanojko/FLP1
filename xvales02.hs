import System.Environment
import Data.List
-- import System.IO
-- import System.IO.Error
-- import System.Exit
import System.Directory
import Data.List.Split

import FSMachine


main :: IO ()
main = do
    args <- getArgs
    let (process, filename) = parseArgs args
    inputContent <- getInput filename
    let fSMachine = parseContent (lines inputContent)

    -- if process then minimalizeFSM fSMachine
    if process then printFSMachine $ minimalizeFSM fSMachine
    else printFSMachine fSMachine



printFSMachine :: FSMachine -> IO ()
printFSMachine fsm = do
    putStrLn (intercalate "," (states fsm))
    -- putStrLn (alphabet fsm)
    putStrLn (startState fsm)
    putStrLn (intercalate "," (endStates fsm))   
    mapM_ printTransition (transitions fsm)



printTransition :: Transition -> IO ()
printTransition transition =
    putStrLn $ fromState transition ++ "," ++ [withSymbol transition] ++ "," ++ toState transition



parseArgs :: [String] -> (Bool, String)
parseArgs [] = error "Error in input arguments. Usage: ./dka-2-mka [-i|-t] [filename]"
parseArgs [x]
    | x == "-i" = (False, "None")
    | x == "-t" = (True, "None")
    | otherwise = error "Error in input arguments. Usage: ./dka-2-mka [-i|-t] [filename]"
parseArgs [x, y]
    | x == "-i" = (False, y)
    | x == "-t" = (True, y)
    | otherwise = error "Error in input arguments. Usage: ./dka-2-mka [-i|-t] [filename]"
parseArgs _ = error "Error in input arguments. Usage: ./dka-2-mka [-i|-t] [filename]"



getInput :: String -> IO String
getInput filename
    | filename == "None" =
        getContents
    | otherwise = do
        correctFilename <- doesFileExist filename
        if correctFilename
            then
                readFile filename
            else error "The input file does not exist!"



parseContent :: [String] -> FSMachine
parseContent (states : startState : finalStates : transitions) =
    if null transitions
        then error "no transitions"
        else FSM getStates (getAlph transitions) (getTrans transitions) startState getFinalStates
    where
        getStates = splitOn "," states
        getFinalStates = splitOn "," finalStates
        getAlph transitions = nub [getSymbol x | x <- transitions, length (splitOn "," x) /= 1]
        getSymbol transition = head (splitOn "," transition !! 1)
        getTrans transitions = [getRule x | x <- transitions, length (splitOn "," x) /= 1]
        getRule rule = getRule2 (splitOn "," rule)
        getRule2 [q1, [sym], q2] = Trans q1 sym q2
        getRule2 _ = error "bad transition syntax"
parseContent _ = error "bad syntax"


minimalizeFSM :: FSMachine -> FSMachine
minimalizeFSM fSMachine = do
    let completeFSM = getCompleteFSM fSMachine
    let prevInd = compute0Indistinguishability (states completeFSM) (endStates completeFSM)
    let result = computeNewKInd completeFSM prevInd
    -- printFSMachine $ reduceFSM completeFSM result
    reduceFSM completeFSM result
    where
        computeNewKInd fSMachine prevInd = do
            let nextInd = computeKIndistinguishability fSMachine prevInd
            if prevInd /= nextInd then computeNewKInd fSMachine nextInd
            else nextInd
        reduceFSM fSMachine result = fSMachine {
            states = sort [minimum x | x <- result],
            transitions = sortOn fromState $ sortOn withSymbol $ nub [renameStates x result | x <- transitions fSMachine],
            startState = getHead [getHead (sort x) | x <- result, startState fSMachine `elem` x],
            endStates = sort $ nub [getHead (sort x) | x <- result, y <- endStates fSMachine, y `elem` x]
        }
        renameStates transition result = transition {
            fromState = getHead [getHead (sort x) | x <- result, fromState transition `elem` x],
            toState = getHead [getHead (sort x) | x <- result, toState transition `elem` x]
        }


getCompleteFSM :: FSMachine -> FSMachine
getCompleteFSM fSMachine =
    if isFSMComplete fSMachine then fSMachine
    else completeFSM fSMachine


isFSMComplete :: FSMachine -> Bool
isFSMComplete fSMachine =
    length (transitions fSMachine) == allTransitionsCount fSMachine
    where 
        allTransitionsCount fSMachine = length (states fSMachine) * length (alphabet fSMachine)


completeFSM :: FSMachine -> FSMachine
completeFSM fSMachine = do
    let sinkState = getSink (states fSMachine)
    let missingTransitions = getMissingTransitions fSMachine sinkState
    let newTransitions = [Trans x y sinkState | (x, y) <- missingTransitions]
    updateFSM fSMachine (sinkState : states fSMachine) (transitions fSMachine ++ newTransitions)
    where
        updateFSM x allStates allTrans = x {states = sort allStates, transitions = sorted allTrans}
        sorted allTrans = sortOn fromState $ sortOn withSymbol allTrans


getSink :: [TState] -> TState
getSink states = do
    let statesInt = [read x :: Int | x <- states]
    show (head ([0..length states + 1] \\ statesInt)) :: TState


getMissingTransitions :: FSMachine -> TState -> [(TState, TSymbol)]
getMissingTransitions fSMachine sinkState = do
    let allTransitions = [(state, symbol) | state <- sinkState : states fSMachine, symbol <- alphabet fSMachine]
    let definedTransitions = [(fromState x, withSymbol x) | x <- transitions fSMachine]    
    allTransitions \\ definedTransitions
    

compute0Indistinguishability :: [TState] -> [TState] -> [[TState]]
compute0Indistinguishability states endStates = endStates : [states \\ endStates]


computeKIndistinguishability :: FSMachine -> [[TState]] -> [[TState]]
computeKIndistinguishability fSMachine prevInd = do
    let endStateTable = [getEndStates x fSMachine prevInd | x <- prevInd]
    nub [x !! idx | x <- map splitGroup endStateTable, idx <- [0..length x - 1]]
    where
        getEndStates oneGroup fSMachine prevInd = [getEndStates2 x fSMachine prevInd | x <- oneGroup]
        getEndStates2 state fSMachine prevInd = (state, [getEndState state x fSMachine prevInd | x <- alphabet fSMachine])
        getEndState state symbol fSMachine prevInd = getHead [getGroup (toState x) prevInd | x <- transitions fSMachine, fromState x == state, withSymbol x == symbol]
        getGroup state prevInd = [x | x <- [0..length prevInd - 1], state `elem` prevInd !! x]

        splitGroup targets = map (getOneGroup targets) (nub [y | (x, y) <- targets])
        getOneGroup targets sequence = [x | (x, y) <- targets, y == sequence]


getHead (x:xs) = x
getHead [] = []

-- TODO remove unused imports, split lines longer than 100 chars
-- TODO README, okomentovat, testy a skript taky prilozit a popsat v README, otestovat na merlinovi
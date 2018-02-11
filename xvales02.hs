-- dka-2-mka
-- Funkcionalni a logicke programovani
-- Nikola Valesova, xvales02

import System.Environment
import Data.List
import System.IO
import System.IO.Error
import System.Exit
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
            transitions = sorted $ nub [renameStates x result | x <- transitions fSMachine],
            startState = getHead [minimum x | x <- result, startState fSMachine `elem` x],
            endStates = sort(nub [minimum x | x <- result, y <- endStates fSMachine, y `elem` x])
        }
        renameStates transition result = transition {
            fromState = getHead [getHead (sort x) | x <- result, fromState transition `elem` x],
            toState = getHead [getHead (sort x) | x <- result, toState transition `elem` x]
        }


sorted :: [Transition] -> [Transition]
sorted transitions = sortOn fromState $ sortOn withSymbol transitions


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


getSink :: [TState] -> TState
getSink states = do
    let statesInt = [read x :: Int | x <- states]
    show (head ([0..length states + 1] \\ statesInt)) :: TState


getMissingTransitions :: FSMachine -> TState -> [(TState, TSymbol)]
getMissingTransitions fSM sinkState = do
    let allT = [(state, symbol) | state <- sinkState : states fSM, symbol <- alphabet fSM]
    let definedT = [(fromState x, withSymbol x) | x <- transitions fSM]    
    allT \\ definedT
    

compute0Indistinguishability :: [TState] -> [TState] -> [[TState]]
compute0Indistinguishability states endStates = endStates : [states \\ endStates]


computeKIndistinguishability :: FSMachine -> [[TState]] -> [[TState]]
computeKIndistinguishability fSM prevInd = do
    let endStateTable = [getEndStates x fSM prevInd | x <- prevInd]
    nub [x !! idx | x <- map splitGroup endStateTable, idx <- [0..length x - 1]]
    where
        getEndStates oneGroup fSM prevInd = [getEndStates2 x fSM prevInd | x <- oneGroup]
        getEndStates2 q fSM prevI = (q, [endState q x (transitions fSM) prevI | x <- alphabet fSM])
        endState q a trans prevI = getHead [getGroup x prevI | x <- endState2 q a trans]
        endState2 q a trans = [toState x | x <- trans, fromState x == q, withSymbol x == a]
        getGroup state prevInd = [x | x <- [0..length prevInd - 1], state `elem` prevInd !! x]
        splitGroup targets = map (getOneGroup targets) (nub [y | (x, y) <- targets])
        getOneGroup targets sequence = [x | (x, y) <- targets, y == sequence]


getHead (x:xs) = x
getHead [] = []

-- TODO okomentovat
-- TODO prilozit testy a skript, popsat v README

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


-- MAIN
main :: IO ()
main = do
    -- get arguments and process them
    args <- getArgs
    let (process, filename) = parseArgs args
    -- get input content and process it
    inputContent <- getInput filename
    let fSMachine = parseContent (lines inputContent)

    -- if minimalization is set, then minimalize FSM and output it
    if process then printFSMachine $ minimalizeFSM fSMachine
    -- else output FSMachine from its inner representation
    else printFSMachine fSMachine


-- print FSMachine according to given format
printFSMachine :: FSMachine -> IO ()
printFSMachine fsm = do
    putStrLn (intercalate "," (states fsm))
    -- putStrLn (alphabet fsm)
    putStrLn (startState fsm)
    putStrLn (intercalate "," (endStates fsm))   
    mapM_ printTransition (transitions fsm)

 
-- print Transition according to given format
printTransition :: Transition -> IO ()
printTransition transition =
    putStrLn $ fromState transition ++ "," ++ [withSymbol transition] ++ "," ++ toState transition


-- parse given arguments, Bool represents if minimalization should be done, String return filename
-- or "None" when reading from stdin
parseArgs :: [String] -> (Bool, String)
parseArgs [x]
    | x == "-i" = (False, "None")
    | x == "-t" = (True, "None")
    | otherwise = error "Error in input arguments. Usage: ./dka-2-mka [-i|-t] [filename]"
parseArgs [x, y]
    | x == "-i" = (False, y)
    | x == "-t" = (True, y)
    | otherwise = error "Error in input arguments. Usage: ./dka-2-mka [-i|-t] [filename]"
parseArgs _ = error "Error in input arguments. Usage: ./dka-2-mka [-i|-t] [filename]"


-- get input content from given file or stdin
getInput :: String -> IO String
getInput filename
    -- reading from stdin
    | filename == "None" =
        getContents
    -- reading from file, test if file exists and get content
    | otherwise = do
        correctFilename <- doesFileExist filename
        if correctFilename then readFile filename
        else error "The input file does not exist!"


-- parse input file and return a filled FSMachine instance
-- TODO
parseContent :: [String] -> FSMachine
parseContent (states : startState : finalStates : transitions) =
    FSM getStates getAlph getTrans startState getFinalStates
    where
        getStates = splitOn "," states
        getFinalStates = splitOn "," finalStates
        getAlph = nub [getSymbol x | x <- transitions, length (splitOn "," x) /= 1]
        getSymbol transition = head (splitOn "," transition !! 1)
        getTrans = [getRule x | x <- transitions, length (splitOn "," x) /= 1]
        getRule rule = getRule2 (splitOn "," rule)
        getRule2 [q1, [sym], q2] = Trans q1 sym q2
        getRule2 _ = error "bad transition syntax"
parseContent _ = error "bad syntax"


-- TODO
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


-- sort lists of transitions according to symbols and origin states
sorted :: [Transition] -> [Transition]
sorted transitions = sortOn fromState $ sortOn withSymbol transitions


-- if FSMachine is not complete, return a fully defined FSMachine
getCompleteFSM :: FSMachine -> FSMachine
getCompleteFSM fSMachine =
    if isFSMComplete fSMachine then fSMachine
    else completeFSM fSMachine


-- test if FSMachine is complete == contains all possible transitions
isFSMComplete :: FSMachine -> Bool
isFSMComplete fSMachine =
    length (transitions fSMachine) == allTransitionsCount
    where 
        allTransitionsCount = length (states fSMachine) * length (alphabet fSMachine)


-- transform a not fully defined FSM to a fully defined one
completeFSM :: FSMachine -> FSMachine
completeFSM fSMachine = do
    let sinkState = getSink (states fSMachine)
    let missingTransitions = getMissingTransitions fSMachine sinkState
    let newTransitions = [Trans x y sinkState | (x, y) <- missingTransitions]
    updateFSM fSMachine (sinkState : states fSMachine) (transitions fSMachine ++ newTransitions)
    where
        updateFSM x allStates allTrans = x {states = sort allStates, transitions = sorted allTrans}


-- get name for additional sink state == the lowest number (0, 1, ...) not yet used
getSink :: [TState] -> TState
getSink states = do
    let statesInt = [read x :: Int | x <- states]
    show (head ([0..length states + 1] \\ statesInt)) :: TState


-- get a list of tuples of missing rules, [(q, a)] from delta(q, a)
getMissingTransitions :: FSMachine -> TState -> [(TState, TSymbol)]
getMissingTransitions fSM sinkState = do
    let allT = [(state, symbol) | state <- sinkState : states fSM, symbol <- alphabet fSM]
    let definedT = [(fromState x, withSymbol x) | x <- transitions fSM]    
    allT \\ definedT


-- get 0-indistinguishability == split states to accept and non-accept states  
compute0Indistinguishability :: [TState] -> [TState] -> [[TState]]
compute0Indistinguishability states endStates = [x | x <- getGroups, x /= [""]]
    where
        getGroups = endStates : [states \\ endStates]


-- TODO
computeKIndistinguishability :: FSMachine -> [[TState]] -> [[TState]]
computeKIndistinguishability fSM prevInd = do
    let endStateTable = [getEndStates x | x <- prevInd]
    nub [x !! idx | x <- map splitGroup endStateTable, idx <- [0..length x - 1]]
    where
        getEndStates oneGroup = [getEndStates2 x fSM prevInd | x <- oneGroup]
        getEndStates2 q fSM prevI = (q, [endState q x (transitions fSM) prevI | x <- alphabet fSM])
        endState q a trans prevI = getHead [getGroup x prevI | x <- endState2 q a trans]
        endState2 q a trans = [toState x | x <- trans, fromState x == q, withSymbol x == a]
        getGroup state prevInd = [x | x <- [0..length prevInd - 1], state `elem` prevInd !! x]
        splitGroup targets = map (getOneGroup targets) (nub [y | (x, y) <- targets])
        getOneGroup targets sequence = [x | (x, y) <- targets, y == sequence]


-- get first element of a list, return empty list if list is empty
getHead (x:xs) = x
getHead [] = []

-- TODO okomentovat
-- TODO prilozit testy a skript, popsat v README

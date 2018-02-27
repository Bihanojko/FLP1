-- dka-2-mka
-- Funkcionalni a logicke programovani
-- Nikola Valesova, xvales02

import System.Environment
import Data.List
import System.Directory
import Data.List.Split

import FSMachine


-- error message printed when given arguments are not valid
errorMessageArgs = "Error in input arguments. Usage: ./dka-2-mka [-i|-t] [filename]" :: String
-- error message printed when a bad syntax in transitions is found
errorMessageSyntax = "Error in parsing input file due to bad syntax" :: String


-- MAIN
main :: IO ()
main = do
    -- get arguments and process them
    args <- getArgs
    let (process, filename) = parseArgs args
    -- get input content and process it
    inputContent <- getInput filename
    let fSMachine = checkFSMachine $ parseContent (lines inputContent)

    -- if minimalization is set, then minimalize FSM and output it
    if process then printFSMachine $ minimalizeFSM fSMachine
    -- else output FSMachine from its inner representation
    else printFSMachine fSMachine
    where
        -- checks FSMachine for non-defined states
        checkFSMachine fSMachine = do
            let results = map (isStateValid (states fSMachine)) (getAllUsedStates fSMachine)
            if False `elem` results then error "Invalid input file!"
            else fSMachine


-- get a list of all states used in FSMachine (start, end, in transitions)
getAllUsedStates :: FSMachine -> [TState]
getAllUsedStates fSMachine = do
    let transitionStates = [f x | x <- transitions fSMachine, f <- [fromState, toState]]
    filter (/= "") $ nub([startState fSMachine] ++ endStates fSMachine ++ transitionStates)


-- check if current state is defined in FSMachine states
isStateValid :: [TState] -> TState -> Bool
isStateValid = flip elem


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
    | otherwise = error errorMessageArgs
parseArgs [x, y]
    | x == "-i" = (False, y)
    | x == "-t" = (True, y)
    | otherwise = error errorMessageArgs
parseArgs _ = error errorMessageArgs


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
parseContent :: [String] -> FSMachine
parseContent (states : startState : finalStates : transitions) =
    FSM getStates getAlph getTrans checkStartState getFinalStates
    where
        getStates = splitOn "," states
        checkStartState = if startState == "" then error "Missing start state!"; 
                          else startState;
        getFinalStates = splitOn "," finalStates
        -- create unique list of symbols used in rules
        getAlph = nub [getSymbol x | x <- transitions, length (splitOn "," x) /= 1]
        getSymbol transition = head (splitOn "," transition !! 1)
        -- get list of transitions
        getTrans = [getRule x | x <- transitions]
        getRule rule = getRule2 (splitOn "," rule)
        getRule2 [q1, [sym], q2] = Trans q1 sym q2
        getRule2 _ = error errorMessageSyntax
parseContent _ = error errorMessageSyntax


-- make FSMachine complete and then minimalized
minimalizeFSM :: FSMachine -> FSMachine
minimalizeFSM fSMachine = do
    -- get complete FSMachine
    let completeFSM = getCompleteFSM fSMachine
    -- compute 0-indistinguishability
    let prevInd = compute0Indistinguishability (states completeFSM) (endStates completeFSM)
    -- compute final k-indistinguishability
    let result = computeNewKInd completeFSM prevInd
    -- reduce input FSMachine according to final indistinguishability relation
    reduceFSM completeFSM result
    where
        -- while k+1-indistinguishability is different from k-indistinguishability, 
        -- compute next indistinguishability relation
        computeNewKInd fSMachine prevInd = do
            let nextInd = computeKIndistinguishability fSMachine prevInd
            if prevInd /= nextInd then computeNewKInd fSMachine nextInd
            else nextInd


-- minimalize FSMachine according to final indistinguishability relation
reduceFSM :: FSMachine -> [[TState]] -> FSMachine
reduceFSM fSMachine result = fSMachine {
        -- set states as the lowest member from each group
        states = sort [minimum x | x <- result],
        -- set transitions as unique list of renamed original transitions        
        transitions = sorted $ nub [renameStates x | x <- transitions fSMachine],
        -- set start state as the lowest member from the group that contains original start state        
        startState = getHead [minimum x | x <- result, startState fSMachine `elem` x],
        -- set end states as the lowest member from each group containing any of original end states
        endStates = sort(nub [minimum x | x <- result, y <- endStates fSMachine, y `elem` x])
    }
    where
        -- rename origin and goal states of given transition
        renameStates transition = transition {
            fromState = getHead [getHead (sort x) | x <- result, fromState transition `elem` x],
            toState = getHead [getHead (sort x) | x <- result, toState transition `elem` x]
        }


-- sort lists of transitions according to symbols and origin states
sorted :: [Transition] -> [Transition]
sorted = sortOn fromState . sortOn withSymbol


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
    -- get name for sink state
    let sinkState = getSink (states fSMachine)
    -- get list of missing transitions (fromState, withSymbol)
    let missingTransitions = getMissingTransitions fSMachine sinkState
    -- create a list of transitions that were missing with toState == sinkState
    let newTransitions = [Trans x y sinkState | (x, y) <- missingTransitions]
    -- add sink state and new transitions to FSMachine
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


-- compute (k+1)-indistinguishability from k-indistinguishability (prevInd)
computeKIndistinguishability :: FSMachine -> [[TState]] -> [[TState]]
computeKIndistinguishability fSM prevInd = do
    -- create a "table" of next states after transition execution for every group in prevInd
    let endStateTable = [getEndStates x | x <- prevInd]
    -- split every group in prevInd to smaller groups according to endStateTable
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
getHead :: [[t]] -> [t]
getHead (x:xs) = x
getHead [] = []

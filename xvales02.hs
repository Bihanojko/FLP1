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

    -- print process
    -- print filename
    -- print inputContent
    -- printFSMachine fSMachine

    if process then minimalizeFSM fSMachine
    else printFSMachine fSMachine



printFSMachine :: FSMachine -> IO ()
printFSMachine fsm = do
    putStrLn (concat (intersperse "," (states fsm)))
    -- putStrLn (alphabet fsm)
    putStrLn (startState fsm)
    putStrLn (concat (intersperse "," (endStates fsm)))   
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
        else FSM getStates (getAlph transitions) (map getRule transitions) startState getFinalStates
    where
        getStates = splitOn "," states
        getFinalStates = splitOn "," finalStates
        getAlph transitions = nub (map getSymbol transitions)
        getSymbol transition = head (splitOn "," transition !! 1)
        getRule rule = getRule2 (splitOn "," rule)
        getRule2 [q1, [sym], q2] = Trans q1 sym q2
        getRule2 _ = error "bad transition syntax"
parseContent _ = error "bad syntax"


-- TODO
minimalizeFSM :: FSMachine -> IO ()
minimalizeFSM fSMachine = do
    let completeFSM = getCompleteFSM fSMachine
    let prevInd = compute0Indistinguishability (states completeFSM) (endStates completeFSM)
    -- let nextIndistinguishability = computeKIndistinguishability completeFSM prevInd
    print prevInd
    -- print nextIndistinguishability
    let xxx = [getEndStates x fSMachine prevInd | x <- prevInd]
    print xxx
    where
        getEndStates oneGroup fSMachine prevInd = [getEndStates2 oneGroup x fSMachine prevInd | x <- (alphabet fSMachine)]
        getEndStates2 oneGroup symbol fSMachine prevInd = [getEndState x symbol fSMachine prevInd | x <- oneGroup]
        getEndState state symbol fSMachine prevInd = head [getGroup (toState x) prevInd | x <- (transitions fSMachine), (fromState x) == state, (withSymbol x) == symbol]
        getGroup state prevInd = head [x | x <- [0, length prevInd - 1], state `elem` prevInd !! x]


getCompleteFSM :: FSMachine -> FSMachine
getCompleteFSM fSMachine = do
    if (isFSMComplete fSMachine) then fSMachine
    else completeFSM fSMachine


isFSMComplete :: FSMachine -> Bool
isFSMComplete fSMachine =
    if length (transitions fSMachine) == (allTransitionsCount fSMachine) then True
    else False
    where 
        allTransitionsCount fSMachine = length (states fSMachine) * length (alphabet fSMachine)


completeFSM :: FSMachine -> FSMachine
completeFSM fSMachine = do
    let missingTransitions = getMissingTransitions fSMachine
    let sinkState = getSink (states fSMachine)
    let newTransitions = [Trans x y sinkState | (x, y) <- missingTransitions]
    updateFSM fSMachine (sinkState : states fSMachine) ((transitions fSMachine) ++ newTransitions)
    where
        updateFSM x allStates allTrans = x {states = sort allStates, transitions = allTrans}


getSink :: [TState] -> TState
getSink states = do
    let statesInt = [read x :: Int | x <- states]
    show (head ([0..1000] \\ statesInt)) :: TState


getMissingTransitions :: FSMachine -> [(TState, TSymbol)]
getMissingTransitions fSMachine = do
    let allTransitions = [(state, symbol) | state <- states fSMachine, symbol <- alphabet fSMachine]
    let definedTransitions = [(fromState x, withSymbol x) | x <- transitions fSMachine]    
    allTransitions \\ definedTransitions
    

compute0Indistinguishability :: [TState] -> [TState] -> [[TState]]
compute0Indistinguishability states endStates = endStates : [states \\ endStates]


-- TODO
-- computeKIndistinguishability :: FSMachine -> [[TState]] -> [[TState]]
-- computeKIndistinguishability fSMachine prevInd = do
--     let xxx = [todo x (alphabet fSMachine) | x <- prevInd]
--     xxx
--     where
--         todo oneGroup alphabet = [todo2 x alphabet | x <- oneGroup]
--         todo2 state alphabet = [state ++ [x] | x <- alphabet]
--         -- todo2 oneGroup withSymbol = [x ++ [withSymbol] | x <- oneGroup]        
--     -- [todo x (alphabet fSMachine) | x <- prevInd]
--     -- [todo x y | x <- prevInd, y <- (alphabet fSMachine)]
--     -- prevInd

-- -- todo :: [[TState]] -> TSymbol -> [[TState]]
-- -- -- todo :: [TState] -> [TSymbol] -> [TState]
-- -- todo prevInd withSymbol = [x ++ [withSymbol] | x <- prevInd]
-- -- -- todo oneGroup alphabet = [x ++ [y] | x <- oneGroup, y <- alphabet]

-- TODO remove unused functions and imports
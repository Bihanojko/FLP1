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
    putStrLn (alphabet fsm)
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
        -- else FSM getStates (map getRule transitions) (read startState :: Int) getFinalStates
    where
        getStates = splitOn "," states
        -- getStates = map (read :: String -> Int) (splitOn "," states)
        getAlph transitions = nub (map getSymbol transitions)
        getSymbol transition = head (splitOn "," transition !! 1)
        getRule rule = getRule2 (splitOn "," rule)
        getRule2 :: [String] -> Transition
        getRule2 [q1, [sym], q2] =
        -- Trans (read q1 :: Int) sym (read q2 :: Int)
            Trans q1 sym q2
        getRule2 _ = error "bad transition syntax"
        getFinalStates = splitOn "," finalStates
        -- getFinalStates = map (read :: String -> Int) (splitOn "," finalStates)
parseContent _ = error "bad syntax"



minimalizeFSM :: FSMachine -> IO ()
minimalizeFSM fSMachine = do
    let prevIndistinguishability = compute0Indistinguishability (states fSMachine) (endStates fSMachine)
    let nextIndistinguishability = computeKIndistinguishability (transitions fSMachine) prevIndistinguishability
    print prevIndistinguishability
    print nextIndistinguishability



compute0Indistinguishability :: [TState] -> [TState] -> [[TState]]
compute0Indistinguishability states endStates = endStates : [states \\ endStates]



computeKIndistinguishability :: [Transition] -> [[TState]] -> [[TState]]
computeKIndistinguishability transitions prevInd = do
    let x = map todo prevInd
    x

todo :: [TState] -> [TState]
todo listStates = listStates

-- TODO zuplnit KA pred minimalizaci, check vstup, zda je validni
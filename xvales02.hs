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
    let (fSMachine) = parseContent (lines inputContent)

    -- print process
    -- print filename
    -- print inputContent
    -- printFSMachine fSMachine

    print (computeKIndistinguishability fSMachine 2)

    -- if process then minimalizeFSM fSMachine
    -- else printFSMachine fSMachine

    return ()



printFSMachine :: FSMachine -> IO ()
printFSMachine fsm = do
    putStrLn (concat (intersperse "," (states fsm)))
    -- putStrLn (alphabet fsm)
    putStrLn (startState fsm)
    putStrLn (endStates fsm)
    mapM_ printTransition (transitions fsm)



printTransition :: Transition -> IO ()
printTransition transition = do
    putStrLn $ fromState transition ++ "," ++ [(withSymbol transition)] ++ "," ++ (toState transition)   



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
    | filename == "None" = do
        content <- hGetContents stdin
        return content
    | otherwise = do
        correctFilename <- doesFileExist filename
        if correctFilename
            then do 
                content <- readFile filename
                return content
            else error "The input file does not exist!"



parseContent :: [String] -> FSMachine
parseContent (states:startState:finalStates:transitions) =
    if null transitions
        then error "no transitions"
        else FSM getStates (map getRule transitions) startState finalStates
        -- else FSM getStates getAlph (map getRule transitions) startState finalStates
    where
        getStates = splitOn "," states
        -- getAlph = "we do not really care for alphabet"
        getRule rule = getRule2 (splitOn "," rule)
        getRule2 :: [String] -> Transition
        getRule2 [q1, [sym], q2] =
            Trans q1 sym q2
        getRule2 _ = error "bad transition syntax"
parseContent _ = error "bad syntax"



-- minimalizeFSM :: FSMachine -> Int
-- minimalizeFSM fSMachine =
--     computeKIndistinguishability 0 1


computeKIndistinguishability :: FSMachine -> Int -> Int
computeKIndistinguishability fSMachine k = k

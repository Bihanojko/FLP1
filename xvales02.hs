import System.Environment
import Data.List
import System.IO
import System.IO.Error
import System.Exit
import System.Directory
import Data.List.Split


main :: IO ()
main = do
    args <- getArgs
    let (process, filename) = parseArgs args
    inputContent <- getInput filename
    let (parsedContent) = parseContent inputContent

    print process
    print filename
    print inputContent
    print parsedContent

    return ()


parseArgs :: [String] -> (Bool, String)
parseArgs args
    | param == "-i" = (False, filename)
    | param == "-t" = (True, filename)
    | otherwise = error "Error in input arguments. Usage: ./dka-2-mka [-i|-t] [filename]"
    where   param = if length args == 1 || length args == 2
                    then args !! 0
                    else ""
            filename =  if length args == 2
                        then args !! 1
                        else "None"


getInput :: String -> IO String
getInput filename
    | filename == "None" = do
        content <- getContents
        return content
        -- return (lines content)
    | otherwise = do
        correctFilename <- doesFileExist filename
        if correctFilename
            then do 
                content <- readFile filename
                return content
                -- return (lines content)
            else error "The input file does not exist!"


parseContent :: String -> [[Char]]
parseContent content = do
    -- ct <- lines content
    ctt <- splitOn "," content
    return ctt

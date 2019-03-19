import System.IO  
import Control.Monad
import Data.List.Split 
import Text.Parsec

readInt :: String -> String
readInt = read

-- "TOK:\tTYPE\tLTOK?\tTEXT\tVAL\tLINE\tCOL\tFEAT"
-- data Token = Token String Bool String String Int Int String

breakStr :: String -> [[String]]
breakStr = (filter ((/=) [""])). (map (splitOn "\t")) . (splitOn "\n")

processRec :: [String] -> (String, Bool, String, String, (Int, Int), String)
processRec [_, typee, lang, text, val, line, col, feat] =
    (typee, b, text, val, (read line, read col), feat)
    where
        b = if lang == "true" then True else False        


stripRedundantFields :: (String, Bool, String, String, (Int, Int), String) -> (String, String, (Int, Int), String) 
stripRedundantFields (typee, b, text, val, lc, feat)  = (typee, text, lc, feat) 
main = do  
        contents <- readFile "test_token_input.txt"
        --print contents
        --print $ breakStr $ contents
        mapM_ print $ map stripRedundantFields . (filter (\(_,b,_,_,_,_) -> b)) . map processRec . breakStr $ contents

        

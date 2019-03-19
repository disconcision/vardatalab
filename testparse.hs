import System.IO  
import Control.Monad
import Data.List.Split 

import Text.Parsec
import Text.Parsec.String (Parser)

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

-- features parsing

data PC = TT | FF | Feature String | Not PC | And [PC] | Or [PC] | Parens Int 
    deriving Show

_pc :: Parser PC
_pc = choice [ (try _and <|> _or), _def, _true, _false, _not]


_true :: Parser PC
_true = do
    void $ string "True"
    return $ TT

_false :: Parser PC
_false = do
    void $ string "False"
    return $ FF    

_def :: Parser PC
_def = do
    void $ string "def"
    void $ char '('
    e <- many1 letter
    void $ char ')'
    return $ Feature $  e

_not :: Parser PC
_not = do
    void $ char '!'
    a <- _pc
    return $ Not a

_and :: Parser PC
_and = do
    void $ char '('
    a <- _pc
    b <- many1 (do
        void $ char '&'
        c <- _pc
        return c)
    void $ char ')'
    return $ And $ a : b

_or :: Parser PC
_or = do
    void $ char '('
    a <- _pc
    b <- many1 (do
        void $ char '|'
        c <- _pc
        return c)
    void $ char ')'
    return $ Or $ a : b


regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

main = do  
        contents <- readFile "test_token_input.txt"
        --print contents
        --print $ breakStr $ contents
        mapM_ print $ map stripRedundantFields . (filter (\(_,b,_,_,_,_) -> b)) . map processRec . breakStr $ contents
        print $ regularParse _def "def(AB)"
        print $ regularParse _and "(True&def(A))"
        print $ regularParse _pc "(def(C)&!def(B))"
        print $ regularParse _pc "(def(C)&def(B)&def(C))"
        print $ regularParse _pc "(def(C)&!def(B)&def(B)&def(C))"
        print $ regularParse _pc "(def(C)&(def(B)&def(C)))"
        print $ regularParse _pc "(def(C)|def(D))"
        print $ regularParse _pc "(def(C)&!def(B)&(def(B)|def(C)))"
        print $ regularParse _pc "(!def(B)&!def(C)&(!def(C)|def(B)|(!def(B)&!def(C))))"

        

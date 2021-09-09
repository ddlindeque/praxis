module Praxis.Lexer
(
    Location,
    Token (..),
    tokenise
)
where

import Debug.Trace

type Location = (Int, Int, Int, Int)

data Token = 
      Identifier Location String
    | Comment Location
    | String Location String
    | KwAnalyser Location
    | KwScanner Location
    | KwProcessing Location
    | KwIgnoring Location
    | KwReturn Location
    | KwGoto Location
    | KwError Location
    | OpenBracket Location
    | CloseBracket Location
    | OpenCurly Location
    | CloseCurly Location
    | Comma Location
    | SemiColon Location
    | Arrow Location
    | Hat Location
    | Dot Location
    | Asterisk Location
    | Plus Location
    | Question Location
    | OpenParenthesis Location
    | CloseParenthesis Location
    | Pipe Location
    | ReStart Location
    | ReEnd Location
    | ReCharacter Location Char
    | ReHat Location
    | ReDot Location
    | ReAsterisk Location
    | RePlus Location
    | ReQuestion Location
    | ReOpenBracket Location
    | ReCloseBracket Location
    | ReOpenParenthesis Location
    | ReCloseParenthesis Location
    | ReHyphen Location
    | ReNewLine Location
    | ReReturn Location
    | ReTab Location
    | ReFormFeed Location
    | ReWhitespaceClass Location
    | ReDigitClass Location
    | ReAlphaNumClass Location
    | ReOr Location
    | Error Location String
    deriving (Show)

keyword :: Location -> String -> Token
keyword loc "analyser" = KwAnalyser loc
keyword loc "scanner" = KwScanner loc
keyword loc "processing" = KwProcessing loc
keyword loc "ignoring" = KwIgnoring loc
keyword loc "return" = KwReturn loc
keyword loc "goto" = KwGoto loc
keyword loc "error" = KwError loc
keyword loc ('@':value) = Identifier loc value
keyword loc value = Identifier loc value

tokenise' :: Int -> Location -> String -> String -> [Token] -- State Location CurrentTokenText Text
tokenise' 0 _ _ [] = []
tokenise' 0 (_, _, eline, ecol) _ (' ':xs) = tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs
tokenise' 0 (_, _, eline, ecol) _ ('\t':xs) = tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs
tokenise' 0 (_, _, eline, ecol) _ ('\n':xs) = tokenise'' 0 (eline+1, 1, eline+1, 1) "" xs
tokenise' 0 (_, _, eline, ecol) _ ('\r':xs) = tokenise'' 0 (eline, 1, eline, 1) "" xs
tokenise' 0 (sline, scol, eline, ecol) _ ('\'':xs) = (ReStart (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ ('#':xs) = tokenise'' 3 (sline, scol, eline, ecol+1) "" xs
tokenise' 0 (sline, scol, eline, ecol) _ ('[':xs) = (OpenBracket (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ (']':xs) = (CloseBracket (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ ('{':xs) = (OpenCurly (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ ('}':xs) = (CloseCurly (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ (',':xs) = (Comma (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ (';':xs) = (SemiColon (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ ('^':xs) = (Hat (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ ('.':xs) = (Dot (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ ('*':xs) = (Asterisk (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ ('+':xs) = (Plus (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ ('?':xs) = (Question (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ ('(':xs) = (OpenParenthesis (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ (')':xs) = (CloseParenthesis (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ ('|':xs) = (Pipe (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ ('-':'>':xs) = (Arrow (sline, scol, eline, ecol+2)):(tokenise'' 0 (eline, ecol+2, eline, ecol+2) "" xs)
tokenise' 0 (sline, scol, eline, ecol) _ ('"':xs) = tokenise'' 5 (sline, scol, eline, ecol+1) "" xs
tokenise' 0 (sline, scol, eline, ecol) _ ('@':xs) = tokenise'' 4 (sline, scol, eline, ecol+1) "@" xs
tokenise' 0 (sline, scol, eline, ecol) _ (x:xs)
    | (x>='a' && x<='z') || (x>='A' && x<='Z') || x=='_' = tokenise'' 4 (sline, scol, eline, ecol+1) [x] xs
tokenise' 1 (sline, scol, eline, ecol) _ [] = [Error (sline, scol, eline, ecol) "Unexpected end-of-data while reading a regular expression"]
tokenise' 1 (sline, scol, eline, ecol) _ ('^':xs) = (ReHat (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ ('.':xs) = (ReDot (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ ('*':xs) = (ReAsterisk (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ ('+':xs) = (RePlus (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ ('?':xs) = (ReQuestion (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ ('[':xs) = (ReOpenBracket (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ (']':xs) = (ReCloseBracket (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ ('(':xs) = (ReOpenParenthesis (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ (')':xs) = (ReCloseParenthesis (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ ('|':xs) = (ReOr (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ ('-':xs) = (ReHyphen (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ ('\\':xs) = tokenise'' 2 (sline, scol, eline, ecol+1) "" xs
tokenise' 1 (sline, scol, eline, ecol) _ ('\'':xs) = (ReEnd (sline, scol, eline, ecol+1)):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ ('\n':xs) = (Error (sline, scol, eline+1, 1) "Unexpected end-of-line while reading a regular expression"):(tokenise'' 1 (eline+1, 1, eline+1, 1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ ('\r':xs) = (Error (sline, scol, eline+1, 1) "Unexpected return character encountered while reading a regular expression"):(tokenise'' 1 (eline, 1, eline, 1) "" xs)
tokenise' 1 (sline, scol, eline, ecol) _ (x:xs)   = (ReCharacter (sline, scol, eline, ecol+1) x):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 2 (sline, scol, eline, ecol) _ [] = [Error (sline, scol, eline, ecol) "Unexpected end-of-data while reading a regular expression"]
tokenise' 2 (sline, scol, eline, ecol) _ ('n':xs) = (ReNewLine (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 2 (sline, scol, eline, ecol) _ ('r':xs) = (ReReturn (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 2 (sline, scol, eline, ecol) _ ('t':xs) = (ReTab (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 2 (sline, scol, eline, ecol) _ ('f':xs) = (ReFormFeed (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 2 (sline, scol, eline, ecol) _ ('s':xs) = (ReWhitespaceClass (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 2 (sline, scol, eline, ecol) _ ('d':xs) = (ReDigitClass (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 2 (sline, scol, eline, ecol) _ ('w':xs) = (ReAlphaNumClass (sline, scol, eline, ecol+1)):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 2 (sline, scol, eline, ecol) _ ('\n':xs) = (Error (sline, scol, eline+1, 1) "Unexpected end-of-line while reading a regular expression"):(tokenise'' 1 (eline+1, 1, eline+1, 1) "" xs)
tokenise' 2 (sline, scol, eline, ecol) _ ('\r':xs) = (Error (sline, scol, eline+1, 1) "Unexpected return character encountered while reading a regular expression"):(tokenise'' 1 (eline, 1, eline, 1) "" xs)
tokenise' 2 (sline, scol, eline, ecol) _ (x:xs)   = (ReCharacter (sline, scol, eline, ecol+1) x):(tokenise'' 1 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 3 loc _ [] = [Comment loc]
tokenise' 3 (sline, scol, eline, ecol) _ ('\n':xs) = (Comment (sline, scol, eline+1, 1)):(tokenise'' 0 (eline+1, 1, eline+1, 1) "" xs)
tokenise' 3 (sline, scol, eline, ecol) _ ('\r':xs) = tokenise'' 3 (sline, scol, eline, 1) "" xs
tokenise' 3 (sline, scol, eline, ecol) _ (_:xs) = tokenise'' 3 (sline, scol, eline, ecol+1) "" xs
tokenise' 4 loc cur [] = [keyword loc cur]
tokenise' 4 (sline, scol, eline, ecol) cur (x:xs)
    | (x>='a' && x<='z') || (x>='A' && x<='Z') || (x>='0' && x<='9') || x=='_' = tokenise'' 4 (sline, scol, eline, ecol+1) (cur++[x]) xs
    | otherwise = (keyword (sline, scol, eline, ecol) cur):(tokenise'' 0 (eline, ecol, eline, ecol) "" (x:xs))
tokenise' 5 loc _ [] = [Error loc "Unexpected end-of-data while reading a string"]
tokenise' 5 (sline, scol, eline, ecol) cur ('"':xs) = (String (sline, scol, eline, ecol+1) cur):(tokenise'' 0 (eline, ecol+1, eline, ecol+1) "" xs)
tokenise' 5 (sline, scol, eline, ecol) cur ('\n':xs) = (Error (sline, scol, eline+1, 1) "Unexpected end-of-line while reading a string"):(tokenise'' 5 (eline+1, 1, eline+1, 1) cur xs)
tokenise' 5 (sline, scol, eline, ecol) cur ('\r':xs) = (Error (sline, scol, eline+1, 1) "Unexpected return character encountered while reading a string"):(tokenise'' 5 (eline, 1, eline, 1) cur xs)
tokenise' 5 (sline, scol, eline, ecol) cur ('\\':xs) = tokenise'' 6 (sline, scol, eline, ecol+1) cur xs
tokenise' 5 (sline, scol, eline, ecol) cur (x:xs) = tokenise'' 5 (sline, scol, eline, ecol+1) (cur++[x]) xs
tokenise' 6 (sline, scol, eline, ecol) cur ('n':xs) = tokenise'' 5 (sline, scol, eline, ecol+1) (cur++"\n") xs
tokenise' 6 (sline, scol, eline, ecol) cur ('r':xs) = tokenise'' 5 (sline, scol, eline, ecol+1) (cur++"\r") xs
tokenise' 6 (sline, scol, eline, ecol) cur ('t':xs) = tokenise'' 5 (sline, scol, eline, ecol+1) (cur++"\t") xs
tokenise' 6 (sline, scol, eline, ecol) cur ('f':xs) = tokenise'' 5 (sline, scol, eline, ecol+1) (cur++"\f") xs
tokenise' 6 (sline, scol, eline, ecol) cur ('v':xs) = tokenise'' 5 (sline, scol, eline, ecol+1) (cur++"\v") xs
tokenise' 6 (sline, scol, eline, ecol) cur ('\\':xs) = tokenise'' 5 (sline, scol, eline, ecol+1) (cur++"\\\\") xs
tokenise' 6 (sline, scol, eline, ecol) cur (x:xs) = tokenise'' 5 (sline, scol, eline, ecol+1) (cur++[x]) xs

--tokenise'' state loc cur [] = trace ("tokenise'' " ++ (show state) ++ " " ++ (show loc) ++ " " ++ (show cur) ++ " []") $ tokenise' state loc cur []
--tokenise'' state loc cur text@(c:_) = trace ("tokenise'' " ++ (show state) ++ " " ++ (show loc) ++ " " ++ (show cur) ++ " " ++ (show c)) $ tokenise' state loc cur text
tokenise'' = tokenise'

tokenise :: String -> [Token]
tokenise text = tokenise'' 0 (1, 1, 1, 1) "" text
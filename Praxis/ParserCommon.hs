module Praxis.ParserCommon
(
    formatError,
    parserTokens
)
where

import Praxis.ManualLexer as Lexer

formatError :: Lexer.Location -> String -> String
formatError (sl,sc,el,ec) msg = ((show sl) ++ ":" ++ (show sc) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ " : " ++ msg)

-- A temporary function to remove comments from the token stream
parserTokens :: [Lexer.Token] -> [Lexer.Token]
parserTokens [] = []
parserTokens ((Lexer.Comment _):xs) = parserTokens xs
parserTokens (x:xs) = x:(parserTokens xs)
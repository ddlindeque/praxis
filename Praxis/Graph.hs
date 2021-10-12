module Praxis.Graph
(
    process
)
where

import System.IO
import qualified Praxis.ManualLexer as Lexer
import qualified Praxis.ManualParser as Parser
import qualified Praxis.ParserCommon as ParserCommon

-- object, evolution, output handle, input source
writeGraph :: String -> String -> Handle -> String -> IO ()
-- Write the AST
writeGraph "" "" handle source =
    do
        case Parser.parse $ ParserCommon.parserTokens $ Lexer.tokenise source of
            Parser.ResultError loc msg -> 
                do
                    putStrLn (ParserCommon.formatError loc msg)
            Parser.ResultAst ast -> 
                do
                    Parser.writeDot handle ast
writeGraph "" x _ _ =
    do
        putStrLn ("The default 'AST' graph cannot graph an evolution (" ++ x ++ ")")

-- Write an NFA
writeGraph object "nfa" handle source =
    do
        case Parser.parse $ ParserCommon.parserTokens $ Lexer.tokenise source of
            Parser.ResultError loc msg ->
                do
                    putStrLn (ParserCommon.formatError loc msg)
            Parser.ResultAst ast ->
                do
                    hPutStr handle "NFA dot graph"

writeGraph _ "" _ _ =
    do
        putStrLn ("The evoluation is required when graphing an automata")

writeGraph _ evolution _ _ =
    do
        putStrLn ("The evolution " ++ evolution ++ " is not supported.")

-- object, evolution, input, output
process :: String -> String -> String -> String -> IO ()
process object evolution "" "" =
    do
        source <- hGetContents stdin
        writeGraph object evolution stdout source
process object evolution "" output =
    withFile output WriteMode $ \handle -> do
        source <- hGetContents stdin
        writeGraph object evolution handle source
process object evolution input "" =
    do
        source <- readFile input
        writeGraph object evolution stdout source
process object evolution input output =
    withFile output WriteMode $ \handle -> do
        source <- readFile input
        writeGraph object evolution handle source
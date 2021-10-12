module Praxis.Colourise
(
    process
)
where

import System.IO
import qualified Praxis.ManualLexer as Lexer
import Debug.Trace

getTokenLocation :: Lexer.Token -> Lexer.Location
getTokenLocation (Lexer.EOD loc) = loc
getTokenLocation (Lexer.Identifier loc _) = loc
getTokenLocation (Lexer.Comment loc) = loc
getTokenLocation (Lexer.String loc _) = loc
getTokenLocation (Lexer.Integer loc _) = loc
getTokenLocation (Lexer.KwAnalyser loc) = loc
getTokenLocation (Lexer.KwScanner loc) = loc
getTokenLocation (Lexer.KwParser loc) = loc
getTokenLocation (Lexer.KwProcessing loc) = loc
getTokenLocation (Lexer.KwIgnoring loc) = loc
getTokenLocation (Lexer.KwReturn loc) = loc
getTokenLocation (Lexer.KwGoto loc) = loc
getTokenLocation (Lexer.KwLeft loc) = loc
getTokenLocation (Lexer.KwRight loc) = loc
getTokenLocation (Lexer.KwNone loc) = loc
getTokenLocation (Lexer.KwError loc) = loc
getTokenLocation (Lexer.OpenBracket loc) = loc
getTokenLocation (Lexer.CloseBracket loc) = loc
getTokenLocation (Lexer.OpenCurly loc) = loc
getTokenLocation (Lexer.CloseCurly loc) = loc
getTokenLocation (Lexer.Comma loc) = loc
getTokenLocation (Lexer.Colon loc) = loc
getTokenLocation (Lexer.Equal loc) = loc
getTokenLocation (Lexer.SemiColon loc) = loc
getTokenLocation (Lexer.Arrow loc) = loc
getTokenLocation (Lexer.Hat loc) = loc
getTokenLocation (Lexer.Dot loc) = loc
getTokenLocation (Lexer.Asterisk loc) = loc
getTokenLocation (Lexer.Plus loc) = loc
getTokenLocation (Lexer.Question loc) = loc
getTokenLocation (Lexer.OpenParenthesis loc) = loc
getTokenLocation (Lexer.CloseParenthesis loc) = loc
getTokenLocation (Lexer.Pipe loc) = loc
getTokenLocation (Lexer.ReStart loc) = loc
getTokenLocation (Lexer.ReEnd loc) = loc
getTokenLocation (Lexer.ReCharacter loc _) = loc
getTokenLocation (Lexer.ReHat loc) = loc
getTokenLocation (Lexer.ReDot loc) = loc
getTokenLocation (Lexer.ReAsterisk loc) = loc
getTokenLocation (Lexer.RePlus loc) = loc
getTokenLocation (Lexer.ReQuestion loc) = loc
getTokenLocation (Lexer.ReOpenBracket loc) = loc
getTokenLocation (Lexer.ReCloseBracket loc) = loc
getTokenLocation (Lexer.ReOpenParenthesis loc) = loc
getTokenLocation (Lexer.ReCloseParenthesis loc) = loc
getTokenLocation (Lexer.ReHyphen loc) = loc
getTokenLocation (Lexer.ReNewLine loc) = loc
getTokenLocation (Lexer.ReReturn loc) = loc
getTokenLocation (Lexer.ReTab loc) = loc
getTokenLocation (Lexer.ReFormFeed loc) = loc
getTokenLocation (Lexer.ReWhitespaceClass loc) = loc
getTokenLocation (Lexer.ReDigitClass loc) = loc
getTokenLocation (Lexer.ReAlphaNumClass loc) = loc
getTokenLocation (Lexer.ReOr loc) = loc
getTokenLocation (Lexer.Error loc _) = loc

getTokenClass :: Lexer.Token -> String
getTokenClass (Lexer.EOD _) = "punctuation"
getTokenClass (Lexer.Identifier _ _) = "identifier"
getTokenClass (Lexer.Comment _) = "comment"
getTokenClass (Lexer.String _ _) = "string"
getTokenClass (Lexer.Integer _ _) = "integer"
getTokenClass (Lexer.KwAnalyser _) = "keyword"
getTokenClass (Lexer.KwScanner _) = "keyword"
getTokenClass (Lexer.KwParser _) = "keyword"
getTokenClass (Lexer.KwProcessing _) = "keyword"
getTokenClass (Lexer.KwIgnoring _) = "keyword"
getTokenClass (Lexer.KwReturn _) = "keyword"
getTokenClass (Lexer.KwGoto _) = "keyword"
getTokenClass (Lexer.KwLeft _) = "keyword"
getTokenClass (Lexer.KwRight _) = "keyword"
getTokenClass (Lexer.KwNone _) = "keyword"
getTokenClass (Lexer.KwError _) = "keyword"
getTokenClass (Lexer.OpenBracket _) = "punctuation"
getTokenClass (Lexer.CloseBracket _) = "punctuation"
getTokenClass (Lexer.OpenCurly _) = "punctuation"
getTokenClass (Lexer.CloseCurly _) = "punctuation"
getTokenClass (Lexer.Comma _) = "punctuation"
getTokenClass (Lexer.Colon _) = "punctuation"
getTokenClass (Lexer.Equal _) = "punctuation"
getTokenClass (Lexer.SemiColon _) = "punctuation"
getTokenClass (Lexer.Arrow _) = "punctuation"
getTokenClass (Lexer.Hat _) = "operator"
getTokenClass (Lexer.Dot _) = "class"
getTokenClass (Lexer.Asterisk _) = "operator"
getTokenClass (Lexer.Plus _) = "operator"
getTokenClass (Lexer.Question _) = "operator"
getTokenClass (Lexer.OpenParenthesis _) = "punctuation"
getTokenClass (Lexer.CloseParenthesis _) = "punctuation"
getTokenClass (Lexer.Pipe _) = "punctuation"
getTokenClass (Lexer.ReStart _ )= "re-block"
getTokenClass (Lexer.ReEnd _ )= "re-block"
getTokenClass (Lexer.ReCharacter _ _) = "re-char"
getTokenClass (Lexer.ReHat _) = "re-operator"
getTokenClass (Lexer.ReDot _) = "re-class"
getTokenClass (Lexer.ReAsterisk _) = "re-operator"
getTokenClass (Lexer.RePlus _) = "re-operator"
getTokenClass (Lexer.ReQuestion _) = "re-operator"
getTokenClass (Lexer.ReOpenBracket _) = "re-punctuation"
getTokenClass (Lexer.ReCloseBracket _) = "re-punctuation"
getTokenClass (Lexer.ReOpenParenthesis _) = "re-punctuation"
getTokenClass (Lexer.ReCloseParenthesis _) = "re-punctuation"
getTokenClass (Lexer.ReHyphen _) = "re-punctuation"
getTokenClass (Lexer.ReNewLine _) = "re-char"
getTokenClass (Lexer.ReReturn _) = "re-char"
getTokenClass (Lexer.ReTab _) = "re-char"
getTokenClass (Lexer.ReFormFeed _) = "re-char"
getTokenClass (Lexer.ReWhitespaceClass _) = "re-class"
getTokenClass (Lexer.ReDigitClass _) = "re-class"
getTokenClass (Lexer.ReAlphaNumClass _) = "re-class"
getTokenClass (Lexer.ReOr _) = "re-operator"
getTokenClass (Lexer.Error _ _) = "error"

getTokenStartLine :: Lexer.Token -> Int
getTokenStartLine tkn =
    let (v, _, _, _) = getTokenLocation tkn
    in v

getTokenEndLine :: Lexer.Token -> Int
getTokenEndLine tkn =
    let (_, _, v, _) = getTokenLocation tkn
    in v

getTokenStartCol :: Lexer.Token -> Int
getTokenStartCol tkn =
    let (_, v, _, _) = getTokenLocation tkn
    in v

getTokenEndCol :: Lexer.Token -> Int
getTokenEndCol tkn =
    let (_, _, _, v) = getTokenLocation tkn
    in v

writeHtml :: Handle -> String -> String -> Bool -> Bool -> IO ()
writeHtml handle source _ False tooltips =
    let escapeHtml ' ' = "&nbsp;"
        escapeHtml '"' = "&quot;"
        escapeHtml '\'' = "&apos;"
        escapeHtml '<' = "&lt;"
        escapeHtml '>' = "&gt;"
        escapeHtml c = [c]

        writeTokenFooter (Lexer.CloseCurly _) =
            hPutStr handle "<hr />"
        writeTokenFooter _ =
            return ()

        writeHtmlString [] = return ()
        writeHtmlString (c:cs) =
            do
                hPutStr handle $ escapeHtml c
                writeHtmlString cs

        -- writeHtml' :: Int -> Int -> [Token] -> String; line, col, tokens, source
        writeHtml' _ _ [] [] =
            do
                return ()
        writeHtml' _ _ [] (c:cs) =
            do
                hPutStr handle $ escapeHtml c
                writeHtml'' 0 0 [] cs
        writeHtml' _ _ [_, Lexer.EOD _] [] =
            do
                hPutStr handle "</span>"
        --writeHtml' _ _ tokens [] = trace ("tokens = " ++ (show tokens)) $ hPutStr handle "</span>"
        writeHtml' line col tokens@(tkn:tkns) source'@('\n':cs)
            | line == (getTokenEndLine tkn) && col == (getTokenEndCol tkn) =
                do
                    hPutStr handle "</span>"
                    writeTokenFooter tkn
                    writeHtml'' line col tkns source'
            | otherwise =
                do
                    hPutStrLn handle "<br />"
                    writeHtml'' (line+1) 1 tokens cs
        writeHtml' line col tokens@(tkn:tkns) source'@(c:cs)
            | line == (getTokenStartLine tkn) && col == (getTokenStartCol tkn) =
                case tooltips of
                    True -> 
                        do
                            hPutStr handle ("<span class=\"" ++ (getTokenClass tkn) ++ "\" title=\"")
                            writeHtmlString $ show tkn
                            hPutStr handle "\">"
                            hPutStr handle $ escapeHtml c
                            writeHtml'' line (col+1) (tkn:tkns) cs
                    False -> 
                        do
                            hPutStr handle ("<span class=\"" ++ (getTokenClass tkn) ++ "\">")
                            hPutStr handle $ escapeHtml c
                            writeHtml'' line (col+1) (tkn:tkns) cs
            | line == (getTokenEndLine tkn) && col == (getTokenEndCol tkn) =
                do
                    hPutStr handle "</span>"
                    writeTokenFooter tkn
                    writeHtml'' line col tkns source'
            | otherwise =
                do
                    hPutStr handle $ escapeHtml c
                    writeHtml'' line (col+1) tokens cs

        --writeHtml'' line col [] source'@(c:_) =
        --    trace ("writeHtml' " ++ (show line) ++ " " ++ (show col) ++ " [] " ++ (show c)) $ writeHtml' line col [] source'
        --writeHtml'' line col tokens@(tkn:_) [] =
        --    trace ("writeHtml' " ++ (show line) ++ " " ++ (show col) ++ " <" ++ (show tkn) ++ "> []") $ writeHtml' line col tokens []
        --writeHtml'' line col [] [] =
        --    trace ("writeHtml' " ++ (show line) ++ " " ++ (show col) ++ " [] []") $ writeHtml' line col [] []
        --writeHtml'' line col tokens@(tkn:_) source'@(c:_) =
        --    trace ("writeHtml' " ++ (show line) ++ " " ++ (show col) ++ " <" ++ (show tkn) ++ "> " ++ (show c)) $ writeHtml' line col tokens source'
        writeHtml'' = writeHtml'
    in
        do
            let tokens = Lexer.tokenise source
            writeHtml'' 1 1 tokens source
            hPutStrLn handle ""
    
writeHtml handle source stylesheet True tooltips =
    do
        hPutStrLn handle "<!DOCTYPE html>"
        hPutStrLn handle "<html>"
        hPutStrLn handle "  <head>"
        hPutStrLn handle ("    <link rel=\"stylesheet\" type=\"text/css\" href=\"" ++ stylesheet ++ "\">")
        hPutStrLn handle "  </head>"
        hPutStrLn handle "  <body>"
        hPutStr handle "    "
        writeHtml handle source "" False tooltips
        hPutStrLn handle "  </body>"
        hPutStrLn handle "</html>"

process :: String -> String -> String -> Bool -> Bool -> IO ()
process "" "" stylesheet doc tooltips =
    do
        source <- hGetContents stdin
        writeHtml stdout source stylesheet doc tooltips
process "" output stylesheet doc tooltips =
    withFile output WriteMode $ \handle -> do
        source <- hGetContents stdin
        writeHtml handle source stylesheet doc tooltips
process input "" stylesheet doc tooltips =
    do
        source <- readFile input
        writeHtml stdout source stylesheet doc tooltips
process input output stylesheet doc tooltips =
    withFile output WriteMode $ \handle -> do
        source <- readFile input
        writeHtml handle source stylesheet doc tooltips

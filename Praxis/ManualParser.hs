module Praxis.ManualParser
(
    parse,
    writeDot,
    Ast (..),
    ParseResult (..)
)
where

import System.IO
import Praxis.ManualLexer as Lexer
import Common

-- Document: Document = Statements
-- Statements = Statement
-- Statements = Statements Statement
-- Statement = Analyser
-- Statement = Scanner
-- Statement = Parser
-- Analyser = analyser identifier opencurly AnalyserMatches closecurly
-- Analyser = analyser identifier openbracket Strings closebracket opencurly AnalyserMatches closecurly
-- AnalyserMatches = AnalyserMatch
-- AnalyserMatches = AnalyserMatches AnalyserMatch
-- AnalyserMatch = restart Re reend arrow identifier semicolon
-- AnalyserMatch = restart Re reend arrow identifier comma return semicolon
-- AnalyserMatch = restart Re reend arrow identifier comma goto identifier semicolon
-- Strings = string
-- Strings = Strings comma string
-- Scanner = scanner identifier ScannerOptions opencurly closecurly
-- Scanner = scanner identifier ScannerOptions opencurly ScannerMatches closecurly
-- ScannerOptions = ScannerOption
-- ScannerOptions = ScannerOptions ScannerOption
-- ScannerOption = processing identifier
-- ScannerOption = ignoring openbracket Identifiers closebracket
-- ScannerMatches = ScannerMatch
-- ScannerMatches = ScannerMatches ScannerMatch
-- ScannerMatch = Te arrow identifier semicolon
-- ScannerMatch = Te arrow identifier comma return semicolon
-- ScannerMatch = Te arrow identifier comma goto identifier semicolon
-- Identifiers = identifier
-- Identifiers = Identifiers comma Identifier

escapeDot :: String -> String
escapeDot [] = []
escapeDot ('"':cs) = '\\':'"':(escapeDot cs)
escapeDot ('\\':cs) = '\\':'\\':(escapeDot cs)
escapeDot (c:cs) = c:(escapeDot cs)

showDot :: Show a => a -> String
showDot = escapeDot . show

-- Parser parse type declarations

data Ast =
          -- Analyser
          AnalyserNoOptions Location Token Token Token Ast Token
        | AnalyserWithOptions Location Token Token Token Ast Token Token Ast Token
          -- AnalyserMatch
        | AnalyserMatchNoAction Location Token Ast Token Token Token Token
        | AnalyserMatchWithReturn Location Token Ast Token Token Token Token Token Token
        | AnalyserMatchWithGoto Location Token Ast Token Token Token Token Token Token Token
        | AnalyserMatchError Location Token Ast Token Token Token Token
          -- AnalyserMatches
        | FirstAnalyserMatch Location Ast
        | RestAnalyserMatches Location Ast Ast
          -- Assoc
        | LeftAssoc Location Token
        | RightAssoc Location Token
        | NoAssoc Location Token
          -- CommaIdentifiers
        | FirstCommaIdentifier Location Token
        | RestCommaIdentifiers Location Ast Token Token
          -- Document
        | Document Location Ast Token
          -- Identifiers
        | FirstIdentifier Location Token
        | RestIdentifiers Location Ast Token
          -- Items
        | FirstItem Location Token
        | RestItems Location Ast Token
          -- Parser
        | Parser Location Token Token Token Token Token Ast Token
          -- Production
        | ProductionNoOptions Location Token Token Token Token Ast Token
        | ProductionWithAssoc Location Token Token Token Token Ast Token Ast Token Token
        | ProductionWithPrec Location Token Token Token Token Ast Token Ast Token Token Token Token
          -- Productions
        | FirstProduction Location Ast
        | RestProductions Location Ast Ast
          -- RE
        | RECharacterExpression Location Token
        | REWhitespaceCharExpression Location Token
        | REDigitCharExpression Location Token
        | REAlphaNumCharExpression Location Token
        | RENewLineCharExpression Location Token
        | REReturnCharExpression Location Token
        | RETabCharExpression Location Token
        | REFormFeedCharExpression Location Token
        | RERangeExpression Location Token Ast Token
        | RENotRangeExpression Location Token Token Ast Token
        | REAnyCharExpression Location Token
        | REThenExpression Location Ast Ast
        | REZeroOrMoreExpression Location Ast Token
        | REOneOrMoreExpression Location Ast Token
        | REZeroOrOneExpression Location Ast Token
        | REScopedExpression Location Token Ast Token
        | REOrExpression Location Ast Token Ast
          -- RERange
        | RECharacterRangeExpression Location Token
        | REWhitespaceCharRangeExpression Location Token
        | REDigitCharRangeExpression Location Token
        | REAlphaNumCharRangeExpression Location Token
        | RENewLineCharRangeExpression Location Token
        | REReturnCharRangeExpression Location Token
        | RETabCharRangeExpression Location Token
        | REFormFeedCharRangeExpression Location Token
        | RECharRangeRangeExpression Location Token Token Token
          -- RERanges
        | REFirstRangeExpression Location Ast
        | RERestRangeExpression Location Ast Ast
          -- Scanner
        | EmptyScanner Location Token Token Ast Token Token
        | ScannerWithRules Location Token Token Ast Token Ast Token
          -- ScannerMatch
        | ScannerMatchNoAction Location Ast Token Token Token
        | ScannerMatchWithReturn Location Ast Token Token Token Token Token
        | ScannerMatchWithGoto Location Ast Token Token Token Token Token Token
        | ScannerMatchError Location Ast Token Token Token
          -- ScannerMatches
        | FirstScannerMatch Location Ast
        | RestScannerMatches Location Ast Ast
          -- ScannerOption
        | ProcessingScannerOption Location Token Token
        | IgnoringScannerOption Location Token Token Ast Token
          -- ScannerOptions
        | FirstScannerOption Location Ast
        | RestScannerOptions Location Ast Ast
          -- Statement
        | AnalyserStatement Location Ast
        | ScannerStatement Location Ast
        | ParserStatement Location Ast
          -- Statements
        | FirstStatement Location Ast
        | RestStatements Location Ast Ast
          -- Strings
        | FirstString Location Token
        | RestStrings Location Ast Token Token
          -- TE
        | TEIdentifierExpression Location Token
        | TENotRangeExpression Location Token Token Ast Token
        | TEThenExpression Location Ast Ast
        | TEZeroOrMoreExpression Location Ast Token
        | TEOneOrMoreExpression Location Ast Token
        | TEZeroOrOneExpression Location Ast Token
        | TEScopedExpression Location Token Ast Token
        | TEOrExpression Location Ast Token Ast
          deriving (Show)
data ParserValue = ParserError Location String | ParserToken Token | ParserAst Ast deriving (Show)
data ParseResult = ResultError Location String | ResultAst Ast deriving (Show)

-- Debugging
trace' :: String -> a -> a
trace' _ x = x
-- trace' = trace

writeDot :: Handle -> Ast -> IO ()
writeDot handle ast =
    let dummy = 0 -- Get rid of 'let' issue
        -- Analyser
        writeAst id (AnalyserNoOptions (sl,sc,el,ec) v1 v2 v3 v4 v5) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"AnalyserNoOptions\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                writeAst (id ++ ".4") v4 -- AnalyserMatches
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".5\" [label=\"" ++ (showDot v5) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
        -- Analyser
        writeAst id (AnalyserWithOptions (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7 v8) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"AnalyserWithOptions\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                writeAst (id ++ ".4") v4 -- Strings
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".5\" [label=\"" ++ (showDot v5) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".6\" [label=\"" ++ (showDot v6) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".6\" [label=\"p6\"]")
                writeAst (id ++ ".7") v7 -- AnalyserMatches
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".7\" [label=\"p7\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".8\" [label=\"" ++ (showDot v8) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".8\" [label=\"p8\"]")
        -- AnalyserMatch
        writeAst id (AnalyserMatchNoAction (sl,sc,el,ec) v1 v2 v3 v4 v5 v6) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"AnalyserMatchNoAction\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- RE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".5\" [label=\"" ++ (showDot v5) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".6\" [label=\"" ++ (showDot v6) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".6\" [label=\"p6\"]")
        -- AnalyserMatch
        writeAst id (AnalyserMatchWithReturn (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7 v8) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"AnalyserMatchWithReturn\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- RE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".5\" [label=\"" ++ (showDot v5) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".6\" [label=\"" ++ (showDot v6) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".6\" [label=\"p6\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".7\" [label=\"" ++ (showDot v7) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".7\" [label=\"p7\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".8\" [label=\"" ++ (showDot v8) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".8\" [label=\"p8\"]")
        -- AnalyserMatch
        writeAst id (AnalyserMatchWithGoto (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7 v8 v9) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"AnalyserMatchWithGoto\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- RE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".5\" [label=\"" ++ (showDot v5) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".6\" [label=\"" ++ (showDot v6) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".6\" [label=\"p6\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".7\" [label=\"" ++ (showDot v7) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".7\" [label=\"p7\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".8\" [label=\"" ++ (showDot v8) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".8\" [label=\"p8\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".9\" [label=\"" ++ (showDot v9) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".9\" [label=\"p9\"]")
        -- AnalyserMatch
        writeAst id (AnalyserMatchError (sl,sc,el,ec) v1 v2 v3 v4 v5 v6) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"AnalyserMatchError\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- RE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".5\" [label=\"" ++ (showDot v5) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".6\" [label=\"" ++ (showDot v6) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".6\" [label=\"p6\"]")
        -- AnalyserMatches
        writeAst id (FirstAnalyserMatch (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"FirstAnalyserMatch\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- AnalyserMatch
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- AnalyserMatches
        writeAst id (RestAnalyserMatches (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RestAnalyserMatches\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- AnalyserMatches
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- AnalyserMatch
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- Assoc
        writeAst id (LeftAssoc (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"LeftAssoc\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- Assoc
        writeAst id (RightAssoc (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RightAssoc\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- Assoc
        writeAst id (NoAssoc (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"NoAssoc\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- CommaIdentifiers
        writeAst id (FirstCommaIdentifier (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"FirstCommaIdentifier\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- CommaIdentifiers
        writeAst id (RestCommaIdentifiers (sl,sc,el,ec) v1 v2 v3) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RestCommaIdentifiers\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- CommaIdentifiers
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
        -- Document
        writeAst id (Document (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"Document\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- Statements
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- Identifiers
        writeAst id (FirstIdentifier (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"FirstIdentifier\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- Identifiers
        writeAst id (RestIdentifiers (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RestIdentifiers\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- Identifiers
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- Items
        writeAst id (FirstItem (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"FirstItem\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- Items
        writeAst id (RestItems (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RestItems\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- Items
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- Parser
        writeAst id (Parser (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"Parser\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".5\" [label=\"" ++ (showDot v5) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
                writeAst (id ++ ".6") v6 -- Productions
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".6\" [label=\"p6\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".7\" [label=\"" ++ (showDot v7) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".7\" [label=\"p7\"]")
        -- Production
        writeAst id (ProductionNoOptions (sl,sc,el,ec) v1 v2 v3 v4 v5 v6) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"ProductionNoOptions\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                writeAst (id ++ ".5") v5 -- Items
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".6\" [label=\"" ++ (showDot v6) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".6\" [label=\"p6\"]")
        -- Production
        writeAst id (ProductionWithAssoc (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7 v8 v9) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"ProductionWithAssoc\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                writeAst (id ++ ".5") v5 -- Items
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".6\" [label=\"" ++ (showDot v6) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".6\" [label=\"p6\"]")
                writeAst (id ++ ".7") v7 -- Assoc
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".7\" [label=\"p7\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".8\" [label=\"" ++ (showDot v8) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".8\" [label=\"p8\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".9\" [label=\"" ++ (showDot v9) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".9\" [label=\"p9\"]")
        -- Production
        writeAst id (ProductionWithPrec (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"ProductionWithPrec\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                writeAst (id ++ ".5") v5 -- Items
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".6\" [label=\"" ++ (showDot v6) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".6\" [label=\"p6\"]")
                writeAst (id ++ ".7") v7 -- Assoc
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".7\" [label=\"p7\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".8\" [label=\"" ++ (showDot v8) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".8\" [label=\"p8\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".9\" [label=\"" ++ (showDot v9) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".9\" [label=\"p9\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".10\" [label=\"" ++ (showDot v10) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".10\" [label=\"p10\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".11\" [label=\"" ++ (showDot v11) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".11\" [label=\"p11\"]")
        -- Productions
        writeAst id (FirstProduction (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"FirstProduction\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- Production
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- Productions
        writeAst id (RestProductions (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RestProductions\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- Productions
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- Production
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- RE
        writeAst id (RECharacterExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RECharacterExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RE
        writeAst id (REWhitespaceCharExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REWhitespaceCharExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RE
        writeAst id (REDigitCharExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REDigitCharExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RE
        writeAst id (REAlphaNumCharExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REAlphaNumCharExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RE
        writeAst id (RENewLineCharExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RENewLineCharExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RE
        writeAst id (REReturnCharExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REReturnCharExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RE
        writeAst id (RETabCharExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RETabCharExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RE
        writeAst id (REFormFeedCharExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REFormFeedCharExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RE
        writeAst id (RERangeExpression (sl,sc,el,ec) v1 v2 v3) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RERangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- RERanges
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
        -- RE
        writeAst id (RENotRangeExpression (sl,sc,el,ec) v1 v2 v3 v4) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RENotRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                writeAst (id ++ ".3") v3 -- RERanges
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
        -- RE
        writeAst id (REAnyCharExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REAnyCharExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RE
        writeAst id (REThenExpression (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REThenExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- RE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- RE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- RE
        writeAst id (REZeroOrMoreExpression (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REZeroOrMoreExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- RE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- RE
        writeAst id (REOneOrMoreExpression (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REOneOrMoreExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- RE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- RE
        writeAst id (REZeroOrOneExpression (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REZeroOrOneExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- RE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- RE
        writeAst id (REScopedExpression (sl,sc,el,ec) v1 v2 v3) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REScopedExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- RE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
        -- RE
        writeAst id (REOrExpression (sl,sc,el,ec) v1 v2 v3) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REOrExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- RE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                writeAst (id ++ ".3") v3 -- RE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
        -- RERange
        writeAst id (RECharacterRangeExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RECharacterRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RERange
        writeAst id (REWhitespaceCharRangeExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REWhitespaceCharRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RERange
        writeAst id (REDigitCharRangeExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REDigitCharRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RERange
        writeAst id (REAlphaNumCharRangeExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REAlphaNumCharRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RERange
        writeAst id (RENewLineCharRangeExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RENewLineCharRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RERange
        writeAst id (REReturnCharRangeExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REReturnCharRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RERange
        writeAst id (RETabCharRangeExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RETabCharRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RERange
        writeAst id (REFormFeedCharRangeExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REFormFeedCharRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RERange
        writeAst id (RECharRangeRangeExpression (sl,sc,el,ec) v1 v2 v3) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RECharRangeRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
        -- RERanges
        writeAst id (REFirstRangeExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"REFirstRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- RERange
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- RERanges
        writeAst id (RERestRangeExpression (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RERestRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- RERanges
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- RERange
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- Scanner
        writeAst id (EmptyScanner (sl,sc,el,ec) v1 v2 v3 v4 v5) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"EmptyScanner\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                writeAst (id ++ ".3") v3 -- ScannerOptions
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".5\" [label=\"" ++ (showDot v5) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
        -- Scanner
        writeAst id (ScannerWithRules (sl,sc,el,ec) v1 v2 v3 v4 v5 v6) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"ScannerWithRules\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                writeAst (id ++ ".3") v3 -- ScannerOptions
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                writeAst (id ++ ".5") v5 -- ScannerMatches
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".6\" [label=\"" ++ (showDot v6) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".6\" [label=\"p6\"]")
        -- ScannerMatch
        writeAst id (ScannerMatchNoAction (sl,sc,el,ec) v1 v2 v3 v4) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"ScannerMatchNoAction\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- TE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
        -- ScannerMatch
        writeAst id (ScannerMatchWithReturn (sl,sc,el,ec) v1 v2 v3 v4 v5 v6) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"ScannerMatchWithReturn\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- TE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".5\" [label=\"" ++ (showDot v5) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".6\" [label=\"" ++ (showDot v6) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".6\" [label=\"p6\"]")
        -- ScannerMatch
        writeAst id (ScannerMatchWithGoto (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"ScannerMatchWithGoto\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- TE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".5\" [label=\"" ++ (showDot v5) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".5\" [label=\"p5\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".6\" [label=\"" ++ (showDot v6) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".6\" [label=\"p6\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".7\" [label=\"" ++ (showDot v7) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".7\" [label=\"p7\"]")
        -- ScannerMatch
        writeAst id (ScannerMatchError (sl,sc,el,ec) v1 v2 v3 v4) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"ScannerMatchError\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- TE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
        -- ScannerMatches
        writeAst id (FirstScannerMatch (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"FirstScannerMatch\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- ScannerMatch
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- ScannerMatches
        writeAst id (RestScannerMatches (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RestScannerMatches\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- ScannerMatches
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- ScannerMatch
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- ScannerOption
        writeAst id (ProcessingScannerOption (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"ProcessingScannerOption\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- ScannerOption
        writeAst id (IgnoringScannerOption (sl,sc,el,ec) v1 v2 v3 v4) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"IgnoringScannerOption\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                writeAst (id ++ ".3") v3 -- CommaIdentifiers
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
        -- ScannerOptions
        writeAst id (FirstScannerOption (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"FirstScannerOption\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- ScannerOption
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- ScannerOptions
        writeAst id (RestScannerOptions (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RestScannerOptions\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- ScannerOptions
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- ScannerOption
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- Statement
        writeAst id (AnalyserStatement (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"AnalyserStatement\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- Analyser
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- Statement
        writeAst id (ScannerStatement (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"ScannerStatement\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- Scanner
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- Statement
        writeAst id (ParserStatement (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"ParserStatement\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- Parser
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- Statements
        writeAst id (FirstStatement (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"FirstStatement\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- Statement
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- Statements
        writeAst id (RestStatements (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RestStatements\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- Statements
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- Statement
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- Strings
        writeAst id (FirstString (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"FirstString\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- Strings
        writeAst id (RestStrings (sl,sc,el,ec) v1 v2 v3) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"RestStrings\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- Strings
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
        -- TE
        writeAst id (TEIdentifierExpression (sl,sc,el,ec) v1) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"TEIdentifierExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
        -- TE
        writeAst id (TENotRangeExpression (sl,sc,el,ec) v1 v2 v3 v4) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"TENotRangeExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                writeAst (id ++ ".3") v3 -- Identifiers
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".4\" [label=\"" ++ (showDot v4) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".4\" [label=\"p4\"]")
        -- TE
        writeAst id (TEThenExpression (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"TEThenExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- TE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- TE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- TE
        writeAst id (TEZeroOrMoreExpression (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"TEZeroOrMoreExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- TE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- TE
        writeAst id (TEOneOrMoreExpression (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"TEOneOrMoreExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- TE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- TE
        writeAst id (TEZeroOrOneExpression (sl,sc,el,ec) v1 v2) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"TEZeroOrOneExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- TE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
        -- TE
        writeAst id (TEScopedExpression (sl,sc,el,ec) v1 v2 v3) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"TEScopedExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".1\" [label=\"" ++ (showDot v1) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                writeAst (id ++ ".2") v2 -- TE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".3\" [label=\"" ++ (showDot v3) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
        -- TE
        writeAst id (TEOrExpression (sl,sc,el,ec) v1 v2 v3) =
            do
                hPutStrLn handle ("  \"" ++ id ++ "\" [label=\"TEOrExpression\\n" ++ (show sl) ++ ":" ++ (show ec) ++ "-" ++ (show el) ++ ":" ++ (show ec) ++ "\"]")
                writeAst (id ++ ".1") v1 -- TE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".1\" [label=\"p1\"]")
                hPutStrLn handle ("  \"" ++ id ++ ".2\" [label=\"" ++ (showDot v2) ++ "\"]")
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".2\" [label=\"p2\"]")
                writeAst (id ++ ".3") v3 -- TE
                hPutStrLn handle ("  \"" ++ id ++ "\" -> \"" ++ id ++ ".3\" [label=\"p3\"]")
    in
        do
            hPutStrLn handle "digraph Document {"
            writeAst "n.1" ast
            hPutStrLn handle "}"

-- Parser function

parse :: [Token] -> ParseResult
parse tokens =
    let dummy = 0 -- handle the 'let' problem
        -- getTokenLocation :: Token -> (Int,Int,Int,Int)
        getTokenLocation (ReOpenParenthesis loc ) = loc
        getTokenLocation (ReCloseParenthesis loc ) = loc
        getTokenLocation (ReAsterisk loc ) = loc
        getTokenLocation (RePlus loc ) = loc
        getTokenLocation (Comma loc ) = loc
        getTokenLocation (ReHyphen loc ) = loc
        getTokenLocation (Arrow loc ) = loc
        getTokenLocation (ReDot loc ) = loc
        getTokenLocation (Colon loc ) = loc
        getTokenLocation (SemiColon loc ) = loc
        getTokenLocation (Equal loc ) = loc
        getTokenLocation (ReQuestion loc ) = loc
        getTokenLocation (ReOpenBracket loc ) = loc
        getTokenLocation (ReDigitClass loc ) = loc
        getTokenLocation (ReFormFeed loc ) = loc
        getTokenLocation (ReNewLine loc ) = loc
        getTokenLocation (ReReturn loc ) = loc
        getTokenLocation (ReWhitespaceClass loc ) = loc
        getTokenLocation (ReTab loc ) = loc
        getTokenLocation (ReAlphaNumClass loc ) = loc
        getTokenLocation (ReCloseBracket loc ) = loc
        getTokenLocation (ReHat loc ) = loc
        getTokenLocation (KwAnalyser loc ) = loc
        getTokenLocation (ReCharacter loc _) = loc
        getTokenLocation (CloseBracket loc ) = loc
        getTokenLocation (KwError loc ) = loc
        getTokenLocation (KwGoto loc ) = loc
        getTokenLocation (Identifier loc _) = loc
        getTokenLocation (KwIgnoring loc ) = loc
        getTokenLocation (KwLeft loc ) = loc
        getTokenLocation (KwNone loc ) = loc
        getTokenLocation (Integer loc _) = loc
        getTokenLocation (OpenBracket loc ) = loc
        getTokenLocation (KwParser loc ) = loc
        getTokenLocation (Pipe loc ) = loc
        getTokenLocation (KwProcessing loc ) = loc
        getTokenLocation (ReEnd loc ) = loc
        getTokenLocation (ReStart loc ) = loc
        getTokenLocation (KwReturn loc ) = loc
        getTokenLocation (KwRight loc ) = loc
        getTokenLocation (KwScanner loc ) = loc
        getTokenLocation (String loc _) = loc
        getTokenLocation (Asterisk loc ) = loc
        getTokenLocation (CloseParenthesis loc ) = loc
        getTokenLocation (Hat loc ) = loc
        getTokenLocation (OpenParenthesis loc ) = loc
        getTokenLocation (Plus loc ) = loc
        getTokenLocation (Question loc ) = loc
        getTokenLocation (OpenCurly loc ) = loc
        getTokenLocation (ReOr loc ) = loc
        getTokenLocation (CloseCurly loc ) = loc
        getTokenLocation _ = (0,0,0,0)
        getLocation (ParserError loc _) = loc
        getLocation (ParserToken tkn) = getTokenLocation tkn
        getLocation (ParserAst (Document loc _ _)) = loc
        getLocation (ParserAst (FirstStatement loc _)) = loc
        getLocation (ParserAst (RestStatements loc _ _)) = loc
        getLocation (ParserAst (AnalyserStatement loc _)) = loc
        getLocation (ParserAst (ScannerStatement loc _)) = loc
        getLocation (ParserAst (ParserStatement loc _)) = loc
        getLocation (ParserAst (AnalyserNoOptions loc _ _ _ _ _)) = loc
        getLocation (ParserAst (AnalyserWithOptions loc _ _ _ _ _ _ _ _)) = loc
        getLocation (ParserAst (FirstAnalyserMatch loc _)) = loc
        getLocation (ParserAst (RestAnalyserMatches loc _ _)) = loc
        getLocation (ParserAst (AnalyserMatchNoAction loc _ _ _ _ _ _)) = loc
        getLocation (ParserAst (AnalyserMatchWithReturn loc _ _ _ _ _ _ _ _)) = loc
        getLocation (ParserAst (AnalyserMatchWithGoto loc _ _ _ _ _ _ _ _ _)) = loc
        getLocation (ParserAst (AnalyserMatchError loc _ _ _ _ _ _)) = loc
        getLocation (ParserAst (EmptyScanner loc _ _ _ _ _)) = loc
        getLocation (ParserAst (ScannerWithRules loc _ _ _ _ _ _)) = loc
        getLocation (ParserAst (FirstScannerOption loc _)) = loc
        getLocation (ParserAst (RestScannerOptions loc _ _)) = loc
        getLocation (ParserAst (ProcessingScannerOption loc _ _)) = loc
        getLocation (ParserAst (IgnoringScannerOption loc _ _ _ _)) = loc
        getLocation (ParserAst (FirstScannerMatch loc _)) = loc
        getLocation (ParserAst (RestScannerMatches loc _ _)) = loc
        getLocation (ParserAst (ScannerMatchNoAction loc _ _ _ _)) = loc
        getLocation (ParserAst (ScannerMatchWithReturn loc _ _ _ _ _ _)) = loc
        getLocation (ParserAst (ScannerMatchWithGoto loc _ _ _ _ _ _ _)) = loc
        getLocation (ParserAst (ScannerMatchError loc _ _ _ _)) = loc
        getLocation (ParserAst (Parser loc _ _ _ _ _ _ _)) = loc
        getLocation (ParserAst (FirstProduction loc _)) = loc
        getLocation (ParserAst (RestProductions loc _ _)) = loc
        getLocation (ParserAst (ProductionNoOptions loc _ _ _ _ _ _)) = loc
        getLocation (ParserAst (ProductionWithAssoc loc _ _ _ _ _ _ _ _ _)) = loc
        getLocation (ParserAst (ProductionWithPrec loc _ _ _ _ _ _ _ _ _ _ _)) = loc
        getLocation (ParserAst (LeftAssoc loc _)) = loc
        getLocation (ParserAst (RightAssoc loc _)) = loc
        getLocation (ParserAst (NoAssoc loc _)) = loc
        getLocation (ParserAst (FirstItem loc _)) = loc
        getLocation (ParserAst (RestItems loc _ _)) = loc
        getLocation (ParserAst (FirstString loc _)) = loc
        getLocation (ParserAst (RestStrings loc _ _ _)) = loc
        getLocation (ParserAst (FirstCommaIdentifier loc _)) = loc
        getLocation (ParserAst (RestCommaIdentifiers loc _ _ _)) = loc
        getLocation (ParserAst (FirstIdentifier loc _)) = loc
        getLocation (ParserAst (RestIdentifiers loc _ _)) = loc
        getLocation (ParserAst (RECharacterExpression loc _)) = loc
        getLocation (ParserAst (REWhitespaceCharExpression loc _)) = loc
        getLocation (ParserAst (REDigitCharExpression loc _)) = loc
        getLocation (ParserAst (REAlphaNumCharExpression loc _)) = loc
        getLocation (ParserAst (RENewLineCharExpression loc _)) = loc
        getLocation (ParserAst (REReturnCharExpression loc _)) = loc
        getLocation (ParserAst (RETabCharExpression loc _)) = loc
        getLocation (ParserAst (REFormFeedCharExpression loc _)) = loc
        getLocation (ParserAst (RERangeExpression loc _ _ _)) = loc
        getLocation (ParserAst (RENotRangeExpression loc _ _ _ _)) = loc
        getLocation (ParserAst (REAnyCharExpression loc _)) = loc
        getLocation (ParserAst (REThenExpression loc _ _)) = loc
        getLocation (ParserAst (REZeroOrMoreExpression loc _ _)) = loc
        getLocation (ParserAst (REOneOrMoreExpression loc _ _)) = loc
        getLocation (ParserAst (REZeroOrOneExpression loc _ _)) = loc
        getLocation (ParserAst (REScopedExpression loc _ _ _)) = loc
        getLocation (ParserAst (REOrExpression loc _ _ _)) = loc
        getLocation (ParserAst (REFirstRangeExpression loc _)) = loc
        getLocation (ParserAst (RERestRangeExpression loc _ _)) = loc
        getLocation (ParserAst (RECharacterRangeExpression loc _)) = loc
        getLocation (ParserAst (REWhitespaceCharRangeExpression loc _)) = loc
        getLocation (ParserAst (REDigitCharRangeExpression loc _)) = loc
        getLocation (ParserAst (REAlphaNumCharRangeExpression loc _)) = loc
        getLocation (ParserAst (RENewLineCharRangeExpression loc _)) = loc
        getLocation (ParserAst (REReturnCharRangeExpression loc _)) = loc
        getLocation (ParserAst (RETabCharRangeExpression loc _)) = loc
        getLocation (ParserAst (REFormFeedCharRangeExpression loc _)) = loc
        getLocation (ParserAst (RECharRangeRangeExpression loc _ _ _)) = loc
        getLocation (ParserAst (TEIdentifierExpression loc _)) = loc
        getLocation (ParserAst (TENotRangeExpression loc _ _ _ _)) = loc
        getLocation (ParserAst (TEThenExpression loc _ _)) = loc
        getLocation (ParserAst (TEZeroOrMoreExpression loc _ _)) = loc
        getLocation (ParserAst (TEOneOrMoreExpression loc _ _)) = loc
        getLocation (ParserAst (TEZeroOrOneExpression loc _ _)) = loc
        getLocation (ParserAst (TEScopedExpression loc _ _ _)) = loc
        getLocation (ParserAst (TEOrExpression loc _ _ _)) = loc
        getSpan x y =
            let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation x, getLocation y)
            in (sl,sc,el,ec)

        -- getGoto :: Int -> String -> Int
        getGoto 0 "Analyser" = 1
        getGoto 0 "Parser" = 2
        getGoto 0 "Scanner" = 3
        getGoto 0 "Statement" = 4
        getGoto 0 "Statements" = 5
        getGoto 5 "Analyser" = 1
        getGoto 5 "Parser" = 2
        getGoto 5 "Scanner" = 3
        getGoto 5 "Statement" = 149
        getGoto 9 "ScannerOption" = 10
        getGoto 9 "ScannerOptions" = 11
        getGoto 11 "ScannerOption" = 21
        getGoto 15 "CommaIdentifiers" = 16
        getGoto 22 "ScannerMatch" = 23
        getGoto 22 "ScannerMatches" = 24
        getGoto 22 "TE" = 25
        getGoto 24 "ScannerMatch" = 54
        getGoto 24 "TE" = 25
        getGoto 25 "TE" = 31
        getGoto 28 "TE" = 30
        getGoto 30 "TE" = 31
        getGoto 32 "TE" = 37
        getGoto 38 "Identifiers" = 39
        getGoto 59 "Production" = 60
        getGoto 59 "Productions" = 61
        getGoto 61 "Production" = 81
        getGoto 65 "Items" = 66
        getGoto 70 "Assoc" = 71
        getGoto 84 "Strings" = 140
        getGoto 85 "AnalyserMatch" = 86
        getGoto 85 "AnalyserMatches" = 87
        getGoto 87 "AnalyserMatch" = 138
        getGoto 88 "RE" = 91
        getGoto 89 "RE" = 136
        getGoto 91 "RE" = 121
        getGoto 92 "RERange" = 101
        getGoto 92 "RERanges" = 102
        getGoto 102 "RERange" = 115
        getGoto 110 "RERange" = 101
        getGoto 110 "RERanges" = 114
        getGoto 114 "RERange" = 115
        getGoto 123 "RE" = 124
        getGoto 136 "RE" = 121
        getGoto 144 "AnalyserMatch" = 86
        getGoto 144 "AnalyserMatches" = 145
        getGoto 145 "AnalyserMatch" = 138

        -- generateError [ParserValue] -> ParserValue
        generateError [] = ParserError (0,0,0,0) "Internal Parser Error"
        generateError (f:ps) =
            let last [] = getLocation f
                last [x] = getLocation x
                last (x:xs) = last xs
                ((_,_,el,ec),(sl,sc,_,_)) = (getLocation f, last ps)
            in ParserError (sl,sc,el,ec) "Internal Parser Error"

        -- reduceDocument :: [ParserValue] -> ParserValue
        reduceDocument [value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (Document (sl,sc,el,ec) v1 v2 )
        reduceDocument values = generateError values

        -- reduceFirstStatement :: [ParserValue] -> ParserValue
        reduceFirstStatement [value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (FirstStatement (sl,sc,el,ec) v1 )
        reduceFirstStatement values = generateError values

        -- reduceRestStatements :: [ParserValue] -> ParserValue
        reduceRestStatements [value2@(ParserAst v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (RestStatements (sl,sc,el,ec) v1 v2 )
        reduceRestStatements values = generateError values

        -- reduceAnalyserStatement :: [ParserValue] -> ParserValue
        reduceAnalyserStatement [value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (AnalyserStatement (sl,sc,el,ec) v1 )
        reduceAnalyserStatement values = generateError values

        -- reduceScannerStatement :: [ParserValue] -> ParserValue
        reduceScannerStatement [value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (ScannerStatement (sl,sc,el,ec) v1 )
        reduceScannerStatement values = generateError values

        -- reduceParserStatement :: [ParserValue] -> ParserValue
        reduceParserStatement [value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (ParserStatement (sl,sc,el,ec) v1 )
        reduceParserStatement values = generateError values

        -- reduceAnalyserNoOptions :: [ParserValue] -> ParserValue
        reduceAnalyserNoOptions [value5@(ParserToken v5), value4@(ParserAst v4), value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value5) in ParserAst (AnalyserNoOptions (sl,sc,el,ec) v1 v2 v3 v4 v5 )
        reduceAnalyserNoOptions values = generateError values

        -- reduceAnalyserWithOptions :: [ParserValue] -> ParserValue
        reduceAnalyserWithOptions [value8@(ParserToken v8), value7@(ParserAst v7), value6@(ParserToken v6), value5@(ParserToken v5), value4@(ParserAst v4), value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value8) in ParserAst (AnalyserWithOptions (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7 v8 )
        reduceAnalyserWithOptions values = generateError values

        -- reduceFirstAnalyserMatch :: [ParserValue] -> ParserValue
        reduceFirstAnalyserMatch [value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (FirstAnalyserMatch (sl,sc,el,ec) v1 )
        reduceFirstAnalyserMatch values = generateError values

        -- reduceRestAnalyserMatches :: [ParserValue] -> ParserValue
        reduceRestAnalyserMatches [value2@(ParserAst v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (RestAnalyserMatches (sl,sc,el,ec) v1 v2 )
        reduceRestAnalyserMatches values = generateError values

        -- reduceAnalyserMatchNoAction :: [ParserValue] -> ParserValue
        reduceAnalyserMatchNoAction [value6@(ParserToken v6), value5@(ParserToken v5), value4@(ParserToken v4), value3@(ParserToken v3), value2@(ParserAst v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value6) in ParserAst (AnalyserMatchNoAction (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 )
        reduceAnalyserMatchNoAction values = generateError values

        -- reduceAnalyserMatchWithReturn :: [ParserValue] -> ParserValue
        reduceAnalyserMatchWithReturn [value8@(ParserToken v8), value7@(ParserToken v7), value6@(ParserToken v6), value5@(ParserToken v5), value4@(ParserToken v4), value3@(ParserToken v3), value2@(ParserAst v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value8) in ParserAst (AnalyserMatchWithReturn (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7 v8 )
        reduceAnalyserMatchWithReturn values = generateError values

        -- reduceAnalyserMatchWithGoto :: [ParserValue] -> ParserValue
        reduceAnalyserMatchWithGoto [value9@(ParserToken v9), value8@(ParserToken v8), value7@(ParserToken v7), value6@(ParserToken v6), value5@(ParserToken v5), value4@(ParserToken v4), value3@(ParserToken v3), value2@(ParserAst v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value9) in ParserAst (AnalyserMatchWithGoto (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7 v8 v9 )
        reduceAnalyserMatchWithGoto values = generateError values

        -- reduceAnalyserMatchError :: [ParserValue] -> ParserValue
        reduceAnalyserMatchError [value6@(ParserToken v6), value5@(ParserToken v5), value4@(ParserToken v4), value3@(ParserToken v3), value2@(ParserAst v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value6) in ParserAst (AnalyserMatchError (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 )
        reduceAnalyserMatchError values = generateError values

        -- reduceEmptyScanner :: [ParserValue] -> ParserValue
        reduceEmptyScanner [value5@(ParserToken v5), value4@(ParserToken v4), value3@(ParserAst v3), value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value5) in ParserAst (EmptyScanner (sl,sc,el,ec) v1 v2 v3 v4 v5 )
        reduceEmptyScanner values = generateError values

        -- reduceScannerWithRules :: [ParserValue] -> ParserValue
        reduceScannerWithRules [value6@(ParserToken v6), value5@(ParserAst v5), value4@(ParserToken v4), value3@(ParserAst v3), value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value6) in ParserAst (ScannerWithRules (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 )
        reduceScannerWithRules values = generateError values

        -- reduceFirstScannerOption :: [ParserValue] -> ParserValue
        reduceFirstScannerOption [value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (FirstScannerOption (sl,sc,el,ec) v1 )
        reduceFirstScannerOption values = generateError values

        -- reduceRestScannerOptions :: [ParserValue] -> ParserValue
        reduceRestScannerOptions [value2@(ParserAst v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (RestScannerOptions (sl,sc,el,ec) v1 v2 )
        reduceRestScannerOptions values = generateError values

        -- reduceProcessingScannerOption :: [ParserValue] -> ParserValue
        reduceProcessingScannerOption [value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (ProcessingScannerOption (sl,sc,el,ec) v1 v2 )
        reduceProcessingScannerOption values = generateError values

        -- reduceIgnoringScannerOption :: [ParserValue] -> ParserValue
        reduceIgnoringScannerOption [value4@(ParserToken v4), value3@(ParserAst v3), value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value4) in ParserAst (IgnoringScannerOption (sl,sc,el,ec) v1 v2 v3 v4 )
        reduceIgnoringScannerOption values = generateError values

        -- reduceFirstScannerMatch :: [ParserValue] -> ParserValue
        reduceFirstScannerMatch [value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (FirstScannerMatch (sl,sc,el,ec) v1 )
        reduceFirstScannerMatch values = generateError values

        -- reduceRestScannerMatches :: [ParserValue] -> ParserValue
        reduceRestScannerMatches [value2@(ParserAst v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (RestScannerMatches (sl,sc,el,ec) v1 v2 )
        reduceRestScannerMatches values = generateError values

        -- reduceScannerMatchNoAction :: [ParserValue] -> ParserValue
        reduceScannerMatchNoAction [value4@(ParserToken v4), value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value4) in ParserAst (ScannerMatchNoAction (sl,sc,el,ec) v1 v2 v3 v4 )
        reduceScannerMatchNoAction values = generateError values

        -- reduceScannerMatchWithReturn :: [ParserValue] -> ParserValue
        reduceScannerMatchWithReturn [value6@(ParserToken v6), value5@(ParserToken v5), value4@(ParserToken v4), value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value6) in ParserAst (ScannerMatchWithReturn (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 )
        reduceScannerMatchWithReturn values = generateError values

        -- reduceScannerMatchWithGoto :: [ParserValue] -> ParserValue
        reduceScannerMatchWithGoto [value7@(ParserToken v7), value6@(ParserToken v6), value5@(ParserToken v5), value4@(ParserToken v4), value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value7) in ParserAst (ScannerMatchWithGoto (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7 )
        reduceScannerMatchWithGoto values = generateError values

        -- reduceScannerMatchError :: [ParserValue] -> ParserValue
        reduceScannerMatchError [value4@(ParserToken v4), value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value4) in ParserAst (ScannerMatchError (sl,sc,el,ec) v1 v2 v3 v4 )
        reduceScannerMatchError values = generateError values

        -- reduceParser :: [ParserValue] -> ParserValue
        reduceParser [value7@(ParserToken v7), value6@(ParserAst v6), value5@(ParserToken v5), value4@(ParserToken v4), value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value7) in ParserAst (Parser (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7 )
        reduceParser values = generateError values

        -- reduceFirstProduction :: [ParserValue] -> ParserValue
        reduceFirstProduction [value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (FirstProduction (sl,sc,el,ec) v1 )
        reduceFirstProduction values = generateError values

        -- reduceRestProductions :: [ParserValue] -> ParserValue
        reduceRestProductions [value2@(ParserAst v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (RestProductions (sl,sc,el,ec) v1 v2 )
        reduceRestProductions values = generateError values

        -- reduceProductionNoOptions :: [ParserValue] -> ParserValue
        reduceProductionNoOptions [value6@(ParserToken v6), value5@(ParserAst v5), value4@(ParserToken v4), value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value6) in ParserAst (ProductionNoOptions (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 )
        reduceProductionNoOptions values = generateError values

        -- reduceProductionWithAssoc :: [ParserValue] -> ParserValue
        reduceProductionWithAssoc [value9@(ParserToken v9), value8@(ParserToken v8), value7@(ParserAst v7), value6@(ParserToken v6), value5@(ParserAst v5), value4@(ParserToken v4), value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value9) in ParserAst (ProductionWithAssoc (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7 v8 v9 )
        reduceProductionWithAssoc values = generateError values

        -- reduceProductionWithPrec :: [ParserValue] -> ParserValue
        reduceProductionWithPrec [value11@(ParserToken v11), value10@(ParserToken v10), value9@(ParserToken v9), value8@(ParserToken v8), value7@(ParserAst v7), value6@(ParserToken v6), value5@(ParserAst v5), value4@(ParserToken v4), value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value11) in ParserAst (ProductionWithPrec (sl,sc,el,ec) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 )
        reduceProductionWithPrec values = generateError values

        -- reduceLeftAssoc :: [ParserValue] -> ParserValue
        reduceLeftAssoc [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (LeftAssoc (sl,sc,el,ec) v1 )
        reduceLeftAssoc values = generateError values

        -- reduceRightAssoc :: [ParserValue] -> ParserValue
        reduceRightAssoc [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (RightAssoc (sl,sc,el,ec) v1 )
        reduceRightAssoc values = generateError values

        -- reduceNoAssoc :: [ParserValue] -> ParserValue
        reduceNoAssoc [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (NoAssoc (sl,sc,el,ec) v1 )
        reduceNoAssoc values = generateError values

        -- reduceFirstItem :: [ParserValue] -> ParserValue
        reduceFirstItem [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (FirstItem (sl,sc,el,ec) v1 )
        reduceFirstItem values = generateError values

        -- reduceRestItems :: [ParserValue] -> ParserValue
        reduceRestItems [value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (RestItems (sl,sc,el,ec) v1 v2 )
        reduceRestItems values = generateError values

        -- reduceFirstString :: [ParserValue] -> ParserValue
        reduceFirstString [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (FirstString (sl,sc,el,ec) v1 )
        reduceFirstString values = generateError values

        -- reduceRestStrings :: [ParserValue] -> ParserValue
        reduceRestStrings [value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value3) in ParserAst (RestStrings (sl,sc,el,ec) v1 v2 v3 )
        reduceRestStrings values = generateError values

        -- reduceFirstCommaIdentifier :: [ParserValue] -> ParserValue
        reduceFirstCommaIdentifier [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (FirstCommaIdentifier (sl,sc,el,ec) v1 )
        reduceFirstCommaIdentifier values = generateError values

        -- reduceRestCommaIdentifiers :: [ParserValue] -> ParserValue
        reduceRestCommaIdentifiers [value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value3) in ParserAst (RestCommaIdentifiers (sl,sc,el,ec) v1 v2 v3 )
        reduceRestCommaIdentifiers values = generateError values

        -- reduceFirstIdentifier :: [ParserValue] -> ParserValue
        reduceFirstIdentifier [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (FirstIdentifier (sl,sc,el,ec) v1 )
        reduceFirstIdentifier values = generateError values

        -- reduceRestIdentifiers :: [ParserValue] -> ParserValue
        reduceRestIdentifiers [value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (RestIdentifiers (sl,sc,el,ec) v1 v2 )
        reduceRestIdentifiers values = generateError values

        -- reduceRECharacterExpression :: [ParserValue] -> ParserValue
        reduceRECharacterExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (RECharacterExpression (sl,sc,el,ec) v1 )
        reduceRECharacterExpression values = generateError values

        -- reduceREWhitespaceCharExpression :: [ParserValue] -> ParserValue
        reduceREWhitespaceCharExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (REWhitespaceCharExpression (sl,sc,el,ec) v1 )
        reduceREWhitespaceCharExpression values = generateError values

        -- reduceREDigitCharExpression :: [ParserValue] -> ParserValue
        reduceREDigitCharExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (REDigitCharExpression (sl,sc,el,ec) v1 )
        reduceREDigitCharExpression values = generateError values

        -- reduceREAlphaNumCharExpression :: [ParserValue] -> ParserValue
        reduceREAlphaNumCharExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (REAlphaNumCharExpression (sl,sc,el,ec) v1 )
        reduceREAlphaNumCharExpression values = generateError values

        -- reduceRENewLineCharExpression :: [ParserValue] -> ParserValue
        reduceRENewLineCharExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (RENewLineCharExpression (sl,sc,el,ec) v1 )
        reduceRENewLineCharExpression values = generateError values

        -- reduceREReturnCharExpression :: [ParserValue] -> ParserValue
        reduceREReturnCharExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (REReturnCharExpression (sl,sc,el,ec) v1 )
        reduceREReturnCharExpression values = generateError values

        -- reduceRETabCharExpression :: [ParserValue] -> ParserValue
        reduceRETabCharExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (RETabCharExpression (sl,sc,el,ec) v1 )
        reduceRETabCharExpression values = generateError values

        -- reduceREFormFeedCharExpression :: [ParserValue] -> ParserValue
        reduceREFormFeedCharExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (REFormFeedCharExpression (sl,sc,el,ec) v1 )
        reduceREFormFeedCharExpression values = generateError values

        -- reduceRERangeExpression :: [ParserValue] -> ParserValue
        reduceRERangeExpression [value3@(ParserToken v3), value2@(ParserAst v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value3) in ParserAst (RERangeExpression (sl,sc,el,ec) v1 v2 v3 )
        reduceRERangeExpression values = generateError values

        -- reduceRENotRangeExpression :: [ParserValue] -> ParserValue
        reduceRENotRangeExpression [value4@(ParserToken v4), value3@(ParserAst v3), value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value4) in ParserAst (RENotRangeExpression (sl,sc,el,ec) v1 v2 v3 v4 )
        reduceRENotRangeExpression values = generateError values

        -- reduceREAnyCharExpression :: [ParserValue] -> ParserValue
        reduceREAnyCharExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (REAnyCharExpression (sl,sc,el,ec) v1 )
        reduceREAnyCharExpression values = generateError values

        -- reduceREThenExpression :: [ParserValue] -> ParserValue
        reduceREThenExpression [value2@(ParserAst v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (REThenExpression (sl,sc,el,ec) v1 v2 )
        reduceREThenExpression values = generateError values

        -- reduceREZeroOrMoreExpression :: [ParserValue] -> ParserValue
        reduceREZeroOrMoreExpression [value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (REZeroOrMoreExpression (sl,sc,el,ec) v1 v2 )
        reduceREZeroOrMoreExpression values = generateError values

        -- reduceREOneOrMoreExpression :: [ParserValue] -> ParserValue
        reduceREOneOrMoreExpression [value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (REOneOrMoreExpression (sl,sc,el,ec) v1 v2 )
        reduceREOneOrMoreExpression values = generateError values

        -- reduceREZeroOrOneExpression :: [ParserValue] -> ParserValue
        reduceREZeroOrOneExpression [value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (REZeroOrOneExpression (sl,sc,el,ec) v1 v2 )
        reduceREZeroOrOneExpression values = generateError values

        -- reduceREScopedExpression :: [ParserValue] -> ParserValue
        reduceREScopedExpression [value3@(ParserToken v3), value2@(ParserAst v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value3) in ParserAst (REScopedExpression (sl,sc,el,ec) v1 v2 v3 )
        reduceREScopedExpression values = generateError values

        -- reduceREOrExpression :: [ParserValue] -> ParserValue
        reduceREOrExpression [value3@(ParserAst v3), value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value3) in ParserAst (REOrExpression (sl,sc,el,ec) v1 v2 v3 )
        reduceREOrExpression values = generateError values

        -- reduceREFirstRangeExpression :: [ParserValue] -> ParserValue
        reduceREFirstRangeExpression [value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (REFirstRangeExpression (sl,sc,el,ec) v1 )
        reduceREFirstRangeExpression values = generateError values

        -- reduceRERestRangeExpression :: [ParserValue] -> ParserValue
        reduceRERestRangeExpression [value2@(ParserAst v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (RERestRangeExpression (sl,sc,el,ec) v1 v2 )
        reduceRERestRangeExpression values = generateError values

        -- reduceRECharacterRangeExpression :: [ParserValue] -> ParserValue
        reduceRECharacterRangeExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (RECharacterRangeExpression (sl,sc,el,ec) v1 )
        reduceRECharacterRangeExpression values = generateError values

        -- reduceREWhitespaceCharRangeExpression :: [ParserValue] -> ParserValue
        reduceREWhitespaceCharRangeExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (REWhitespaceCharRangeExpression (sl,sc,el,ec) v1 )
        reduceREWhitespaceCharRangeExpression values = generateError values

        -- reduceREDigitCharRangeExpression :: [ParserValue] -> ParserValue
        reduceREDigitCharRangeExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (REDigitCharRangeExpression (sl,sc,el,ec) v1 )
        reduceREDigitCharRangeExpression values = generateError values

        -- reduceREAlphaNumCharRangeExpression :: [ParserValue] -> ParserValue
        reduceREAlphaNumCharRangeExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (REAlphaNumCharRangeExpression (sl,sc,el,ec) v1 )
        reduceREAlphaNumCharRangeExpression values = generateError values

        -- reduceRENewLineCharRangeExpression :: [ParserValue] -> ParserValue
        reduceRENewLineCharRangeExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (RENewLineCharRangeExpression (sl,sc,el,ec) v1 )
        reduceRENewLineCharRangeExpression values = generateError values

        -- reduceREReturnCharRangeExpression :: [ParserValue] -> ParserValue
        reduceREReturnCharRangeExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (REReturnCharRangeExpression (sl,sc,el,ec) v1 )
        reduceREReturnCharRangeExpression values = generateError values

        -- reduceRETabCharRangeExpression :: [ParserValue] -> ParserValue
        reduceRETabCharRangeExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (RETabCharRangeExpression (sl,sc,el,ec) v1 )
        reduceRETabCharRangeExpression values = generateError values

        -- reduceREFormFeedCharRangeExpression :: [ParserValue] -> ParserValue
        reduceREFormFeedCharRangeExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (REFormFeedCharRangeExpression (sl,sc,el,ec) v1 )
        reduceREFormFeedCharRangeExpression values = generateError values

        -- reduceRECharRangeRangeExpression :: [ParserValue] -> ParserValue
        reduceRECharRangeRangeExpression [value3@(ParserToken v3), value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value3) in ParserAst (RECharRangeRangeExpression (sl,sc,el,ec) v1 v2 v3 )
        reduceRECharRangeRangeExpression values = generateError values

        -- reduceTEIdentifierExpression :: [ParserValue] -> ParserValue
        reduceTEIdentifierExpression [value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value1) in ParserAst (TEIdentifierExpression (sl,sc,el,ec) v1 )
        reduceTEIdentifierExpression values = generateError values

        -- reduceTENotRangeExpression :: [ParserValue] -> ParserValue
        reduceTENotRangeExpression [value4@(ParserToken v4), value3@(ParserAst v3), value2@(ParserToken v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value4) in ParserAst (TENotRangeExpression (sl,sc,el,ec) v1 v2 v3 v4 )
        reduceTENotRangeExpression values = generateError values

        -- reduceTEThenExpression :: [ParserValue] -> ParserValue
        reduceTEThenExpression [value2@(ParserAst v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (TEThenExpression (sl,sc,el,ec) v1 v2 )
        reduceTEThenExpression values = generateError values

        -- reduceTEZeroOrMoreExpression :: [ParserValue] -> ParserValue
        reduceTEZeroOrMoreExpression [value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (TEZeroOrMoreExpression (sl,sc,el,ec) v1 v2 )
        reduceTEZeroOrMoreExpression values = generateError values

        -- reduceTEOneOrMoreExpression :: [ParserValue] -> ParserValue
        reduceTEOneOrMoreExpression [value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (TEOneOrMoreExpression (sl,sc,el,ec) v1 v2 )
        reduceTEOneOrMoreExpression values = generateError values

        -- reduceTEZeroOrOneExpression :: [ParserValue] -> ParserValue
        reduceTEZeroOrOneExpression [value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value2) in ParserAst (TEZeroOrOneExpression (sl,sc,el,ec) v1 v2 )
        reduceTEZeroOrOneExpression values = generateError values

        -- reduceTEScopedExpression :: [ParserValue] -> ParserValue
        reduceTEScopedExpression [value3@(ParserToken v3), value2@(ParserAst v2), value1@(ParserToken v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value3) in ParserAst (TEScopedExpression (sl,sc,el,ec) v1 v2 v3 )
        reduceTEScopedExpression values = generateError values

        -- reduceTEOrExpression :: [ParserValue] -> ParserValue
        reduceTEOrExpression [value3@(ParserAst v3), value2@(ParserToken v2), value1@(ParserAst v1)] = let ((sl,sc,_,_),(_,_,el,ec)) = (getLocation value1, getLocation value3) in ParserAst (TEOrExpression (sl,sc,el,ec) v1 v2 v3 )
        reduceTEOrExpression values = generateError values

        -- process :: [ParserValue] -> ([ParserValue] -> ParserValue) -> ParserValue
        process values f =
            let process' [] x = f x
                process' (err@(ParserError _ _):is) x = err
                process' (i:is) x = process' is (i:x)
            in process' values []

        -- parse' :: [Int] -> [Token] -> [ParserValue] -> ParserValue

        -- State 0
        parse' stateStack@(0:states) ((token@(KwAnalyser _)):tokens) valueStack = -- On analyser S6
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 0 " ++ (show token) ++ ": S6") $ parse' (6:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(0:states) ((token@(KwParser _)):tokens) valueStack = -- On parser S7
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 0 " ++ (show token) ++ ": S7") $ parse' (7:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(0:states) ((token@(KwScanner _)):tokens) valueStack = -- On scanner S8
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 0 " ++ (show token) ++ ": S8") $ parse' (8:stateStack) tokens ((ParserToken token):valueStack)
        parse' (0:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (0:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 1
        parse' (1:gs:states) tokens (value1:values) = 
            trace' ("Reduce AnalyserStatement: Statement = Analyser") $ parse' ((getGoto gs "Statement"):gs:states) tokens ((process [value1] reduceAnalyserStatement):values)
        parse' (1:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (1:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 2
        parse' (2:gs:states) tokens (value1:values) = 
            trace' ("Reduce ParserStatement: Statement = Parser") $ parse' ((getGoto gs "Statement"):gs:states) tokens ((process [value1] reduceParserStatement):values)
        parse' (2:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (2:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 3
        parse' (3:gs:states) tokens (value1:values) = 
            trace' ("Reduce ScannerStatement: Statement = Scanner") $ parse' ((getGoto gs "Statement"):gs:states) tokens ((process [value1] reduceScannerStatement):values)
        parse' (3:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (3:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 4
        parse' (4:gs:states) tokens (value1:values) = 
            trace' ("Reduce FirstStatement: Statements = Statement") $ parse' ((getGoto gs "Statements"):gs:states) tokens ((process [value1] reduceFirstStatement):values)
        parse' (4:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (4:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 5
        parse' stateStack@(5:states) ((token@(EOD _)):tokens) valueStack = -- On $ S148
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 5 " ++ (show token) ++ ": S148") $ parse' (148:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(5:states) ((token@(KwAnalyser _)):tokens) valueStack = -- On analyser S6
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 5 " ++ (show token) ++ ": S6") $ parse' (6:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(5:states) ((token@(KwParser _)):tokens) valueStack = -- On parser S7
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 5 " ++ (show token) ++ ": S7") $ parse' (7:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(5:states) ((token@(KwScanner _)):tokens) valueStack = -- On scanner S8
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 5 " ++ (show token) ++ ": S8") $ parse' (8:stateStack) tokens ((ParserToken token):valueStack)
        parse' (5:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (5:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 6
        parse' stateStack@(6:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S83
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 6 " ++ (show token) ++ ": S83") $ parse' (83:stateStack) tokens ((ParserToken token):valueStack)
        parse' (6:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (6:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 7
        parse' stateStack@(7:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S56
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 7 " ++ (show token) ++ ": S56") $ parse' (56:stateStack) tokens ((ParserToken token):valueStack)
        parse' (7:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (7:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 8
        parse' stateStack@(8:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S9
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 8 " ++ (show token) ++ ": S9") $ parse' (9:stateStack) tokens ((ParserToken token):valueStack)
        parse' (8:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (8:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 9
        parse' stateStack@(9:states) ((token@(KwIgnoring _)):tokens) valueStack = -- On ignoring S12
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 9 " ++ (show token) ++ ": S12") $ parse' (12:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(9:states) ((token@(KwProcessing _)):tokens) valueStack = -- On processing S13
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 9 " ++ (show token) ++ ": S13") $ parse' (13:stateStack) tokens ((ParserToken token):valueStack)
        parse' (9:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (9:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 10
        parse' (10:gs:states) tokens (value1:values) = 
            trace' ("Reduce FirstScannerOption: ScannerOptions = ScannerOption") $ parse' ((getGoto gs "ScannerOptions"):gs:states) tokens ((process [value1] reduceFirstScannerOption):values)
        parse' (10:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (10:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 11
        parse' stateStack@(11:states) ((token@(KwIgnoring _)):tokens) valueStack = -- On ignoring S12
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 11 " ++ (show token) ++ ": S12") $ parse' (12:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(11:states) ((token@(KwProcessing _)):tokens) valueStack = -- On processing S13
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 11 " ++ (show token) ++ ": S13") $ parse' (13:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(11:states) ((token@(OpenCurly _)):tokens) valueStack = -- On { S22
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 11 " ++ (show token) ++ ": S22") $ parse' (22:stateStack) tokens ((ParserToken token):valueStack)
        parse' (11:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (11:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 12
        parse' stateStack@(12:states) ((token@(OpenBracket _)):tokens) valueStack = -- On openbracket S15
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 12 " ++ (show token) ++ ": S15") $ parse' (15:stateStack) tokens ((ParserToken token):valueStack)
        parse' (12:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (12:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 13
        parse' stateStack@(13:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S14
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 13 " ++ (show token) ++ ": S14") $ parse' (14:stateStack) tokens ((ParserToken token):valueStack)
        parse' (13:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (13:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 14
        parse' (14:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce ProcessingScannerOption: ScannerOption = processing identifier") $ parse' ((getGoto gs "ScannerOption"):gs:states) tokens ((process [value1,value2] reduceProcessingScannerOption):values)
        parse' (14:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (14:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 15
        parse' stateStack@(15:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S17
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 15 " ++ (show token) ++ ": S17") $ parse' (17:stateStack) tokens ((ParserToken token):valueStack)
        parse' (15:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (15:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 16
        parse' stateStack@(16:states) ((token@(Comma _)):tokens) valueStack = -- On , S18
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 16 " ++ (show token) ++ ": S18") $ parse' (18:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(16:states) ((token@(CloseBracket _)):tokens) valueStack = -- On closebracket S19
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 16 " ++ (show token) ++ ": S19") $ parse' (19:stateStack) tokens ((ParserToken token):valueStack)
        parse' (16:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (16:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 17
        parse' (17:gs:states) tokens (value1:values) = 
            trace' ("Reduce FirstCommaIdentifier: CommaIdentifiers = identifier") $ parse' ((getGoto gs "CommaIdentifiers"):gs:states) tokens ((process [value1] reduceFirstCommaIdentifier):values)
        parse' (17:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (17:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 18
        parse' stateStack@(18:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S20
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 18 " ++ (show token) ++ ": S20") $ parse' (20:stateStack) tokens ((ParserToken token):valueStack)
        parse' (18:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (18:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 19
        parse' (19:_:_:_:gs:states) tokens (value4:value3:value2:value1:values) = 
            trace' ("Reduce IgnoringScannerOption: ScannerOption = ignoring openbracket CommaIdentifiers closebracket") $ parse' ((getGoto gs "ScannerOption"):gs:states) tokens ((process [value1,value2,value3,value4] reduceIgnoringScannerOption):values)
        parse' (19:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (19:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 20
        parse' (20:_:_:gs:states) tokens (value3:value2:value1:values) = 
            trace' ("Reduce RestCommaIdentifiers: CommaIdentifiers = CommaIdentifiers , identifier") $ parse' ((getGoto gs "CommaIdentifiers"):gs:states) tokens ((process [value1,value2,value3] reduceRestCommaIdentifiers):values)
        parse' (20:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (20:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 21
        parse' (21:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce RestScannerOptions: ScannerOptions = ScannerOptions ScannerOption") $ parse' ((getGoto gs "ScannerOptions"):gs:states) tokens ((process [value1,value2] reduceRestScannerOptions):values)
        parse' (21:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (21:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 22
        parse' stateStack@(22:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S26
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 22 " ++ (show token) ++ ": S26") $ parse' (26:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(22:states) ((token@(OpenBracket _)):tokens) valueStack = -- On openbracket S27
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 22 " ++ (show token) ++ ": S27") $ parse' (27:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(22:states) ((token@(OpenParenthesis _)):tokens) valueStack = -- On te-openparenthesis S28
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 22 " ++ (show token) ++ ": S28") $ parse' (28:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(22:states) ((token@(CloseCurly _)):tokens) valueStack = -- On } S29
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 22 " ++ (show token) ++ ": S29") $ parse' (29:stateStack) tokens ((ParserToken token):valueStack)
        parse' (22:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (22:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 23
        parse' (23:gs:states) tokens (value1:values) = 
            trace' ("Reduce FirstScannerMatch: ScannerMatches = ScannerMatch") $ parse' ((getGoto gs "ScannerMatches"):gs:states) tokens ((process [value1] reduceFirstScannerMatch):values)
        parse' (23:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (23:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 24
        parse' stateStack@(24:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S26
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 24 " ++ (show token) ++ ": S26") $ parse' (26:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(24:states) ((token@(OpenBracket _)):tokens) valueStack = -- On openbracket S27
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 24 " ++ (show token) ++ ": S27") $ parse' (27:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(24:states) ((token@(OpenParenthesis _)):tokens) valueStack = -- On te-openparenthesis S28
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 24 " ++ (show token) ++ ": S28") $ parse' (28:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(24:states) ((token@(CloseCurly _)):tokens) valueStack = -- On } S55
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 24 " ++ (show token) ++ ": S55") $ parse' (55:stateStack) tokens ((ParserToken token):valueStack)
        parse' (24:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (24:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 25
        parse' stateStack@(25:states) ((token@(Arrow _)):tokens) valueStack = -- On -> S43
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 25 " ++ (show token) ++ ": S43") $ parse' (43:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(25:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S26
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 25 " ++ (show token) ++ ": S26") $ parse' (26:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(25:states) ((token@(OpenBracket _)):tokens) valueStack = -- On openbracket S27
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 25 " ++ (show token) ++ ": S27") $ parse' (27:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(25:states) ((token@(Pipe _)):tokens) valueStack = -- On pipe S32
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 25 " ++ (show token) ++ ": S32") $ parse' (32:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(25:states) ((token@(Asterisk _)):tokens) valueStack = -- On te-asterisk S33
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 25 " ++ (show token) ++ ": S33") $ parse' (33:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(25:states) ((token@(OpenParenthesis _)):tokens) valueStack = -- On te-openparenthesis S28
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 25 " ++ (show token) ++ ": S28") $ parse' (28:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(25:states) ((token@(Plus _)):tokens) valueStack = -- On te-plus S35
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 25 " ++ (show token) ++ ": S35") $ parse' (35:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(25:states) ((token@(Question _)):tokens) valueStack = -- On te-question S36
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 25 " ++ (show token) ++ ": S36") $ parse' (36:stateStack) tokens ((ParserToken token):valueStack)
        parse' (25:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (25:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 26
        parse' (26:gs:states) tokens (value1:values) = 
            trace' ("Reduce TEIdentifierExpression: TE = identifier") $ parse' ((getGoto gs "TE"):gs:states) tokens ((process [value1] reduceTEIdentifierExpression):values)
        parse' (26:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (26:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 27
        parse' stateStack@(27:states) ((token@(Hat _)):tokens) valueStack = -- On te-hat S38
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 27 " ++ (show token) ++ ": S38") $ parse' (38:stateStack) tokens ((ParserToken token):valueStack)
        parse' (27:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (27:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 28
        parse' stateStack@(28:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S26
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 28 " ++ (show token) ++ ": S26") $ parse' (26:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(28:states) ((token@(OpenBracket _)):tokens) valueStack = -- On openbracket S27
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 28 " ++ (show token) ++ ": S27") $ parse' (27:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(28:states) ((token@(OpenParenthesis _)):tokens) valueStack = -- On te-openparenthesis S28
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 28 " ++ (show token) ++ ": S28") $ parse' (28:stateStack) tokens ((ParserToken token):valueStack)
        parse' (28:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (28:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 29
        parse' (29:_:_:_:_:gs:states) tokens (value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce EmptyScanner: Scanner = scanner identifier ScannerOptions { }") $ parse' ((getGoto gs "Scanner"):gs:states) tokens ((process [value1,value2,value3,value4,value5] reduceEmptyScanner):values)
        parse' (29:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (29:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 30
        parse' stateStack@(30:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S26
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 30 " ++ (show token) ++ ": S26") $ parse' (26:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(30:states) ((token@(OpenBracket _)):tokens) valueStack = -- On openbracket S27
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 30 " ++ (show token) ++ ": S27") $ parse' (27:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(30:states) ((token@(Pipe _)):tokens) valueStack = -- On pipe S32
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 30 " ++ (show token) ++ ": S32") $ parse' (32:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(30:states) ((token@(Asterisk _)):tokens) valueStack = -- On te-asterisk S33
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 30 " ++ (show token) ++ ": S33") $ parse' (33:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(30:states) ((token@(CloseParenthesis _)):tokens) valueStack = -- On te-closeparenthesis S34
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 30 " ++ (show token) ++ ": S34") $ parse' (34:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(30:states) ((token@(OpenParenthesis _)):tokens) valueStack = -- On te-openparenthesis S28
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 30 " ++ (show token) ++ ": S28") $ parse' (28:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(30:states) ((token@(Plus _)):tokens) valueStack = -- On te-plus S35
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 30 " ++ (show token) ++ ": S35") $ parse' (35:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(30:states) ((token@(Question _)):tokens) valueStack = -- On te-question S36
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 30 " ++ (show token) ++ ": S36") $ parse' (36:stateStack) tokens ((ParserToken token):valueStack)
        parse' (30:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (30:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 31
          -- Resolved shift-reduce conflict with next token "identifier" for "TEIdentifierExpression: TE = identifier" vs reducing "TEThenExpression: TE = TE TE" as 'reduce' using Left associativity TEIdentifierExpression vs TEThenExpression
          -- Resolved shift-reduce conflict with next token "openbracket" for "TENotRangeExpression: TE = openbracket te-hat Identifiers closebracket" vs reducing "TEThenExpression: TE = TE TE" as 'reduce' using Left associativity TENotRangeExpression vs TEThenExpression
          -- Resolved shift-reduce conflict with next token "pipe" for "TEOrExpression: TE = TE pipe TE" vs reducing "TEThenExpression: TE = TE TE" as 'reduce' using Left associativity TEOrExpression vs TEThenExpression
          -- Resolved shift-reduce conflict with next token "te-asterisk" for "TEZeroOrMoreExpression: TE = TE te-asterisk" vs reducing "TEThenExpression: TE = TE TE" as 'shift' using precedence TEZeroOrMoreExpression (1) > TEThenExpression (0)
          -- Resolved shift-reduce conflict with next token "te-openparenthesis" for "TEScopedExpression: TE = te-openparenthesis TE te-closeparenthesis" vs reducing "TEThenExpression: TE = TE TE" as 'reduce' using Left associativity TEScopedExpression vs TEThenExpression
          -- Resolved shift-reduce conflict with next token "te-plus" for "TEOneOrMoreExpression: TE = TE te-plus" vs reducing "TEThenExpression: TE = TE TE" as 'shift' using precedence TEOneOrMoreExpression (1) > TEThenExpression (0)
          -- Resolved shift-reduce conflict with next token "te-question" for "TEZeroOrOneExpression: TE = TE te-question" vs reducing "TEThenExpression: TE = TE TE" as 'shift' using precedence TEZeroOrOneExpression (1) > TEThenExpression (0)
        parse' stateStack@(31:states) ((token@(Asterisk _)):tokens) valueStack = -- On te-asterisk S33
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 31 " ++ (show token) ++ ": S33") $ parse' (33:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(31:states) ((token@(Plus _)):tokens) valueStack = -- On te-plus S35
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 31 " ++ (show token) ++ ": S35") $ parse' (35:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(31:states) ((token@(Question _)):tokens) valueStack = -- On te-question S36
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 31 " ++ (show token) ++ ": S36") $ parse' (36:stateStack) tokens ((ParserToken token):valueStack)
        parse' (31:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce TEThenExpression: TE = TE TE") $ parse' ((getGoto gs "TE"):gs:states) tokens ((process [value1,value2] reduceTEThenExpression):values)
        parse' (31:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (31:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 32
        parse' stateStack@(32:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S26
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 32 " ++ (show token) ++ ": S26") $ parse' (26:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(32:states) ((token@(OpenBracket _)):tokens) valueStack = -- On openbracket S27
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 32 " ++ (show token) ++ ": S27") $ parse' (27:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(32:states) ((token@(OpenParenthesis _)):tokens) valueStack = -- On te-openparenthesis S28
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 32 " ++ (show token) ++ ": S28") $ parse' (28:stateStack) tokens ((ParserToken token):valueStack)
        parse' (32:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (32:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 33
        parse' (33:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce TEZeroOrMoreExpression: TE = TE te-asterisk") $ parse' ((getGoto gs "TE"):gs:states) tokens ((process [value1,value2] reduceTEZeroOrMoreExpression):values)
        parse' (33:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (33:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 34
        parse' (34:_:_:gs:states) tokens (value3:value2:value1:values) = 
            trace' ("Reduce TEScopedExpression: TE = te-openparenthesis TE te-closeparenthesis") $ parse' ((getGoto gs "TE"):gs:states) tokens ((process [value1,value2,value3] reduceTEScopedExpression):values)
        parse' (34:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (34:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 35
        parse' (35:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce TEOneOrMoreExpression: TE = TE te-plus") $ parse' ((getGoto gs "TE"):gs:states) tokens ((process [value1,value2] reduceTEOneOrMoreExpression):values)
        parse' (35:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (35:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 36
        parse' (36:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce TEZeroOrOneExpression: TE = TE te-question") $ parse' ((getGoto gs "TE"):gs:states) tokens ((process [value1,value2] reduceTEZeroOrOneExpression):values)
        parse' (36:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (36:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 37
          -- Resolved shift-reduce conflict with next token "identifier" for "TEIdentifierExpression: TE = identifier" vs reducing "TEOrExpression: TE = TE pipe TE" as 'reduce' using Left associativity TEIdentifierExpression vs TEOrExpression
          -- Resolved shift-reduce conflict with next token "openbracket" for "TENotRangeExpression: TE = openbracket te-hat Identifiers closebracket" vs reducing "TEOrExpression: TE = TE pipe TE" as 'reduce' using Left associativity TENotRangeExpression vs TEOrExpression
          -- Resolved shift-reduce conflict with next token "pipe" for "TEOrExpression: TE = TE pipe TE" vs reducing "TEOrExpression: TE = TE pipe TE" as 'reduce' using Left associativity TEOrExpression vs TEOrExpression
          -- Resolved shift-reduce conflict with next token "te-asterisk" for "TEZeroOrMoreExpression: TE = TE te-asterisk" vs reducing "TEOrExpression: TE = TE pipe TE" as 'shift' using precedence TEZeroOrMoreExpression (1) > TEOrExpression (0)
          -- Resolved shift-reduce conflict with next token "te-openparenthesis" for "TEScopedExpression: TE = te-openparenthesis TE te-closeparenthesis" vs reducing "TEOrExpression: TE = TE pipe TE" as 'reduce' using Left associativity TEScopedExpression vs TEOrExpression
          -- Resolved shift-reduce conflict with next token "te-plus" for "TEOneOrMoreExpression: TE = TE te-plus" vs reducing "TEOrExpression: TE = TE pipe TE" as 'shift' using precedence TEOneOrMoreExpression (1) > TEOrExpression (0)
          -- Resolved shift-reduce conflict with next token "te-question" for "TEZeroOrOneExpression: TE = TE te-question" vs reducing "TEOrExpression: TE = TE pipe TE" as 'shift' using precedence TEZeroOrOneExpression (1) > TEOrExpression (0)
        parse' stateStack@(37:states) ((token@(Asterisk _)):tokens) valueStack = -- On te-asterisk S33
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 37 " ++ (show token) ++ ": S33") $ parse' (33:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(37:states) ((token@(Plus _)):tokens) valueStack = -- On te-plus S35
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 37 " ++ (show token) ++ ": S35") $ parse' (35:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(37:states) ((token@(Question _)):tokens) valueStack = -- On te-question S36
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 37 " ++ (show token) ++ ": S36") $ parse' (36:stateStack) tokens ((ParserToken token):valueStack)
        parse' (37:_:_:gs:states) tokens (value3:value2:value1:values) = 
            trace' ("Reduce TEOrExpression: TE = TE pipe TE") $ parse' ((getGoto gs "TE"):gs:states) tokens ((process [value1,value2,value3] reduceTEOrExpression):values)
        parse' (37:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (37:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 38
        parse' stateStack@(38:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S40
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 38 " ++ (show token) ++ ": S40") $ parse' (40:stateStack) tokens ((ParserToken token):valueStack)
        parse' (38:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (38:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 39
        parse' stateStack@(39:states) ((token@(CloseBracket _)):tokens) valueStack = -- On closebracket S41
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 39 " ++ (show token) ++ ": S41") $ parse' (41:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(39:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S42
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 39 " ++ (show token) ++ ": S42") $ parse' (42:stateStack) tokens ((ParserToken token):valueStack)
        parse' (39:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (39:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 40
        parse' (40:gs:states) tokens (value1:values) = 
            trace' ("Reduce FirstIdentifier: Identifiers = identifier") $ parse' ((getGoto gs "Identifiers"):gs:states) tokens ((process [value1] reduceFirstIdentifier):values)
        parse' (40:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (40:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 41
        parse' (41:_:_:_:gs:states) tokens (value4:value3:value2:value1:values) = 
            trace' ("Reduce TENotRangeExpression: TE = openbracket te-hat Identifiers closebracket") $ parse' ((getGoto gs "TE"):gs:states) tokens ((process [value1,value2,value3,value4] reduceTENotRangeExpression):values)
        parse' (41:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (41:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 42
        parse' (42:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce RestIdentifiers: Identifiers = Identifiers identifier") $ parse' ((getGoto gs "Identifiers"):gs:states) tokens ((process [value1,value2] reduceRestIdentifiers):values)
        parse' (42:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (42:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 43
        parse' stateStack@(43:states) ((token@(KwError _)):tokens) valueStack = -- On error S44
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 43 " ++ (show token) ++ ": S44") $ parse' (44:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(43:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S45
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 43 " ++ (show token) ++ ": S45") $ parse' (45:stateStack) tokens ((ParserToken token):valueStack)
        parse' (43:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (43:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 44
        parse' stateStack@(44:states) ((token@(SemiColon _)):tokens) valueStack = -- On ; S53
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 44 " ++ (show token) ++ ": S53") $ parse' (53:stateStack) tokens ((ParserToken token):valueStack)
        parse' (44:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (44:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 45
        parse' stateStack@(45:states) ((token@(Comma _)):tokens) valueStack = -- On , S46
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 45 " ++ (show token) ++ ": S46") $ parse' (46:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(45:states) ((token@(SemiColon _)):tokens) valueStack = -- On ; S47
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 45 " ++ (show token) ++ ": S47") $ parse' (47:stateStack) tokens ((ParserToken token):valueStack)
        parse' (45:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (45:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 46
        parse' stateStack@(46:states) ((token@(KwGoto _)):tokens) valueStack = -- On goto S48
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 46 " ++ (show token) ++ ": S48") $ parse' (48:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(46:states) ((token@(KwReturn _)):tokens) valueStack = -- On return S49
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 46 " ++ (show token) ++ ": S49") $ parse' (49:stateStack) tokens ((ParserToken token):valueStack)
        parse' (46:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (46:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 47
        parse' (47:_:_:_:gs:states) tokens (value4:value3:value2:value1:values) = 
            trace' ("Reduce ScannerMatchNoAction: ScannerMatch = TE -> identifier ;") $ parse' ((getGoto gs "ScannerMatch"):gs:states) tokens ((process [value1,value2,value3,value4] reduceScannerMatchNoAction):values)
        parse' (47:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (47:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 48
        parse' stateStack@(48:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S51
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 48 " ++ (show token) ++ ": S51") $ parse' (51:stateStack) tokens ((ParserToken token):valueStack)
        parse' (48:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (48:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 49
        parse' stateStack@(49:states) ((token@(SemiColon _)):tokens) valueStack = -- On ; S50
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 49 " ++ (show token) ++ ": S50") $ parse' (50:stateStack) tokens ((ParserToken token):valueStack)
        parse' (49:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (49:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 50
        parse' (50:_:_:_:_:_:gs:states) tokens (value6:value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce ScannerMatchWithReturn: ScannerMatch = TE -> identifier , return ;") $ parse' ((getGoto gs "ScannerMatch"):gs:states) tokens ((process [value1,value2,value3,value4,value5,value6] reduceScannerMatchWithReturn):values)
        parse' (50:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (50:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 51
        parse' stateStack@(51:states) ((token@(SemiColon _)):tokens) valueStack = -- On ; S52
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 51 " ++ (show token) ++ ": S52") $ parse' (52:stateStack) tokens ((ParserToken token):valueStack)
        parse' (51:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (51:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 52
        parse' (52:_:_:_:_:_:_:gs:states) tokens (value7:value6:value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce ScannerMatchWithGoto: ScannerMatch = TE -> identifier , goto identifier ;") $ parse' ((getGoto gs "ScannerMatch"):gs:states) tokens ((process [value1,value2,value3,value4,value5,value6,value7] reduceScannerMatchWithGoto):values)
        parse' (52:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (52:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 53
        parse' (53:_:_:_:gs:states) tokens (value4:value3:value2:value1:values) = 
            trace' ("Reduce ScannerMatchError: ScannerMatch = TE -> error ;") $ parse' ((getGoto gs "ScannerMatch"):gs:states) tokens ((process [value1,value2,value3,value4] reduceScannerMatchError):values)
        parse' (53:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (53:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 54
        parse' (54:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce RestScannerMatches: ScannerMatches = ScannerMatches ScannerMatch") $ parse' ((getGoto gs "ScannerMatches"):gs:states) tokens ((process [value1,value2] reduceRestScannerMatches):values)
        parse' (54:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (54:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 55
        parse' (55:_:_:_:_:_:gs:states) tokens (value6:value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce ScannerWithRules: Scanner = scanner identifier ScannerOptions { ScannerMatches }") $ parse' ((getGoto gs "Scanner"):gs:states) tokens ((process [value1,value2,value3,value4,value5,value6] reduceScannerWithRules):values)
        parse' (55:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (55:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 56
        parse' stateStack@(56:states) ((token@(KwProcessing _)):tokens) valueStack = -- On processing S57
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 56 " ++ (show token) ++ ": S57") $ parse' (57:stateStack) tokens ((ParserToken token):valueStack)
        parse' (56:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (56:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 57
        parse' stateStack@(57:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S58
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 57 " ++ (show token) ++ ": S58") $ parse' (58:stateStack) tokens ((ParserToken token):valueStack)
        parse' (57:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (57:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 58
        parse' stateStack@(58:states) ((token@(OpenCurly _)):tokens) valueStack = -- On { S59
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 58 " ++ (show token) ++ ": S59") $ parse' (59:stateStack) tokens ((ParserToken token):valueStack)
        parse' (58:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (58:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 59
        parse' stateStack@(59:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S62
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 59 " ++ (show token) ++ ": S62") $ parse' (62:stateStack) tokens ((ParserToken token):valueStack)
        parse' (59:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (59:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 60
        parse' (60:gs:states) tokens (value1:values) = 
            trace' ("Reduce FirstProduction: Productions = Production") $ parse' ((getGoto gs "Productions"):gs:states) tokens ((process [value1] reduceFirstProduction):values)
        parse' (60:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (60:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 61
        parse' stateStack@(61:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S62
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 61 " ++ (show token) ++ ": S62") $ parse' (62:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(61:states) ((token@(CloseCurly _)):tokens) valueStack = -- On } S82
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 61 " ++ (show token) ++ ": S82") $ parse' (82:stateStack) tokens ((ParserToken token):valueStack)
        parse' (61:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (61:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 62
        parse' stateStack@(62:states) ((token@(Colon _)):tokens) valueStack = -- On : S63
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 62 " ++ (show token) ++ ": S63") $ parse' (63:stateStack) tokens ((ParserToken token):valueStack)
        parse' (62:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (62:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 63
        parse' stateStack@(63:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S64
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 63 " ++ (show token) ++ ": S64") $ parse' (64:stateStack) tokens ((ParserToken token):valueStack)
        parse' (63:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (63:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 64
        parse' stateStack@(64:states) ((token@(Equal _)):tokens) valueStack = -- On = S65
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 64 " ++ (show token) ++ ": S65") $ parse' (65:stateStack) tokens ((ParserToken token):valueStack)
        parse' (64:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (64:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 65
        parse' stateStack@(65:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S67
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 65 " ++ (show token) ++ ": S67") $ parse' (67:stateStack) tokens ((ParserToken token):valueStack)
        parse' (65:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (65:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 66
        parse' stateStack@(66:states) ((token@(SemiColon _)):tokens) valueStack = -- On ; S68
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 66 " ++ (show token) ++ ": S68") $ parse' (68:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(66:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S69
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 66 " ++ (show token) ++ ": S69") $ parse' (69:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(66:states) ((token@(OpenBracket _)):tokens) valueStack = -- On openbracket S70
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 66 " ++ (show token) ++ ": S70") $ parse' (70:stateStack) tokens ((ParserToken token):valueStack)
        parse' (66:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (66:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 67
        parse' (67:gs:states) tokens (value1:values) = 
            trace' ("Reduce FirstItem: Items = identifier") $ parse' ((getGoto gs "Items"):gs:states) tokens ((process [value1] reduceFirstItem):values)
        parse' (67:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (67:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 68
        parse' (68:_:_:_:_:_:gs:states) tokens (value6:value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce ProductionNoOptions: Production = identifier : identifier = Items ;") $ parse' ((getGoto gs "Production"):gs:states) tokens ((process [value1,value2,value3,value4,value5,value6] reduceProductionNoOptions):values)
        parse' (68:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (68:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 69
        parse' (69:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce RestItems: Items = Items identifier") $ parse' ((getGoto gs "Items"):gs:states) tokens ((process [value1,value2] reduceRestItems):values)
        parse' (69:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (69:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 70
        parse' stateStack@(70:states) ((token@(KwLeft _)):tokens) valueStack = -- On left S72
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 70 " ++ (show token) ++ ": S72") $ parse' (72:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(70:states) ((token@(KwNone _)):tokens) valueStack = -- On none S73
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 70 " ++ (show token) ++ ": S73") $ parse' (73:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(70:states) ((token@(KwRight _)):tokens) valueStack = -- On right S74
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 70 " ++ (show token) ++ ": S74") $ parse' (74:stateStack) tokens ((ParserToken token):valueStack)
        parse' (70:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (70:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 71
        parse' stateStack@(71:states) ((token@(Comma _)):tokens) valueStack = -- On , S75
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 71 " ++ (show token) ++ ": S75") $ parse' (75:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(71:states) ((token@(CloseBracket _)):tokens) valueStack = -- On closebracket S76
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 71 " ++ (show token) ++ ": S76") $ parse' (76:stateStack) tokens ((ParserToken token):valueStack)
        parse' (71:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (71:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 72
        parse' (72:gs:states) tokens (value1:values) = 
            trace' ("Reduce LeftAssoc: Assoc = left") $ parse' ((getGoto gs "Assoc"):gs:states) tokens ((process [value1] reduceLeftAssoc):values)
        parse' (72:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (72:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 73
        parse' (73:gs:states) tokens (value1:values) = 
            trace' ("Reduce NoAssoc: Assoc = none") $ parse' ((getGoto gs "Assoc"):gs:states) tokens ((process [value1] reduceNoAssoc):values)
        parse' (73:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (73:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 74
        parse' (74:gs:states) tokens (value1:values) = 
            trace' ("Reduce RightAssoc: Assoc = right") $ parse' ((getGoto gs "Assoc"):gs:states) tokens ((process [value1] reduceRightAssoc):values)
        parse' (74:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (74:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 75
        parse' stateStack@(75:states) ((token@(Integer _ _)):tokens) valueStack = -- On number S78
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 75 " ++ (show token) ++ ": S78") $ parse' (78:stateStack) tokens ((ParserToken token):valueStack)
        parse' (75:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (75:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 76
        parse' stateStack@(76:states) ((token@(SemiColon _)):tokens) valueStack = -- On ; S77
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 76 " ++ (show token) ++ ": S77") $ parse' (77:stateStack) tokens ((ParserToken token):valueStack)
        parse' (76:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (76:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 77
        parse' (77:_:_:_:_:_:_:_:_:gs:states) tokens (value9:value8:value7:value6:value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce ProductionWithAssoc: Production = identifier : identifier = Items openbracket Assoc closebracket ;") $ parse' ((getGoto gs "Production"):gs:states) tokens ((process [value1,value2,value3,value4,value5,value6,value7,value8,value9] reduceProductionWithAssoc):values)
        parse' (77:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (77:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 78
        parse' stateStack@(78:states) ((token@(CloseBracket _)):tokens) valueStack = -- On closebracket S79
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 78 " ++ (show token) ++ ": S79") $ parse' (79:stateStack) tokens ((ParserToken token):valueStack)
        parse' (78:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (78:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 79
        parse' stateStack@(79:states) ((token@(SemiColon _)):tokens) valueStack = -- On ; S80
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 79 " ++ (show token) ++ ": S80") $ parse' (80:stateStack) tokens ((ParserToken token):valueStack)
        parse' (79:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (79:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 80
        parse' (80:_:_:_:_:_:_:_:_:_:_:gs:states) tokens (value11:value10:value9:value8:value7:value6:value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce ProductionWithPrec: Production = identifier : identifier = Items openbracket Assoc , number closebracket ;") $ parse' ((getGoto gs "Production"):gs:states) tokens ((process [value1,value2,value3,value4,value5,value6,value7,value8,value9,value10,value11] reduceProductionWithPrec):values)
        parse' (80:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (80:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 81
        parse' (81:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce RestProductions: Productions = Productions Production") $ parse' ((getGoto gs "Productions"):gs:states) tokens ((process [value1,value2] reduceRestProductions):values)
        parse' (81:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (81:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 82
        parse' (82:_:_:_:_:_:_:gs:states) tokens (value7:value6:value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce Parser: Parser = parser identifier processing identifier { Productions }") $ parse' ((getGoto gs "Parser"):gs:states) tokens ((process [value1,value2,value3,value4,value5,value6,value7] reduceParser):values)
        parse' (82:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (82:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 83
        parse' stateStack@(83:states) ((token@(OpenBracket _)):tokens) valueStack = -- On openbracket S84
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 83 " ++ (show token) ++ ": S84") $ parse' (84:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(83:states) ((token@(OpenCurly _)):tokens) valueStack = -- On { S85
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 83 " ++ (show token) ++ ": S85") $ parse' (85:stateStack) tokens ((ParserToken token):valueStack)
        parse' (83:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (83:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 84
        parse' stateStack@(84:states) ((token@(String _ _)):tokens) valueStack = -- On string S141
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 84 " ++ (show token) ++ ": S141") $ parse' (141:stateStack) tokens ((ParserToken token):valueStack)
        parse' (84:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (84:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 85
        parse' stateStack@(85:states) ((token@(ReStart _)):tokens) valueStack = -- On re-start S88
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 85 " ++ (show token) ++ ": S88") $ parse' (88:stateStack) tokens ((ParserToken token):valueStack)
        parse' (85:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (85:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 86
        parse' (86:gs:states) tokens (value1:values) = 
            trace' ("Reduce FirstAnalyserMatch: AnalyserMatches = AnalyserMatch") $ parse' ((getGoto gs "AnalyserMatches"):gs:states) tokens ((process [value1] reduceFirstAnalyserMatch):values)
        parse' (86:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (86:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 87
        parse' stateStack@(87:states) ((token@(ReStart _)):tokens) valueStack = -- On re-start S88
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 87 " ++ (show token) ++ ": S88") $ parse' (88:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(87:states) ((token@(CloseCurly _)):tokens) valueStack = -- On } S139
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 87 " ++ (show token) ++ ": S139") $ parse' (139:stateStack) tokens ((ParserToken token):valueStack)
        parse' (87:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (87:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 88
        parse' stateStack@(88:states) ((token@(ReOpenParenthesis _)):tokens) valueStack = -- On ( S89
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 88 " ++ (show token) ++ ": S89") $ parse' (89:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(88:states) ((token@(ReDot _)):tokens) valueStack = -- On . S90
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 88 " ++ (show token) ++ ": S90") $ parse' (90:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(88:states) ((token@(ReOpenBracket _)):tokens) valueStack = -- On [ S92
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 88 " ++ (show token) ++ ": S92") $ parse' (92:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(88:states) ((token@(ReDigitClass _)):tokens) valueStack = -- On \d S93
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 88 " ++ (show token) ++ ": S93") $ parse' (93:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(88:states) ((token@(ReFormFeed _)):tokens) valueStack = -- On \f S94
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 88 " ++ (show token) ++ ": S94") $ parse' (94:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(88:states) ((token@(ReNewLine _)):tokens) valueStack = -- On \n S95
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 88 " ++ (show token) ++ ": S95") $ parse' (95:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(88:states) ((token@(ReReturn _)):tokens) valueStack = -- On \r S96
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 88 " ++ (show token) ++ ": S96") $ parse' (96:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(88:states) ((token@(ReWhitespaceClass _)):tokens) valueStack = -- On \s S97
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 88 " ++ (show token) ++ ": S97") $ parse' (97:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(88:states) ((token@(ReTab _)):tokens) valueStack = -- On \t S98
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 88 " ++ (show token) ++ ": S98") $ parse' (98:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(88:states) ((token@(ReAlphaNumClass _)):tokens) valueStack = -- On \w S99
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 88 " ++ (show token) ++ ": S99") $ parse' (99:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(88:states) ((token@(ReCharacter _ _)):tokens) valueStack = -- On character S100
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 88 " ++ (show token) ++ ": S100") $ parse' (100:stateStack) tokens ((ParserToken token):valueStack)
        parse' (88:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (88:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 89
        parse' stateStack@(89:states) ((token@(ReOpenParenthesis _)):tokens) valueStack = -- On ( S89
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 89 " ++ (show token) ++ ": S89") $ parse' (89:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(89:states) ((token@(ReDot _)):tokens) valueStack = -- On . S90
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 89 " ++ (show token) ++ ": S90") $ parse' (90:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(89:states) ((token@(ReOpenBracket _)):tokens) valueStack = -- On [ S92
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 89 " ++ (show token) ++ ": S92") $ parse' (92:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(89:states) ((token@(ReDigitClass _)):tokens) valueStack = -- On \d S93
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 89 " ++ (show token) ++ ": S93") $ parse' (93:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(89:states) ((token@(ReFormFeed _)):tokens) valueStack = -- On \f S94
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 89 " ++ (show token) ++ ": S94") $ parse' (94:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(89:states) ((token@(ReNewLine _)):tokens) valueStack = -- On \n S95
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 89 " ++ (show token) ++ ": S95") $ parse' (95:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(89:states) ((token@(ReReturn _)):tokens) valueStack = -- On \r S96
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 89 " ++ (show token) ++ ": S96") $ parse' (96:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(89:states) ((token@(ReWhitespaceClass _)):tokens) valueStack = -- On \s S97
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 89 " ++ (show token) ++ ": S97") $ parse' (97:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(89:states) ((token@(ReTab _)):tokens) valueStack = -- On \t S98
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 89 " ++ (show token) ++ ": S98") $ parse' (98:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(89:states) ((token@(ReAlphaNumClass _)):tokens) valueStack = -- On \w S99
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 89 " ++ (show token) ++ ": S99") $ parse' (99:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(89:states) ((token@(ReCharacter _ _)):tokens) valueStack = -- On character S100
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 89 " ++ (show token) ++ ": S100") $ parse' (100:stateStack) tokens ((ParserToken token):valueStack)
        parse' (89:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (89:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 90
        parse' (90:gs:states) tokens (value1:values) = 
            trace' ("Reduce REAnyCharExpression: RE = .") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1] reduceREAnyCharExpression):values)
        parse' (90:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (90:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 91
        parse' stateStack@(91:states) ((token@(ReOpenParenthesis _)):tokens) valueStack = -- On ( S89
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S89") $ parse' (89:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReAsterisk _)):tokens) valueStack = -- On * S118
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S118") $ parse' (118:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(RePlus _)):tokens) valueStack = -- On + S119
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S119") $ parse' (119:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReDot _)):tokens) valueStack = -- On . S90
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S90") $ parse' (90:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReQuestion _)):tokens) valueStack = -- On ? S120
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S120") $ parse' (120:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReOpenBracket _)):tokens) valueStack = -- On [ S92
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S92") $ parse' (92:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReDigitClass _)):tokens) valueStack = -- On \d S93
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S93") $ parse' (93:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReFormFeed _)):tokens) valueStack = -- On \f S94
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S94") $ parse' (94:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReNewLine _)):tokens) valueStack = -- On \n S95
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S95") $ parse' (95:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReReturn _)):tokens) valueStack = -- On \r S96
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S96") $ parse' (96:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReWhitespaceClass _)):tokens) valueStack = -- On \s S97
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S97") $ parse' (97:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReTab _)):tokens) valueStack = -- On \t S98
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S98") $ parse' (98:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReAlphaNumClass _)):tokens) valueStack = -- On \w S99
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S99") $ parse' (99:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReCharacter _ _)):tokens) valueStack = -- On character S100
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S100") $ parse' (100:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReEnd _)):tokens) valueStack = -- On re-end S122
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S122") $ parse' (122:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(91:states) ((token@(ReOr _)):tokens) valueStack = -- On | S123
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 91 " ++ (show token) ++ ": S123") $ parse' (123:stateStack) tokens ((ParserToken token):valueStack)
        parse' (91:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (91:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 92
        parse' stateStack@(92:states) ((token@(ReDigitClass _)):tokens) valueStack = -- On \d S103
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 92 " ++ (show token) ++ ": S103") $ parse' (103:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(92:states) ((token@(ReFormFeed _)):tokens) valueStack = -- On \f S104
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 92 " ++ (show token) ++ ": S104") $ parse' (104:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(92:states) ((token@(ReNewLine _)):tokens) valueStack = -- On \n S105
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 92 " ++ (show token) ++ ": S105") $ parse' (105:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(92:states) ((token@(ReReturn _)):tokens) valueStack = -- On \r S106
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 92 " ++ (show token) ++ ": S106") $ parse' (106:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(92:states) ((token@(ReWhitespaceClass _)):tokens) valueStack = -- On \s S107
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 92 " ++ (show token) ++ ": S107") $ parse' (107:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(92:states) ((token@(ReTab _)):tokens) valueStack = -- On \t S108
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 92 " ++ (show token) ++ ": S108") $ parse' (108:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(92:states) ((token@(ReAlphaNumClass _)):tokens) valueStack = -- On \w S109
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 92 " ++ (show token) ++ ": S109") $ parse' (109:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(92:states) ((token@(ReHat _)):tokens) valueStack = -- On ^ S110
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 92 " ++ (show token) ++ ": S110") $ parse' (110:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(92:states) ((token@(ReCharacter _ _)):tokens) valueStack = -- On character S111
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 92 " ++ (show token) ++ ": S111") $ parse' (111:stateStack) tokens ((ParserToken token):valueStack)
        parse' (92:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (92:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 93
        parse' (93:gs:states) tokens (value1:values) = 
            trace' ("Reduce REDigitCharExpression: RE = \\d") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1] reduceREDigitCharExpression):values)
        parse' (93:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (93:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 94
        parse' (94:gs:states) tokens (value1:values) = 
            trace' ("Reduce REFormFeedCharExpression: RE = \\f") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1] reduceREFormFeedCharExpression):values)
        parse' (94:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (94:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 95
        parse' (95:gs:states) tokens (value1:values) = 
            trace' ("Reduce RENewLineCharExpression: RE = \\n") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1] reduceRENewLineCharExpression):values)
        parse' (95:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (95:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 96
        parse' (96:gs:states) tokens (value1:values) = 
            trace' ("Reduce REReturnCharExpression: RE = \\r") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1] reduceREReturnCharExpression):values)
        parse' (96:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (96:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 97
        parse' (97:gs:states) tokens (value1:values) = 
            trace' ("Reduce REWhitespaceCharExpression: RE = \\s") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1] reduceREWhitespaceCharExpression):values)
        parse' (97:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (97:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 98
        parse' (98:gs:states) tokens (value1:values) = 
            trace' ("Reduce RETabCharExpression: RE = \\t") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1] reduceRETabCharExpression):values)
        parse' (98:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (98:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 99
        parse' (99:gs:states) tokens (value1:values) = 
            trace' ("Reduce REAlphaNumCharExpression: RE = \\w") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1] reduceREAlphaNumCharExpression):values)
        parse' (99:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (99:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 100
        parse' (100:gs:states) tokens (value1:values) = 
            trace' ("Reduce RECharacterExpression: RE = character") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1] reduceRECharacterExpression):values)
        parse' (100:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (100:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 101
        parse' (101:gs:states) tokens (value1:values) = 
            trace' ("Reduce REFirstRangeExpression: RERanges = RERange") $ parse' ((getGoto gs "RERanges"):gs:states) tokens ((process [value1] reduceREFirstRangeExpression):values)
        parse' (101:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (101:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 102
        parse' stateStack@(102:states) ((token@(ReDigitClass _)):tokens) valueStack = -- On \d S103
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 102 " ++ (show token) ++ ": S103") $ parse' (103:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(102:states) ((token@(ReFormFeed _)):tokens) valueStack = -- On \f S104
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 102 " ++ (show token) ++ ": S104") $ parse' (104:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(102:states) ((token@(ReNewLine _)):tokens) valueStack = -- On \n S105
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 102 " ++ (show token) ++ ": S105") $ parse' (105:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(102:states) ((token@(ReReturn _)):tokens) valueStack = -- On \r S106
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 102 " ++ (show token) ++ ": S106") $ parse' (106:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(102:states) ((token@(ReWhitespaceClass _)):tokens) valueStack = -- On \s S107
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 102 " ++ (show token) ++ ": S107") $ parse' (107:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(102:states) ((token@(ReTab _)):tokens) valueStack = -- On \t S108
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 102 " ++ (show token) ++ ": S108") $ parse' (108:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(102:states) ((token@(ReAlphaNumClass _)):tokens) valueStack = -- On \w S109
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 102 " ++ (show token) ++ ": S109") $ parse' (109:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(102:states) ((token@(ReCloseBracket _)):tokens) valueStack = -- On ] S117
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 102 " ++ (show token) ++ ": S117") $ parse' (117:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(102:states) ((token@(ReCharacter _ _)):tokens) valueStack = -- On character S111
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 102 " ++ (show token) ++ ": S111") $ parse' (111:stateStack) tokens ((ParserToken token):valueStack)
        parse' (102:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (102:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 103
        parse' (103:gs:states) tokens (value1:values) = 
            trace' ("Reduce REDigitCharRangeExpression: RERange = \\d") $ parse' ((getGoto gs "RERange"):gs:states) tokens ((process [value1] reduceREDigitCharRangeExpression):values)
        parse' (103:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (103:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 104
        parse' (104:gs:states) tokens (value1:values) = 
            trace' ("Reduce REFormFeedCharRangeExpression: RERange = \\f") $ parse' ((getGoto gs "RERange"):gs:states) tokens ((process [value1] reduceREFormFeedCharRangeExpression):values)
        parse' (104:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (104:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 105
        parse' (105:gs:states) tokens (value1:values) = 
            trace' ("Reduce RENewLineCharRangeExpression: RERange = \\n") $ parse' ((getGoto gs "RERange"):gs:states) tokens ((process [value1] reduceRENewLineCharRangeExpression):values)
        parse' (105:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (105:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 106
        parse' (106:gs:states) tokens (value1:values) = 
            trace' ("Reduce REReturnCharRangeExpression: RERange = \\r") $ parse' ((getGoto gs "RERange"):gs:states) tokens ((process [value1] reduceREReturnCharRangeExpression):values)
        parse' (106:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (106:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 107
        parse' (107:gs:states) tokens (value1:values) = 
            trace' ("Reduce REWhitespaceCharRangeExpression: RERange = \\s") $ parse' ((getGoto gs "RERange"):gs:states) tokens ((process [value1] reduceREWhitespaceCharRangeExpression):values)
        parse' (107:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (107:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 108
        parse' (108:gs:states) tokens (value1:values) = 
            trace' ("Reduce RETabCharRangeExpression: RERange = \\t") $ parse' ((getGoto gs "RERange"):gs:states) tokens ((process [value1] reduceRETabCharRangeExpression):values)
        parse' (108:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (108:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 109
        parse' (109:gs:states) tokens (value1:values) = 
            trace' ("Reduce REAlphaNumCharRangeExpression: RERange = \\w") $ parse' ((getGoto gs "RERange"):gs:states) tokens ((process [value1] reduceREAlphaNumCharRangeExpression):values)
        parse' (109:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (109:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 110
        parse' stateStack@(110:states) ((token@(ReDigitClass _)):tokens) valueStack = -- On \d S103
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 110 " ++ (show token) ++ ": S103") $ parse' (103:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(110:states) ((token@(ReFormFeed _)):tokens) valueStack = -- On \f S104
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 110 " ++ (show token) ++ ": S104") $ parse' (104:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(110:states) ((token@(ReNewLine _)):tokens) valueStack = -- On \n S105
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 110 " ++ (show token) ++ ": S105") $ parse' (105:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(110:states) ((token@(ReReturn _)):tokens) valueStack = -- On \r S106
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 110 " ++ (show token) ++ ": S106") $ parse' (106:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(110:states) ((token@(ReWhitespaceClass _)):tokens) valueStack = -- On \s S107
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 110 " ++ (show token) ++ ": S107") $ parse' (107:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(110:states) ((token@(ReTab _)):tokens) valueStack = -- On \t S108
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 110 " ++ (show token) ++ ": S108") $ parse' (108:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(110:states) ((token@(ReAlphaNumClass _)):tokens) valueStack = -- On \w S109
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 110 " ++ (show token) ++ ": S109") $ parse' (109:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(110:states) ((token@(ReCharacter _ _)):tokens) valueStack = -- On character S111
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 110 " ++ (show token) ++ ": S111") $ parse' (111:stateStack) tokens ((ParserToken token):valueStack)
        parse' (110:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (110:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 111
          -- Resolved shift-reduce conflict with next token "-" for "RECharRangeRangeExpression: RERange = character - character" vs reducing "RECharacterRangeExpression: RERange = character" as 'shift' using precedence RECharRangeRangeExpression (1) > RECharacterRangeExpression (0)
        parse' stateStack@(111:states) ((token@(ReHyphen _)):tokens) valueStack = -- On - S112
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 111 " ++ (show token) ++ ": S112") $ parse' (112:stateStack) tokens ((ParserToken token):valueStack)
        parse' (111:gs:states) tokens (value1:values) = 
            trace' ("Reduce RECharacterRangeExpression: RERange = character") $ parse' ((getGoto gs "RERange"):gs:states) tokens ((process [value1] reduceRECharacterRangeExpression):values)
        parse' (111:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (111:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 112
        parse' stateStack@(112:states) ((token@(ReCharacter _ _)):tokens) valueStack = -- On character S113
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 112 " ++ (show token) ++ ": S113") $ parse' (113:stateStack) tokens ((ParserToken token):valueStack)
        parse' (112:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (112:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 113
        parse' (113:_:_:gs:states) tokens (value3:value2:value1:values) = 
            trace' ("Reduce RECharRangeRangeExpression: RERange = character - character") $ parse' ((getGoto gs "RERange"):gs:states) tokens ((process [value1,value2,value3] reduceRECharRangeRangeExpression):values)
        parse' (113:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (113:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 114
        parse' stateStack@(114:states) ((token@(ReDigitClass _)):tokens) valueStack = -- On \d S103
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 114 " ++ (show token) ++ ": S103") $ parse' (103:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(114:states) ((token@(ReFormFeed _)):tokens) valueStack = -- On \f S104
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 114 " ++ (show token) ++ ": S104") $ parse' (104:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(114:states) ((token@(ReNewLine _)):tokens) valueStack = -- On \n S105
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 114 " ++ (show token) ++ ": S105") $ parse' (105:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(114:states) ((token@(ReReturn _)):tokens) valueStack = -- On \r S106
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 114 " ++ (show token) ++ ": S106") $ parse' (106:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(114:states) ((token@(ReWhitespaceClass _)):tokens) valueStack = -- On \s S107
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 114 " ++ (show token) ++ ": S107") $ parse' (107:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(114:states) ((token@(ReTab _)):tokens) valueStack = -- On \t S108
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 114 " ++ (show token) ++ ": S108") $ parse' (108:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(114:states) ((token@(ReAlphaNumClass _)):tokens) valueStack = -- On \w S109
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 114 " ++ (show token) ++ ": S109") $ parse' (109:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(114:states) ((token@(ReCloseBracket _)):tokens) valueStack = -- On ] S116
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 114 " ++ (show token) ++ ": S116") $ parse' (116:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(114:states) ((token@(ReCharacter _ _)):tokens) valueStack = -- On character S111
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 114 " ++ (show token) ++ ": S111") $ parse' (111:stateStack) tokens ((ParserToken token):valueStack)
        parse' (114:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (114:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 115
        parse' (115:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce RERestRangeExpression: RERanges = RERanges RERange") $ parse' ((getGoto gs "RERanges"):gs:states) tokens ((process [value1,value2] reduceRERestRangeExpression):values)
        parse' (115:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (115:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 116
        parse' (116:_:_:_:gs:states) tokens (value4:value3:value2:value1:values) = 
            trace' ("Reduce RENotRangeExpression: RE = [ ^ RERanges ]") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1,value2,value3,value4] reduceRENotRangeExpression):values)
        parse' (116:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (116:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 117
        parse' (117:_:_:gs:states) tokens (value3:value2:value1:values) = 
            trace' ("Reduce RERangeExpression: RE = [ RERanges ]") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1,value2,value3] reduceRERangeExpression):values)
        parse' (117:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (117:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 118
        parse' (118:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce REZeroOrMoreExpression: RE = RE *") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1,value2] reduceREZeroOrMoreExpression):values)
        parse' (118:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (118:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 119
        parse' (119:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce REOneOrMoreExpression: RE = RE +") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1,value2] reduceREOneOrMoreExpression):values)
        parse' (119:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (119:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 120
        parse' (120:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce REZeroOrOneExpression: RE = RE ?") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1,value2] reduceREZeroOrOneExpression):values)
        parse' (120:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (120:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 121
          -- Resolved shift-reduce conflict with next token "(" for "REScopedExpression: RE = ( RE )" vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity REScopedExpression vs REThenExpression
          -- Resolved shift-reduce conflict with next token "*" for "REZeroOrMoreExpression: RE = RE *" vs reducing "REThenExpression: RE = RE RE" as 'shift' using precedence REZeroOrMoreExpression (1) > REThenExpression (0)
          -- Resolved shift-reduce conflict with next token "+" for "REOneOrMoreExpression: RE = RE +" vs reducing "REThenExpression: RE = RE RE" as 'shift' using precedence REOneOrMoreExpression (1) > REThenExpression (0)
          -- Resolved shift-reduce conflict with next token "." for "REAnyCharExpression: RE = ." vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity REAnyCharExpression vs REThenExpression
          -- Resolved shift-reduce conflict with next token "?" for "REZeroOrOneExpression: RE = RE ?" vs reducing "REThenExpression: RE = RE RE" as 'shift' using precedence REZeroOrOneExpression (1) > REThenExpression (0)
          -- Resolved shift-reduce conflict with next token "[" for "RENotRangeExpression: RE = [ ^ RERanges ]" vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity RENotRangeExpression vs REThenExpression
          -- Resolved shift-reduce conflict with next token "[" for "RERangeExpression: RE = [ RERanges ]" vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity RERangeExpression vs REThenExpression
          -- Resolved shift-reduce conflict with next token "\d" for "REDigitCharExpression: RE = \d" vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity REDigitCharExpression vs REThenExpression
          -- Resolved shift-reduce conflict with next token "\f" for "REFormFeedCharExpression: RE = \f" vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity REFormFeedCharExpression vs REThenExpression
          -- Resolved shift-reduce conflict with next token "\n" for "RENewLineCharExpression: RE = \n" vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity RENewLineCharExpression vs REThenExpression
          -- Resolved shift-reduce conflict with next token "\r" for "REReturnCharExpression: RE = \r" vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity REReturnCharExpression vs REThenExpression
          -- Resolved shift-reduce conflict with next token "\s" for "REWhitespaceCharExpression: RE = \s" vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity REWhitespaceCharExpression vs REThenExpression
          -- Resolved shift-reduce conflict with next token "\t" for "RETabCharExpression: RE = \t" vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity RETabCharExpression vs REThenExpression
          -- Resolved shift-reduce conflict with next token "\w" for "REAlphaNumCharExpression: RE = \w" vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity REAlphaNumCharExpression vs REThenExpression
          -- Resolved shift-reduce conflict with next token "character" for "RECharacterExpression: RE = character" vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity RECharacterExpression vs REThenExpression
          -- Resolved shift-reduce conflict with next token "|" for "REOrExpression: RE = RE | RE" vs reducing "REThenExpression: RE = RE RE" as 'reduce' using Left associativity REOrExpression vs REThenExpression
        parse' stateStack@(121:states) ((token@(ReAsterisk _)):tokens) valueStack = -- On * S118
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 121 " ++ (show token) ++ ": S118") $ parse' (118:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(121:states) ((token@(RePlus _)):tokens) valueStack = -- On + S119
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 121 " ++ (show token) ++ ": S119") $ parse' (119:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(121:states) ((token@(ReQuestion _)):tokens) valueStack = -- On ? S120
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 121 " ++ (show token) ++ ": S120") $ parse' (120:stateStack) tokens ((ParserToken token):valueStack)
        parse' (121:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce REThenExpression: RE = RE RE") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1,value2] reduceREThenExpression):values)
        parse' (121:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (121:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 122
        parse' stateStack@(122:states) ((token@(Arrow _)):tokens) valueStack = -- On -> S125
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 122 " ++ (show token) ++ ": S125") $ parse' (125:stateStack) tokens ((ParserToken token):valueStack)
        parse' (122:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (122:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 123
        parse' stateStack@(123:states) ((token@(ReOpenParenthesis _)):tokens) valueStack = -- On ( S89
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 123 " ++ (show token) ++ ": S89") $ parse' (89:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(123:states) ((token@(ReDot _)):tokens) valueStack = -- On . S90
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 123 " ++ (show token) ++ ": S90") $ parse' (90:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(123:states) ((token@(ReOpenBracket _)):tokens) valueStack = -- On [ S92
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 123 " ++ (show token) ++ ": S92") $ parse' (92:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(123:states) ((token@(ReDigitClass _)):tokens) valueStack = -- On \d S93
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 123 " ++ (show token) ++ ": S93") $ parse' (93:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(123:states) ((token@(ReFormFeed _)):tokens) valueStack = -- On \f S94
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 123 " ++ (show token) ++ ": S94") $ parse' (94:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(123:states) ((token@(ReNewLine _)):tokens) valueStack = -- On \n S95
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 123 " ++ (show token) ++ ": S95") $ parse' (95:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(123:states) ((token@(ReReturn _)):tokens) valueStack = -- On \r S96
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 123 " ++ (show token) ++ ": S96") $ parse' (96:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(123:states) ((token@(ReWhitespaceClass _)):tokens) valueStack = -- On \s S97
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 123 " ++ (show token) ++ ": S97") $ parse' (97:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(123:states) ((token@(ReTab _)):tokens) valueStack = -- On \t S98
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 123 " ++ (show token) ++ ": S98") $ parse' (98:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(123:states) ((token@(ReAlphaNumClass _)):tokens) valueStack = -- On \w S99
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 123 " ++ (show token) ++ ": S99") $ parse' (99:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(123:states) ((token@(ReCharacter _ _)):tokens) valueStack = -- On character S100
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 123 " ++ (show token) ++ ": S100") $ parse' (100:stateStack) tokens ((ParserToken token):valueStack)
        parse' (123:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (123:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 124
          -- Resolved shift-reduce conflict with next token "(" for "REScopedExpression: RE = ( RE )" vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity REScopedExpression vs REOrExpression
          -- Resolved shift-reduce conflict with next token "*" for "REZeroOrMoreExpression: RE = RE *" vs reducing "REOrExpression: RE = RE | RE" as 'shift' using precedence REZeroOrMoreExpression (1) > REOrExpression (0)
          -- Resolved shift-reduce conflict with next token "+" for "REOneOrMoreExpression: RE = RE +" vs reducing "REOrExpression: RE = RE | RE" as 'shift' using precedence REOneOrMoreExpression (1) > REOrExpression (0)
          -- Resolved shift-reduce conflict with next token "." for "REAnyCharExpression: RE = ." vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity REAnyCharExpression vs REOrExpression
          -- Resolved shift-reduce conflict with next token "?" for "REZeroOrOneExpression: RE = RE ?" vs reducing "REOrExpression: RE = RE | RE" as 'shift' using precedence REZeroOrOneExpression (1) > REOrExpression (0)
          -- Resolved shift-reduce conflict with next token "[" for "RENotRangeExpression: RE = [ ^ RERanges ]" vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity RENotRangeExpression vs REOrExpression
          -- Resolved shift-reduce conflict with next token "[" for "RERangeExpression: RE = [ RERanges ]" vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity RERangeExpression vs REOrExpression
          -- Resolved shift-reduce conflict with next token "\d" for "REDigitCharExpression: RE = \d" vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity REDigitCharExpression vs REOrExpression
          -- Resolved shift-reduce conflict with next token "\f" for "REFormFeedCharExpression: RE = \f" vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity REFormFeedCharExpression vs REOrExpression
          -- Resolved shift-reduce conflict with next token "\n" for "RENewLineCharExpression: RE = \n" vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity RENewLineCharExpression vs REOrExpression
          -- Resolved shift-reduce conflict with next token "\r" for "REReturnCharExpression: RE = \r" vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity REReturnCharExpression vs REOrExpression
          -- Resolved shift-reduce conflict with next token "\s" for "REWhitespaceCharExpression: RE = \s" vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity REWhitespaceCharExpression vs REOrExpression
          -- Resolved shift-reduce conflict with next token "\t" for "RETabCharExpression: RE = \t" vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity RETabCharExpression vs REOrExpression
          -- Resolved shift-reduce conflict with next token "\w" for "REAlphaNumCharExpression: RE = \w" vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity REAlphaNumCharExpression vs REOrExpression
          -- Resolved shift-reduce conflict with next token "character" for "RECharacterExpression: RE = character" vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity RECharacterExpression vs REOrExpression
          -- Resolved shift-reduce conflict with next token "|" for "REOrExpression: RE = RE | RE" vs reducing "REOrExpression: RE = RE | RE" as 'reduce' using Left associativity REOrExpression vs REOrExpression
        parse' stateStack@(124:states) ((token@(ReAsterisk _)):tokens) valueStack = -- On * S118
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 124 " ++ (show token) ++ ": S118") $ parse' (118:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(124:states) ((token@(RePlus _)):tokens) valueStack = -- On + S119
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 124 " ++ (show token) ++ ": S119") $ parse' (119:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(124:states) ((token@(ReQuestion _)):tokens) valueStack = -- On ? S120
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 124 " ++ (show token) ++ ": S120") $ parse' (120:stateStack) tokens ((ParserToken token):valueStack)
        parse' (124:_:_:gs:states) tokens (value3:value2:value1:values) = 
            trace' ("Reduce REOrExpression: RE = RE | RE") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1,value2,value3] reduceREOrExpression):values)
        parse' (124:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (124:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 125
        parse' stateStack@(125:states) ((token@(KwError _)):tokens) valueStack = -- On error S126
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 125 " ++ (show token) ++ ": S126") $ parse' (126:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(125:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S127
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 125 " ++ (show token) ++ ": S127") $ parse' (127:stateStack) tokens ((ParserToken token):valueStack)
        parse' (125:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (125:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 126
        parse' stateStack@(126:states) ((token@(SemiColon _)):tokens) valueStack = -- On ; S135
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 126 " ++ (show token) ++ ": S135") $ parse' (135:stateStack) tokens ((ParserToken token):valueStack)
        parse' (126:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (126:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 127
        parse' stateStack@(127:states) ((token@(Comma _)):tokens) valueStack = -- On , S128
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 127 " ++ (show token) ++ ": S128") $ parse' (128:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(127:states) ((token@(SemiColon _)):tokens) valueStack = -- On ; S129
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 127 " ++ (show token) ++ ": S129") $ parse' (129:stateStack) tokens ((ParserToken token):valueStack)
        parse' (127:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (127:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 128
        parse' stateStack@(128:states) ((token@(KwGoto _)):tokens) valueStack = -- On goto S130
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 128 " ++ (show token) ++ ": S130") $ parse' (130:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(128:states) ((token@(KwReturn _)):tokens) valueStack = -- On return S131
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 128 " ++ (show token) ++ ": S131") $ parse' (131:stateStack) tokens ((ParserToken token):valueStack)
        parse' (128:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (128:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 129
        parse' (129:_:_:_:_:_:gs:states) tokens (value6:value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce AnalyserMatchNoAction: AnalyserMatch = re-start RE re-end -> identifier ;") $ parse' ((getGoto gs "AnalyserMatch"):gs:states) tokens ((process [value1,value2,value3,value4,value5,value6] reduceAnalyserMatchNoAction):values)
        parse' (129:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (129:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 130
        parse' stateStack@(130:states) ((token@(Identifier _ _)):tokens) valueStack = -- On identifier S133
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 130 " ++ (show token) ++ ": S133") $ parse' (133:stateStack) tokens ((ParserToken token):valueStack)
        parse' (130:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (130:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 131
        parse' stateStack@(131:states) ((token@(SemiColon _)):tokens) valueStack = -- On ; S132
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 131 " ++ (show token) ++ ": S132") $ parse' (132:stateStack) tokens ((ParserToken token):valueStack)
        parse' (131:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (131:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 132
        parse' (132:_:_:_:_:_:_:_:gs:states) tokens (value8:value7:value6:value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce AnalyserMatchWithReturn: AnalyserMatch = re-start RE re-end -> identifier , return ;") $ parse' ((getGoto gs "AnalyserMatch"):gs:states) tokens ((process [value1,value2,value3,value4,value5,value6,value7,value8] reduceAnalyserMatchWithReturn):values)
        parse' (132:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (132:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 133
        parse' stateStack@(133:states) ((token@(SemiColon _)):tokens) valueStack = -- On ; S134
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 133 " ++ (show token) ++ ": S134") $ parse' (134:stateStack) tokens ((ParserToken token):valueStack)
        parse' (133:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (133:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 134
        parse' (134:_:_:_:_:_:_:_:_:gs:states) tokens (value9:value8:value7:value6:value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce AnalyserMatchWithGoto: AnalyserMatch = re-start RE re-end -> identifier , goto identifier ;") $ parse' ((getGoto gs "AnalyserMatch"):gs:states) tokens ((process [value1,value2,value3,value4,value5,value6,value7,value8,value9] reduceAnalyserMatchWithGoto):values)
        parse' (134:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (134:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 135
        parse' (135:_:_:_:_:_:gs:states) tokens (value6:value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce AnalyserMatchError: AnalyserMatch = re-start RE re-end -> error ;") $ parse' ((getGoto gs "AnalyserMatch"):gs:states) tokens ((process [value1,value2,value3,value4,value5,value6] reduceAnalyserMatchError):values)
        parse' (135:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (135:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 136
        parse' stateStack@(136:states) ((token@(ReOpenParenthesis _)):tokens) valueStack = -- On ( S89
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S89") $ parse' (89:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReCloseParenthesis _)):tokens) valueStack = -- On ) S137
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S137") $ parse' (137:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReAsterisk _)):tokens) valueStack = -- On * S118
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S118") $ parse' (118:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(RePlus _)):tokens) valueStack = -- On + S119
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S119") $ parse' (119:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReDot _)):tokens) valueStack = -- On . S90
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S90") $ parse' (90:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReQuestion _)):tokens) valueStack = -- On ? S120
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S120") $ parse' (120:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReOpenBracket _)):tokens) valueStack = -- On [ S92
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S92") $ parse' (92:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReDigitClass _)):tokens) valueStack = -- On \d S93
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S93") $ parse' (93:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReFormFeed _)):tokens) valueStack = -- On \f S94
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S94") $ parse' (94:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReNewLine _)):tokens) valueStack = -- On \n S95
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S95") $ parse' (95:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReReturn _)):tokens) valueStack = -- On \r S96
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S96") $ parse' (96:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReWhitespaceClass _)):tokens) valueStack = -- On \s S97
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S97") $ parse' (97:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReTab _)):tokens) valueStack = -- On \t S98
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S98") $ parse' (98:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReAlphaNumClass _)):tokens) valueStack = -- On \w S99
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S99") $ parse' (99:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReCharacter _ _)):tokens) valueStack = -- On character S100
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S100") $ parse' (100:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(136:states) ((token@(ReOr _)):tokens) valueStack = -- On | S123
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 136 " ++ (show token) ++ ": S123") $ parse' (123:stateStack) tokens ((ParserToken token):valueStack)
        parse' (136:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (136:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 137
        parse' (137:_:_:gs:states) tokens (value3:value2:value1:values) = 
            trace' ("Reduce REScopedExpression: RE = ( RE )") $ parse' ((getGoto gs "RE"):gs:states) tokens ((process [value1,value2,value3] reduceREScopedExpression):values)
        parse' (137:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (137:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 138
        parse' (138:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce RestAnalyserMatches: AnalyserMatches = AnalyserMatches AnalyserMatch") $ parse' ((getGoto gs "AnalyserMatches"):gs:states) tokens ((process [value1,value2] reduceRestAnalyserMatches):values)
        parse' (138:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (138:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 139
        parse' (139:_:_:_:_:gs:states) tokens (value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce AnalyserNoOptions: Analyser = analyser identifier { AnalyserMatches }") $ parse' ((getGoto gs "Analyser"):gs:states) tokens ((process [value1,value2,value3,value4,value5] reduceAnalyserNoOptions):values)
        parse' (139:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (139:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 140
        parse' stateStack@(140:states) ((token@(Comma _)):tokens) valueStack = -- On , S142
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 140 " ++ (show token) ++ ": S142") $ parse' (142:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(140:states) ((token@(CloseBracket _)):tokens) valueStack = -- On closebracket S143
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 140 " ++ (show token) ++ ": S143") $ parse' (143:stateStack) tokens ((ParserToken token):valueStack)
        parse' (140:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (140:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 141
        parse' (141:gs:states) tokens (value1:values) = 
            trace' ("Reduce FirstString: Strings = string") $ parse' ((getGoto gs "Strings"):gs:states) tokens ((process [value1] reduceFirstString):values)
        parse' (141:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (141:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 142
        parse' stateStack@(142:states) ((token@(String _ _)):tokens) valueStack = -- On string S147
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 142 " ++ (show token) ++ ": S147") $ parse' (147:stateStack) tokens ((ParserToken token):valueStack)
        parse' (142:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (142:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 143
        parse' stateStack@(143:states) ((token@(OpenCurly _)):tokens) valueStack = -- On { S144
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 143 " ++ (show token) ++ ": S144") $ parse' (144:stateStack) tokens ((ParserToken token):valueStack)
        parse' (143:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (143:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 144
        parse' stateStack@(144:states) ((token@(ReStart _)):tokens) valueStack = -- On re-start S88
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 144 " ++ (show token) ++ ": S88") $ parse' (88:stateStack) tokens ((ParserToken token):valueStack)
        parse' (144:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (144:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 145
        parse' stateStack@(145:states) ((token@(ReStart _)):tokens) valueStack = -- On re-start S88
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 145 " ++ (show token) ++ ": S88") $ parse' (88:stateStack) tokens ((ParserToken token):valueStack)
        parse' stateStack@(145:states) ((token@(CloseCurly _)):tokens) valueStack = -- On } S146
            trace' ("stateStack = " ++ (show stateStack) ++ "; valueStack = " ++ (show valueStack) ++ "; 145 " ++ (show token) ++ ": S146") $ parse' (146:stateStack) tokens ((ParserToken token):valueStack)
        parse' (145:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (145:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 146
        parse' (146:_:_:_:_:_:_:_:gs:states) tokens (value8:value7:value6:value5:value4:value3:value2:value1:values) = 
            trace' ("Reduce AnalyserWithOptions: Analyser = analyser identifier openbracket Strings closebracket { AnalyserMatches }") $ parse' ((getGoto gs "Analyser"):gs:states) tokens ((process [value1,value2,value3,value4,value5,value6,value7,value8] reduceAnalyserWithOptions):values)
        parse' (146:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (146:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 147
        parse' (147:_:_:gs:states) tokens (value3:value2:value1:values) = 
            trace' ("Reduce RestStrings: Strings = Strings , string") $ parse' ((getGoto gs "Strings"):gs:states) tokens ((process [value1,value2,value3] reduceRestStrings):values)
        parse' (147:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (147:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 148
        parse' (148:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce Document: Document = Statements $") $ process [value1,value2] reduceDocument
        parse' (148:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (148:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")

        -- State 149
        parse' (149:_:gs:states) tokens (value2:value1:values) = 
            trace' ("Reduce RestStatements: Statements = Statements Statement") $ parse' ((getGoto gs "Statements"):gs:states) tokens ((process [value1,value2] reduceRestStatements):values)
        parse' (149:_) [] _ = ParserError (0,0,0,0) "Unexpected end-of-text reached"
        parse' (149:_) (tkn:_) _ = ParserError (getTokenLocation tkn) ("Syntax error. Did not expect " ++ (show tkn) ++ ".")
    in case parse' [0] tokens [] of
        ParserAst value -> ResultAst value
        ParserError loc msg -> ResultError loc msg
        x -> let loc = getLocation x in ResultError loc "Internal Parser Error"
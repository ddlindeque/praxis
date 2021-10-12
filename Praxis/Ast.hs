module Praxis.Ast
(
    process
)
where

import System.IO
import Praxis.ParserCommon as ParserCommon
import qualified Praxis.FormatCommon as FormatCommon
import Praxis.ManualLexer as Lexer
import Praxis.ManualParser as Parser

import qualified Praxis.Formats.Json as Json

formatRange :: Parser.Ast -> String
formatRange (Parser.RECharacterRangeExpression _ (ReCharacter _ ch)) = [ch]
formatRange (Parser.REWhitespaceCharRangeExpression _ _) = "\\s"
formatRange (Parser.REDigitCharRangeExpression _ _) = "\\d"
formatRange (Parser.REAlphaNumCharRangeExpression _ _) = "\\w"
formatRange (Parser.RENewLineCharRangeExpression _ _) = "\\n"
formatRange (Parser.REReturnCharRangeExpression _ _) = "\\r"
formatRange (Parser.RETabCharRangeExpression _ _) = "\\t"
formatRange (Parser.REFormFeedCharRangeExpression _ _) = "\\f"
formatRange (Parser.RECharRangeRangeExpression _ (ReCharacter _ from) _ (ReCharacter _ to)) = from:'-':[to]

formatReRanges :: Parser.Ast -> String
formatReRanges (Parser.RERestRangeExpression _ rest range) = (formatRange range) ++ (formatReRanges rest)
formatReRanges (Parser.REFirstRangeExpression _ range) = formatRange range

formatRe :: Parser.Ast -> String
formatRe (Parser.RECharacterExpression _ (Lexer.ReCharacter _ ch)) = [ch]
formatRe (Parser.REWhitespaceCharExpression _ _) = "\\s"
formatRe (Parser.REDigitCharExpression _ _) = "\\d"
formatRe (Parser.REAlphaNumCharExpression _ _) = "\\w"
formatRe (Parser.RENewLineCharExpression _ _) = "\\n"
formatRe (Parser.REReturnCharExpression _ _) = "\\r"
formatRe (Parser.RETabCharExpression _ _) = "\\t"
formatRe (Parser.REFormFeedCharExpression _ _) = "\\f"
formatRe (Parser.RERangeExpression _ _ ranges _) = '[':((formatReRanges ranges) ++ "]")
formatRe (Parser.RENotRangeExpression _ _ _ ranges _) = '[':'^':((formatReRanges ranges) ++ "]")
formatRe (Parser.REAnyCharExpression _ _) = "."
formatRe (Parser.REThenExpression _ re1 re2) = (formatRe re1) ++ (formatRe re2)
formatRe (Parser.REZeroOrMoreExpression _ re _) = (formatRe re) ++ "*"
formatRe (Parser.REOneOrMoreExpression _ re _) = (formatRe re) ++ "+"
formatRe (Parser.REZeroOrOneExpression _ re _) = (formatRe re) ++ "?"
formatRe (Parser.REScopedExpression _ _ re _) = '(':((formatRe re) ++ ")")
formatRe (Parser.REOrExpression _ re1 _ re2) = (formatRe re1) ++ "|" ++ (formatRe re2)

transformAnalyserRow :: Parser.Ast -> FormatCommon.AnalyserRow
transformAnalyserRow (Parser.AnalyserMatchNoAction _ _ re _ _ (Lexer.Identifier _ token) _) = FormatCommon.AnalyserRow (formatRe re) token Nothing
transformAnalyserRow (Parser.AnalyserMatchWithReturn _ _ re _ _ (Lexer.Identifier _ token) _ _ _) = FormatCommon.AnalyserRow (formatRe re) token (Just FormatCommon.Return)
transformAnalyserRow (Parser.AnalyserMatchWithGoto _ _ re _ _ (Lexer.Identifier _ token) _ _ (Lexer.Identifier _ target) _) = FormatCommon.AnalyserRow (formatRe re) token (Just (FormatCommon.Goto target))
transformAnalyserRow (Parser.AnalyserMatchError _ _ re _ _ _ _) = FormatCommon.AnalyserRow (formatRe re) "error" Nothing

transformAnalyserRows :: Parser.Ast -> [FormatCommon.AnalyserRow]
transformAnalyserRows (Parser.RestAnalyserMatches _ rest row) = (transformAnalyserRow row):(transformAnalyserRows rest)
transformAnalyserRows (Parser.FirstAnalyserMatch _ row) = [transformAnalyserRow row]

formatTeIdentifiers :: Parser.Ast -> String
formatTeIdentifiers (Parser.RestIdentifiers _ rest (Lexer.Identifier _ value)) = value ++ (' ':(formatTeIdentifiers rest))
formatTeIdentifiers (Parser.FirstIdentifier _ (Lexer.Identifier _ value)) = value

formatTe :: Parser.Ast -> String
formatTe (Parser.TEIdentifierExpression _ (Lexer.Identifier _ identifier)) = identifier
formatTe (Parser.TENotRangeExpression _ _ _ identifiers _) = formatTeIdentifiers identifiers
formatTe (Parser.REAnyCharExpression _ _) = "."
formatTe (Parser.TEThenExpression _ te1 te2) = (formatTe te1) ++ (formatTe te2)
formatTe (Parser.TEZeroOrMoreExpression _ te _) = (formatTe te) ++ "*"
formatTe (Parser.TEOneOrMoreExpression _ te _) = (formatTe te) ++ "+"
formatTe (Parser.TEZeroOrOneExpression _ te _) = (formatTe te) ++ "?"
formatTe (Parser.TEScopedExpression _ _ te _) = '(':((formatTe te) ++ ")")
formatTe (Parser.TEOrExpression _ te1 _ te2) = (formatTe te1) ++ "|" ++ (formatTe te2)

transformScannerRow :: Parser.Ast -> FormatCommon.ScannerRow
transformScannerRow (Parser.ScannerMatchNoAction _ te _ (Lexer.Identifier _ token) _) = FormatCommon.ScannerRow (formatTe te) token Nothing
transformScannerRow (Parser.ScannerMatchWithReturn _ te _ (Lexer.Identifier _ token) _ _ _) = FormatCommon.ScannerRow (formatTe te) token (Just FormatCommon.Return)
transformScannerRow (Parser.ScannerMatchWithGoto _ te _ (Lexer.Identifier _ token) _ _ (Lexer.Identifier _ target) _) = FormatCommon.ScannerRow (formatTe te) token (Just (FormatCommon.Goto target))
transformScannerRow (Parser.ScannerMatchError _ te _ _ _) = FormatCommon.ScannerRow (formatTe te) "error" Nothing

transformScannerRows :: Parser.Ast -> [FormatCommon.ScannerRow]
transformScannerRows (Parser.RestScannerMatches _ rest row) = (transformScannerRow row):(transformScannerRows rest)
transformScannerRows (Parser.FirstScannerMatch _ row) = [transformScannerRow row]

getProcessing :: Parser.Ast -> String
getProcessing (Parser.FirstScannerOption _ (Parser.ProcessingScannerOption _ _ (Lexer.Identifier _ value))) = value
getProcessing (Parser.RestScannerOptions _ rest (Parser.ProcessingScannerOption _ _ (Lexer.Identifier _ value))) = value
getProcessing (Parser.RestScannerOptions _ rest _) = getProcessing rest

getCommaIdentifiers :: Parser.Ast -> [String]
getCommaIdentifiers (Parser.RestCommaIdentifiers _ rest _ (Lexer.Identifier _ identifier)) = identifier:(getCommaIdentifiers rest)
getCommaIdentifiers (Parser.FirstCommaIdentifier _ (Lexer.Identifier _ identifier)) = [identifier]

getIgnoring :: Parser.Ast -> [String]
getIgnoring (Parser.FirstScannerOption _ (Parser.IgnoringScannerOption _ _ _ identifiers _)) = getCommaIdentifiers identifiers
getIgnoring (Parser.FirstScannerOption _ _) = []
getIgnoring (Parser.RestScannerOptions _ rest (Parser.IgnoringScannerOption _ _ _ identifiers _)) = getCommaIdentifiers identifiers
getIgnoring (Parser.RestScannerOptions _ rest _) = getIgnoring rest

getItems :: Parser.Ast -> [String]
getItems (Parser.RestItems _ rest (Lexer.Identifier _ value)) = value:(getItems rest)
getItems (Parser.FirstItem _ (Lexer.Identifier _ value)) = [value]

getAssoc :: Parser.Ast -> String
getAssoc (Parser.LeftAssoc _ _) = "left"
getAssoc (Parser.RightAssoc _ _) = "right"
getAssoc (Parser.NoAssoc _ _) = "none"

transformProduction :: Parser.Ast -> FormatCommon.ParserRow
-- "identifier", ":", "identifier", "=", "Items", ";"
transformProduction (Parser.ProductionNoOptions _ (Lexer.Identifier _ name) _ (Lexer.Identifier _ nt) _ items _) = FormatCommon.ParserRow name nt (getItems items) "none" 0
-- "identifier", ":", "identifier", "=", "Items", "openbracket", "Assoc", "closebracket", ";"
transformProduction (Parser.ProductionWithAssoc _ (Lexer.Identifier _ name) _ (Lexer.Identifier _ nt) _ items _ assoc _ _) = FormatCommon.ParserRow name nt (getItems items) (getAssoc assoc) 0
-- "identifier", ":", "identifier", "=", "Items", "openbracket", "Assoc", ",", "number", "closebracket", ";"
transformProduction (Parser.ProductionWithPrec _ (Lexer.Identifier _ name) _ (Lexer.Identifier _ nt) _ items _ assoc _ (Integer _ number) _ _) = FormatCommon.ParserRow name nt (getItems items) (getAssoc assoc) number

transformProductions :: Parser.Ast -> [FormatCommon.ParserRow]
transformProductions (Parser.RestProductions _ rest row) = (transformProduction row):(transformProductions rest)
transformProductions (Parser.FirstProduction _ row) = [transformProduction row]

transformStatement :: Parser.Ast -> FormatCommon.Statement
transformStatement (Parser.AnalyserStatement _ (Parser.AnalyserNoOptions _ _ (Lexer.Identifier _ name) _ rows _)) = FormatCommon.Analyser name (transformAnalyserRows rows)
transformStatement (Parser.AnalyserStatement _ (Parser.AnalyserWithOptions _ _ (Lexer.Identifier _ name) _ _ _ _ rows _)) = FormatCommon.Analyser name (transformAnalyserRows rows)
transformStatement (Parser.ScannerStatement _ (Parser.EmptyScanner _ _ (Lexer.Identifier _ name) scannerOptions _ _)) = FormatCommon.Scanner name (getProcessing scannerOptions) (getIgnoring scannerOptions) []
transformStatement (Parser.ScannerStatement _ (Parser.ScannerWithRules _ _ (Lexer.Identifier _ name) scannerOptions _ rows _)) = FormatCommon.Scanner name (getProcessing scannerOptions) (getIgnoring scannerOptions) (transformScannerRows rows)
transformStatement (Parser.ParserStatement _ (Parser.Parser _ _ (Lexer.Identifier _ name) _ (Lexer.Identifier _ processing) _ productions _)) = FormatCommon.Parser name processing (transformProductions productions)

transformStatements :: Parser.Ast -> [FormatCommon.Statement]
transformStatements (Parser.RestStatements _ rest statement) = (transformStatement statement):(transformStatements rest)
transformStatements (Parser.FirstStatement _ statement) = [transformStatement statement]

transform :: Parser.Ast -> [FormatCommon.Statement]
transform (Parser.Document _ statements _) = transformStatements statements

-- format handle source
fmt :: String -> Handle -> String -> IO ()
fmt "json" handle source =
    case Parser.parse $ ParserCommon.parserTokens $ Lexer.tokenise source of
        Parser.ResultError loc msg -> 
            do
                putStrLn (ParserCommon.formatError loc msg)
        Parser.ResultAst ast -> 
            do
                Json.formatAst handle $ transform ast

-- format input output
process :: String -> String -> String -> IO ()
process format "" "" =
    do
        source <- hGetContents stdin
        fmt format stdout source
process format "" output =
    withFile output WriteMode $ \handle -> do
        source <- hGetContents stdin
        fmt format handle source
process format input "" =
    do
        source <- readFile input
        fmt format stdout source
process format input output =
    withFile output WriteMode $ \handle -> do
        source <- readFile input
        fmt format handle source
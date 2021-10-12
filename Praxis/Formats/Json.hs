module Praxis.Formats.Json
(
    formatAst
)
where

import System.IO
import Praxis.FormatCommon

escapeJson :: String -> String
escapeJson [] = []
escapeJson ('\"':cs) = '\\':'"':(escapeJson cs)
escapeJson ('\n':cs) = '\\':'\\':'n':(escapeJson cs)
escapeJson ('\r':cs) = '\\':'\\':'r':(escapeJson cs)
escapeJson ('\t':cs) = '\\':'\\':'t':(escapeJson cs)
escapeJson ('\f':cs) = '\\':'\\':'f':(escapeJson cs)
escapeJson ('\\':cs) = '\\':'\\':(escapeJson cs)
escapeJson (c:cs) = c:(escapeJson cs)

writeStringList :: String -> Handle -> [String] -> IO ()
writeStringList _ _ [] =
    do
        return ()
writeStringList indent handle [s] =
    do
        hPutStr handle (indent ++ "\"" ++ (escapeJson s) ++ "\"")
writeStringList indent handle (s:ss) =
    do
        writeStringList indent handle [s]
        hPutStrLn handle ","
        writeStringList indent handle ss

writeAction :: Handle -> (Maybe Action) -> IO ()
writeAction handle Nothing =
    do
        return ()
writeAction handle (Just (Goto name)) =
    do
        hPutStrLn handle ("                    {")
        hPutStrLn handle ("                        \"action\": \"goto\",")
        hPutStrLn handle ("                        \"to\": \"" ++ (escapeJson name) ++ "\"")
        hPutStrLn handle ("                    }")
writeAction handle (Just (Return)) =
    do
        hPutStrLn handle ("                    {")
        hPutStrLn handle ("                        \"action\": \"return\"")
        hPutStrLn handle ("                    }")

writeStatement :: Handle -> Statement -> IO ()
writeStatement handle (Analyser name rows) =
    let writeRows [] =
            do
                return ()
        writeRows [AnalyserRow re token action] =
            do
                hPutStrLn handle ("            {")
                hPutStrLn handle ("                \"re\": \"" ++ (escapeJson re) ++ "\",")
                hPutStrLn handle ("                \"token\": \"" ++ (escapeJson token) ++ "\",")
                hPutStrLn handle ("                \"actions\": [")
                writeAction handle action
                hPutStrLn handle ("                ]")
                hPutStr   handle ("            }")
        writeRows (r:rs) =
            do
                writeRows [r]
                hPutStrLn handle ","
                writeRows rs
    in
        do
            hPutStrLn handle ("    {")
            hPutStrLn handle ("        \"kind\": \"analyser\",")
            hPutStrLn handle ("        \"name\": \"" ++ (escapeJson name) ++ "\",")
            hPutStrLn handle ("        \"rows\": [")
            writeRows rows
            hPutStrLn handle ""
            hPutStrLn handle ("        ]")
            hPutStr   handle ("    }")
writeStatement handle (Scanner name processing ignoring rows) =
    let writeRows [] =
            do
                return ()
        writeRows [ScannerRow re token action] =
            do
                hPutStrLn handle ("            {")
                hPutStrLn handle ("                \"re\": \"" ++ (escapeJson re) ++ "\",")
                hPutStrLn handle ("                \"token\": \"" ++ (escapeJson token) ++ "\",")
                hPutStrLn handle ("                \"actions\": [")
                writeAction handle action
                hPutStrLn handle ("                ]")
                hPutStr   handle ("            }")
        writeRows (r:rs) =
            do
                writeRows [r]
                hPutStrLn handle ","
                writeRows rs
    in
        do
            hPutStrLn handle ("    {")
            hPutStrLn handle ("        \"kind\": \"scanner\",")
            hPutStrLn handle ("        \"name\": \"" ++ (escapeJson name) ++ "\",")
            hPutStrLn handle ("        \"processing\": \"" ++ (escapeJson processing) ++ "\",")
            hPutStrLn handle ("        \"ignoring\": [")
            writeStringList "            " handle ignoring
            hPutStrLn handle ""
            hPutStrLn handle ("        ],")
            hPutStrLn handle ("        \"rows\": [")
            writeRows rows
            hPutStrLn handle ""
            hPutStrLn handle ("        ]")
            hPutStr   handle ("    }")
writeStatement handle (Parser name processing rows) =
    let writeRows [] =
            do
                return ()
        writeRows [ParserRow name nt items assoc prec] =
            do
                hPutStrLn handle ("            {")
                hPutStrLn handle ("                \"name\": \"" ++ (escapeJson name) ++ "\",")
                hPutStrLn handle ("                \"nonTerminal\": \"" ++ (escapeJson nt) ++ "\",")
                hPutStrLn handle ("                \"items\": [")
                writeStringList "                    " handle items
                hPutStrLn handle ""
                hPutStrLn handle ("                ],")
                hPutStrLn handle ("                \"associativity\": \"" ++ (escapeJson assoc) ++ "\",")
                hPutStrLn handle ("                \"precedence\": \"" ++ (show prec) ++ "\"")
                hPutStr   handle ("            }")
        writeRows (r:rs) =
            do
                writeRows [r]
                hPutStrLn handle ","
                writeRows rs
    in
        do
            hPutStrLn handle ("    {")
            hPutStrLn handle ("        \"kind\": \"parser\",")
            hPutStrLn handle ("        \"name\": \"" ++ (escapeJson name) ++ "\",")
            hPutStrLn handle ("        \"processing\": \"" ++ (escapeJson processing) ++ "\",")
            hPutStrLn handle ("        \"rows\": [")
            writeRows rows
            hPutStrLn handle ""
            hPutStrLn handle ("        ]")
            hPutStr   handle ("    }")

writeStatements :: Handle -> [Statement] -> IO ()
writeStatements handle statements =
    let writeStatements' [] =
            do
                return ()
        writeStatements' [s] =
            do
                writeStatement handle s
        writeStatements' (s:ss) =
            do
                writeStatement handle s
                hPutStrLn handle ","
                writeStatements' ss
    in
        do
            hPutStrLn handle "["
            writeStatements' statements
            hPutStrLn handle ""
            hPutStrLn handle "]"

formatAst :: Handle -> [Statement] -> IO ()
formatAst = writeStatements

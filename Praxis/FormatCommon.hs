module Praxis.FormatCommon
(
    Action(..),
    AnalyserRow(..),
    ScannerRow(..),
    ParserRow(..),
    Statement(..)
)
where

data Action =
      Goto String
    | Return
    deriving (Show)

data AnalyserRow = AnalyserRow String String (Maybe Action) deriving (Show)

data ScannerRow = ScannerRow String String (Maybe Action) deriving (Show)

data ParserRow = ParserRow String String [String] String Int deriving (Show)

data Statement =
      Analyser String [AnalyserRow]
    | Scanner String String [String] [ScannerRow]
    | Parser String String [ParserRow]
    deriving (Show)
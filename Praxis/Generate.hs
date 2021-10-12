module Praxis.Generate
(
    process
)
where

import System.IO
import qualified Praxis.Languages.Haskell as Haskell

--write :: Handle -> String -> String -> [String] -> IO ()
--write handle source "haskell" objects = Haskell.generate handle source objects

generate :: Handle -> String -> String -> [String] -> IO ()
generate _ _ _ _ =
    do
        putStrLn "Not implemented"

-- [objects], language, source, output
process :: [String] -> String -> String -> String -> IO ()
process objects language "" "" =
    do
        source <- hGetContents stdin
        generate stdout source language objects
process objects language "" output =
    withFile output WriteMode $ \handle -> do
        source <- hGetContents stdin
        generate handle source language objects
process objects language input "" =
    do
        source <- readFile input
        generate stdout source language objects
process objects language input output =
    withFile output WriteMode $ \handle -> do
        source <- readFile input
        generate handle source language objects
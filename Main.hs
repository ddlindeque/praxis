import System.Environment
import Args
import qualified Praxis.Colourise as Colourise

colourise :: [(String, Args.ArgValue)] -> IO ()
colourise args =
    let colourise' input output stylesheet doc [] = Colourise.process input output stylesheet doc
        colourise' input output stylesheet doc (("input",Args.ParameterValue value):xs) = colourise' value output stylesheet doc xs
        colourise' input output stylesheet doc (("output",Args.ParameterValue value):xs) = colourise' input value stylesheet doc xs
        colourise' input output stylesheet doc (("stylesheet",Args.ParameterValue value):xs) = colourise' input output value doc xs
        colourise' input output stylesheet doc (("document",Args.FlagValue value):xs) = colourise' input output stylesheet value xs
    in colourise' "" "" "" False args

colouriseSpecs = (
        "colourise", 
        "Generate a colourised version of the input.", 
        [
            Args.Parameter (Just 'o') "output" Nothing "The output filename.", 
            Args.Parameter (Just 'i') "input" Nothing "The input filename.",
            Args.Parameter Nothing "stylesheet" (Just "colourise.css") "The stylesheet to link.",
            Args.Flag (Just 'd') "document" "Specify this flag to generate the whole HTML document."
        ], 
        colourise
    )

main = do
    args <- getArgs
    Args.process "praxis" "Lexer/parser generator" "1.0" [colouriseSpecs] args
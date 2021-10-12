import System.Environment
import Args
import qualified Praxis.Colourise as Colourise
import qualified Praxis.Graph as Graph
import qualified Praxis.Generate as Generate
import qualified Praxis.Ast as Ast

colourise :: [(String, Args.ArgValue)] -> IO ()
colourise args =
    let colourise' input output stylesheet doc tooltips [] = Colourise.process input output stylesheet doc tooltips
        colourise' input output stylesheet doc tooltips (("input",Args.ParameterValue value):xs) = colourise' value output stylesheet doc tooltips xs
        colourise' input output stylesheet doc tooltips (("output",Args.ParameterValue value):xs) = colourise' input value stylesheet doc tooltips xs
        colourise' input output stylesheet doc tooltips (("stylesheet",Args.ParameterValue value):xs) = colourise' input output value doc tooltips xs
        colourise' input output stylesheet doc tooltips (("document",Args.FlagValue value):xs) = colourise' input output stylesheet value tooltips xs
        colourise' input output stylesheet doc tooltips (("tooltips",Args.FlagValue value):xs) = colourise' input output stylesheet doc value xs
    in colourise' "" "" "" False False args

colouriseSpecs = (
        "colourise", 
        "Generate a colourised version of the input.", 
        [
            Args.Parameter (Just 'o') "output" (Just "") "The output filename. Will use stdout by default.", 
            Args.Parameter (Just 'i') "input" (Just "") "The input filename. Will use stdin by default.",
            Args.Parameter (Just 's') "stylesheet" (Just "colourise.css") "The stylesheet to link.",
            Args.Flag (Just 'd') "document" "Specify this flag to generate the whole HTML document.",
            Args.Flag (Just 't') "tooltips" "Specify this flag to generate tooltips on tokens"
        ],
        colourise
    )

graph :: [(String, Args.ArgValue)] -> IO ()
graph args =
    let graph' object evolution input output [] = Graph.process object evolution input output
        graph' object evolution input output (("object", Args.ParameterValue value):xs) = graph' value evolution input output xs
        graph' object evolution input output (("input", Args.ParameterValue value):xs) = graph' object evolution value output xs
        graph' object evolution input output (("output", Args.ParameterValue value):xs) = graph' object evolution input value xs
        graph' object evolution input output (("evolution", Args.ParameterValue value):xs) = graph' object value input output xs
    in graph' "" "" "" "" args

graphSpecs = (
        "graph",
        "Generate a graphviz diagram",
        [
            Args.Parameter (Just 'o') "output" (Just "") "The output filename. Will use stdout by default.", 
            Args.Parameter (Just 'i') "input" (Just "") "The input filename. Will use stdin by default.",
            Args.Parameter Nothing "object" (Just "") "The automata to graph. Will graph the definition AST by default.",
            Args.Parameter (Just 'e') "evolution" (Just "") "The evoluation of the automata to graph. Valid values include 'nfa' and 'dfa'."
        ],
        graph
    )

generate :: [(String, Args.ArgValue)] -> IO ()
generate args =
    let generate' objects language source output [] = Generate.process objects language source output
        generate' objects language source output (("objects", Args.ParameterListValue values):xs) = generate' values language source output xs
        generate' objects language source output (("language", Args.ParameterValue value):xs) = generate' objects value source output xs
        generate' objects language source output (("input", Args.ParameterValue value):xs) = generate' objects language value output xs
        generate' objects language source output (("output", Args.ParameterValue value):xs) = generate' objects language source value xs
    in generate' [] "" "" "" args

generateSpecs = (
        "generate",
        "Generate the specified objects in the specified language",
        [
            Args.ParameterList Nothing "objects" "The list of objects to generate. The default is to generate all objects.",
            Args.Parameter (Just 'l') "language" (Just "haskell") "The language to generate.",
            Args.Parameter (Just 'i') "input" (Just "") "The input filename. Will use stdin by default.",
            Args.Parameter (Just 'o') "output" (Just "") "The output filename. Will use stdout by default."
        ],
        generate
    )

ast :: [(String, Args.ArgValue)] -> IO ()
ast args =
    let ast' format source output [] = Ast.process format source output
        ast' format source output (("format", Args.ParameterValue value):xs) = ast' value source output xs
        ast' format source output (("input", Args.ParameterValue value):xs) = ast' format value output xs
        ast' format source output (("output", Args.ParameterValue value):xs) = ast' format source value xs
    in ast' "" "" "" args

astSpecs = (
        "ast",
        "Write the AST in the specified format",
        [
            Args.Parameter (Just 'f') "format" (Just "json") "The format to write to.",
            Args.Parameter (Just 'i') "input" (Just "") "The input filename. Will use stdin by default.",
            Args.Parameter (Just 'o') "output" (Just "") "The output filename. Will use stdout by default."
        ],
        ast
    )

main = do
    args <- getArgs
    Args.process "praxis" "Lexer/parser generator" "1.0" [colouriseSpecs, graphSpecs, generateSpecs, astSpecs] args
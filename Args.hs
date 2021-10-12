module Args
(
    ArgOption (..),
    ArgValue (..),
    process
)
where

import System.IO
import Debug.Trace
import Common

-- praxis --help
-- praxis --version
-- praxis cmd [options]
-- cmd = string
-- [options]:
  -- -x/--xyz
  -- -x/--xyz value
  -- -x/--xyz=value
  -- -x/--xyz = value

data ArgOption =
      Flag (Maybe Char) String String
    | Parameter (Maybe Char) String (Maybe String) String
    | ParameterList (Maybe Char) String String
    deriving (Show)

data ArgValue =
      FlagValue Bool
    | ParameterValue String
    | ParameterListValue [String]
    deriving (Show)

writeHelp :: String -> String -> [(String, String, [ArgOption], [(String, ArgValue)] -> IO ())] -> IO ()
writeHelp name description specs =
    let writeSpecs [] =
            do
                putStrLn ""
        writeSpecs ((cmd, desc, options, _):xs) =
            do
                putStrLn ""
                putStrLn cmd
                putStrLn ("  " ++ desc)
                putStrLn ("  Use '" ++ name ++ " " ++ cmd ++ " --help' for more information.")
                writeSpecs xs
    in do
            putStrLn name
            putStrLn description
            putStrLn ""
            putStrLn ("  --help")
            putStrLn "             Show this help"
            putStrLn ("  --version")
            putStrLn "             Show the version"
            writeSpecs specs
            putStrLn ""
            putStrLn "Use '@' to escape - or -- at the start of a value, i.e.: '--name = \"@--some name\"'."

writeCmdHelp :: String -> String -> String -> [ArgOption] -> IO ()
writeCmdHelp name cmd desc options =
    let getName Nothing name = "--" ++ name
        getName (Just c) name = '-':c:("/--" ++ name)
        getDef Nothing = "Mandatory"
        getDef (Just "") = "Optional"
        getDef (Just x) = "Optional (" ++ x ++ ")"
        writeOptions [] =
            do
                return ()
        writeOptions ((Flag c name' help):xs) =
            do
                putStrLn ("  " ++ (getName c name'))
                putStrLn ("             Flag. " ++ help)
                writeOptions xs
        writeOptions ((Parameter c name' def help):xs) =
            do
                putStrLn ("  " ++ (getName c name'))
                putStrLn ("             " ++ (getDef def) ++ ". " ++ help)
                writeOptions xs
        writeOptions ((ParameterList c name' help):xs) =
            do
                putStrLn ("  " ++ (getName c name'))
                putStrLn ("             Parameter List. " ++ help)
                writeOptions xs
    in do
            putStrLn (name ++ " " ++ cmd)
            putStrLn desc
            putStrLn ""
            putStrLn ("  --help")
            putStrLn "             Show this help"
            writeOptions options

data ArgName = Short Char | Long String deriving (Eq, Ord)

instance Show ArgName where
    show (Short c) = '-':[c]
    show (Long name) = '-':'-':name

data Token = Name ArgName | Equal | Value String deriving (Eq, Ord)

instance Show Token where
    show (Name arg) = show arg
    show Equal = "="
    show (Value value) = value

analyse :: [String] -> [Token]
analyse [] = []
analyse ("=":args) = Equal:(analyse args)
analyse (('-':'-':[]):args) = (Value "--"):(analyse args)
analyse (('@':'-':'-':text):args) = (Value ("--" ++ text)):(analyse args)
analyse (('@':'-':text):args) = (Value ("-" ++ text)):(analyse args)
analyse (('-':'-':text):args) =
    let analyseText 0 name _ [] = (name, Nothing)
        analyseText 0 name _ ('=':cs) = analyseText 1 name "" cs
        analyseText 0 name _ (c:cs) = analyseText 0 (name ++ [c]) "" cs
        analyseText 1 name value [] = (name, Just value)
        analyseText 1 name value (c:cs) = analyseText 1 name (value ++ [c]) cs
    in
        case analyseText 0 "" "" text of
            (name, Nothing) -> (Name $ Long name):(analyse args)
            (name, Just value) -> (Name $ Long name):(Equal):(Value value):(analyse args)
analyse (('-':c:[]):args) = (Name $ Short c):(analyse args)
analyse (('-':c:'=':[]):args) = (Name $ Short c):(Equal):(Value ""):(analyse args)
analyse (('-':c:'=':value):args) = (Name $ Short c):(Equal):(Value value):(analyse args)
analyse (value:args) = (Value value):(analyse args)

-- 1. ARGS =                   {[]}
-- 2. ARGS = ARGS ARG          {ARG:ARGS}
-- 3. ARG = name               {ARG=AstFlag name}
-- 4. ARG = name value         {ARG=AstParameter name value}
-- 5. ARG = name equal value   {ARG=AstParameter name value}

-- state 0
-- ARGS = .
-- ARGS = . ARGS ARG

-- state 1 (0 ARGS)
-- ARGS = ARGS .ARG
-- ARG = .name
-- ARG = .name value
-- ARG = .name equal value

-- state 2 (1 ARG)
-- ARGS = ARGS ARG .

-- state 3 (1 name)
-- ARG = name .
-- ARG = name .value
-- ARG = name .equal value

-- state 4 (3 value)
-- ARG = name value .

-- state 5 (3 equal)
-- ARG = name equal .value

-- state 6 (5 value)
-- ARG = name equal value .

-- state | name     | equal     | value     | ARG       | ARGS
-- 0     |          |           |           |           | G1
-- 1     | S3       |           |           | G2        |
-- 2     | R2       | R2        | R2        |           |
-- 3     | R3       | S5        | S4        |           |
-- 4     | R4       | R4        | R4        |           |
-- 5     |          |           | S6        |           |
-- 6     | R5       | R5        | R5        |           |

data AstArg =
      AstToken Token
    | AstFlag ArgName Bool
    | AstParameter ArgName String
    | AstParameterList ArgName [String]
    deriving (Show)

parse :: [Token] -> Common.Result [AstArg]
parse tkns =
    --  parse'' :: [Int] -> [Token] -> [[AstArg]] -> Common.Result [AstArg]
    let parse'' (0:ss) [] [] = Common.Ok []
        parse'' (0:ss) [] [v] = Common.Ok v
        -- goto 1
        parse'' (0:ss) tkns' vs = parse' (1:0:ss) tkns' ([]:vs)
        -- shift 3
        parse'' (1:ss) [] [v] = Common.Ok v
        parse'' (1:ss) (tkn@(Name _):tkns') vs = parse' (3:1:ss) tkns' ([AstToken tkn]:vs)
        parse'' (1:ss) (tkn:_) _ = Common.Error ("Unexpected parameter value '" ++ (show tkn) ++ "'.")
        -- reduce ARGS=ARGS ARG (expect to end in state 0, goto 1)
        parse'' (2:_:0:ss) tkns' ([v]:l:v2) = parse' (1:0:ss) tkns' ((v:l):v2)
        -- shift 5
        parse'' (3:ss) (Equal:tkns') vs = parse' (5:3:ss) tkns' ([AstToken Equal]:vs)
        -- shift 4
        parse'' (3:ss) (tkn@(Value value):tkns') vs = parse' (4:3:ss) tkns' ([AstToken tkn]:vs)
        -- reduce ARG=name (expect to end in state 1, goto 2)
        parse'' (3:1:ss) tkns' ([AstToken (Name name)]:vs) = parse' (2:1:ss) tkns' ([AstFlag name True]:vs)
        -- reduce ARG=name value (expect to end in state 1, goto 2)
        parse'' (4:_:1:ss) tkns' ([AstToken (Value value)]:[AstToken (Name name)]:vs) = parse' (2:1:ss) tkns' ([AstParameter name value]:vs)
        -- shift 6
        parse'' (5:ss) (tkn@(Value value):tkns') vs = parse' (6:5:ss) tkns' ([AstToken tkn]:vs)
        parse'' (5:ss) (tkn:_) _ = Common.Error ("Unexpected parameter value '" ++ (show tkn) ++ "'.")
        -- reduce ARG=name equal value (expect to end in state 1, goto 2)
        parse'' (6:_:_:1:ss) tkns' ([AstToken (Value value)]:[AstToken Equal]:[AstToken (Name name)]:vs) = parse' (2:1:ss) tkns' ([AstParameter name value]:vs)

        -- parse' ss tkns' vs = trace ("parse' " ++ (show ss) ++ " " ++ (show tkns') ++ " " ++ (show vs)) $ parse'' ss tkns' vs
        parse' = parse''
    in parse' [0] tkns []

runFunc :: [ArgOption] -> [String] -> ([(String, ArgValue)] -> IO ()) -> IO ()
runFunc options args func =
    --  isOption :: ArgOption -> ArgName -> Boolean
    let isOption (Flag Nothing _ _) (Short _) = False
        isOption (Flag Nothing lhs _) (Long rhs) = lhs == rhs
        isOption (Flag (Just lhs) _ _) (Short rhs) = lhs == rhs
        isOption (Flag (Just _) lhs _) (Long rhs) = lhs == rhs
        isOption (Parameter Nothing _ _ _) (Short _) = False
        isOption (Parameter Nothing lhs _ _) (Long rhs) = lhs == rhs
        isOption (Parameter (Just lhs) _ _ _) (Short rhs) = lhs == rhs
        isOption (Parameter (Just _) lhs _ _) (Long rhs) = lhs == rhs
        isOption (ParameterList Nothing _ _) (Short _) = False
        isOption (ParameterList Nothing lhs _) (Long rhs) = lhs == rhs
        isOption (ParameterList (Just lhs) _ _) (Short rhs) = lhs == rhs
        isOption (ParameterList (Just _) lhs _) (Long rhs) = lhs == rhs

        optionExist [] _ = False
        optionExist (opt:opts) name
            | isOption opt name = True
            | otherwise = optionExist opts name

        isAst opt (AstFlag name _) = isOption opt name
        isAst opt (AstParameter name _) = isOption opt name
        isAst opt (AstParameterList name _) = isOption opt name

        validateAsts [] = Common.Ok ()
        validateAsts ((AstFlag name _):asts)
            | not (optionExist options name) = Common.Error ("Undefined parameter " ++ (show name) ++ ".")
            | otherwise = validateAsts asts
        validateAsts ((AstParameter name _):asts)
            | not (optionExist options name) = Common.Error ("Undefined parameter " ++ (show name) ++ ".")
            | otherwise = validateAsts asts

        validateAstToOption (Flag _ _ _) (AstFlag _ _) = Common.Ok ()
        validateAstToOption (Flag _ _ _) (AstParameter name _) = Common.Error ("The parameter " ++ (show name) ++ " cannot have a value.")
        validateAstToOption (Flag _ _ _) (AstParameterList name _) = Common.Error ("The parameter " ++ (show name) ++ " cannot have a value.")
        validateAstToOption (Parameter _ _ _ _) (AstFlag name _) = Common.Error ("The parameter " ++ (show name) ++ " must have a value.")
        validateAstToOption (Parameter _ _ _ _) (AstParameter _ _) = Common.Ok ()
        validateAstToOption (Parameter _ _ _ _) (AstParameterList name _) = Common.Error ("The parameter " ++ (show name) ++ " cannot have more than one value.")
        validateAstToOption (ParameterList _ _ _) (AstFlag name _) = Common.Error ("The parameter " ++ (show name) ++ " must have a value.")
        validateAstToOption (ParameterList _ _ _) (AstParameter _ _) = Common.Ok ()
        validateAstToOption (ParameterList _ _ _) (AstParameterList _ _) = Common.Ok ()

        validateAllAstsToOptions [] = Common.Ok ()
        validateAllAstsToOptions ((opt, ast):rest) = 
            do
                _ <- validateAstToOption opt ast
                validateAllAstsToOptions rest

        zipOptions [] _ = Common.Ok []
        zipOptions (opt:opts) asts =
            let findAsts [] = []
                findAsts (ast:asts')
                    | isAst opt ast = ast:(findAsts asts')
                    | otherwise = findAsts asts'
            in case (findAsts asts, opt) of
                    -- The argument was not found, and the option is mandatory
                    ([], Parameter _ name Nothing _) -> Common.Error ("Missing mandatory argument '" ++ name ++ "'.")
                    -- The argument was not found, but the option is optional, or a list
                    ([], Flag _ name _) ->
                        do
                            rest <- zipOptions opts asts
                            return ((opt, AstFlag (Long name) False):rest)
                    ([], Parameter _ name (Just x) _) ->
                        do
                            rest <- zipOptions opts asts
                            return ((opt, AstParameter (Long name) x):rest)
                    ([], ParameterList _ name _) ->
                        do
                            rest <- zipOptions opts asts
                            return ((opt, AstParameterList (Long name) []):rest)
                    -- The argument was found once
                    ([ast], Flag _ _ _) ->
                        do
                            rest <- zipOptions opts asts
                            return ((opt, ast):rest)
                    ([ast], Parameter _ _ _ _) ->
                        do
                            rest <- zipOptions opts asts
                            return ((opt, ast):rest)
                    -- The argument was found more than once
                    (asts', ParameterList _ name _) ->
                        let getValues [] = []
                            getValues ((AstParameter _ value):asts'') = value:(getValues asts'')
                        in
                            do
                                rest <- zipOptions opts asts
                                return ((opt, AstParameterList (Long name) (getValues asts')):rest)
                    (_, Flag _ name _) -> Common.Error ("The flag '" ++ name ++ "' cannot be specified more than once.")
                    (_, Parameter _ name _ _) -> Common.Error ("The parameter '" ++ name ++ "' cannot be specified more than once.")

        mapZipped [] = []
        mapZipped ((Flag _ name _, AstFlag _ value):rest) = (name, FlagValue value):(mapZipped rest)
        mapZipped ((Parameter _ name _ _, AstParameter _ value):rest) = (name, ParameterValue value):(mapZipped rest)
        mapZipped ((ParameterList _ name _, AstParameterList _ values):rest) = (name, ParameterListValue values):(mapZipped rest)

        runFunc' =
            do
                asts <- parse $ analyse args
                _ <- validateAsts asts
                zipped <- zipOptions options asts
                _ <- validateAllAstsToOptions zipped
                return $ mapZipped zipped
    in case runFunc' of
        Error msg -> hPutStrLn stderr (msg ++ " Use --help for more information.")
        Ok x -> func x
        -- Ok x -> putStrLn $ show x


process :: String -> String -> String -> [(String, String, [ArgOption], [(String, ArgValue)] -> IO ())] -> [String] -> IO ()
process name description _ specs [] = writeHelp name description specs
process name description _ specs ["--help"] = writeHelp name description specs
process _ _ _ _ ("--help":xs) = hPutStrLn stderr "Unsupported arguments specified. Use --help for more information."
process name _ version _ ["--version"] = hPutStrLn stderr (name ++ " " ++ version)
process _ _ _ _ ("--version":xs) = hPutStrLn stderr "Unsupported arguments specified. Use --help for more information."
process name _ _ specs (cmd:xs) =
    let process' [] = hPutStrLn stderr ("Unsupported command '" ++ cmd ++ "'. Use --help for more information.")
        process' ((arg, desc, options, func):args)
            | cmd == arg = case xs of
                ["--help"] -> writeCmdHelp name cmd desc options
                _ -> runFunc options xs func
            | otherwise = process' args
    in process' specs

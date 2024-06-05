import Control.Monad.Except  -- Import from this library
import Data.Char (isDigit, isSpace)
import Debug.Trace (trace)
import Control.Arrow (Arrow(second))
type InterpM = Except String -- used for finding errors, will return a String error type


-- Haskell doesn't have Symbol type :(

-- defining the ExprC data type
data ExprC
  = NumC Float    -- constructor for numeric literal
  | IdC String    -- Constructor for identifiers
  | StringC String -- Constructor for string literals
  | IfC ExprC ExprC ExprC
  | LambC [String] ExprC
  | AppC ExprC [ExprC]
  deriving (Show) -- need this to print out the 'ExprC' values


-- defining the Value data type
data Value
  = NumV Float
  | BoolV Bool
  | StringV String
  | CloV [String] ExprC [Binding]
  | OpV String
  deriving (Show)



-- Define Binding
data Binding = Binding String Value
    deriving (Show)

-- Define Environment
type Env = [Binding]


-- BNF is
-- <expr> ::= <num> | <id> | <string> | <if> | <lambda> | <app>
-- <num> ::= number
-- <id> ::= identifier
-- <string> ::= string
-- <if> ::= {if <expr> <expr> <expr>}
-- <lambda> ::= {lambda {<id>*} <expr>}
-- <app> ::= {<expr> <expr>*}

-- Converts {lamb {x y z} {{c 3} 4 5}} to [lamb, {x y z}, {{c 3} 4 5}]

data StringOrNumberOrBoolean
  = VString String
  | VNumber Int
  | VNest [StringOrNumberOrBoolean]
  deriving (Show, Eq)

parseAtom :: String -> StringOrNumberOrBoolean
parseAtom s = case reads s of
  [(n, "")] -> VNumber n
  _ -> VString s

-- Parses {lamb {x y z} {{c 3} 4 5}} into [lamb, [x y z] [[c 3] 4 5]]
parseBasic :: String -> [StringOrNumberOrBoolean]
parseBasic s =
  let arr = [] :: [StringOrNumberOrBoolean]
      s' = tail s  -- remove the { from the string
  in parseBasicHelper s' arr

parseBasicHelper :: String -> [StringOrNumberOrBoolean] -> [StringOrNumberOrBoolean]
parseBasicHelper [] arr = arr
parseBasicHelper s arr
  | head s == '{' =
      let (nested, remaining) = parseNested s
      in parseBasicHelper (skipWhitespace remaining) (arr ++ [VNest nested])
  | head s == ' ' = parseBasicHelper (tail s) arr
  | head s == '}' = arr
  | otherwise =
      let (word, rest) = break (\c -> c == ' ' || c == '}') s
      in parseBasicHelper (skipWhitespace rest) (arr ++ [parseAtom word])

-- Skip leading whitespace
skipWhitespace :: String -> String
skipWhitespace = dropWhile isSpace

-- Helper function to parse nested structures
parseNested :: String -> ([StringOrNumberOrBoolean], String)
parseNested s =
  let (nested, remaining) = extractNested 1 (tail s) []
  in (parseBasicHelper nested [], remaining)

extractNested :: Int -> String -> String -> (String, String)
-- Two bases cases
extractNested 0 remaining current = (reverse current, remaining)
-- Reversing because we have been adding to the front
extractNested _ [] acc = (reverse acc, [])
extractNested n (x:xs) acc
  -- Either accumulate one 
  | x == '{' = extractNested (n + 1) xs (x:acc)
  -- Or deaccumulate one
  | x == '}' = extractNested (n - 1) xs (x:acc)
  | otherwise = extractNested n xs (x:acc)

-- Now the real parsing
-- Take in a list of lists of StringOrNumberOrBoolean
-- Return an ExprC
parse :: [StringOrNumberOrBoolean] -> ExprC
parse [VNumber n] = NumC (fromIntegral n)
parse [VString s] = IdC s
parse [VNest exprs] = parseNestedExprs exprs
parse (VNest exprs : rest) = AppC (parseNestedExprs exprs) (map parseExpr rest)
parse exprs = error $ "Invalid expression: " ++ show exprs

parseNestedExprs :: [StringOrNumberOrBoolean] -> ExprC
parseNestedExprs [VString "if", cond, thenExpr, elseExpr] =
  IfC (parseExpr cond) (parseExpr thenExpr) (parseExpr elseExpr)
parseNestedExprs (VString "lamb" : VNest params : body : []) =
  let paramStrings = map (\(VString s) -> s) params
  in LambC paramStrings (parseExpr body)
parseNestedExprs (func : args) = AppC (parseExpr func) (map parseExpr args)
parseNestedExprs exprs = error $ "Invalid nested expression: " ++ show exprs

-- Helper function to parse individual expressions
parseExpr :: StringOrNumberOrBoolean -> ExprC
parseExpr (VNumber n) = NumC (fromIntegral n)
parseExpr (VString s) = IdC s
parseExpr (VNest exprs) = parseNestedExprs exprs
parseExpr _ = error "Unexpected expression"


topParse :: String -> ExprC
topParse s = parse $ [VNest (parseBasic s)]

----- END OF PARSING -----

-- Lookup from Environment
lookupEnv :: String -> Env -> InterpM Value
-- If the environment is empty, return an Error Statement
lookupEnv _ [] = throwError "Error: Unable to find in Environment"
-- Lookup takes in an id (x) and the environment
lookupEnv key (Binding name value : rest)
    -- If key matches the Binding name, return the value
    | key == name = return value
    -- Otherwise, recurse through lookupEnv with the rest of the environment
    | otherwise = lookupEnv key rest



extendEnv :: Env -> [String] -> [Value] -> InterpM Env
extendEnv env params args =
    -- If the length of the parameters are equal to the length of the arguments, call the helper
    if length params == length args
        then extendEnvHelper env params args
    else
        throwError "Error: Unable to Extend Environment"


-- Helper for Extend Environment
extendEnvHelper :: Env -> [String] -> [Value] -> InterpM Env
-- Base case of the recurssion
extendEnvHelper env [] [] = return env 

-- param:restParams gets the first element (param) from the list (restParams) 
-- bascialy {locals : x = 2 : y = 3 : z = 4 : body}
-- putting all params (i.e x,y,z ) into a list and vals (i.e 2,3,4) into another list
-- create a binding for each of them ( used '++' to append binding to list of bindings)
-- recurse to do the same for the remaining identifiers and values 
extendEnvHelper env (param:restParams) (val:restVals) = extendEnvHelper (env ++ [Binding param val]) restParams restVals


-- Custom equality function for Value
equalValues :: Value -> Value -> Bool
equalValues (NumV a) (NumV b) = a == b
equalValues (BoolV a) (BoolV b) = a == b
equalValues (StringV a) (StringV b) = a == b
equalValues _ _ = False


-- Interp Function
interp :: ExprC -> Env -> InterpM Value
interp (NumC n) _ = return (NumV n)  -- wrapping n in the NumV, 
interp (StringC s) _ = return (StringV s) -- wrapping s in the StringV
interp (IdC var) env = (lookupEnv var env)
interp (IfC exp1 exp2 exp3) env = do  -- do is basically like {seq}
    cond <- interp exp1 env
    case cond of
        BoolV True -> interp exp2 env
        BoolV False -> interp exp3 env
        _ -> throwError "Error: Condition in IfC must be a boolean"
interp (LambC params body) env = return (CloV params body env)
interp (AppC fun args) env = do
    func <- interp fun env
    -- mapM is just mapping 
    argVals <- mapM (`interp` env) args
    case func of
        CloV params body closureEnv -> do
            newEnv <- extendEnv closureEnv params argVals
            interp body newEnv
        OpV op -> applyPrim op argVals
        _ -> throwError "Error: Application of non-function"


-- Primitive operation helper
applyPrim :: String -> [Value] -> InterpM Value
applyPrim "+" [NumV a, NumV b] = return $ NumV (a + b)
applyPrim "-" [NumV a, NumV b] = return $ NumV (a - b)
applyPrim "*" [NumV a, NumV b] = return $ NumV (a * b)
applyPrim "/" [NumV a, NumV b]
  | b /= 0 = return $ NumV (a / b)
  | otherwise = throwError "Error: Division by zero"
applyPrim "<=" [NumV a, NumV b] = return $ BoolV (a <= b)
applyPrim "equal?" [a, b] = return $ BoolV (equalValues a b)
applyPrim "error" _ = throwError "Error: User-defined error"
applyPrim _ _ = throwError "Error: Unknown operator or incorrect arguments"



-- Serialize function to convert Value to String
serialize :: Value -> String
serialize (NumV n) = show n
serialize (StringV s) = show s
serialize (BoolV b) = show b
serialize (CloV _ _ _) = "#<procedure>"
serialize (OpV op) = "#<primitive " ++ op ++ ">"

-- Testing requires us to make our own checkEqual Function 

checkEqual :: (Eq a, Show a) => a -> a -> IO ()
checkEqual expected actual =
  if expected == actual
    then putStrLn $ "Test passed: " ++ show expected ++ " == " ++ show actual
    else putStrLn $ "Test failed: " ++ show expected ++ " != " ++ show actual

topInterp :: String -> InterpM Value
topInterp s = interp (topParse s) [
          Binding "true" (BoolV True),
          Binding "false" (BoolV False),
          Binding "+" (OpV "+"),
          Binding "-" (OpV "-"),
          Binding "*" (OpV "*"),
          Binding "/" (OpV "/"),
          Binding "<=" (OpV "<="),
          Binding "equal?" (OpV "equal?"),
          Binding "error" (OpV "error")
          ]

main = do
  let topEnv = [
          Binding "true" (BoolV True),
          Binding "false" (BoolV False),
          Binding "+" (OpV "+"),
          Binding "-" (OpV "-"),
          Binding "*" (OpV "*"),
          Binding "/" (OpV "/"),
          Binding "<=" (OpV "<="),
          Binding "equal?" (OpV "equal?"),
          Binding "error" (OpV "error"),
          Binding "x" (NumV 42.0),
          Binding "y" (BoolV True),
          Binding "z" (StringV "Test")
          ]
  let handleResult result =
          case result of
            Left err -> err
            Right val -> serialize val


  -- Evaluate expressions and check results
  -- runExcept can take in a returned result of either an error or a Value 
  -- then handleResult will match the result with a case
  -- if error, print error
  -- else serialize the Value :)  

  --------- TESTS ---------
  -- interp tests
  -- Create some ExprC objects
  let numExpr = NumC 3.14
  let idExpr = IdC "x"
  let stringExpr = StringC "hello"
  let ifExpr = IfC (IdC "y") (StringC "hello") (NumC 3.12)
  let lambExpr = LambC ["a", "b"] (AppC (IdC "+") [IdC "a", IdC "b"])
  let appExpr = AppC lambExpr [NumC 3, NumC 4]
  let ifExprFalse = IfC (IdC "false") (StringC "hello") (NumC 3.12)
  let nestedIfExpr = IfC (IdC "true") (NumC 2.0) (NumC 10.0)
  let nestedLambExpr = LambC ["a"] (LambC ["b"] (AppC (IdC "+") [IdC "a", IdC "b"]))
  let nestedAppExpr = AppC (AppC nestedLambExpr [NumC 2]) [NumC 3]
  let addingFourNumbers = AppC (LambC ["a", "b", "c", "d"] (AppC (IdC "+") [AppC (IdC "+") [IdC "a", IdC "b"], AppC (IdC "+") [IdC "c", IdC "d"]])) [NumC 1, NumC 2, NumC 3, NumC 4]

  let numResult = handleResult (runExcept (interp numExpr topEnv))
  let idResult = handleResult (runExcept (interp idExpr topEnv))
  let stringResult = handleResult (runExcept (interp stringExpr topEnv))
  let ifResult = handleResult (runExcept (interp ifExpr topEnv))
  let appResult = handleResult (runExcept (interp appExpr topEnv))
  let nestedIfResult = handleResult (runExcept (interp nestedIfExpr topEnv))
  let nestedAppResult = handleResult (runExcept (interp nestedAppExpr topEnv))

  let addingFourNumbersResult = handleResult (runExcept (interp addingFourNumbers topEnv))
  let errorResult = handleResult (runExcept (interp (AppC (IdC "doesntExist") [(NumC 1)]) topEnv))
  print errorResult
  -- Check equality of results
  checkEqual "3.14" numResult
  checkEqual "42.0" idResult
  checkEqual "\"hello\"" stringResult
  checkEqual "\"hello\"" ifResult
  checkEqual "7.0" appResult
  checkEqual "2.0" nestedIfResult
  checkEqual "5.0" nestedAppResult
  checkEqual "10.0" addingFourNumbersResult
  checkEqual "Error: Unable to find in Environment" errorResult


  -- extendEnv tests
  let handleEnvResult result =
          case result of
            Left err -> err
            Right env -> show env
  let env = []
  let env1 = [Binding "x" (NumV 1.0)]
  let env2 = [Binding "x" (NumV 1.0), Binding "y" (NumV 2.0)]
  let env3 = [Binding "x" (NumV 1.0), Binding "y" (NumV 2.0), Binding "z" (NumV 3.0)]
  let env4 = [Binding "x" (NumV 1.0), Binding "y" (NumV 2.0), Binding "z" (NumV 3.0), Binding "a" (NumV 4.0), Binding "b" (NumV 5.0)]
  let result1 = handleEnvResult (runExcept (extendEnv env ["x"] [NumV 1.0]))
  let result2 = handleEnvResult (runExcept (extendEnv env1 ["y"] [NumV 2.0]))
  let result3 = handleEnvResult (runExcept (extendEnv env2 ["z"] [NumV 3.0]))
  let result4 = handleEnvResult (runExcept (extendEnv env3 ["a", "b"] [NumV 4.0, NumV 5.0]))
  let errorResult1 = handleEnvResult (runExcept (extendEnv env ["x"] [NumV 1.0, NumV 2.0]))
  let errorResult2 = handleEnvResult (runExcept (extendEnv env ["x", "y"] [NumV 1.0]))
  checkEqual "[Binding \"x\" (NumV 1.0)]" result1
  checkEqual "[Binding \"x\" (NumV 1.0),Binding \"y\" (NumV 2.0)]" result2
  checkEqual "[Binding \"x\" (NumV 1.0),Binding \"y\" (NumV 2.0),Binding \"z\" (NumV 3.0)]" result3
  checkEqual "[Binding \"x\" (NumV 1.0),Binding \"y\" (NumV 2.0),Binding \"z\" (NumV 3.0),Binding \"a\" (NumV 4.0),Binding \"b\" (NumV 5.0)]" result4
  checkEqual "Error: Unable to Extend Environment" errorResult1
  checkEqual "Error: Unable to Extend Environment" errorResult2


  -- equalValues tests
  let numValue1 = NumV 1.0
  let numValue2 = NumV 1.0
  let numValue3 = NumV 2.0
  let string1 = StringV "hello"
  let string2 = StringV "hello"
  let string3 = StringV "world"
  let result1 = equalValues numValue1 numValue2
  let result2 = equalValues numValue1 numValue3
  let result3 = equalValues string1 string2
  let result4 = equalValues string1 string3
  let result5 = equalValues numValue1 string1
  checkEqual True result1
  checkEqual False result2
  checkEqual True result3
  checkEqual False result4
  checkEqual False result5

  -- applyPrim tests
  let addResult = handleResult (runExcept (applyPrim "+" [NumV 2.0, NumV 2.0]))
  let subResult = handleResult (runExcept (applyPrim "-" [NumV 1.0, NumV 2.0]))
  let mulResult = handleResult (runExcept (applyPrim "*" [NumV 1.0, NumV 2.0]))
  let divResult = handleResult (runExcept (applyPrim "/" [NumV 1.0, NumV 2.0]))
  let divErrorResult = handleResult (runExcept (applyPrim "/" [NumV 1.0, NumV 0.0]))
  let lessThanResult = handleResult (runExcept (applyPrim "<=" [NumV 1.0, NumV 2.0]))
  let equalResult = handleResult (runExcept (applyPrim "equal?" [NumV 1.0, NumV 2.0]))
  let errorResult = handleResult (runExcept (applyPrim "error" []))
  let unknownOpResult = handleResult (runExcept (applyPrim "unknown" []))
  checkEqual "4.0" addResult
  checkEqual "-1.0" subResult
  checkEqual "2.0" mulResult
  checkEqual "0.5" divResult
  checkEqual "Error: Division by zero" divErrorResult
  checkEqual "True" lessThanResult
  checkEqual "False" equalResult
  checkEqual "Error: User-defined error" errorResult
  checkEqual "Error: Unknown operator or incorrect arguments" unknownOpResult

  -- serialize tests
  let numValue = NumV 1.0
  let stringValue = StringV "wow I'm so cool"
  let boolValue = BoolV True
  let cloValue = CloV ["a", "b"] (AppC (IdC "+") [IdC "a", IdC "b"]) []
  let opValue = OpV "+"
  let numResult = serialize numValue
  let stringResult = serialize stringValue
  let boolResult = serialize boolValue
  let cloResult = serialize cloValue
  let opResult = serialize opValue
  checkEqual "1.0" numResult
  checkEqual "\"wow I'm so cool\"" stringResult
  checkEqual "True" boolResult
  checkEqual "#<procedure>" cloResult
  checkEqual "#<primitive +>" opResult


  -- parseAtom tests
  let stringAtom = parseAtom "hello"
  let numberAtom = parseAtom "42"
  let boolAtom = parseAtom "true"
  let errorAtom = parseAtom "error"
  checkEqual (VString "hello") stringAtom
  checkEqual (VNumber 42) numberAtom
  checkEqual (VString "error") errorAtom

  -- topInterp tests
  let basic
        = "{{lamb {a b c d} {+ {+ a b} {+ c d}}} 4 5 6 7}"
  let basicResult = handleResult (runExcept (topInterp basic))
  let ifTest = "{if true {+ 4 5} {- 4 5}}"
  let ifTestResult = handleResult (runExcept (topInterp ifTest))
  checkEqual basicResult "22.0"
  checkEqual ifTestResult "9.0"
  putStrLn "All tests above passed!"


 {-main = do
    -- Create some ExprC objects
    let numExpr = NumC 3.14
    let idExpr = IdC "x"
    let stringExpr = StringC "hello"
    let ifExpr = IfC (IdC "x") (StringC "hello") (NumC 3.12)
    let lambExpr = LambC ["a", "b", "c"] (IfC (IdC "x") (StringC "hello") (NumC 3.12))
    let appExpr = AppC (LambC ["a", "b", "c"] (IfC (IdC "x") (StringC "hello") (NumC 3.12))) [(IfC (IdC "x") (StringC "hello") (NumC 3.12)), (IdC "x")]

    -- Print the ExprC objects
    putStrLn $ "Number expression: " ++ show numExpr
    putStrLn $ "Identifier expression: " ++ show idExpr
    putStrLn $ "String expression: " ++ show stringExpr
    putStrLn $ "If expression: " ++ show ifExpr
    putStrLn $ "Lamb expression: " ++ show lambExpr
    putStrLn $ "App expression: " ++ show appExpr  -}











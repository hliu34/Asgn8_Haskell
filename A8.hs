import Control.Monad.Except  -- Import from this library
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
        BoolV True -> interp exp3 env
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


-- Testing requires us to make our own checkEqual Function 

checkEqual :: (Eq a, Show a) => a -> a -> IO ()
checkEqual expected actual =
  if expected == actual
    then putStrLn $ "Test passed: " ++ show expected ++ " == " ++ show actual
    else putStrLn $ "Test failed: " ++ show expected ++ " != " ++ show actual


main = do
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

  let numResult = handleResult (runExcept (interp numExpr topEnv))
  let idResult = handleResult (runExcept (interp idExpr topEnv))
  let stringResult = handleResult (runExcept (interp stringExpr topEnv))
  let ifResult = handleResult (runExcept (interp ifExpr topEnv))
  let appResult = handleResult (runExcept (interp appExpr topEnv))
  let nestedIfResult = handleResult (runExcept (interp nestedIfExpr topEnv))
  let nestedAppResult = handleResult (runExcept (interp nestedAppExpr topEnv))


  

  -- Check equality of results
  checkEqual "3.14" numResult
  checkEqual "42.0" idResult
  checkEqual "\"hello\"" stringResult
  checkEqual "\"hello\"" ifResult
  checkEqual "7.0" appResult
  checkEqual "2.0" nestedIfResult
  checkEqual "5.0" nestedAppResult




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











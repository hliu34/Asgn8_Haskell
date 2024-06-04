{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}

import Control.Monad.Except
import Data.Map (argSet)
type InterpM = Except String -- Type Alias used for finding errors, will return a String error message

-- Define the ExprC type
data ExprC
    = NumC Float
    | IdC String
    | StringC String
    | IfC ExprC ExprC ExprC
    | LambC [String] ExprC
    | AppC ExprC [ExprC]
    deriving (Show, Eq) -- allows you to print out the ExprC and compare if an ExprC is equal to another


-- Define Value Type
data Value
  = NumV Float
  | BoolV Bool
  | StringV String
  | CloV [String] [ExprC] [Binding]
  | OpV String
  deriving (Show, Eq)

-- Define Binding
data Binding = Binding String Value
    deriving (Show, Eq)

-- Define Environment as list of Bindings
type Env = [Binding]


-- Lookup from Environment, returns an Error Message or a Value
lookupEnv :: String -> Env -> InterpM Value
-- If the environment is empty, return an Error Statement. _ ignores the first parameter
lookupEnv _ [] = throwError "Error: Unable to find in Environment"
-- Lookup takes in an id (x) and the environment
lookupEnv key ((Binding name value) : rest)
    -- If key matches the Binding name, return the value
    | key == name = return value
    -- Otherwise, recurse through lookupEnv with the rest of the environment
    | otherwise = lookupEnv key rest



-- Extend Env takes in an evironment, a list of parameters, and a list of values, Returns an Error Message or Update Environment
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
-- param:restParams gets the first element (param) from the list (restParams)      **Same with (arg:restArgs) Then recurse
extendEnvHelper env (param:restParams) (arg:restArgs) = extendEnvHelper (env ++ [Binding param arg]) restParams restArgs


-- Test Cases
main :: IO ()
main = do
    -- Create some ExprC objects
    let numExpr = NumC 3.14
    let idExpr = IdC "x"
    let stringExpr = StringC "hello"
    let ifExpr = IfC (IdC "x") (StringC "hello") (NumC 3.12)
    let lambExpr = LambC ["a", "b", "c"] (IfC (IdC "x") (StringC "hello") (NumC 3.12))
    let appExpr = AppC (LambC ["a", "b", "c"] (IfC (IdC "x") (StringC "hello") (NumC 3.12))) [IfC (IdC "x") (StringC "hello") (NumC 3.12), IdC "x"]

    -- Print the ExprC objects
    putStrLn $ "Number expression: " ++ show numExpr
    putStrLn $ "Identifier expression: " ++ show idExpr
    putStrLn $ "String expression: " ++ show stringExpr
    putStrLn $ "If expression: " ++ show ifExpr
    putStrLn $ "Lamb expression: " ++ show lambExpr
    putStrLn $ "App expression: " ++ show appExpr

    -- temp evironment
    let tempEnv = [Binding "x" (NumV 42.0), Binding "y" (BoolV True), Binding "z" (StringV "Test")]

    -- Print the bindings in the temporary environment by calling mapM_. mapM maps each binding in tempEnv and printsi t
    mapM_ print tempEnv

    -- Test lookupEnv with the temporary environment
    case runExcept (lookupEnv "x" tempEnv) of
        Left err -> putStrLn $ "Error: " ++ err
        Right val -> putStrLn $ "Found value for 'x': " ++ show val

    case runExcept (lookupEnv "y" tempEnv) of
        Left err -> putStrLn $ "Error: " ++ err
        Right val -> putStrLn $ "Found value for 'y': " ++ show val

    case runExcept (lookupEnv "z" tempEnv) of
        Left err -> putStrLn $ "Error: " ++ err
        Right val -> putStrLn $ "Found value for 'z': " ++ show val

    case runExcept (lookupEnv "nonexistent" tempEnv) of
        Left err -> putStrLn $ "Error: " ++ err
        Right val -> putStrLn $ "Found value for 'nonexistent': " ++ show val

    -- Test cases for extendEnv
    putStrLn "\nTesting extendEnv:"

    -- Basic Test Case: Matching lengths of parameters and arguments
    let params1 = ["a", "b", "c"]
    let args1 = [NumV 1.0, BoolV False, StringV "test"]
    case runExcept (extendEnv tempEnv params1 args1) of
        Left err -> putStrLn $ "Error: " ++ err
        Right newEnv -> putStrLn $ "Extended environment: " ++ show newEnv

    -- Error Case: Mismatching lengths of parameters and arguments
    let params2 = ["a", "b"]
    let args2 = [NumV 1.0, BoolV False, StringV "test"]
    case runExcept (extendEnv tempEnv params2 args2) of
        Left err -> putStrLn $ "Error: " ++ err
        Right newEnv -> putStrLn $ "Extended environment: " ++ show newEnv

    -- Empty Lists: Both parameters and arguments are empty
    let params3 = []
    let args3 = []
    case runExcept (extendEnv tempEnv params3 args3) of
        Left err -> putStrLn $ "Error: " ++ err
        Right newEnv -> putStrLn $ "Extended environment: " ++ show newEnv

    -- Environment Update: Extending an existing environment with new bindings
    let params4 = ["a", "b"]
    let args4 = [NumV 100.0, BoolV True]
    case runExcept (extendEnv tempEnv params4 args4) of
        Left err -> putStrLn $ "Error: " ++ err
        Right newEnv -> putStrLn $ "Extended environment: " ++ show newEnv
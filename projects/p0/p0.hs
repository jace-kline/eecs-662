{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Testing
import System.Random
import Test.QuickCheck

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Modified by: Jace Kline
-- Date: Tue Sept 15 4:37 PM CDT 2020
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }
  
lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]
  
numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)
                     

term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.


-- Utility Functions

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe _        = error "Error: Attempt to unwrap value from Nothing constructor"

evalAEMaybeBinaryOp :: AE -> AE -> (Int -> Int -> Int) -> (Int -> Int -> Bool) -> Maybe Int
evalAEMaybeBinaryOp l r op p =
  let (l', r') = (evalAEMaybe l, evalAEMaybe r)
  in case (l', r') of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    _            ->
      let (l'', r'') = (fromMaybe l', fromMaybe r')
      in if p l'' r'' then Just $ l'' `op` r'' else Nothing


-- (>>=) :: Maybe a -> (a -> Maybe a) -> Maybe a
evalMBinaryOp :: AE -> AE -> (Int -> Int -> Int) -> (Int -> Int -> Bool) -> Maybe Int
evalMBinaryOp l r op p = do
  l' <- evalM l
  r' <- evalM r
  if p l' r' then return $ l' `op` r' else Nothing


-- Evaluator implementations

evalAE :: AE -> Int
evalAE (Num x) = if x < 0 then error "Error: Negative Number Primitive" else x
evalAE (Plus l r) = evalAE l + evalAE r
evalAE (Minus l r) = 
  let exp = evalAE l - evalAE r
  in if exp > 0 then exp else error "Error: Negative Number obtained on subtraction"
evalAE (Mult l r) = evalAE l * evalAE r
evalAE (Div l r) =
  let
    r' = evalAE r 
    exp = evalAE l `div` r'
  in if r' == 0 then error "Error: Divide by 0" else exp
evalAE (If0 c e1 e2) = if evalAE c == 0 then evalAE e1 else evalAE e2


evalAEMaybe :: AE -> Maybe Int
evalAEMaybe (Num x) = if x < 0 then Nothing else Just x
evalAEMaybe (Plus l r) = evalAEMaybeBinaryOp l r (+) (\l' r' -> True)
evalAEMaybe (Minus l r) = evalAEMaybeBinaryOp l r (-) (\l' r' -> l' >= r')
evalAEMaybe (Mult l r) = evalAEMaybeBinaryOp l r (*) (\l' r' -> True)
evalAEMaybe (Div l r) = evalAEMaybeBinaryOp l r div (\l' r' -> r' /= 0)
evalAEMaybe (If0 c l r) =
  case evalAEMaybe c of
    Nothing -> Nothing
    (Just c') -> if c' == 0 then evalAEMaybe l else evalAEMaybe r


evalM :: AE -> Maybe Int
evalM (Num x) = if x < 0 then Nothing else Just x
evalM (Plus l r) = evalMBinaryOp l r (+) (\l' r' -> True)
evalM (Minus l r) = evalMBinaryOp l r (-) (\l' r' -> l' >= r')
evalM (Mult l r) = evalMBinaryOp l r (*) (\l' r' -> True)
evalM (Div l r) = evalMBinaryOp l r div (\l' r' -> r' /= 0)
evalM (If0 c l r) = do
  c' <- evalM c
  if c' == 0 then evalM l else evalM r

interpAE :: String -> Maybe Int
interpAE = evalM . parseAE



{- TESTING SECTION:

Run the following command to generate test results:
`runhaskell p0.hs > test-results.txt`
Test output will be piped to the 'test-results.txt' file

-}

main :: IO ()
main = do
    testEvalValid 50
    testEvalInvalid

testEvalValid :: Int -> IO ()
testEvalValid n = do
    pairs <- genAETests n
    let lifted_pairs = map (\(s,v) -> (s, liftValid v)) pairs
    putStrLn "Generating random valid AE test expressions..."
    putStrLn "Testing evalAE (1st implementation) under valid cases:"
    sequence_ $ map (testEvalProp prop_evalAE) pairs
    putStrLn "Testing evalAEMaybe (2nd implementation) under valid cases:"
    sequence_ $ map (testEvalProp prop_evalAEMaybe) lifted_pairs
    putStrLn "Testing evalM (3rd implementation) under valid cases:"
    sequence_ $ map (testEvalProp prop_evalM) lifted_pairs

testEvalInvalid :: IO ()
testEvalInvalid = do
    putStrLn "Testing invalid AE expressions..."
    putStrLn "Testing evalAEMaybe (2nd implementation) under invalid cases:"
    sequence_ $ map (testEvalProp prop_evalAEMaybe) bad_cases
    putStrLn "Testing evalM (3rd implementation) under invalid cases:"
    sequence_ $ map (testEvalProp prop_evalM) bad_cases

prop_evalAE s n = (evalAE . parseAE) s === n
prop_evalAEMaybe s maybe_n = (evalAEMaybe . parseAE) s === maybe_n
prop_evalM s maybe_n = interpAE s === maybe_n

liftValid n = if n < 0 then Nothing else Just n

testEvalProp p (s,v) = putStrLn ("AE expression: " ++ s) >> (verboseCheckWith stdArgs { maxSize = 1 , maxSuccess = 1 }) (p s v)

bad_cases = zip [
    "2 - 3"
    , "16 - 22"
    , "100 - 1000"
    , "5 / 0"
    , "20 / 0"
    , "100 / 0" ] (repeat (Nothing :: Maybe Int))

mkValidInt :: (RandomGen g) => g -> (Int -> Bool) -> (Int, g)
mkValidInt g constraint =
    let (x, g') = randomR (0,1000) g
    in if constraint x then (x, g') else mkValidInt g' constraint

-- should return True roughly one in n times
shouldTest :: (RandomGen g) => g -> Int -> (Bool, g)
shouldTest g n =
    let (x, g') = randomR (1, n) g
    in (x == 1, g)

-- choose if0 expression once every 5 times
shouldTestIf0 :: (RandomGen g) => g -> (Bool, g)
shouldTestIf0 g = shouldTest g 5

-- expand arith expression once every 2 times
shouldExpandArith :: (RandomGen g) => g -> (Bool, g)
shouldExpandArith g = shouldTest g 2

genAETests :: Int -> IO [(String, Int)]
genAETests n = do
    g <- getStdGen
    return $ mkAETests g n

mkAETests :: (RandomGen g) => g -> Int -> [(String, Int)]
mkAETests = go where
    go g 0 = []
    go g n =
        let (pair, g') = mkAETest g
        in pair : go g' (n-1)

mkAETest :: (RandomGen g) => g -> ((String, Int), g)
mkAETest g = 
    let (c, g') = shouldTestIf0 g
    in (if c then mkIf0Expr else mkArithExpr) g

mkIf0Expr :: (RandomGen g) => g -> ((String, Int), g)
mkIf0Expr g =
    let (g', g_) = split g
        (g'', g''') = split g_
        ((sC, c), g0) = mkAETest g'
        ((s1, v1), g1) = mkAETest g''
        ((s2, v2), g2) = mkAETest g'''
        s_ret = "if0 " ++ sC ++ " then " ++ s1 ++ " else " ++ s2
        val_ret = if c == 0 then v1 else v2
    in ((s_ret, val_ret), g0)

mkArithExpr :: (RandomGen g) => g -> ((String, Int), g)
mkArithExpr g =
    let (b, g') = shouldExpandArith g
    in if not b then mkPrimInt g' positive else mkRecInt g'

mkPrimInt :: (RandomGen g) => g -> (Int -> Bool) -> ((String, Int), g)
mkPrimInt g constraint =
    let (v, g') = mkValidInt g constraint
    in ((show v, v), g')

mkRecInt :: (RandomGen g) => g -> ((String, Int), g)
mkRecInt g =
    let ((s1, v1), g1) = mkPrimInt g positive
        ((s_op, op), g2) = mkOp g1
        ((s2, v2), g3) = mkPrimInt g2 (opConstraint s_op v1)
    in ((s1 ++ s_op ++ s2, v1 `op` v2), g3)

mkOp :: (RandomGen g) => g -> ((String, (Int -> Int -> Int)), g)
mkOp g =
    let (v, g') = randomR (0, 3) g
    in (zip oplist $ repeat g') !! v

opConstraint :: String -> Int -> Int -> Bool
opConstraint " + " l r = positive r
opConstraint " - " l r = l >= r
opConstraint " * " l r = positive r
opConstraint " / " l r = positive r
opConstraint _ _ _ = error "Invalid operator given"

positive :: Int -> Bool
positive x = x > 0

oplist = [(" + ", (+)), (" - ", (-)), (" * ", (*)), (" / ", div)]
{-# LANGUAGE GADTs, FlexibleContexts #-}

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

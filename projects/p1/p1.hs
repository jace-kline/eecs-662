{-# LANGUAGE GADTs,FlexibleContexts #-}
module Main where
-- Imports for Monads

import Control.Monad


-- Imports for Testing

import Test.QuickCheck
import Data.Char
import Data.List

-- BBAE AST and Type Definitions

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving (Show,Eq)

type Env = [(String,BBAE)]

type Cont = [(String,TBBAE)]

evalS :: BBAE -> (Maybe BBAE)
evalS (Num x) = return $ Num x
evalS (Plus l r) = do
  (Num l') <- evalS l
  (Num r') <- evalS r
  return $ Num $ l' + r'
evalS (Minus l r) = do
  (Num l') <- evalS l
  (Num r') <- evalS r
  let v = l' - r'
  if v < 0 then Nothing else return $ Num v
evalS (Bind id bexp exp) = evalS bexp >>= \v -> evalS $ subst id v exp
evalS (Id s) = Nothing
evalS (Boolean b) = return $ Boolean b
evalS (And l r) = do
  (Boolean l') <- evalS l
  (Boolean r') <- evalS r
  return $ Boolean $ l' && r'
evalS (Leq l r) = do
  (Num l') <- evalS l
  (Num r') <- evalS r
  return $ Boolean $ l' <= r'
evalS (IsZero exp) = evalS exp >>= \(Num x) -> return $ Boolean (x == 0)
evalS (If c l r) = evalS c >>= \(Boolean b) -> if b then evalS l else evalS r

subst :: String -> BBAE -> BBAE -> BBAE
subst id v exp = case exp of
  (Num x) -> Num x
  (Plus l r) -> binarySubst Plus l r
  (Minus l r) -> binarySubst Minus l r
  (Bind id' bexp' exp') -> Bind id' (f bexp') (if id' == id then exp' else f exp')
  (Id s) -> if s == id then v else (Id s)
  (Boolean b) -> Boolean b
  (And l r) -> binarySubst And l r
  (Leq l r) -> binarySubst Leq l r
  (IsZero exp') -> IsZero $ f exp'
  (If c l r) -> If (f c) (f l) (f r)
  where
    f :: BBAE -> BBAE
    f = subst id v
    binarySubst :: (BBAE -> BBAE -> BBAE) -> BBAE -> BBAE -> BBAE
    binarySubst constructor l r = constructor (f l) (f r)

evalM :: Env -> BBAE -> (Maybe BBAE)
evalM _ (Num x) = return $ Num x
evalM env (Plus l r) = do
  (Num l') <- evalM env l
  (Num r') <- evalM env r
  return $ Num $ l' + r'
evalM env (Minus l r) = do
  (Num l') <- evalM env l
  (Num r') <- evalM env r
  let v = l' - r'
  if v < 0 then Nothing else return $ Num v
evalM env (Bind id bexp exp) = evalM env bexp >>= \v -> evalM ((id,v):env) exp
evalM env (Id s) = lookup s env
evalM _ (Boolean b) = return $ Boolean b
evalM env (And l r) = do
  (Boolean l') <- evalM env l
  (Boolean r') <- evalM env r
  return $ Boolean $ l' && r'
evalM env (Leq l r) = do
  (Num l') <- evalM env l
  (Num r') <- evalM env r
  return $ Boolean $ l' <= r'
evalM env (IsZero exp) = evalM env exp >>= \(Num x) -> return $ Boolean (x == 0)
evalM env (If c l r) = evalM env c >>= \(Boolean b) -> if b then evalM env l else evalM env r

testBBAE :: BBAE -> Bool
testBBAE e = evalS e == evalM [] e

typeofM :: Cont -> BBAE -> (Maybe TBBAE)
typeofM cont exp = case exp of
  (Num _) -> return TNum
  (Plus l r) -> binary l r TNum TNum TNum
  (Minus l r) -> binary l r TNum TNum TNum
  (Bind id be e) -> do
    t <- typeofM cont be
    typeofM ((id,t):cont) e
  (Id s) -> lookup s cont
  (Boolean _) -> return TBool
  (And l r) -> binary l r TBool TBool TBool
  (Leq l r) -> binary l r TNum TNum TBool
  (IsZero e) -> typeofM cont e >>= \TNum -> return TBool
  (If c l r) -> do
    TBool <- typeofM cont c
    tL <- typeofM cont l
    tR <- typeofM cont r
    if tL == tR then return tL else Nothing
  where
    binary :: BBAE -> BBAE -> TBBAE -> TBBAE -> TBBAE -> Maybe TBBAE
    binary l r ltype rtype rettype = do
      l' <- typeofM cont l
      if l' == ltype then (do
        r' <- typeofM cont r
        if r' == rtype then return rettype else Nothing)
      else Nothing

evalT :: BBAE -> (Maybe BBAE)
evalT exp = typeofM [] exp >> evalM [] exp


-- TESTING SECTION --

-- run this function to perform tests
main :: IO ()
main = do
    putStrLn "Testing Evaluators:"
    verboseCheck prop_testBBAE
    putStrLn "Testing Type Checker:"
    verboseCheck prop_testTBBAE

-- test that the type checker outputs proper results depending on the expression evaluation
prop_testTBBAE :: BBAE -> Property
prop_testTBBAE e = collect resultStr p
    where
        t = typeofM [] e
        e_val = evalM [] e
        p = case e_val of
            Just (Num a) -> t == Just TNum
            Just (Boolean b) -> t == Just TBool
            _ -> True
        strEVal = "EvalM: " ++ show e_val
        strType = "Type: " ++ show t
        resultStr = strEVal ++ " <--> " ++ strType

-- test that all 3 evaluators output the same results
prop_testBBAE :: BBAE -> Property
prop_testBBAE e = collect resultStr $ all id [evalS_val == evalM_val, evalS_val == evalT_val, evalM_val == evalT_val]
    where
        evalS_val = evalS e
        evalM_val = evalM [] e
        evalT_val = evalT e
        strS = "evalS: " ++ show evalS_val
        strM = "evalM: " ++ show evalM_val
        strT = "evalT: " ++ show evalT_val
        resultStr = strS ++ " <--> " ++ strM ++ " <--> " ++ strT

type Length = Int

numConstructors :: Int
numConstructors = 10

instance Arbitrary TBBAE where
    arbitrary = do
        b <- (arbitrary :: Gen Bool)
        return $ if b then TNum else TBool

arbitraryNum :: Gen BBAE
arbitraryNum = (arbitrary :: Gen Int) >>= \x -> if x >= 0 then return (Num x) else arbitraryNum

arbitraryBoolean :: Gen BBAE
arbitraryBoolean = (arbitrary :: Gen Bool) >>= \b -> return (Boolean b)

-- generate a one letter lowercase ID
arbitraryId :: Gen String
arbitraryId = do
    k <- choose (97,122)
    return $ [chr k]

positive :: Int -> Int
positive n = if n < 0 then (-1) * n else n

-- This allows us to generate random objects of this type
-- Sometimes produces invalid semantic constructs that should evaluate to Nothing
instance Arbitrary BBAE where
    arbitrary = sized $ \n -> do
        b <- (arbitrary :: Gen Bool)
        genExp [] (if b then TNum else TBool) (positive n)

genExp :: Cont -> TBBAE -> Int -> Gen BBAE
genExp cont t n = case n of 
    0 -> do
        b <- (arbitrary :: Gen Bool)
        if b 
            then genPrimitive 
            else genId
    _ -> do
        k <- choose (1, numConstructors)
        case k of
            1 -> genBind
            2 -> genId
            3 -> genIf
            _ -> do
                k' <- (choose (1, if t == TNum then 3 else 4) :: Gen Int)
                case t of
                    TNum -> case k' of
                        1 -> arbitraryNum
                        2 -> genPlus
                        3 -> genMinus
                        _ -> error "error"
                    TBool -> case k' of
                        1 -> arbitraryBoolean
                        2 -> genAnd
                        3 -> genLeq
                        4 -> genIsZero
                        _ -> error "error"
    where
        recurse cont t = do
            k <- (choose (1,5) :: Gen Int)
            genExp cont t (n `div` k)
        genPrimitive = case t of
            TBool -> arbitraryBoolean
            TNum -> arbitraryNum
        genBind = do
            id <- arbitraryId
            t' <- (arbitrary :: Gen TBBAE)
            bexp <- recurse cont t'
            exp <- recurse ((id,t'):cont) t
            return $ Bind id bexp exp
        genId =
            let cont' = filter (\(_,t') -> t' == t) $ nubBy (\x y -> fst x == fst y) cont
                l = length cont'
            in if l == 0 
                then recurse cont t
                else do
                    i <- choose (0, l - 1)
                    let (id,_) = cont' !! i
                    return $ Id id
        genIf = do
            c <- recurse cont TBool
            l <- recurse cont t
            r <- recurse cont t
            return $ If c l r
        genPlus = do
            l <- recurse cont TNum
            r <- recurse cont TNum
            return $ Plus l r
        genMinus = do
            l <- recurse cont TNum
            r <- recurse cont TNum
            return $ Minus l r
        genAnd = do
            l <- recurse cont TBool
            r <- recurse cont TBool
            return $ And l r
        genLeq = do
            l <- recurse cont TNum
            r <- recurse cont TNum
            return $ Leq l r
        genIsZero = do
            e <- recurse cont TNum
            return $ IsZero e

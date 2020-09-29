{-# LANGUAGE GADTs,FlexibleContexts #-}
module P1 where
-- Imports for Monads

import Control.Monad

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
  (Bind id be e) -> typeofM cont be >>= \t -> typeofM ((id,t):cont) exp
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

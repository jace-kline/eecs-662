{-# LANGUAGE GADTs #-}

-- Imports for Monads

import Control.Monad

-- FAE AST and Type Definitions

data FAE where
  Num :: Int -> FAE
  Plus :: FAE -> FAE -> FAE
  Minus :: FAE -> FAE -> FAE
  Lambda :: String -> FAE -> FAE
  App :: FAE -> FAE -> FAE
  Id :: String -> FAE
  deriving (Show,Eq)

type Env = [(String,FAE)]

-- I'm using an alias 'f' for implementation because 'evalDynFAE' is quite verbose
evalDynFAE :: Env -> FAE -> (Maybe FAE)
evalDynFAE = f

f :: Env -> FAE -> (Maybe FAE)
f _ e@(Num n) = return e
f env (Plus l r) = f env l >>= \(Num l') -> f env r >>= \(Num r') -> return $ Num (l' + r')
f env (Minus l r) = f env l >>= \(Num l') -> f env r >>= \(Num r') -> if r' > l' then Nothing else return $ Num (l' - r')
f env e@(Lambda i b) = return e
f env (App (Lambda i b) a) = f ((i,a):env) b
f env (Id i) = lookup i env
f _ _ = Nothing


data FAEValue where
  NumV :: Int -> FAEValue
  ClosureV :: String -> FAE -> Env' -> FAEValue
  deriving (Show,Eq)
  
type Env' = [(String,FAEValue)]

evalStatFAE :: Env' -> FAE -> (Maybe FAEValue)
evalStatFAE = f'

f' :: Env' -> FAE -> (Maybe FAEValue)
f' _ (Num n) = return $ NumV n
f' env' (Plus l r) = f' env' l >>= \(NumV l') -> f' env' r >>= \(NumV r') -> return $ NumV (l' + r')
f' env' (Minus l r) = f' env' l >>= \(NumV l') -> f' env' r >>= \(NumV r') -> if r' > l' then Nothing else return $ NumV (l' - r')
f' env' (Lambda i b) = return $ ClosureV i b env'
f' env' (App l r) = do
  (ClosureV i b env_closure) <- f' env' l
  r' <- f' env' r
  f' ((i,r'):env') b
f' env' (Id i) = lookup i env'

-- FBAE AST and Type Definitions

data FBAE where
  NumD :: Int -> FBAE
  PlusD :: FBAE -> FBAE -> FBAE
  MinusD :: FBAE -> FBAE -> FBAE
  LambdaD :: String -> FBAE -> FBAE
  AppD :: FBAE -> FBAE -> FBAE
  BindD :: String -> FBAE -> FBAE -> FBAE
  IdD :: String -> FBAE
  deriving (Show,Eq)

elabFBAE :: FBAE -> FAE
elabFBAE (NumD n) = Num n
elabFBAE (PlusD l r) = Plus (elabFBAE l) (elabFBAE r)
elabFBAE (MinusD l r) = Minus (elabFBAE l) (elabFBAE r)
elabFBAE (LambdaD i b) = Lambda i (elabFBAE b)
elabFBAE (AppD l r) = App (elabFBAE l) (elabFBAE r)
elabFBAE (BindD i a b) = App (Lambda i (elabFBAE a)) (elabFBAE b)
elabFBAE (IdD i) = Id i

evalFBAE :: Env' -> FBAE -> (Maybe FAEValue)
evalFBAE env' = evalStatFAE env' . elabFBAE

-- FBAEC AST and Type Definitions

data FBAEC where
  NumE :: Int -> FBAEC
  PlusE :: FBAEC -> FBAEC -> FBAEC
  MinusE :: FBAEC -> FBAEC -> FBAEC
  TrueE :: FBAEC
  FalseE :: FBAEC
  AndE :: FBAEC -> FBAEC -> FBAEC
  OrE :: FBAEC -> FBAEC -> FBAEC
  NotE :: FBAEC -> FBAEC
  IfE :: FBAEC -> FBAEC -> FBAEC -> FBAEC
  LambdaE :: String -> FBAEC -> FBAEC
  AppE :: FBAEC -> FBAEC -> FBAEC
  BindE :: String -> FBAEC -> FBAEC -> FBAEC
  IdE :: String -> FBAEC
  deriving (Show,Eq)

elabFBAEC :: FBAEC -> FAE
elabFBAEC (NumE n) = Num n
elabFBAEC (PlusE l r) = Plus (elabFBAEC l) (elabFBAEC r)
elabFBAEC (MinusE l r) = Minus (elabFBAEC l) (elabFBAEC r)
elabFBAEC TrueE = Lambda "t" (Lambda "f" (Id "t"))
elabFBAEC FalseE = Lambda "t" (Lambda "f" (Id "f"))
elabFBAEC (AndE l r) = App (App (elabFBAEC l) (elabFBAEC r)) (elabFBAEC FalseE)
elabFBAEC (OrE l r) = App (App (elabFBAEC l) (elabFBAEC TrueE)) (elabFBAEC r)
elabFBAEC (NotE b) = App (App (elabFBAEC b) (elabFBAEC FalseE)) (elabFBAEC TrueE)
elabFBAEC (IfE c l r) = App (App (elabFBAEC c) (elabFBAEC l)) (elabFBAEC r)
elabFBAEC (LambdaE i b) = Lambda i (elabFBAEC b)
elabFBAEC (AppE l r) = App (elabFBAEC l) (elabFBAEC r)
elabFBAEC (BindE i a b) = App (Lambda i (elabFBAEC a)) (elabFBAEC b)
elabFBAEC (IdE i) = Id i

evalFBAEC :: Env' -> FBAEC -> Maybe FAEValue
evalFBAEC env' = evalStatFAE env' . elabFBAEC

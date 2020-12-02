{-# LANGUAGE GADTs #-}

-- import Control.Monad

-- Calculator language extended with an environment to hold defined variables

data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  Fix :: FBAE -> FBAE
  FixClosure :: String -> FBAE -> Env -> FBAE -- can only be constructed when injecting fix expression
  deriving (Show,Eq)

-- Value defintion for statically scoped eval

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)

-- Enviornment for statically scoped eval

type Env = [(String,FBAEVal)]

-- Statically scoped eval
         
evalM :: Env -> FBAE -> (Maybe FBAEVal)
evalM _ (Num n) = return $ NumV n
evalM env (Plus l r) = evalBinArith env l r (+) (\x y -> True)
evalM env (Minus l r) = evalBinArith env l r (-) (\x y -> x >= y)
evalM env (Mult l r) = evalBinArith env l r (*) (\x y -> True)
evalM env (Div l r) = evalBinArith env l r div (\x y -> y /= 0)
evalM env (Bind id x b) = evalM env x >>= \x' -> evalM ((id,x'):env) b
evalM env (Lambda id t b) = return $ ClosureV id b env
evalM env (App f x) = do
  (ClosureV id b env') <- evalM env f
  x' <- evalM env x
  evalM ((id,x'):env') b
evalM env (Id id) = lookup id env
evalM _ (Boolean b) = return $ BooleanV b
evalM env (And l r) = evalBinLogic env l r (&&)
evalM env (Or l r) = evalBinLogic env l r (||)
evalM env (Leq l r) = do
  (NumV l') <- evalM env l
  (NumV r') <- evalM env r
  return $ BooleanV  (l' <= r')
evalM env (IsZero x) = evalM env x >>= \(NumV x') -> return $ BooleanV (x' == 0)
evalM env (If c l r) = do
  (BooleanV c') <- evalM env c
  evalM env (if c' then l else r)
evalM env (Fix f) = do
  (ClosureV g b env') <- evalM env f
  evalM env' $ subst g (Fix (FixClosure g b env')) b
evalM env (FixClosure id b env') = return $ ClosureV id b env'

subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num n) = Num n
subst id e (Plus l r) = Plus (subst id e l) (subst id e r)
subst id e (Minus l r) = Minus (subst id e l) (subst id e r)
subst id e (Mult l r) = Mult (subst id e l) (subst id e r)
subst id e (Div l r) = Div (subst id e l) (subst id e r)
subst id e exp@(Bind id' x b) = if id' == id then exp else (Bind id' (subst id e x) (subst id e b))
subst id e exp@(Lambda id' t b) = if id' == id then exp else (Lambda id' t (subst id e b))
subst id e (App f x) = App (subst id e f) (subst id e x)
subst id e (Id id') = if id' == id then e else (Id id')
subst _ _ (Boolean b) = Boolean b
subst id e (And l r) = And (subst id e l) (subst id e r)
subst id e (Or l r) = Or (subst id e l) (subst id e r)
subst id e (Leq l r) = Leq (subst id e l) (subst id e r)
subst id e (IsZero exp) = IsZero (subst id e exp)
subst id e (If c l r) = If (subst id e c) (subst id e l) (subst id e r)
subst id e (Fix f) = Fix (subst id e f)
subst id e exp@(FixClosure id' b env') = if id' == id then exp else (FixClosure id' (subst id e b) env')

evalBinArith :: Env -> FBAE -> FBAE -> (Int -> Int -> Int) -> (Int -> Int -> Bool) -> Maybe FBAEVal
evalBinArith env l r op cond = do
  (NumV l') <- evalM env l
  (NumV r') <- evalM env r
  if (cond l' r') 
    then return (NumV (op l' r'))
    else Nothing

evalBinLogic :: Env -> FBAE -> FBAE -> (Bool -> Bool -> Bool) -> Maybe FBAEVal
evalBinLogic env l r op = do
  (BooleanV l') <- evalM env l
  (BooleanV r') <- evalM env r
  return $ BooleanV (op l' r')

-- Type inference function

type Cont = [(String,TFBAE)]

typeofM :: Cont -> FBAE -> (Maybe TFBAE)
typeofM _ _ = Nothing


-- Interpreter

interp :: FBAE -> (Maybe FBAEVal)
interp _ = Nothing

-- Factorial function for testing evalM and typeofM.  the type of test1 should
-- be TNum and the result of evaluating test1`should be (NumV 6).  Remember
-- that Just is used to return both the value and type.

test1 = (Bind "f" (Lambda "g" ((:->:) TNum TNum)
                    (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1)
                                         (Mult (Id "x")
                                               (App (Id "g")
                                                    (Minus (Id "x")
                                                           (Num 1)))))))
         (App (Fix (Id "f")) (Num 3)))

test2 = (Bind "inc" (Lambda "x" TNum (Plus (Id "x") (Num 1))) (App (Id "inc") (Num 5)))
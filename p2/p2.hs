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
f env (Plus l r) = do
  (Num l') <- f env l
  (Num r') <- f env r
  return $ Num (l' + r')
f env (Minus l r) = do
  (Num l') <- f env l
  (Num r') <- f env r
  guard (l' >= r')
  return $ Num (l' - r')
f env e@(Lambda i b) = return e
f env (App l a) = do
  (Lambda i b) <- f env l
  f ((i,a):env) b
f env (Id i) = lookup i env


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
  f' ((i,r'):env_closure) b
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
elabFBAE (BindD i a b) = App (Lambda i (elabFBAE b)) (elabFBAE a)
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
elabFBAEC (BindE i a b) = App (Lambda i (elabFBAEC b)) (elabFBAEC a)
elabFBAEC (IdE i) = Id i

evalFBAEC :: Env' -> FBAEC -> Maybe FAEValue
evalFBAEC env' = evalStatFAE env' . elabFBAEC

----------------------TESTING ----------------------

printTestList :: (Show a, Show b) => String -> (a -> b) -> [a] -> IO ()
printTestList s interp ts = do
  putStrLn $ "\n---------------" ++ s ++ "----------------\n"
  sequence_ $ map showAction ts
  where 
    showAction t = do
      print t
      putStrLn "=="
      print $ interp t
      putStr "\n"

main :: IO ()
main = do
  printTestList "FAE Dynamic" (evalDynFAE []) tests
  printTestList "FAE Static" (evalStatFAE []) tests
  printTestList "FBAE Static" (evalFBAE []) testsX
  printTestList "FBAEC Static" (evalFBAEC []) testsXX
  printTestList "FBAEC Church Booleans (Truth Table Combos)" (evalFBAEC []) boolTests
  printTestList "FBAEC If Expression" (evalFBAEC []) ifTests

-- Tests for evalDynFAE and evalStatFAE.  test2 should demonstrate
-- the difference between static and dynamic scoping.  If you get the same
-- results with both interpreters, you've got problems.

test0=(App (Lambda "inc" (Id "inc")) (Lambda "x" (Plus (Id "x") (Num 1))))
test1=(App (Lambda "inc" (App (Id "inc") (Num 3))) (Lambda "x" (Plus (Id "x") (Num 1))))
test2=(App (Lambda "n" (App (Lambda "inc" (App (Lambda "n" (App (Id "inc") (Num 3))) (Num 3))) (Lambda "x" (Plus (Id "x") (Id "n"))))) (Num 1))

-- List of tests if you would like to use map for testing

tests = [test0,test1,test2]

-- Tests for evalFBAE.  These are the same tests as above
-- using Bind.

test0X= (BindD "inc" (LambdaD "x" (PlusD (IdD "x") (NumD 1))) (IdD "inc"))
test1X = (BindD "inc" (LambdaD "x" (PlusD (IdD "x") (NumD 1))) (AppD (IdD "inc") (NumD 3)))
test2X = (BindD "n" (NumD 1) (BindD "inc" (LambdaD "x" (PlusD (IdD "x") (IdD "n"))) (BindD "n" (NumD 3) (AppD (IdD "inc") (NumD 3)))))

-- List of tests if you would like to use map for testing

testsX = [test0X,test1X,test2X]


-- Tests for evalFBAEC.  These are the same tests as above
-- using the AST for FBAEC.

test0XX= (BindE "inc" (LambdaE "x" (PlusE (IdE "x") (NumE 1))) (IdE "inc"))
test1XX = (BindE "inc" (LambdaE "x" (PlusE (IdE "x") (NumE 1))) (AppE (IdE "inc") (NumE 3)))
test2XX = (BindE "n" (NumE 1) (BindE "inc" (LambdaE "x" (PlusE (IdE "x") (IdE "n"))) (BindE "n" (NumE 3) (AppE (IdE "inc") (NumE 3)))))

-- List of tests if you would like to use map for testing

testsXX = [test0XX,test1XX,test2XX]

boolTests :: [FBAEC]
boolTests = do
  op <- [AndE, OrE]
  l <- [TrueE, FalseE]
  r <- [TrueE, FalseE]
  return $ op l r

ifTests :: [FBAEC]
ifTests = [IfE TrueE TrueE FalseE, IfE FalseE TrueE FalseE]
module Tests where

import System.Random
import Test.QuickCheck
import P0 (AE, parseAE, evalAE, evalAEMaybe, evalM, interpAE)

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

testEvalProp p (s,v) = putStrLn ("AE expression: " ++ s) >> (verboseCheckWith stdArgs { maxSize = 1, maxSuccess = 1 }) (p s v)

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

-- expand arith expression once every 3 times
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

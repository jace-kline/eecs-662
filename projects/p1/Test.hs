{-# LANGUAGE GADTs,FlexibleContexts #-}
module Test where

import P1
import Test.QuickCheck
import Data.Char
import Data.List

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





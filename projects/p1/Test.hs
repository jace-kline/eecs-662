{-# LANGUAGE GADTs,FlexibleContexts #-}
module Test where

import P1
import Test.QuickCheck
import Data.Char

type Length = Int

numConstructors :: Int
numConstructors = 10

instance Arbitrary BBAE where
    arbitrary = sized arbitraryBBAE

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

arbitraryBBAE :: Int -> Gen BBAE
arbitraryBBAE 0 = do
    b <- (arbitrary :: Gen Bool)
    if b then arbitraryNum else arbitraryBoolean
arbitraryBBAE n = do
    k <- choose (1, numConstructors)
    case k of
        1 -> arbitraryNum
        2 -> binary Plus
        3 -> binary Minus
        4 -> do
            id <- arbitraryId
            bexp <- arbitraryRecurse
            exp <- arbitraryRecurse
            return $ Bind id bexp exp
        5 -> arbitraryId >>= \s -> return $ Id s
        6 -> arbitraryBoolean
        7 -> binary And
        8 -> binary Leq
        9 -> arbitraryRecurse >>= \exp -> return $ IsZero exp
        10 -> do
            c <- arbitraryRecurse
            l <- arbitraryRecurse
            r <- arbitraryRecurse
            return $ If c l r
        _ -> error "Invalid value generated"
    where
        arbitraryRecurse :: Gen BBAE
        arbitraryRecurse = arbitraryBBAE ((positive n) `div` 2)
        binary :: (BBAE -> BBAE -> BBAE) -> Gen BBAE
        binary constructor = do
            l' <- arbitraryRecurse
            r' <- arbitraryRecurse
            return $ constructor l' r'


-- newtype ValidBBAE = ValidBBAE { toBBAE :: BBAE }
--     deriving (Eq, Show)

-- instance Arbitrary ValidBBAE where

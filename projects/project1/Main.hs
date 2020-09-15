module Main where

import Tests

main :: IO ()
main = do
    testEvalValid 50
    testEvalInvalid
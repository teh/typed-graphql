module Main (main) where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Data.GraphQL.Combinators


main :: IO ()
main = defaultMain tests

-- ok_1 = "{ add(a: 1, b: 1) }"
-- broken_1 = "{ add(a: 1) }"


tests :: TestTree
tests = testGroup "single-query-object" []

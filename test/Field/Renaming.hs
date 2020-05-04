{-# LANGUAGE QuasiQuotes #-}

-- | Unit tests for
-- |
-- | * renaming
-- | * free variables

module Field.Renaming
    (test_rename) where

-- import           TinyLang.Prelude

import           TinyLang.Field.Typed.Core
import           TinyLang.Field.Typed.Parser
import           TinyLang.Field.Rename
import           TinyLang.Field.Printer

import qualified Data.String.Interpolate.IsString as QQ
import           Test.Tasty
import           Test.Tasty.HUnit

test_rename :: TestTree
test_rename = testGroup "renaming unit tests"
    [   testFreeVars
    ] where
        testFreeVars = testCase "free variables 1" $ freeVars @?= Env mempty
        prog :: Program (AField Rational) = [QQ.i| for x = 0 to 0 do end; assert x == 1; |]
        freeVars = progFreeVarSigs prog


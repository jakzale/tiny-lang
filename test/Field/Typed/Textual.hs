module Field.Typed.Textual
    ( gen_test_typechecking
    ) where

import           Field.TestUtils
import           TinyLang.Field.Typed.Core
import           TinyLang.Field.Typed.TypeChecker
import           TinyLang.Var


import           Data.String
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.Golden

testDir :: FilePath
testDir = "test" </> "Field" </> "Typed" </> "golden"

typeCheckFilePath :: FilePath -> IO (Either String [Statement Rational])
typeCheckFilePath filePath = do
    parsed <- parseFilePath filePath
    pure $ runSupplyT . fmap _scopedValue . typeProgram =<< parsed

genTest :: FilePath -> TestTree
genTest filePath = goldenVsString name golden action
    where name = takeBaseName filePath
          golden = goldenFile filePath
          action = fromString <$> either id show <$> typeCheckFilePath filePath

gen_test_typechecking :: IO TestTree
gen_test_typechecking =
    discoverTests "type checking" testDir genTest
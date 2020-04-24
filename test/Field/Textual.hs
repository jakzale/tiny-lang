{- A simple printer/parser test: generate a random expression and see if
   you get the same thing back (modulo uniques) when you convert it to
   a string and then parse it again.
-}

module Field.Textual
    ( test_textual
    , gen_test_roundtrip
    ) where

import           Field.TestUtils
import           Data.Field.F17
import           TinyLang.Field.Generator
import qualified TinyLang.Field.Jubjub       as JJ
import           TinyLang.Field.Printer
import           TinyLang.Field.Typed.Core
import           TinyLang.Field.Typed.Parser
import           TinyLang.Prelude
import           TinyLang.Field.Rename
import           TinyLang.Field.Evaluator

-- import           Data.Bifunctor
import           System.FilePath
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.QuickCheck

-- TODO: we shouldn't forget uniques, because we ignore name shadowing problems in
-- generators. I.e. we should implement alpha-equality (but it is kind of weird to change
-- uniques of free variables and so we probably want a newtype wrapper around @Expr@ with
-- that very specific @Eq@ instance).


-- TODO: I can probably remove a lot of code now that @Statements@ and @Program@
-- are functors.
forgetProgramIDs :: Program f -> Program f
forgetProgramIDs = fmap forgetStatementIDs

    -- forgetStatementsIDs :: Statements f -> Statements f
-- forgetStatementsIDs = fmap forgetStatementIDs

forgetID :: UniVar f a -> UniVar f a
forgetID (UniVar u v) = UniVar u $ Var (Unique 0) (_varName v)

forgetStatementIDs :: Statement f -> Statement f
forgetStatementIDs (ELet uvar d)  = ELet (forgetID uvar) (forgetIDs d)
forgetStatementIDs (EAssert expr) = EAssert $ forgetIDs expr
forgetStatementIDs (EFor uvar start end stmts) =
    EFor (forgetID uvar) start end stmts' where
    stmts' = forgetStatementIDs <$> stmts

forgetIDs :: Expr f a -> Expr f a
forgetIDs (EConst uval)        = EConst uval
forgetIDs (EVar uvar)          = EVar $ forgetID uvar
forgetIDs (EAppUnOp op e)      = EAppUnOp op (forgetIDs e)
forgetIDs (EAppBinOp op e1 e2) = EAppBinOp op (forgetIDs e1) (forgetIDs e2)
forgetIDs (EIf e e1 e2)        = EIf (forgetIDs e) (forgetIDs e1) (forgetIDs e2)

{- Call this with eg
       quickCheck (withMaxSuccess 1000 (prop_Ftest :: SomeUniExpr Rational -> Bool))
   or
       quickCheck (stdArgs {maxSuccess=500, maxSize=1000}) (prop_Ftest :: SomeUniExpr F17 -> Bool)
-}

prop_prog_roundtrip :: forall f. (Eq f, TextField f) => Program f -> Either String ()
prop_prog_roundtrip prog = do
    prog' <- runSupplyT $ parseProgram @f $ progToString NoIDs prog
    when (forgetProgramIDs prog /= forgetProgramIDs prog) . Left $ concat
        [ progToString NoIDs prog
        , " is not equal to "
        , progToString NoIDs prog'
        ]

data Binding f = forall a. Binding (UniVar f a) (Expr f a)

deriving instance TextField f => Show (Binding f)

instance (Field f, Arbitrary f) => Arbitrary (Binding f) where
    arbitrary =
        withOneOfUnis $ \(_ :: Uni f a) ->
            Binding @f @a . unDefaultUniVar <$> arbitrary <*> arbitrary

prop_nested_let
    :: forall f. (Eq f, TextField f)
    => [Binding f] -> Either String ()
prop_nested_let bindings = prop_prog_roundtrip $ mkProgram $ mkStatements $ map bind bindings where
    bind :: Binding f -> Statement f
    bind (Binding uniVar body) = ELet uniVar body

test_checkParseGeneric :: TestTree
test_checkParseGeneric =
    testProperty "checkParseGeneric2" $
        withMaxSuccess 1000 . property $ prop_prog_roundtrip @JJ.F

test_checkParseNestedLets :: TestTree
test_checkParseNestedLets =
    testProperty "checkParseNestedLets" $
        withMaxSuccess 100 . property $ prop_nested_let @F17

test_printerParserRoundtrip :: TestTree
test_printerParserRoundtrip =
    testGroup "printerParserRoundtrip"
        [ test_checkParseGeneric
        , test_checkParseNestedLets
        ]

test_textual :: TestTree
test_textual =
    testGroup "textual"
        [ test_printerParserRoundtrip
       , test_renaming
        ]

parsePrintFilePath :: FilePath -> IO String
parsePrintFilePath filePath = either id (progToString WithIDs) <$> typeCheckFilePath filePath

testDir :: FilePath
testDir = "test" </> "Field" </> "golden"

genTest :: FilePath -> TestTree
genTest filePath = goldenVsString name golden action
    where name   = takeBaseName filePath
          golden = goldenFile filePath
          action = fromString <$> parsePrintFilePath filePath

gen_test_roundtrip :: IO TestTree
gen_test_roundtrip =
    discoverTests "roundtrip golden" testDir genTest


prop_rename_same_norm :: forall f. (Eq f, TextField f, AsInteger f)
    => ProgramWithEnv f -> Either String ()
prop_rename_same_norm (ProgramWithEnv prog env) =
    when (norm /= norm') . Left $ show norm ++ " /= " ++ show norm' where
        prog' = runSupply $ renameProgram prog
        norm  = either show (progToString NoIDs) $ normProgram env prog
        norm' = either show (progToString NoIDs) $ normProgram env prog'

test_renaming :: TestTree
test_renaming =
    testProperty "evaluation is stable with renaming" $
        withMaxSuccess 1000 . property $ prop_rename_same_norm @F17

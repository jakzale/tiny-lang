import           TinyLang.Field.Generator  ()
import           TinyLang.Field.Typed.Core

import           Test.QuickCheck

-- A couple of functions for checking the output of generators
progNodes :: Program f -> Int
progNodes = stmtsNodes . unProgram

stmtsNodes :: Statements f -> Int
stmtsNodes = sum . map stmtNodes . unStatements

stmtNodes :: Statement f -> Int
stmtNodes (ELet _ e)         = 1 + exprNodes e
stmtNodes (EAssert e)        = 1 + exprNodes e
stmtNodes (EFor _ _ _ stmts) = 1 + stmtsNodes stmts

exprNodes :: Expr f a -> Int
exprNodes (EConst _)          = 1
exprNodes (EVar   _)          = 1
exprNodes (EAppUnOp _ e)      = 1 + exprNodes e
exprNodes (EAppBinOp _ e1 e2) = 1 + exprNodes e1 + exprNodes e2
exprNodes (EIf e e1 e2)       = 1 + exprNodes e + exprNodes e1 + exprNodes e2

progDepth :: Program f -> Int
progDepth = stmtsDepth . unProgram

stmtsDepth :: Statements f -> Int
stmtsDepth = maximum . map stmtDepth . unStatements

stmtDepth :: Statement f -> Int
stmtDepth (ELet _ e)         = 1 + exprDepth e
stmtDepth (EAssert e)        = 1 + exprDepth e
stmtDepth (EFor _ _ _ stmts) = 1 + stmtsDepth stmts

exprDepth :: Expr f a -> Int
exprDepth (EConst _)          = 1
exprDepth (EVar _)            = 1
exprDepth (EAppUnOp _ e)      = 1 + exprDepth e
exprDepth (EAppBinOp _ e1 e2) = 1 + max (exprDepth e1) (exprDepth e2)
exprDepth (EIf e e1 e2)       = 1 + max (exprDepth e)  (max (exprDepth e1) (exprDepth e2))

-- data TestResult = TestResult { nodes :: Int
--                              , depth :: Int
--                              }

testGen :: Int -> Int -> IO ()
testGen n size =
    let arb = arbitrary :: Gen (Program (AField Rational))
        -- ^ Just so that we can define the generator near the top.
        maxInt = maxBound :: Int
    in do
      loop n arb maxInt 0 0 maxInt 0 0
    where
      loop k arb mind maxd sumd minn maxn sumn =
          if k <= 0 then
              let meand = Prelude.div sumd n
                  meann = Prelude.div sumn n
              in do
                putStrLn $ "\nRequested size = " ++ show size
                putStrLn ""
                putStrLn $ "Minimum depth = " ++ show mind
                putStrLn $ "Maximum depth = " ++ show maxd
                putStrLn $ "Mean depth    = " ++ show meand
                putStrLn ""
                putStrLn $ "Minimum number of nodes = " ++ show minn
                putStrLn $ "Maximum number of nodes = " ++ show maxn
                putStrLn $ "Mean number of nodes    = " ++ show meann
                putStrLn ""
          else
              do
                putStr $ "Generated " ++ show (n-k+1) ++ " ASTs\r"
                e <- generate (resize size arb)
                let d = progDepth e
                    m = progNodes e
                loop (k-1) arb (min mind d) (max maxd d) (sumd + d)
                               (min minn m) (max maxn m) (sumn + m)

main :: IO ()
main = testGen 5 100

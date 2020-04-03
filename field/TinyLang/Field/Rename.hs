module TinyLang.Field.Rename
    ( renameStmts
    , renameExpr
    ) where

import           TinyLang.Prelude

import           TinyLang.Field.Typed.Core

renameExpr :: MonadSupply m => Expr f a -> m (Expr f a)
renameExpr expr = do
    exprSupplyFromAtLeastFree expr
    runRenameM $ renameExprM expr

renameStmts :: MonadSupply m => [Statement f] -> m [Statement f]
renameStmts stmts = do
    mapM_ stmtSupplyFromAtLeastFree stmts
    runRenameM $ mapM renameStatementM stmts

type RenameM = ReaderT (Env Unique) Supply

runRenameM :: MonadSupply m => RenameM a -> m a
runRenameM a = liftSupply $ runReaderT a mempty

withFreshenedVar :: Var -> (Var -> RenameM c) -> RenameM c
withFreshenedVar var cont = do
    uniqNew <- freshUnique
    local (insertVar var uniqNew) . cont $ setUnique uniqNew var

renameVarM :: Var -> RenameM Var
renameVarM var = do
    mayUniq <- asks $ lookupVar var
    pure $ case mayUniq of
        Nothing   -> var
        Just uniq -> setUnique uniq var

renameStatementM :: Statement f -> RenameM (Statement f)
renameStatementM (ELet (UniVar uni var) def) = do
    defRen <- renameExprM def
    withFreshenedVar var $ \varFr -> pure $ ELet (UniVar uni varFr) defRen 
renameStatementM (EAssert expr) = EAssert <$> renameExprM expr

renameExprM :: Expr f a -> RenameM (Expr f a)
renameExprM (EConst uniConst)            = pure $ EConst uniConst
renameExprM (EVar (UniVar uni var))      = EVar . UniVar uni <$> renameVarM var
renameExprM (EIf cond expr1 expr2)       =
    EIf <$> renameExprM cond <*> renameExprM expr1 <*> renameExprM expr2
renameExprM (EAppUnOp op expr)           = EAppUnOp op <$> renameExprM expr
renameExprM (EAppBinOp op expr1 expr2)   = EAppBinOp op <$> renameExprM expr1 <*> renameExprM expr2


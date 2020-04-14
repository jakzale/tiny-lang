{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Typed Parser.

This module exposes the old API of the Typed Parser.

For the new API please refer to "TinyLang.Field.Raw.Parser".

-}
module TinyLang.Field.Typed.Parser
    ( parseScopedExpr
    , parseExpr
    , parseScopedStmts
    , parseStmts
    ) where

import           TinyLang.Prelude                 hiding (many, option, some,
                                                   try)

import           Data.Field
import           TinyLang.Field.Raw.Parser
import           TinyLang.Field.Rename
import           TinyLang.Field.Typed.Core
import           TinyLang.Field.Typed.TypeChecker
import           TinyLang.ParseUtils

import qualified Data.IntMap.Strict               as IntMap
import qualified Data.IntSet                      as IntSet
import qualified Data.Map.Strict                  as Map

-- TODO: use a proper @newtype@.
instance TextField f => IsString (Scoped (Some (Expr f))) where
    fromString = either error (fmap $ forget Some) . runSupplyT . parseScopedExpr

-- instance TextField f => IsString (Some (Expr f)) where
--     fromString = _scopedValue <$> fromString

-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
-- If the result is an error, then return the latest 'Scope', otherwise return the 'Scope'
-- consisting of all free variables of the expression.

parseScopedExpr
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> m (Scoped (SomeUniExpr f))
parseScopedExpr str = do undefined
    -- exprRaw <- parseString (pTop @f) "" str
    -- Scoped scopeTotal (SomeOf uni exprTyped) <- typeCheck exprRaw
    -- exprTypedRen <- renameExpr exprTyped
    -- let indicesFree = IntMap.keysSet . unEnv $ exprFreeVarSigs exprTypedRen
    --     isFree var = unUnique (_varUniq var) `IntSet.member` indicesFree
    --     scopeFree = Map.filter isFree scopeTotal
    -- return . Scoped scopeFree $ SomeOf uni exprTypedRen

-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
parseExpr
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> m (SomeUniExpr f)
parseExpr = fmap _scopedValue . parseScopedExpr

parseScopedStmts
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> m (Scoped [Statement f])
parseScopedStmts str = do
    undefined
    -- case errorOrSomeUniExpr of
    --     Left err -> return . Scoped totalScope $ Left err
    --     Right rawStmts -> do
    --         typed <- runExceptT (typeStatements rawStmts)
    --         case typed of
    --             Left err' -> return . Scoped totalScope $ Left err'
    --             Right stmts -> do
    --                 stmtsRen <- renameStmts stmts
    --                 let freeIndices = IntMap.keysSet . unEnv $ stmtsFreeVarSigs stmtsRen
    --                     isFree var = unUnique (_varUniq var) `IntSet.member` freeIndices
    --                     freeScope = Map.filter isFree totalScope
    --                 return . Scoped freeScope . Right $ stmtsRen


-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
parseStmts
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> m [Statement f]
parseStmts = fmap _scopedValue . parseScopedStmts

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Typed Parser.

This module exposes the old API of the Typed Parser.

For the new API please refer to "TinyLang.Field.Raw.Parser".

-}
module TinyLang.Field.Typed.Parser
    ( parseScopedExpr
    , parseExpr
    , parseScopedProgram
    , parseProgram
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

instance TextField f => IsString (Scoped (Program f)) where
    fromString = either error id . runSupplyT . parseScopedProgram

instance TextField f => IsString (Program f) where
    fromString = _scopedValue <$> fromString

-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
-- If the result is an error, then return the latest 'Scope', otherwise return the 'Scope'
-- consisting of all free variables of the expression.

parseScopedExpr
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> m (Scoped (SomeUniExpr f))
parseScopedExpr str = do
    exprRaw <- parseString (pTopExpr @f) "" str
    Scoped scopeTotal (SomeOf uni exprTyped) <- typeCheck exprRaw
    exprTypedRen <- renameExpr exprTyped
    let indicesFree = IntMap.keysSet . unEnv $ exprFreeVarSigs exprTypedRen
        isFree var = unUnique (_varUniq var) `IntSet.member` indicesFree
        scopeFree = Map.filter isFree scopeTotal
    return . Scoped scopeFree $ SomeOf uni exprTypedRen

-- | Parse a @String@ and return @Either@ an error message or an @Expr@ of some type.
parseExpr
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> m (SomeUniExpr f)
parseExpr = fmap _scopedValue . parseScopedExpr


parseScopedProgram
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> m (Scoped (Program f))
parseScopedProgram str = do
    progRaw <- parseString (pTop @f) "" str
    Scoped scopeTotal progTyped <- typeProgram progRaw
    progTypedRen <- renameProgram progTyped
    let indicesFree = IntMap.keysSet . unEnv $ progFreeVarSigs progTypedRen
        isFree var = unUnique (_varUniq var) `IntSet.member` indicesFree
        scopeFree = Map.filter isFree scopeTotal
    return $ Scoped scopeFree progTypedRen

parseProgram
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> m (Program f)
parseProgram = fmap _scopedValue . parseScopedProgram

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Typed Parser.

This module exposes the old API of the Typed Parser.

For the new API please refer to "TinyLang.Field.Raw.Parser".

-}
module TinyLang.Field.Typed.Parser
    ( parseScopedProgram
    , parseProgram
    , parseScopedProgram'
    , parseProgram'
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

-- | Parse a @String@ and return @Either@ an error message or an @Program@ of some type.
-- If the result is an error, then return the latest 'Scope', otherwise return the 'Scope'
-- consisting of all free variables of the expression.
parseScopedProgram'
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> String -> m (Scoped (Program f))
parseScopedProgram' fileName str = do
    progRaw <- parseString (pTop @f) fileName str
    Scoped scopeTotal progTyped <- typeProgram progRaw
    progTypedRen <- renameProgram progTyped
    let indicesFree = IntMap.keysSet . unEnv $ progFreeVarSigs progTypedRen
        isFree var = unUnique (_varUniq var) `IntSet.member` indicesFree
        scopeFree = Map.filter isFree scopeTotal
    return $ Scoped scopeFree progTypedRen

parseProgram'
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> String -> m (Program f)
parseProgram' fileName = fmap _scopedValue . parseScopedProgram' fileName

parseScopedProgram
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> m (Scoped (Program f))
parseScopedProgram = parseScopedProgram' ""

-- | Convenience version of @parseScopedProgram'@ with an empty file name.
parseProgram
    :: forall f m. (MonadError String m, MonadSupply m, TextField f)
    => String -> m (Program f)
parseProgram = parseProgram' ""

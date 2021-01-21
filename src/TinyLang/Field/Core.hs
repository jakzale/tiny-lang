{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Basic structure of our programs


module TinyLang.Field.Core
    ( Program (..)
    , Statements (..)
    ) where

import           Data.Bifunctor
import           GHC.Generics
import           Quiet

-- | Basic wrapper of statements
newtype Statements stmt = Statements { unStatements :: [stmt] }
    deriving (Generic, Eq, Functor, Foldable, Traversable)
    deriving (Show) via (Quiet (Statements stmt))

-- | Basic wrapper of program
data Program var stmt = Program
    { _programExts :: [var]
    , _programStatements :: Statements stmt
    }
    deriving (Eq, Foldable, Traversable, Functor)

instance Bifunctor Program where
    bimap f g (Program exts stmts) = Program (fmap f exts) (fmap g stmts)

-- NOTE:  Adding explicit Show instance to avoid record syntax
instance (Show var, Show stmt) => Show (Program var stmt) where
    show (Program exts stmts) = "Program " ++ show exts ++ " " ++ show stmts

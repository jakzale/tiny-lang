-- | Basic structure of our programs

module TinyLang.Field.Core
    ( Program (..)
    , Statements (..)
    ) where

import           GHC.Generics
import           Quiet

-- | Basic wrapper of statements
newtype Statements stmt f = Statements { unStatements :: [stmt f] }
    deriving (Generic, Eq)
    deriving (Show) via (Quiet (Statements stmt f))

-- | Basic wrapper of program
newtype Program stmt f = Program { unProgram :: (Statements stmt f) }
    deriving (Generic, Eq)
    deriving (Show) via (Quiet (Program stmt f))

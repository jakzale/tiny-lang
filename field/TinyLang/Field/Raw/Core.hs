{-| A Raw (untyped) AST
-}
module TinyLang.Field.Raw.Core
    ( Identifier
    , Var(..)
    , Expr(..)
    , BinOp(..)
    , UnOp(..)
    , Statement(..)
    , Statements(..)
    , RawExpr
    , RawStatement
    , RawStatements
    ) where

import           TinyLang.Field.UniConst

import           GHC.Generics
import           Quiet

{-| = AST
-}
type Identifier = String

newtype Var = Var { unVar :: Identifier }
    deriving (Eq, Generic)
    deriving (Show) via (Quiet Var)

{-| @Statements v f$, @Statement v f@, and @Expr v f@ are parameterised
  by the type of variable @v@.
-}
newtype Statements v f = Statements { unStatements :: [Statement v f] }
    deriving (Generic)
    deriving (Show) via (Quiet (Statements v f))

data Statement v f
    = ELet    v          (Expr v f)
    | EAssert (Expr v f)
    | EFor    v          Integer    Integer (Statements v f)
    deriving (Show)

data Expr v f
    = EConst     (SomeUniConst f)
    | EVar       v
    | EAppBinOp  BinOp           (Expr v f) (Expr v f)
    | EAppUnOp   UnOp            (Expr v f)
    | EIf        (Expr v f)      (Expr v f) (Expr v f)
    | ETypeAnn   (SomeUni f)     (Expr v f)
    deriving (Show)

data BinOp
    = Or
    | And
    | Xor
    | FEq
    | FLe
    | FLt
    | FGe
    | FGt
    | Add
    | Sub
    | Mul
    | Div
    | BAt
    deriving (Show)

data UnOp
    = Not
    | Neq0
    | Neg
    | Inv
    | Unp
    deriving (Show)

{-| = Utility Type Aliases
-}
type RawExpr       = Expr Var
type RawStatement  = Statement Var
type RawStatements = Statements Var


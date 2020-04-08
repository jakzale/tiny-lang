{-| A Raw (untyped) AST
-}
module TinyLang.Field.Raw.Core
    ( Identifier
    , Var(..)
    , Expr(..)
    , BinOp(..)
    , UnOp(..)
    , Statement(..)
    , Program(..)
    , Statements(..)
    , RawProgram
    , RawStatements
    , RawStatement
    , RawExpr
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

{-| In our AST we have the following

* @Program v f@,

* @Statements v f@,

* @Statement v f@ and

At the moment @Program v f@ is a newtype wrapper around @Statements v f@, and
@Statements v f@ is a newtype wrapper around @[Statement v f]@.

We specify those wrappers as some operations work on the program level, some
operations work on the statements level, and some operations work on the
statement level; the operations acting on statement level are not necessarily
mappable over a list of statements.
-}

{-| @Program v f$, @Statement v f@, and @Expr v f@ are parameterised
  by the type of variable @v@.
-}
newtype Program v f = Program { unProgram :: Statements v f }
    deriving (Generic)
    deriving (Show) via (Quiet (Program v f))

{-| @Statements@ is our topLevel form for parsing programs.
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
type RawProgram    = Program Var
type RawStatements = Statements Var
type RawStatement  = Statement Var
type RawExpr       = Expr Var

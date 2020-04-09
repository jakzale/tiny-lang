module TinyLang.Field.Typed.Core
    ( module Field
    , module Var
    , module Env
    , Some (..)
    , SomeOf (..)
    , Forget (..)
    , traverseSomeOf
    , Uni (..)
    , KnownUni (..)
    , UniConst (..)
    , UniVar (..)
    , SomeUniConst
    , SomeUniVar
    , SomeUniExpr
    , UnOp (..)
    , BinOp (..)
    , Statement (..)
    , Statements (..)
    , Program (..)
    , Expr (..)
    , withUnOpUnis
    , withBinOpUnis
    , withGeqUni
    , withKnownUni
    , VarSig (..)
    , ScopedVarSigs (..)
    , stmtVarSigs
    , stmtFreeVarSigs
    , stmtsFreeVarSigs
    , progFreeVarSigs
    , exprVarSigs
    , exprFreeVarSigs
    , exprSupplyFromAtLeastFree
    , stmtSupplyFromAtLeastFree
    , stmtsSupplyFromAtLeastFree
    , uniOfExpr
    ) where

import           Prelude                    hiding (div)
import           TinyLang.Prelude

import           Data.Field                 as Field
import           TinyLang.Environment       as Env
import           TinyLang.Field.Existential
import           TinyLang.Field.UniConst
import           TinyLang.Var               as Var

import           GHC.Generics ()
import           Quiet

-- Needed for the sake of symmetry with 'UniConst'.
data UniVar f a = UniVar
    { _uniVarUni :: Uni f a
    , _uniVarVar :: Var
    } deriving (Show)

-- -- TODO: We can can unify the two above by the following data type. Should we do that?
-- data Inhabits f a b = Inhabits
--     { _inhabitsUni :: Uni f a
--     , _inhabitsVal :: b
--     }

type SomeUniVar f = Some (UniVar f)
type SomeUniExpr f = SomeOf (Uni f) (Expr f)

data UnOp f a b where
    Not  :: UnOp f Bool       Bool
    Neq0 :: UnOp f (AField f) Bool
    Neg  :: UnOp f (AField f) (AField f)
    Inv  :: UnOp f (AField f) (AField f)
    Unp  :: UnOp f (AField f) (Vector Bool)

data BinOp f a b c where
    Or  :: BinOp f Bool       Bool       Bool
    And :: BinOp f Bool       Bool       Bool
    Xor :: BinOp f Bool       Bool       Bool
    FEq :: BinOp f (AField f) (AField f) Bool
    FLt :: BinOp f (AField f) (AField f) Bool
    FLe :: BinOp f (AField f) (AField f) Bool
    FGe :: BinOp f (AField f) (AField f) Bool
    FGt :: BinOp f (AField f) (AField f) Bool
    Add :: BinOp f (AField f) (AField f) (AField f)
    Sub :: BinOp f (AField f) (AField f) (AField f)
    Mul :: BinOp f (AField f) (AField f) (AField f)
    Div :: BinOp f (AField f) (AField f) (AField f)
    BAt :: BinOp f (AField f) (Vector Bool) Bool

newtype Program f = Program { unProgram :: Statements f }
    deriving (Eq,Generic)
    deriving (Show) via (Quiet (Program f))

newtype Statements f = Statements { unStatements :: [Statement f] }
    deriving (Eq,Generic)
    deriving (Show) via (Quiet (Statements f))

data Statement f where
    ELet    :: UniVar f a -> Expr f a -> Statement f
    -- | Things that get compiled to constraints down the pipeline.
    -- The evaluation semantics is the following: each assertion becomes a check at runtime
    -- and the if the check fails, we have evaluation failure.
    EAssert :: Expr f Bool -> Statement f
    EFor    :: UniVar f (AField f) -> Integer -> Integer -> Statements f -> Statement f

data Expr f a where
    EConst     :: UniConst f a -> Expr f a
    EVar       :: UniVar f a -> Expr f a
    EIf        :: Expr f Bool -> Expr f a -> Expr f a -> Expr f a
    EAppUnOp   :: UnOp f a b -> Expr f a -> Expr f b
    EAppBinOp  :: BinOp f a b c -> Expr f a -> Expr f b -> Expr f c

instance (Field f, af ~ AField f) => Field (Expr f af) where
    zer = EConst zer
    neg = EAppUnOp Neg
    add = EAppBinOp Add
    sub = EAppBinOp Sub
    one = EConst one
    inv = Just . EAppUnOp Inv
    mul = EAppBinOp Mul
    div = Just .* EAppBinOp Div

deriving via AField (Expr   f af) instance (Field f, af ~ AField f) => Num        (Expr   f af)
deriving via AField (Expr   f af) instance (Field f, af ~ AField f) => Fractional (Expr   f af)

deriving instance Show (UnOp f a b)
deriving instance Eq   (UnOp f a b)

deriving instance Show (BinOp f a b c)
deriving instance Eq   (BinOp f a b c)

deriving instance TextField f => Show (Statement f)
deriving instance TextField f => Show (Expr f a)


deriving instance TextField f => Show (SomeUniExpr f)

withUnOpUnis :: UnOp f a b -> (Uni f a -> Uni f b -> c) -> c
withUnOpUnis Not  k = k knownUni knownUni
withUnOpUnis Neq0 k = k knownUni knownUni
withUnOpUnis Inv  k = k knownUni knownUni
withUnOpUnis Neg  k = k knownUni knownUni
withUnOpUnis Unp  k = k knownUni knownUni

withBinOpUnis :: BinOp f a b c -> (Uni f a -> Uni f b -> Uni f c -> d) -> d
withBinOpUnis Or  k = k knownUni knownUni knownUni
withBinOpUnis And k = k knownUni knownUni knownUni
withBinOpUnis Xor k = k knownUni knownUni knownUni
withBinOpUnis FEq k = k knownUni knownUni knownUni
withBinOpUnis FLt k = k knownUni knownUni knownUni
withBinOpUnis FLe k = k knownUni knownUni knownUni
withBinOpUnis FGe k = k knownUni knownUni knownUni
withBinOpUnis FGt k = k knownUni knownUni knownUni
withBinOpUnis Add k = k knownUni knownUni knownUni
withBinOpUnis Sub k = k knownUni knownUni knownUni
withBinOpUnis Mul k = k knownUni knownUni knownUni
withBinOpUnis Div k = k knownUni knownUni knownUni
withBinOpUnis BAt k = k knownUni knownUni knownUni

uniOfExpr :: Expr f a -> Uni f a
uniOfExpr (EConst (UniConst uni _)) = uni
uniOfExpr (EVar (UniVar uni _))     = uni
uniOfExpr (EAppUnOp op _)           = withUnOpUnis op $ \_ resUni -> resUni
uniOfExpr (EAppBinOp op _ _)        = withBinOpUnis op $ \_ _ resUni -> resUni
uniOfExpr (EIf _ x _)               = uniOfExpr x

withGeqUnOp :: UnOp f a1 b1 -> UnOp f a2 b2 -> d -> ((a1 ~ a2, b1 ~ b2) => d) -> d
withGeqUnOp unOp1 unOp2 z y =
    withUnOpUnis unOp1 $ \argUni1 resUni1 ->
    withUnOpUnis unOp2 $ \argUni2 resUni2 ->
    withGeqUni argUni1 argUni2 z $
    withGeqUni resUni1 resUni2 z $
        if unOp1 /= unOp2 then z else y

withGeqBinOp :: BinOp f a1 b1 c1 -> BinOp f a2 b2 c2 -> d -> ((a1 ~ a2, b1 ~ b2, c1 ~ c2) => d) -> d
withGeqBinOp binOp1 binOp2 z y =
    withBinOpUnis binOp1 $ \argUni11 argUni12 resUni1 ->
    withBinOpUnis binOp2 $ \argUni21 argUni22 resUni2 ->
    withGeqUni argUni11 argUni21 z $
    withGeqUni argUni12 argUni22 z $
    withGeqUni resUni1  resUni2  z $
        if binOp1 /= binOp2 then z else y

-- This doesn't type check:
--
-- > UniConst _ x1 == UniConst _ x2 = x1 == x2
--
-- because it requires the type of @x1@ and @x2@ to have an @Eq@ instance.
-- We could provide a similar to 'withGeqUni' combinator that can handle this situation,
-- but then it's easier to just pattern match on universes.
instance Eq f => Eq (UniVar f a) where
    UniVar _ v1 == UniVar _ v2 = v1 == v2

instance Eq f => Eq (Statement f) where
    ELet (UniVar u1 v1) d1 == ELet (UniVar u2 v2) d2 =
        withGeqUni u1 u2 False $ v1 == v2 && d1 == d2
    EAssert as1 == EAssert as2 =
        as1 == as2
    EFor (UniVar u1 v1) i1 j1 stmts1 == EFor (UniVar u2 v2) i2 j2 stmts2 =
        withGeqUni u1 u2 False $ v1 == v2 && i1 == i2 && j1 == j2 && stmts1 == stmts2

    ELet    {} == _ = False
    EAssert {} == _ = False
    EFor    {} == _ = False

instance Eq f => Eq (Expr f a) where
    EConst uval1       == EConst uval2         = uval1 == uval2
    EVar uvar1         == EVar uvar2         = uvar1 == uvar2
    EIf b1 x1 y1       == EIf b2 x2 y2       = b1 == b2 && x1 == x2 && y1 == y2
    EAppUnOp o1 x1     == EAppUnOp o2 x2     = withGeqUnOp o1 o2 False $ x1 == x2
    EAppBinOp o1 x1 y1 == EAppBinOp o2 x2 y2 = withGeqBinOp o1 o2 False $ x1 == x2 && y1 == y2

    -- Here we explicitly pattern match on the first argument again and always return 'False'.
    -- This way we'll get a warning when an additional constructor is added to 'Expr',
    -- instead of erroneously defaulting to 'False'.
    EConst     {} == _ = False
    EVar       {} == _ = False
    EIf        {} == _ = False
    EAppUnOp   {} == _ = False
    EAppBinOp  {} == _ = False

withKnownUni :: Uni f a -> (KnownUni f a => c) -> c
withKnownUni Bool   = id
withKnownUni Field  = id
withKnownUni Vector = id

data VarSig f = forall a. VarSig
    { _varSigName :: String
    , _varSigUni  :: Uni f a
    }

deriving instance Show (VarSig f)

instance Eq (VarSig f) where
    VarSig name1 uni1 == VarSig name2 uni2 = withGeqUni uni1 uni2 False $ name1 == name2

data ScopedVarSigs f = ScopedVarSigs
    { _scopedVarSigsFree  :: Env (VarSig f)
    , _scopedVarSigsBound :: Env (VarSig f)
    } deriving (Show)

isTracked :: (Eq a, Show a) => Unique -> a -> Env a -> Bool
isTracked uniq x env =
    case lookupUnique uniq env of
        Just x'
            | x == x'   -> True
            | otherwise -> error $ concat ["panic: mismatch: '", show x, "' vs '", show x', "'"]
        Nothing -> False

-- TODO: test me somehow.
stmtVarSigs' :: ScopedVarSigs f -> Statement f -> ScopedVarSigs f
stmtVarSigs' sigs (ELet uniVar def) = ScopedVarSigs free $ insertUnique uniq sig bound where
    UniVar uni (Var uniq name) = uniVar
    sig = VarSig name uni
    ScopedVarSigs free bound = exprVarSigs' sigs def
stmtVarSigs' sigs (EAssert expr) = exprVarSigs' sigs expr
stmtVarSigs' sigs (EFor uniVar _ _ stmts) = ScopedVarSigs free $ insertUnique uniq sig bound where
    UniVar uni (Var uniq name) = uniVar
    sig = VarSig name uni
    ScopedVarSigs free bound = foldr (flip stmtVarSigs') sigs (unStatements stmts)

exprVarSigs' :: ScopedVarSigs f -> Expr f a -> ScopedVarSigs f
exprVarSigs' sigs (EConst _) = sigs
exprVarSigs' sigs (EVar (UniVar uni (Var uniq name)))
    | tracked   = sigs
    | otherwise = ScopedVarSigs (insertUnique uniq sig free) bound
    where
        ScopedVarSigs free bound = sigs
        sig = VarSig name uni
        tracked = isTracked uniq sig bound || isTracked uniq sig free
exprVarSigs' sigs (EAppUnOp _ x) = exprVarSigs' sigs x
exprVarSigs' sigs (EAppBinOp _ x y) = exprVarSigs' (exprVarSigs' sigs x) y
exprVarSigs' sigs (EIf b x y) = exprVarSigs' (exprVarSigs' (exprVarSigs' sigs b) x) y

stmtVarSigs :: Statement f -> ScopedVarSigs f
stmtVarSigs = stmtVarSigs' $ ScopedVarSigs mempty mempty

exprVarSigs :: Expr f a -> ScopedVarSigs f
exprVarSigs = exprVarSigs' $ ScopedVarSigs mempty mempty

stmtFreeVarSigs :: Statement f -> Env (VarSig f)
stmtFreeVarSigs = _scopedVarSigsFree . stmtVarSigs

stmtsFreeVarSigs :: Statements f -> Env (VarSig f)
stmtsFreeVarSigs = foldMap stmtFreeVarSigs . unStatements

progFreeVarSigs :: Program f -> Env (VarSig f)
progFreeVarSigs = stmtsFreeVarSigs . unProgram

exprFreeVarSigs :: Expr f a -> Env (VarSig f)
exprFreeVarSigs = _scopedVarSigsFree . exprVarSigs

stmtSupplyFromAtLeastFree :: MonadSupply m => Statement f -> m ()
stmtSupplyFromAtLeastFree =
    supplyFromAtLeast . freeUniqueIntMap . unEnv . _scopedVarSigsFree . stmtVarSigs

stmtsSupplyFromAtLeastFree :: MonadSupply m => Statements f -> m ()
stmtsSupplyFromAtLeastFree = mapM_ stmtSupplyFromAtLeastFree . unStatements

exprSupplyFromAtLeastFree :: MonadSupply m => Expr f a -> m ()
exprSupplyFromAtLeastFree =
    supplyFromAtLeast . freeUniqueIntMap . unEnv . _scopedVarSigsFree . exprVarSigs
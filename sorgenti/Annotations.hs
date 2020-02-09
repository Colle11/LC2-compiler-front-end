
{-# LANGUAGE DeriveFunctor #-}

module Annotations
  (
    GenericAnnotated(..),
    getAnn,
    unAnn,
    HasTypeInfo(..),
    HasLocation(..),
    HasConstnessInfo(..),
    HasValueCategory(..),
    Ann(..),
    Annotated,
    noAnn
  ) where

import {-# SOURCE #-} Types as T
import SourceLocation as SL hiding (getLoc)
import qualified SourceLocation as SL
import PrettyPrinting

data GenericAnnotated a e = A a e
  deriving (Functor, Eq, Ord)

instance Show e => Show (GenericAnnotated a e) where
  show (A a e) = "(" ++ show e ++ ")"

instance PrPrint e => PrPrint (GenericAnnotated a e) where
  prettyDoc (A _ e) = prettyDoc e

getAnn :: GenericAnnotated a e -> a
getAnn (A a _) = a

unAnn :: GenericAnnotated a e -> e
unAnn (A _ e) = e

class HasTypeInfo a where
  getType :: a -> TypeSpec
  setType :: a -> TypeSpec -> a

class HasLocation a where
  getLoc :: a -> SrcSpan
  setLoc :: a -> SrcSpan -> a

class HasConstnessInfo a where
  getConst :: a -> Constness
  setConst :: a -> Constness -> a

class HasValueCategory a where
  getVC :: a -> ValueCategory
  setVC :: a -> ValueCategory -> a

data Ann = Ann {
  _type   :: TypeSpec,
  _loc    :: SrcSpan,
  _const  :: Constness,
  _valcat :: ValueCategory
  } deriving (Show, Eq, Ord)

type Annotated e = GenericAnnotated Ann e

noAnn :: Ann
noAnn = Ann {
  _type   = UnTyped,
  _loc    = SL.noSrcSpan,
  _const  = VarExpr,
  _valcat = RValue
}

instance HasLocation a => HasLocation (GenericLocated a e) where
  getLoc = getLoc . SL.getLoc
  setLoc (L a e) l = flip L e . flip setLoc l $ a

instance HasLocation SrcSpan where
  getLoc = id
  setLoc s l = l

instance (HasTypeInfo a) => HasTypeInfo (GenericAnnotated a e) where
  getType = getType . getAnn
  setType (A a e) t = flip A e . flip setType t $ a

instance HasTypeInfo Ann where
  getType = _type
  setType a t = a { _type = t }

instance (HasLocation a) => HasLocation (GenericAnnotated a e) where
  getLoc = getLoc . getAnn
  setLoc (A a e) l = flip A e . flip setLoc l $ a

instance HasLocation Ann where
  getLoc = _loc
  setLoc a l = a { _loc = l }

instance (HasConstnessInfo a) => HasConstnessInfo (GenericAnnotated a e) where
  getConst = getConst . getAnn
  setConst (A a e) c = flip A e . flip setConst c $ a

instance HasConstnessInfo Ann where
  getConst = _const
  setConst a c = a { _const = c }

instance (HasValueCategory a) => HasValueCategory (GenericAnnotated a e) where
  getVC = getVC . getAnn
  setVC (A a e) c = flip A e . flip setVC c $ a

instance HasValueCategory Ann where
  getVC = _valcat
  setVC a c = a { _valcat = c }


module AST where

import {-# SOURCE #-} Annotations
import PrettyPrinting

data RExpr

instance Eq RExpr
instance Ord RExpr
instance Show RExpr
instance PrPrint RExpr

type AnRExpr = Annotated RExpr


module PrettyPrinting (
  PrPrint(..),
  module PP,
  (<.>)
                      ) where
import Text.PrettyPrint as PP

class PrPrint a where
  {- minimal complete definition -}
  prettyDoc :: a -> Doc

  {- defaulted methods -}
  toString :: a -> String
  toStringS :: a -> ShowS

  {- defaults -}
  toString = render . prettyDoc
  toStringS = shows . prettyDoc

(<.>) = (PP.<>)

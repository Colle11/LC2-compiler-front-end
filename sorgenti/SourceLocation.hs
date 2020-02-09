{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}

module SourceLocation (
         SrcLoc(..),
         RealSrcSpan(..),
         SrcSpan(..),
         srcSpanStartRealLoc,
         srcSpanEndRealLoc,
         mkRealSrcSpan,
         combineRealSrcSpans,
         combineSrcSpans,
         srcSpanFirstCharacter,
         noSrcSpan,
         GenericLocated(..),
         Located,
         unLoc,
         getLoc,
         Loc(..),
         srcSpanStartLoc,
         highlightSrcSpan
       )where

import PrettyPrinting as PP
import Data.Ord
import Data.List (intercalate)
import Data.Functor.Classes
import Data.Foldable
import qualified Data.Vector as V

data RealLoc
  = RealLoc' !Int  -- Line number
             !Int  -- Column number
  deriving (Eq, Ord, Show)

instance PrPrint RealLoc where
  prettyDoc (RealLoc' l c) = int l <.> comma <.> int c

data Loc
  = NoLoc String
  | RealLoc RealLoc
  deriving (Eq, Ord, Show)

instance PrPrint Loc where
  prettyDoc (NoLoc _)   = empty
  prettyDoc (RealLoc r) = prettyDoc r

data SrcLoc
  = SrcLoc String   -- File name
           !Int     -- Line number
           !Int     -- Column number
  deriving (Eq, Ord)

data RealSrcSpan
  = RealSrcSpan'
    { srcSpanFile   :: String, -- File name
      srcSpanSLine  :: !Int,   -- Start line
      srcSpanSCol   :: !Int,   -- Start column
      srcSpanELine  :: !Int,   -- End line
      srcSpanECol   :: !Int    -- End column
    }
  deriving (Eq)

data SrcSpan
  = NoSrcSpan !String       -- String contains a message
  | RealSrcSpan !RealSrcSpan
  deriving(Eq, Ord, Show)

data GenericLocated l e = L l e
  deriving (Eq, Ord, Show, Functor)

type Located e = GenericLocated SrcSpan e

noSrcSpan :: SrcSpan
noSrcSpan = NoSrcSpan "<no location info>"

srcSpanStartRealLoc :: RealSrcSpan -> RealLoc
srcSpanStartRealLoc (RealSrcSpan' _ sl sc _ _) = RealLoc' sl sc

srcSpanEndRealLoc :: RealSrcSpan -> RealLoc
srcSpanEndRealLoc (RealSrcSpan' _ _ _ el ec) = RealLoc' el ec

isPointSpan :: RealSrcSpan -> Bool
isPointSpan (RealSrcSpan' _ l1 c1 l2 c2) =
  l1 == l2 && c1 == c2

isLineSpan :: RealSrcSpan -> Bool
isLineSpan (RealSrcSpan' _ l1 _ l2 _ ) = l1 == l2

mkRealSrcSpan :: SrcLoc -> SrcLoc -> RealSrcSpan
mkRealSrcSpan (SrcLoc f l1 c1) (SrcLoc _ l2 c2) = RealSrcSpan' f l1 c1 l2 c2

instance Ord RealSrcSpan where
  a `compare` b =
    case srcSpanStartRealLoc a `compare` srcSpanStartRealLoc b of
      EQ -> srcSpanEndRealLoc a `compare` srcSpanEndRealLoc b
      x  -> x

instance Show SrcLoc where
  show (SrcLoc file l c)
    = "SrcLoc " ++ file ++ " " ++ show l ++ ":" ++ show c

instance Show RealSrcSpan where
  show span@(RealSrcSpan' file sl sc el ec)
    | isPointSpan span
    = "SrcSpanPoint " ++ show file ++ " " ++ intercalate ":" (map show [sl,sc])
    | isLineSpan span =
      let ll = intercalate ":"
             $ intercalate "-"
             <$> ((fmap . fmap) show [[sl], [sc, ec]])
      in "SrcSpanOneLine " ++ show file ++ " " ++ ll
    | otherwise =
      let ll = intercalate " "
             $ intercalate ":"
             <$> ((fmap . fmap) show [[sl, sc], [el,ec]])
      in "SrcSpanMultiLine " ++ show file ++ " " ++ ll

instance PrPrint RealSrcSpan where
  prettyDoc span@(RealSrcSpan' file sl sc el ec)
    | isPointSpan span
    = text file <+> (hcat . punctuate colon $ map PP.int [sl,sc])
    | isLineSpan span
    = let ll = hcat $
               punctuate colon $
               hcat . punctuate (char '-') <$>
               (fmap . fmap) PP.int [[sl], [sc, ec]]
      in text file PP.<> colon <+> ll
    | otherwise =
      let ll = hsep $
               hcat . punctuate (char ':') <$>
               (fmap .fmap) PP.int [[sl, sc], [el,ec]]
      in text file <+> ll

instance PrPrint SrcSpan where
  prettyDoc (NoSrcSpan _) = empty
  prettyDoc (RealSrcSpan s) = prettyDoc s

combineRealSrcSpans :: RealSrcSpan -> RealSrcSpan -> RealSrcSpan
combineRealSrcSpans span1 span2 = RealSrcSpan' f sl sc el ec
  where
    f = srcSpanFile span1
    get_start = [srcSpanSLine, srcSpanSCol]
    get_end   = [srcSpanELine, srcSpanECol]
    applyto span = map ($ span)
    [sl, sc] = min (applyto span1 get_start)
                   (applyto span2 get_start)
    [el, ec] = max (applyto span1 get_end)
                   (applyto span2 get_end)

combineSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans (NoSrcSpan _) r = r
combineSrcSpans l (NoSrcSpan _) = l
combineSrcSpans (RealSrcSpan l) (RealSrcSpan r) = RealSrcSpan (combineRealSrcSpans l r)

srcSpanFirstCharacter :: SrcSpan -> SrcSpan
srcSpanFirstCharacter l@(NoSrcSpan _) = l
srcSpanFirstCharacter (RealSrcSpan (RealSrcSpan' f l c _ _)) = RealSrcSpan $ RealSrcSpan' f l c l (c+1)

srcSpanStartLoc :: SrcSpan -> Loc
srcSpanStartLoc (NoSrcSpan s) = NoLoc s
srcSpanStartLoc (RealSrcSpan s) = RealLoc $ srcSpanStartRealLoc s

unLoc :: GenericLocated l e -> e
unLoc (L _ e) = e

getLoc :: GenericLocated l e -> l
getLoc (L l _) = l

type FileContent = V.Vector (V.Vector Char)

highlightSrcSpan :: FileContent -> SrcSpan -> String
highlightSrcSpan fc (NoSrcSpan _) = ""
highlightSrcSpan fc (RealSrcSpan rss) = highlightRealScrSpan fc rss

highlightRealScrSpan :: FileContent -> RealSrcSpan -> String
highlightRealScrSpan fc s@(RealSrcSpan' _ sl sc el ec) = render $ ppRealSrcSpan fc s

ppRealSrcSpan :: FileContent -> RealSrcSpan -> Doc
ppRealSrcSpan fc s@(RealSrcSpan' _ sl sc el ec) = nest 4 output
  where
    userSource = text . V.toList $ (fc V.! (sl-1))
    tilde = text "~"
    marker = (fold . take (sc-1) $ repeat space) <.> text "^" <.> (fold . take (ec-sc) $ repeat tilde)
    output = userSource $+$ marker

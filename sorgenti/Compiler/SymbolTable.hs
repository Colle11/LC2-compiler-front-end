
{-# LANGUAGE TemplateHaskell #-}

module Compiler.SymbolTable (
  Addr(..),
  LocalEnv(..),
  Env,
  Width,
  IsInit,
  emptyEnv,
  initialEnv,
  -- Lenses: LocalEnv
  vars,
  funs,
  localenv
  ) where

import AST
import Types
import SourceLocation
import PrettyPrinting

import Data.Bool (bool)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Hash
import Lens.Micro.Platform

-- Address of TAC
data Addr
  = TName Ident Loc
  | TConst Const
  | TTemp Int
  | TInvAddr
  | TStrPtr Int
  deriving (Show, Eq, Ord)

instance PrPrint Addr where
  prettyDoc = pd
    where
      pd (TName id loc) = let lDoc = prettyDoc loc in
                            prettyDoc id <.>
                            bool (text "@" <.> lDoc) empty (isEmpty lDoc)
      pd (TConst c)  = prettyDoc c
      pd (TTemp i)   = text "t" <.> int i
      pd (TStrPtr i) = text "ptr$str$" <.> int i

type IsInit = Bool
type Width  = Addr

data LocalEnv = LocalEnv {
  -- Variables
  _vars :: Hash.HashMap Ident (
      TypeSpec
      , SrcSpan
      , Modality
      , [Width]  -- List of widths for the types
      , IsInit
      ),
  -- Functions
  _funs :: Hash.HashMap Ident (TypeSpec, SrcSpan, [Addr])
  }

-- TemplateHaskell: generates the lenses (getters+setters) for LocalEnv
makeLenses ''LocalEnv

localenv :: Traversal' [LocalEnv] LocalEnv
localenv = ix 0

type Env = [LocalEnv]

emptyEnv :: LocalEnv
emptyEnv = LocalEnv Hash.empty Hash.empty

initialEnv :: LocalEnv
initialEnv = emptyEnv & funs .~ predFunHash
  where
    predFunHash :: Hash.HashMap Ident (TypeSpec, SrcSpan, [Addr])
    predFunHash =
      let int = BasicType BasicTypeInt
          writeIntType = FunType [ModTypeSpec Value int] Unit
          writeInt = (Ident "writeInt", (writeIntType, noSrcSpan, [TConst . Int $ sizeof Unit]))
          float = BasicType BasicTypeFloat
          writeFloatType = FunType [ModTypeSpec Value float] Unit
          writeFloat = (Ident "writeFloat", (writeFloatType, noSrcSpan, [TConst . Int $ sizeof Unit]))
          char = BasicType BasicTypeChar
          writeCharType = FunType [ModTypeSpec Value char] Unit
          writeChar = (Ident "writeChar", (writeCharType, noSrcSpan, [TConst . Int $ sizeof Unit]))
          string = BasicType BasicTypeString
          writeStringType = FunType [ModTypeSpec Value string] Unit
          writeString = (Ident "writeString", (writeStringType, noSrcSpan, [TConst . Int $ sizeof Unit]))
          readIntType = FunType [] int
          readInt = (Ident "readInt", (readIntType, noSrcSpan, [TConst . Int $ sizeof int]))
          readFloatType = FunType [] float
          readFloat = (Ident "readFloat", (readFloatType, noSrcSpan, [TConst . Int $ sizeof float]))
          readCharType = FunType [] char
          readChar = (Ident "readChar", (readCharType, noSrcSpan, [TConst . Int $ sizeof char]))
          readStringType = FunType [] string
          readString = (Ident "readString", (readStringType, noSrcSpan, [TConst . Int $ sizeof string]))
      in
      Hash.fromList [writeInt,
                     writeFloat,
                     writeChar,
                     writeString,
                     readInt,
                     readFloat,
                     readChar,
                     readString]

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module AST where

import SourceLocation
import Types
import {-# SOURCE #-} Annotations
import PrettyPrinting
import GHC.Generics (Generic)
import Data.Hashable
import Data.Char

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Generic)

instance Hashable Ident

instance PrPrint Ident where
  prettyDoc (Ident s) = text s

type AnIdent = Annotated Ident

newtype Program = Prog [AnStmt]
  deriving (Eq, Ord, Show)

instance PrPrint Program where
  prettyDoc (Prog stmts) = vcat $ map prettyDoc stmts

data Decl
  = Dvar TypeSpec AnIdent (Maybe AnRExpr)
  | Dfun TypeSpec AnIdent [AnFormalParameter] [AnStmt]
  deriving (Eq, Ord, Show)

instance PrPrint Decl where
  prettyDoc = pd
    where
      pd (Dvar t id m) = prettyDoc t <+>
                         prettyDoc id <+>
                         maybe empty ((equals <+>) . prettyDoc) m
      pd (Dfun (FunType _ rt) id fps stmts) = text "def" <+>
                                              prettyDoc rt <+>
                                              prettyDoc id <.>
                                              parens (hsep . punctuate comma $ map prettyDoc fps) $+$
                                              nest 2 (vcat $ map prettyDoc stmts) $+$
                                              text "end"

type AnDecl = Annotated Decl

data FormalParameter = FormalParameter Modality TypeSpec AnIdent
  deriving (Eq, Ord, Show)

instance PrPrint FormalParameter where
  prettyDoc (FormalParameter m t id) = prettyDoc m <+> prettyDoc t <+> prettyDoc id

type AnFormalParameter = Annotated FormalParameter

data ArithOp = Add | Sub | Mul | Mod | Div | Exp
  deriving (Eq,Ord,Show)

instance PrPrint ArithOp where
  prettyDoc = pd
    where
      pd (Add) = char '+'
      pd (Sub) = char '-'
      pd (Mul) = char '*'
      pd (Mod) = char '%'
      pd (Div) = char '/'
      pd (Exp) = text "??"

type AnArithOp = Annotated ArithOp

data BoolOp = And | Or
  deriving (Eq,Ord,Show)

instance PrPrint BoolOp where
  prettyDoc (And) = text "&&"
  prettyDoc (Or)  = text "||"

type AnBoolOp = Annotated BoolOp

data BitOp = BAnd | BOr | BXor | LShift | RShift
  deriving (Eq, Ord, Show)

instance PrPrint BitOp where
  prettyDoc = pd
    where
      pd (BAnd)   = char '&'
      pd (BOr)    = char '|'
      pd (BXor)   = char '^'
      pd (LShift) = text "<<"
      pd (RShift) = text ">>"

type AnBitOp = Annotated BitOp

data RelOp = Eq | Neq | Lt | LtE | Gt | GtE
  deriving (Eq,Ord,Show)

instance PrPrint RelOp where
  prettyDoc = pd
    where
      pd (Eq)  = text "=="
      pd (Neq) = text "!="
      pd (Lt)  = char '<'
      pd (LtE) = text "<="
      pd (Gt)  = char '>'
      pd (GtE) = text ">="

type AnRelOp = Annotated RelOp

data InfixOp
  = ArithOp AnArithOp
  | RelOp AnRelOp
  | BoolOp AnBoolOp
  | BitOp AnBitOp
  deriving (Eq,Ord,Show)

instance PrPrint InfixOp where
  prettyDoc = pd
    where
      pd (ArithOp a)= prettyDoc a
      pd (RelOp r)  = prettyDoc r
      pd (BoolOp b) = prettyDoc b
      pd (BitOp b)  = prettyDoc b

data UnaryOp = Not | Neg | Plus | BComp
  deriving (Eq,Ord,Show)

instance PrPrint UnaryOp where
  prettyDoc (Not)   = char '!'
  prettyDoc (Neg)   = char '-'
  prettyDoc (Plus)  = char '+'
  prettyDoc (BComp) = char '~'

type AnUnaryOp = Annotated UnaryOp

data LExpr
 = VarIdent AnIdent
 | ArrayElem AnLExpr AnRExpr
 | Deref AnRExpr
 | LBrackets AnLExpr -- Useful for pretty-printing
 | PrePostIncDecr PrePost IncDecr AnLExpr
  deriving (Eq,Ord,Show)

instance PrPrint LExpr where
  prettyDoc = pd
    where
      pd (VarIdent id) = prettyDoc id
      pd (ArrayElem lexpr rexpr) = prettyDoc lexpr <.> brackets (prettyDoc rexpr)
      pd (Deref rexpr) = char '*' <.> prettyDoc rexpr
      pd (PrePostIncDecr Pre incdecr lexpr) = prettyDoc incdecr <.> prettyDoc lexpr
      pd (PrePostIncDecr Post incdecr lexpr) = prettyDoc lexpr <.> prettyDoc incdecr
      pd (LBrackets lexpr) = parens $ prettyDoc lexpr

type AnLExpr = Annotated LExpr

data Const
 = Bool Bool
 | Char Char
 | Float Double
 | Int Int
 | Array [AnRExpr]
 | String String
  deriving (Eq,Ord,Show)

instance PrPrint Const where
  prettyDoc = pd
    where
      pd (Bool b) = text . map toLower $ show b
      pd (Char c) = quotes . text $ showLitChar c ""
      pd (Float d) = double d
      pd (Int i) = int i
      pd (Array rs) = brackets . sep . punctuate comma . map prettyDoc $ rs
      pd (String s) = doubleQuotes $ text s

type AnConst = Annotated AST.Const

data PrePost = Post | Pre
  deriving (Eq,Ord,Show)

data IncDecr = Inc | Decr
  deriving (Eq,Ord,Show)

instance PrPrint IncDecr where
  prettyDoc (Inc) = text "++"
  prettyDoc (Decr) = text "--"


data RExpr
  = InfixOp InfixOp AnRExpr AnRExpr
  | UnaryOp AnUnaryOp AnRExpr
  | FCall AnIdent [AnRExpr]
  | Const TypeSpec Const
  | RBrackets AnRExpr -- Useful for pretty-printing
  | LExpr AnLExpr
  | Ref AnLExpr
  | Coercion TypeSpec AnRExpr
  | TernaryOp AnRExpr AnRExpr AnRExpr
  deriving (Eq, Ord, Show)

type AnRExpr = Annotated RExpr

instance PrPrint RExpr where
  prettyDoc = pd
    where
     pd (InfixOp op l r) = prettyDoc l <+> prettyDoc op <+> prettyDoc r
     pd (UnaryOp op r) = prettyDoc op <.> prettyDoc r
     pd (FCall id args) = (prettyDoc id <.>) . parens . sep . punctuate comma . map prettyDoc $ args
     pd (Const _ c) = prettyDoc c
     pd (RBrackets r) = parens $ prettyDoc r
     pd (LExpr l) = prettyDoc l
     pd (Ref l) = char '&' <.> prettyDoc l
     pd (Coercion t r) = parens (prettyDoc t) <.> prettyDoc r
     pd (TernaryOp g t e) = prettyDoc g <+> char '?' <+> prettyDoc t <+> char ':' <+> prettyDoc e

isLiteral :: RExpr -> Bool
isLiteral (Const _ _) = True
isLiteral _ = False

data IfStmt
 = If AnRExpr [AnStmt] (Maybe AnIfStmt)
 | ElseIf AnRExpr [AnStmt] (Maybe AnIfStmt)
 | IElse [AnStmt]
 deriving (Eq,Ord,Show)

instance PrPrint IfStmt where
  prettyDoc = pd
    where
      pd (If g stmts m)     =  text "if" <+>
                               prettyDoc g $+$
                               (nest 2 . vcat $ map prettyDoc stmts) $+$
                               maybe (text "end") prettyDoc m
      pd (ElseIf g stmts m) =  text "elsif" <+>
                               prettyDoc g $+$
                               (nest 2 . vcat $ map prettyDoc stmts) $+$
                               maybe (text "end") prettyDoc m
      pd (IElse stmts)      =  text "else" $+$
                               (nest 2 . vcat $ map prettyDoc stmts) $+$
                               text "end"

type AnIfStmt = Annotated IfStmt

data UnlessStmt
 = Unless AnRExpr [AnStmt] (Maybe AnUnlessStmt)
 | UElse [AnStmt]
 deriving (Eq,Ord,Show)

instance PrPrint UnlessStmt where
  prettyDoc = pd
    where
      pd (Unless g stmts m) = text "unless" <+>
                              prettyDoc g $+$
                              (nest 2 . vcat $ map prettyDoc stmts) $+$
                              maybe (text "end") prettyDoc m
      pd (UElse stmts)      = text "else" $+$
                              (nest 2 . vcat $ map prettyDoc stmts) $+$
                              text "end"

type AnUnlessStmt = Annotated UnlessStmt

data Stmt
 = Assgn AnLExpr AssignmentOp AnRExpr
 | RExprStmt AnRExpr
 | Block [AnStmt]
 | Decl AnDecl
 | IfStmt AnIfStmt
 | UnlessStmt AnUnlessStmt
 | While AnRExpr [AnStmt]
 | Until AnRExpr [AnStmt]
 | For AnIdent Range [AnStmt]
 | Loop Stmt
 | Break
 | Continue
 | RetExp (Maybe AnRExpr)
 | DoWhile AnRExpr [AnStmt]
 | TryCatch [AnStmt] [AnStmt]
 deriving (Eq,Ord,Show)

instance PrPrint Stmt where
  prettyDoc = pd
    where
      pd (Assgn l ass r) = prettyDoc l <+> prettyDoc ass <+> prettyDoc r
      pd (RExprStmt r)   = prettyDoc r
      pd (Block stmts)   = text "begin" $+$ nest 2 (vcat $ map prettyDoc stmts) $+$ text "end"
      pd (Decl d)        = prettyDoc d
      pd (IfStmt s)      = prettyDoc s
      pd (UnlessStmt s)  = prettyDoc s
      pd (While g stmts) = text "while" <+> prettyDoc g $+$ nest 2 (vcat $ map prettyDoc stmts) $+$ text "end"
      pd (Until g stmts) = text "until" <+> prettyDoc g $+$ nest 2 (vcat $ map prettyDoc stmts) $+$ text "end"
      pd (Loop (Block stmts)) = text "loop do" $+$ nest 2 (vcat $ map prettyDoc stmts) $+$ text "end"
      pd (Break)         = text "break"
      pd (Continue)      = text "next"
      pd (RetExp m)      = text "return" <+> maybe empty prettyDoc m
      pd (For id rng stmts) = text "for" <+> prettyDoc id <+> text "in" <+> prettyDoc rng $+$ nest 2 (vcat $ map prettyDoc stmts) $+$ text "end"
      pd (DoWhile g stmts)  = text "begin" $+$ nest 2 (vcat $ map prettyDoc stmts) $+$ text "end" <+> text "while" <+> prettyDoc g
      pd (TryCatch body catch) = text "begin" $+$
                                 nest 2 (vcat $ map prettyDoc body) $+$
                                 text "rescue" $+$
                                 nest 2 (vcat $ map prettyDoc catch) $+$
                                 text "end"

type AnStmt = Annotated Stmt

data Range
  = Range AnRExpr AnRExpr
  deriving (Eq,Ord,Show)

instance PrPrint Range where
  prettyDoc = pd
    where
      pd (Range rs re) = prettyDoc rs <.> text ".." <.> prettyDoc re

data AssignmentOp = Assign | AssgnArith AnArithOp | AssgnBit AnBitOp | AssgnLogic AnBoolOp
  deriving (Eq,Ord,Show)

instance PrPrint AssignmentOp where
  prettyDoc = pd
    where
      pd (Assign)       = char '='
      pd (AssgnArith a) = prettyDoc a <.> char '='
      pd (AssgnBit a)   = prettyDoc a <.> char '='
      pd (AssgnLogic a) = prettyDoc a <.> char '='

instance PrPrint a => PrPrint [a] where
  prettyDoc = vcat . map prettyDoc

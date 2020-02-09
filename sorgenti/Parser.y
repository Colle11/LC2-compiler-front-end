{

module Parser where

import Lexer
import System.Environment
import AST
import SourceLocation hiding (getLoc)
import Annotations
import Types
import Control.Monad.Except

import Data.Functor
import Data.Foldable
}

  -- %name <Haskell identifier> [ <non-terminal> ]
  --
  -- (optional) The %name directive is followed by a valid Haskell identifier,
  -- and gives the name of the top-level parsing function in the generated parser.
  -- This is the only function that needs to be exported from a parser module.
  --
  -- If the %name directive is omitted, it defaults to happyParse.
  --
  -- The %name directive takes an optional second parameter which specifies
  -- the top-level non-terminal which is to be parsed. If this parameter is omitted,
  -- it defaults to the first non-terminal defined in the grammar.

%name rexpr RExpr -- Parse RExpr
%name lexpr LExpr
%name stmts Program
  -- %monad { <type> } [ { <then> } { <return> } ]
  -- The %monad directive adds monadic support to the parser.
  -- Beware that the type of the main parser function and the parseError function
  -- change
%monad {Parser}

  -- %lexer { <lexer> } { <eof> }
%lexer {lexer} {A _ T_EOF}

  -- The '%error' statement specify the function Happy will call whenever there's
  -- an error in the parsing process.
%error {parseError}

%tokentype{AnToken}

  -- The %token directive is used to tell Happy about all the terminal symbols
  -- used in the grammar. Each terminal has a name, by which it is referred to
  -- in the grammar itself, and a Haskell representation enclosed in braces.
  -- Each of the patterns must be of the same type, given by the %tokentype
  -- directive. %token <name> { <Haskell pattern> }
%token
  '\n'                { A _ (T_RSymb T_NewLine) }
  '('                 { A _ (T_RSymb T_LParen) }
  ')'                 { A _ (T_RSymb T_RParen) }
  '['                 { A _ (T_RSymb T_LSquare) }
  ']'                 { A _ (T_RSymb T_RSquare) }
  '{'                 { A _ (T_RSymb T_LBrace) }
  '}'                 { A _ (T_RSymb T_RBrace) }
  ','                 { A _ (T_RSymb T_Comma) }
  ';'                 { A _ (T_RSymb T_SemiCol) }
  '+'                 { A _ (T_RSymb T_Plus) }
  '-'                 { A _ (T_RSymb T_Minus) }
  '*'                 { A _ (T_RSymb T_Prod) }
  '/'                 { A _ (T_RSymb T_Div) }
  '??'                { A _ (T_RSymb T_Exp) }
  '%'                 { A _ (T_RSymb T_Mod) }
  '&'                 { A _ (T_RSymb T_BAnd) }
  '|'                 { A _ (T_RSymb T_BOr) }
  '^'                 { A _ (T_RSymb T_BXor) }
  '~'                 { A _ (T_RSymb T_BComp) }
  '<<'                { A _ (T_RSymb T_LShift) }
  '>>'                { A _ (T_RSymb T_RShift) }
  '&&'                { A _ (T_RSymb T_LAnd) }
  '||'                { A _ (T_RSymb T_LOr) }
  '=='                { A _ (T_RSymb T_Equals) }
  '!'                 { A _ (T_RSymb T_Not) }
  '!='                { A _ (T_RSymb T_NEq) }
  '<'                 { A _ (T_RSymb T_LAngle) }
  '>'                 { A _ (T_RSymb T_RAngle) }
  '<='                { A _ (T_RSymb T_LAngleEq) }
  '>='                { A _ (T_RSymb T_RAngleEq) }
  '='                 { A _ (T_RSymb T_Eq) }
  '+='                { A _ (T_RSymb T_PlusEq) }
  '-='                { A _ (T_RSymb T_MinusEq) }
  '*='                { A _ (T_RSymb T_MulEq) }
  '/='                { A _ (T_RSymb T_DivEq) }
  '??='               { A _ (T_RSymb T_ExpEq) }
  '%='                { A _ (T_RSymb T_ModEq) }
  '&='                { A _ (T_RSymb T_BAndEq) }
  '|='                { A _ (T_RSymb T_BOrEq) }
  '^='                { A _ (T_RSymb T_BXorEq) }
  '<<='               { A _ (T_RSymb T_LShiftEq) }
  '>>='               { A _ (T_RSymb T_RShiftEq) }
  '&&='               { A _ (T_RSymb T_LAndEq) }
  '||='               { A _ (T_RSymb T_LOrEq) }
  '++'                { A _ (T_RSymb T_PlusPlus) }
  '--'                { A _ (T_RSymb T_MinusMinus) }
  '..'                { A _ (T_RSymb T_Range) }
  '?'                 { A _ (T_RSymb T_ThenE) }
  ':'                 { A _ (T_RSymb T_ElseE) }

  -- Reserved Keywords

  -- Basic Types
  'int'        { A _ (T_RWrds K_Int) }
  'float'      { A _ (T_RWrds K_Float) }
  'bool'       { A _ (T_RWrds K_Bool) }
  'char'       { A _ (T_RWrds K_Char) }
  'string'     { A _ (T_RWrds K_String) }
  'nil'        { A _ (T_RWrds K_Nil) }
  -- Boolean Values
  'false'      { A _ (T_RWrds K_False) }
  'true'       { A _ (T_RWrds K_True) }
  -- Control Constructs
  'for'        { A _ (T_RWrds K_For) }
  'while'      { A _ (T_RWrds K_While) }
  'if'         { A _ (T_RWrds K_If) }
  'then'       { A _ (T_RWrds K_Then) }
  'else'       { A _ (T_RWrds K_Else) }
  'elsif'      { A _ (T_RWrds K_Elsif) }
  'unless'     { A _ (T_RWrds K_Unless) }
  'return'     { A _ (T_RWrds K_Return) }
  'break'      { A _ (T_RWrds K_Break) }
  'next'       { A _ (T_RWrds K_Continue) }
  'until'      { A _ (T_RWrds K_Until) }
  'loop'       { A _ (T_RWrds K_Loop) }
  'in'         { A _ (T_RWrds K_In) }
  -- Block Delimiters
  'do'         { A _ (T_RWrds K_Do) }
  'begin'      { A _ (T_RWrds K_Begin) }
  'end'        { A _ (T_RWrds K_End) }
  -- Function Declaration
  'def'        { A _ (T_RWrds K_Def) }
  -- Catch
  'rescue'     { A _ (T_RWrds K_Catch) }
  -- Parameter Passing Modalities
  'val'        { A _ (T_RWrds K_Val) }
  'const'      { A _ (T_RWrds K_Const) }
  'valres'     { A _ (T_RWrds K_ValeRes) }
  'ref'        { A _ (T_RWrds K_Ref) }
  'res'        { A _ (T_RWrds K_Res) }

  Ident               { A _ (T_Ident _) }
  -- Literals
  Int                 { A _ (T_Int _) }
  Float               { A _ (T_Double _) }
  Char                { A _ (T_Char _) }
  String              { A _ (T_String _) }

-- Operator precedence and associativity
%right '?' ':'
%left '||'
%left '&&'
%nonassoc '==' '!='
%nonassoc '<' '<=' '>' '>='
%left '|' '^'
%left '&'
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%left NEG
%right '??'
%right '!' '~' PLUS DEREF
%nonassoc '++' '--'

%%

Id     :: { AnIdent }
        : Ident                             { let f (T_Ident s) = Ident s in fmap f $1}

Literal :: { AnConst }
        : Int                               { let f (T_Int i) = Int i in fmap f $1 }
        | Float                             { let f (T_Double f) = Float f in fmap f $1 }
        | Char                              { let f (T_Char c) = Char c in fmap f $1 }
        | String                            { let f (T_String s) = String s in fmap f $1 }
        | Bool                              { $1 }
        | ArrayLit                          { $1 }

ArrayLit :: { AnConst }
         : '[' ListRExpr ']'                { cLoc2 $1 $3 $ Array $2 }

RExpr  :: { AnRExpr }
        : RExpr '||' RExpr                  { cLoc2 $1 $3 $ InfixOp (BoolOp (Or <$ $2)) $1 $3 }
        | RExpr '&&' RExpr                  { cLoc2 $1 $3 $ InfixOp (BoolOp (And <$ $2)) $1 $3 }
        | '!' RExpr                         { cLoc2 $1 $2 $ UnaryOp (Not <$ $1) $2 }
        | RExpr '==' RExpr                  { cLoc2 $1 $3 $ InfixOp (RelOp (Eq <$ $2)) $1 $3 }
        | RExpr '!=' RExpr                  { cLoc2 $1 $3 $ InfixOp (RelOp (Neq <$ $2)) $1 $3 }
        | RExpr '<'  RExpr                  { cLoc2 $1 $3 $ InfixOp (RelOp (Lt <$ $2)) $1 $3 }
        | RExpr '<=' RExpr                  { cLoc2 $1 $3 $ InfixOp (RelOp (LtE <$ $2)) $1 $3 }
        | RExpr '>'  RExpr                  { cLoc2 $1 $3 $ InfixOp (RelOp (Gt <$ $2)) $1 $3 }
        | RExpr '>=' RExpr                  { cLoc2 $1 $3 $ InfixOp (RelOp (GtE <$ $2)) $1 $3 }
        | RExpr '+'  RExpr                  { cLoc2 $1 $3 $ InfixOp (ArithOp (Add <$ $2)) $1 $3 }
        | RExpr '-'  RExpr                  { cLoc2 $1 $3 $ InfixOp (ArithOp (Sub <$ $2)) $1 $3 }
        | RExpr '*'  RExpr                  { cLoc2 $1 $3 $ InfixOp (ArithOp (Mul <$ $2)) $1 $3 }
        | RExpr '/'  RExpr                  { cLoc2 $1 $3 $ InfixOp (ArithOp (Div <$ $2)) $1 $3 }
        | RExpr '??'  RExpr                 { cLoc2 $1 $3 $ InfixOp (ArithOp (Exp <$ $2)) $1 $3 }
        | RExpr '%'  RExpr                  { cLoc2 $1 $3 $ InfixOp (ArithOp (Mod <$ $2)) $1 $3 }
        | RExpr '&' RExpr                   { cLoc2 $1 $3 $ InfixOp (BitOp (BAnd <$ $2)) $1 $3 }
        | RExpr '|' RExpr                   { cLoc2 $1 $3 $ InfixOp (BitOp (BOr <$ $2)) $1 $3 }
        | RExpr '^' RExpr                   { cLoc2 $1 $3 $ InfixOp (BitOp (BXor <$ $2)) $1 $3 }
        | '~' RExpr                         { cLoc2 $1 $2 $ UnaryOp (BComp <$ $1) $2}
        | RExpr '<<' RExpr                  { cLoc2 $1 $3 $ InfixOp (BitOp (LShift <$ $2)) $1 $3 }
        | RExpr '>>' RExpr                  { cLoc2 $1 $3 $ InfixOp (BitOp (RShift <$ $2)) $1 $3 }
        | '-' RExpr %prec NEG               { cLoc2 $1 $2 $ UnaryOp (Neg <$ $1) $2 }
        | '+' RExpr %prec PLUS              { cLoc2 $1 $2 $ UnaryOp (Plus <$ $1) $2 }
        | Id '(' ListRExpr ')'              { cLoc2 $1 $4 $ FCall $1 $3 }
        | Literal                           { fmap litToRExpr $ $1}
        | '(' RExpr ')'                     { cLoc2 $1 $3 $ RBrackets $2 }
        | LExpr                             { A (getAnn $1) (LExpr $1) }
        | '&' LExpr %prec DEREF             { cLoc2 $1 $2 $ Ref $2 }
        | RExpr '?' RExpr ':' RExpr         { cLoc2 $1 $5 $ TernaryOp $1 $3 $5 }

Bool :: { AnConst }
        : 'true'                            { let f (T_RWrds K_True) = Bool True in fmap f $1 }
        | 'false'                           { let f (T_RWrds K_False) = Bool False in fmap f $1 }

ListRExpr :: { [AnRExpr] }
        : sep0(RExpr, ',')                  { $1 }


LExpr  :: { AnLExpr }
        : LExpr1                            { $1 }
        | '*' RExpr %prec DEREF             { cLoc2 $1 $2 $ Deref $2 }
        | '++' LExpr                        { cLoc2 $1 $2 $ PrePostIncDecr Pre Inc $2 }
        | '--' LExpr                        { cLoc2 $1 $2 $ PrePostIncDecr Pre Decr $2 }


LExpr1 :: { AnLExpr }
        : LExpr2                            { $1 }
        | LExpr1 '++'                       { cLoc2 $1 $2 $ PrePostIncDecr Post Inc $1 }
        | LExpr1 '--'                       { cLoc2 $1 $2 $ PrePostIncDecr Post Decr $1 }

LExpr2 :: { AnLExpr }
        : '(' LExpr ')'                     { cLoc2 $1 $3 $ LBrackets $2 } -- shift/reduce conflict
        | LExpr3                            { $1 }

LExpr3 :: { AnLExpr }
        : LExpr2 '[' RExpr ']'              { cLoc2 $1 $4 $ ArrayElem $1 $3 }
        | Id                                { A (getAnn $1) (VarIdent $1) }


Decl :: { AnDecl }
        : VarDecl                           { $1 }
        | FunDecl                           { $1 }

FunDecl :: { AnDecl }
        : 'def' Id '(' ListParameter ')' ListStmt 'end'          { cLoc2 $1 $7 . Dfun (mkFunType Unit $4) $2 $4 . unAnn $ $6 }
        | 'def' TypeSpec Id '(' ListParameter ')' ListStmt 'end' { cLoc2 $1 $8 . Dfun (mkFunType (unAnn $2) $5) $3 $5 . unAnn $ $7 }

VarDecl :: { AnDecl }
        : TypeSpec Id                       { cLoc2 $1 $2 $ Dvar (unAnn $1) $2 Nothing }
        | TypeSpec Id '=' RExpr             { cLoc2 $1 $4 $ Dvar (unAnn $1) $2 (Just $4) }


ListParameter :: { [AnFormalParameter] }
       : sep0(Parameter, ',')               { $1 }

Parameter :: { AnFormalParameter }
        : TypeSpec Id                       { cLoc2 $1 $2 $ FormalParameter Value (unAnn $1) $2 }
        | Modality TypeSpec Id              { cLoc2 $1 $3 $ FormalParameter (unAnn $1) (unAnn $2) $3 }


Modality :: { AnModality }
        : 'ref'                             { A (getAnn $1) Reference }
        | 'valres'                          { A (getAnn $1) ValueResult }
        | 'val'                             { A (getAnn $1) Value }
        | 'res'                             { A (getAnn $1) Result }
        | 'const'                           { A (getAnn $1) Constant }


ListStmt :: { Annotated [AnStmt] }
        : msep0(Stmt, EndLine) lst0(EndLine)   { cLocList . $1 $ [] }


TypeSpec :: { AnTypeSpec }
        : ArrayType                         { fmap reverseArrayType $1 }
        | MachineType                       { $1 }
        | 'nil'                             { A (getAnn $1) Unit }

-- Type whose values fits a register
MachineType :: { AnTypeSpec }
             : BasicType                    { A (getAnn $1) $ BasicType (unAnn $1) }
             | RefType                      { $1 }

BasicType :: { AnBasicType }
        : 'bool'                            { A (getAnn $1) BasicTypeBool }
        | 'char'                            { A (getAnn $1) BasicTypeChar }
        | 'float'                           { A (getAnn $1) BasicTypeFloat }
        | 'int'                             { A (getAnn $1) BasicTypeInt }
        | 'string'                          { A (getAnn $1) BasicTypeString }

ArrayType :: { AnTypeSpec }
          : TypeSpec '[' RExpr ']'          { cLoc2 $1 $4 $ ArrayType $3 . unAnn $ $1 }

RefType :: { AnTypeSpec }
        : TypeSpec '*'                      { cLoc2 $1 $2 $ RefType . unAnn $ $1 }

ArgBlock :: { AnStmt } -- Argument blocks of Ruby
        : '{' ListStmt '}'                  { cLoc2 $1 $3 . Block . unAnn $ $2 }
        | 'do' ListStmt 'end'               { cLoc2 $1 $3 . Block . unAnn $ $2 }

CompStmt :: { AnStmt }
        : 'begin' ListStmt 'end'            { cLoc2 $1 $3 . Block . unAnn $ $2 }

EndLine :: {AnToken}
        : '\n'                              { $1 }
        | ';'                               { $1 }

Stmt :: { AnStmt }
       : CompStmt                           { $1 }
       | SimpleStmt                         { $1 }
       | LExpr Assignment_op RExpr          { cLoc2 $1 $3 $ Assgn $1 $2 $3 }
       | JumpStmt                           { $1 }
       | IterStmt                           { $1 }
       | SelectionStmt                      { $1 }
       | Decl                               { A (getAnn $1) $ Decl $1 }
       | TryCatch                           { $1 }

Program :: { Program }
         : ListStmt                         { Prog $ unAnn $1 }

SimpleStmt :: { AnStmt }
        : RExpr                             { A (getAnn $1) (RExprStmt $1) }


Assignment_op :: { AssignmentOp }
        : '='                               { Assign }
        | '*='                              { AssgnArith (A (getAnn $1) Mul) }
        | '+='                              { AssgnArith (A (getAnn $1) Add) }
        | '/='                              { AssgnArith (A (getAnn $1) Div) }
        | '??='                             { AssgnArith (A (getAnn $1) Exp) }
        | '-='                              { AssgnArith (A (getAnn $1) Sub)}
        | '%='                              { AssgnArith (A (getAnn $1) Mod) }
        | '&='                              { AssgnBit (A (getAnn $1) BAnd) }
        | '^='                              { AssgnBit (A (getAnn $1) BXor) }
        | '|='                              { AssgnBit (A (getAnn $1) BOr) }
        | '>>='                             { AssgnBit (A (getAnn $1) RShift) }
        | '<<='                             { AssgnBit (A (getAnn $1) LShift) }
        | '&&='                             { AssgnLogic (A (getAnn $1) And) }
        | '||='                             { AssgnLogic (A (getAnn $1) Or) }


JumpStmt :: { AnStmt }
        : 'break'                           { A (getAnn $1) Break }
        | 'next'                            { A (getAnn $1) Continue }
        | 'return'                          { A (getAnn $1) (RetExp Nothing) }
        | 'return' RExpr                    { cLoc2 $1 $2 $ RetExp $ Just $ $2 }

SelectionStmt :: { AnStmt }
        : 'if' RExpr OptThen ListStmt Elses 'end'    { let l = noAnn{_loc=cSpan2 $1 $6} in A l . IfStmt . A l $ If  $2 (unAnn $4) $5 }
        | 'unless' RExpr OptThen ListStmt Else 'end' { let l = noAnn{_loc=cSpan2 $1 $6}
                                                       in A l . UnlessStmt . A l . Unless  $2 (unAnn $4) . (fmap . fmap) UElse $ $5 }

OptThen :: { AnToken }
        : 'then'                            { $1 }
        | '\n'                              { $1 }

Elses  :: { Maybe AnIfStmt }
        : lst0(ElseIf) Else                 { flip nestElseIf $1 $ (fmap . fmap) IElse $2 }

Else   :: { Maybe (Annotated [AnStmt]) }
        : {- empty -}                       { Nothing }
        | 'else' ListStmt                   { Just . cLoc2 $1 $2 . unAnn $ $2 }

ElseIf :: { AnIfStmt }
        : 'elsif' RExpr OptThen ListStmt    { cLoc2 $1 $4 $ ElseIf $2 (unAnn $4) Nothing }

IterStmt :: { AnStmt }
        : 'while' RExpr OptDo ListStmt 'end'         { cLoc2 $1 $5 . While $2 . unAnn $ $4 }
        | 'until' RExpr OptDo ListStmt 'end'         { cLoc2 $1 $5 . Until $2 . unAnn $ $4 }
        | 'loop' ArgBlock                            { cLoc2 $1 $2 . Loop . unAnn $ $2 }
        | 'for' Id 'in' Range OptDo ListStmt 'end'   { cLoc2 $1 $7 . For $2 $4 . unAnn $ $6 }
        | 'begin' ListStmt 'end' 'while' RExpr       { cLoc2 $1 $5 . DoWhile $5 . unAnn $ $2 }

TryCatch :: { AnStmt }
        : 'begin' ListStmt 'rescue' ListStmt 'end'   { cLoc2 $1 $5 $ TryCatch (unAnn $2) (unAnn $4) }

OptDo :: { AnToken }
        : 'do'                              { $1 }
        | '\n'                              { $1 }

Range :: { Range }
       : RExpr '..' RExpr                   { Range $1 $3 }

-- Parameterized Productions

lst1(p) :: { [a] -> [a] }
           : p                    { ($1:) }
           | lst1(p) p            { $1 . ($2:)}

lst0(p) :: { [a] }
           : {-empty-}            { [] }
           | lst1(p)              { $1 [] }

sep1(p,q) :: { [a] -> [a] }
           : p                    { ($1:) }
           | sep1(p,q) q p        { $1 . ($3:)}

sep0(p,q) :: { [a] }
           : {-empty-}            { [] }
           | sep1(p,q)            { $1 [] }

-- n is the neutral element, i.e., it will not affect the result
-- mlst : monoidal list, i.e., list from elements of p with neutral element n
mlst1(p,n) :: { [a] -> [a] }
           : n                    {  id   }
           | p                    { ($1:) }
           | mlst1(p,n) p         { $1 . ($2:)}
           | mlst1(p,n) n         { $1 }

mlst0(p,n) :: { [a] }
           : {-empty-}            { [] }
           | mlst1(p,n)           { $1 [] }

msep0(p,q) :: { [a] -> [a] }
           : {-empty-}               { id }
           | p                       { ($1:) }
           | msep0(p,q) lst1(q) p    { $1 . ($3:)}

optmsep0(p,o,q) :: {[a] -> [a]}
        : {-empty-}               { id }
        | p                             { ($1:) }
        | optmsep0(p,o,q) lst1(q)  p    { $1 . ($3:)}
        | o p                           { ($1:) . ($2:) }
        | optmsep0(p,o,q) lst1(q) o p   { $1 . ($3:) . ($4:)}

{
-- CALLING THE LEXER

-- When using a monadic lexer, together with a monadic parser, the latter no
-- longer reads a list of tokens, but it calls the lexical analysis function
-- for each new token to be read.
-- This has the side effect of eliminating the intermediate list of tokens,
-- which is a slight performance win.

-- If we want to extend the monad concept to the lexical analizer, we can request
-- it by adding the following declaration to the grammar file:
-- %lexer { <lexer> } { <eof> }

-- The lexical analyser function <lexer> provided with the directive %lexer
-- must have the following type:
-- lexer :: (Token -> P a) -> P a
-- What this is saying is that the lexer should provide us a 'P a' when
-- given a continuation, so if we want a <lexer> of type P a, then just define
-- a wrapper to deal with the continuation:

-- lexer :: (Token -> P a) -> P a
-- lexer cont = real_lexer >>= \token -> cont token

-- A simple wrapper could be defined as follow:

-- lexer :: (Token -> Alex a) -> Alex a
-- lexer cont = do
--     token <- alexMonadScan
--     {- we can do other stuff here with token -}
--     cont token

-- Equivalently to the previous declaration:

lexer :: (AnToken -> Parser a) -> Parser a
lexer = (alexMonadScan >>=)

-- CALLING THE PARSER

-- With a monadic lexer the type of the main parser function is just P a,
-- the input is being handled completely within the monad.
-- The main parser function is the one declared with the %name directive, or, if
-- the directive is omitted, it defaults to happyParse.

-- parser :: P a

-- The parser function will call the lexer as soon as a token is needed.
-- Since that the lexer takes a continuation as an argument, the parser will pass
-- the token to this continuation to carry on with the parse.
-- The parser upon receinving a token will choose between:
-- 1) Shift  : Requesting another token from the lexer and making that token a leaf
--             of the parsing tree.
-- 2) Reduce : Applying a completed grammar rule to some of the recent parse trees,
--             joining them together as one tree with a new root symbol.

-- Both the lexer function and the grammar rules are monadic actions composed
-- toghether to build a huge statefull computation that will eventually return an
-- AST.
-- The monadic actions are performed in the order that they are reduced.
-- If we consider the parse as a tree, then reductions happen in a depth-first
-- left-to-right manner.

-- Here is another rappresentation with the shift/reduce sequence on a Parsing Tree
-- t --> token
-- r --> reduced rule
{-
           _               _               _               _               _           |
          /_\             /_\             /_\             /_\             /_\          |
         /___\           /___\   Shift   /___\  Reduce   /___\   Shift   /___\         |
        /_____\  ---->  /_____\  ---->  /_____\  ---->  /_____\  ---->  /_____\  --    |
       /_______\       /_______\       /_______\       /_______\       /_______\       |
      /_________\     /_________\     /_________\     /r _______\     /r _______\      |
     /___________\   /t _________\   /t t _______\   /    _______\   /   t ______\     |
                      ^                 ^               ^                ^             |
-}
{-
           _               _               _               _               _           |
          /_\             /_\             /_\             /_\             /_\          |
 Shift   /___\  Reduce   /___\  Reduce   /___\   Shift   /___\  Reduce   /___\         |
 ---->  /_____\  ---->  /_____\  ---->  /_____\  ---->  /_____\  ---->  /r____\  --    |
       /_______\       /_______\       /r______\       /r _____\       /   ____\       |
      /r _______\     /r r _____\     /   ______\     /    _____\     /     ____\      |
     /  t t _____\   /      _____\   /     ______\   /     t ____\   /       ____\     |
          ^               ^               ^                ^               ^           |
-}

-- definition of the parse error function
parseError :: AnToken -> Parser a
parseError (A a t) = case getLoc a of
  RealSrcSpan srcspan -> throwError
                         $ "\nParsing error at line "
                         ++ (show . srcSpanSLine $ srcspan)
                         ++ ", column "
                         ++ (show . srcSpanSCol $ srcspan)
                         ++ ": Unexpected token "
                         ++ show t
  NoSrcSpan s -> throwError
                 $ "\nParsing error "
                 ++ s
                 ++ ": Unexpected token "
                 ++ show t

parseRExpr s = run s rexpr
-- parser entry point
parseStmts s = run s stmts

parseFile :: String -> IO (Either String Program)
parseFile filename = runOnFile filename stmts

-- Function return type -> Function Parameters -> Function type
mkFunType:: TypeSpec -> [AnFormalParameter] -> TypeSpec
mkFunType retType params = FunType (map fix params) retType
  where
     fix (A _ (FormalParameter mod ty _)) = ModTypeSpec mod ty

-- combine the RealSrcSpan of two Annotated
cSpan2:: (HasLocation a, HasLocation b) => a -> b -> SrcSpan
cSpan2 l r = combineSrcSpans (getLoc l) (getLoc r)

-- combine the srcspan of two nodes and assign it to the new node c
cLoc2:: Annotated a -> Annotated b -> c -> Annotated c
cLoc2 x y = A noAnn{_loc=cSpan2 x y}

litToRExpr:: AST.Const -> RExpr
litToRExpr c@(Int _)    = Const (BasicType BasicTypeInt) c
litToRExpr c@(Char _)   = Const (BasicType BasicTypeChar) c
litToRExpr c@(Bool _)   = Const (BasicType BasicTypeBool) c
litToRExpr c@(Float _)  = Const (BasicType BasicTypeFloat) c
litToRExpr c@(String _) = Const (BasicType BasicTypeString) c
litToRExpr c@(Array l)  = Const (ArrayType (A noAnn{_type=BasicType BasicTypeInt, _const=ConstExpr} (Const (BasicType BasicTypeInt) . Int . length $ l)) UnTyped) c


reverseArrayType :: TypeSpec -> TypeSpec
reverseArrayType t = myFold id t
  where
    myFold:: (TypeSpec -> TypeSpec) -> TypeSpec -> TypeSpec
    myFold acc (ArrayType rexpr t) = myFold (ArrayType rexpr . acc) t
    myFold acc x = acc x

-- we parse the elsif as a list, then we nest them in single AnIfStmt
nestElseIf :: Maybe AnIfStmt -> [AnIfStmt] -> Maybe AnIfStmt
nestElseIf = foldr' f
  where
    f e Nothing = Just e
    f e@(A l (ElseIf r s _)) j@(Just k) = Just . cLoc2 e k $ ElseIf r s j

-- creates the srcspan that covers all the elements in the list
cLocList :: [Annotated a] -> Annotated [Annotated a]
cLocList []  = A noAnn []
cLocList [x] = flip A [x] . getAnn $ x
cLocList xs  = cLoc2 fst lst xs
               where
                 fst = head xs
                 lst  = last xs
}

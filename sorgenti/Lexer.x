{
{-# LANGUAGE GeneralizedNewtypeDeriving, FunctionalDependencies, MultiParamTypeClasses #-}

module Lexer where

-- Needed by Alex for UTF8 Input Encoding
import Data.Char
import Data.Word (Word8)
import qualified Data.Bits

-- Implementation of a monad based on transformers
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Identity
import Control.Applicative
import Data.Functor
import Data.Tuple

-- Location of tokens
import SourceLocation
import Annotations
}


$digit = [0-9]
$lower = [a-z]
$upper = [A-Z]
$univ = [\0-\255]  -- any character
$escaped = [\\ a b f n r t v \' \" 0]  -- escaped characters

:-

-- Grammar Symbols
\n       {symbolToken T_NewLine}
\(       {symbolToken T_LParen}
\)       {symbolToken T_RParen}
\[       {symbolToken T_LSquare}
\]       {symbolToken T_RSquare}
\{       {symbolToken T_LBrace}
\}       {symbolToken T_RBrace}
\,       {symbolToken T_Comma}
\;       {symbolToken T_SemiCol}
\+       {symbolToken T_Plus}
\-       {symbolToken T_Minus}
\*       {symbolToken T_Prod}
\/       {symbolToken T_Div}
\?\?     {symbolToken T_Exp}
\%       {symbolToken T_Mod}
\&       {symbolToken T_BAnd}
\|       {symbolToken T_BOr}
\^       {symbolToken T_BXor}
\~       {symbolToken T_BComp}
\<\<     {symbolToken T_LShift}
\>\>     {symbolToken T_RShift}
\&\&     {symbolToken T_LAnd}
\|\|     {symbolToken T_LOr}
\=\=     {symbolToken T_Equals}
\!       {symbolToken T_Not}
\!\=     {symbolToken T_NEq}
\<       {symbolToken T_LAngle}
\>       {symbolToken T_RAngle}
\<\=     {symbolToken T_LAngleEq}
\>\=     {symbolToken T_RAngleEq}
\=       {symbolToken T_Eq}
\+\=     {symbolToken T_PlusEq}
\-\=     {symbolToken T_MinusEq}
\*\=     {symbolToken T_MulEq}
\/\=     {symbolToken T_DivEq}
\?\?\=   {symbolToken T_ExpEq}
\%\=     {symbolToken T_ModEq}
\&\=     {symbolToken T_BAndEq}
\|\=     {symbolToken T_BOrEq}
\^\=     {symbolToken T_BXorEq}
\<\<\=   {symbolToken T_LShiftEq}
\>\>\=   {symbolToken T_RShiftEq}
\&\&\=   {symbolToken T_LAndEq}
\|\|\=   {symbolToken T_LOrEq}
\+\+     {symbolToken T_PlusPlus}
\-\-     {symbolToken T_MinusMinus}
\.\.     {symbolToken T_Range}
\?       {symbolToken T_ThenE}
\:       {symbolToken T_ElseE}



-- Reserved Keywords

-- Basic Types
"int"        {keywordToken K_Int}
"float"      {keywordToken K_Float}
"char"       {keywordToken K_Char}
"bool"       {keywordToken K_Bool}
"string"     {keywordToken K_String}
"nil"        {keywordToken K_Nil}
-- Boolean Values
"false"      {keywordToken K_False}
"true"       {keywordToken K_True}
-- Control Constructs
"for"        {keywordToken K_For}
"while"      {keywordToken K_While}
"if"         {keywordToken K_If}
"then"       {keywordToken K_Then}
"else"       {keywordToken K_Else}
"elsif"      {keywordToken K_Elsif}
"unless"     {keywordToken K_Unless}
"return"     {keywordToken K_Return}
"break"      {keywordToken K_Break}
"next"       {keywordToken K_Continue}
"until"      {keywordToken K_Until}
"loop"       {keywordToken K_Loop}
"in"         {keywordToken K_In}
-- Block Delimiters
"do"         {keywordToken K_Do}
"begin"      {keywordToken K_Begin}
"end"        {keywordToken K_End}
-- Function Declaration
"def"        {keywordToken K_Def}
-- Catch
"rescue"     {keywordToken K_Catch}
-- Parameter Passing Modalities
"val"        {keywordToken K_Val}
"const"      {keywordToken K_Const}
"valres"     {keywordToken K_ValeRes}
"ref"        {keywordToken K_Ref}
"res"        {keywordToken K_Res}


<0>             (=begin\n ($univ*\n)* =end\n)?               { begin whitespace } -- Start Of File Multi Line Comment
<whitespace>    [$white # \n]+                               { skip } -- Whitespace
                \#.*\n?                                      { skip } -- Single Line Comment
                \n=begin\n ($univ*\n)* =end\n                { skip } -- Multi Line Comment
$digit+                                                      {readToken $ T_Int . read} -- Int
$digit+ \. $digit+                                           {readToken $ T_Double . read} -- Double
\' ([$univ # \\] | \\ $escaped) \'                           {readToken $ T_Char . fst . head . readLitChar . tail . init } -- Char
\" ([$univ # [\" \\ \n]] | (\\ (\" | \\ | \' | n | t)))* \"  {readToken $ T_String . tail . init} -- String
[$lower $upper _][$lower $upper $digit _]*                   {readToken T_Ident} -- Identifier


{

data TokenType = T_Ident  String
               | T_RSymb  T_Symbol
               | T_RWrds  T_Keyword
               | T_Int    Int
               | T_Double Double
               | T_Char   Char
               | T_String String
               | T_EOF
    deriving (Eq,Show)

type AnToken = Annotated TokenType

data T_Symbol
  = T_NewLine    -- \n
  | T_LParen     -- (
  | T_RParen     -- )
  | T_LSquare    -- [
  | T_RSquare    -- ]
  | T_LBrace     -- {
  | T_RBrace     -- }
  | T_Comma      -- ,
  | T_Dot        -- .
  | T_SemiCol    -- ;
  -- Arithmetic Operators
  | T_Plus       -- +
  | T_Minus      -- -
  | T_Prod       -- *
  | T_Div        -- /
  | T_Exp        -- ??
  | T_Mod        -- %
  -- Bitwise Operators
  | T_BAnd       -- &
  | T_BOr        -- |
  | T_BXor       -- ^
  | T_BComp      -- ~
  | T_LShift     -- <<
  | T_RShift     -- >>
  -- Logical Operators
  | T_LAnd       -- &&
  | T_LOr        -- ||
  -- Comparison Operators
  | T_Equals     -- ==
  | T_Not        -- !
  | T_NEq        -- !=
  | T_LAngle     -- <
  | T_RAngle     -- >
  | T_LAngleEq   -- <=
  | T_RAngleEq   -- >=
  -- Assignment
  | T_Eq         -- =
  -- Arithmetic Assignment Operators
  | T_PlusEq     -- +=
  | T_MinusEq    -- -=
  | T_MulEq      -- *=
  | T_DivEq      -- /=
  | T_ExpEq      -- ??=
  | T_ModEq      -- %=
  -- Bitwise Assignment Operators
  | T_BAndEq     -- &=
  | T_BOrEq      -- |=
  | T_BXorEq     -- ^=
  | T_LShiftEq   -- <<=
  | T_RShiftEq   -- >>=
  -- Logical Assignment Operators
  | T_LAndEq     -- &&=
  | T_LOrEq      -- ||=
  -- Others
  | T_RArrow     -- ->
  | T_PlusPlus   -- ++
  | T_MinusMinus -- --
  | T_Range      -- ..
  | T_ThenE      -- ?
  | T_ElseE      -- :
  deriving Eq

instance Show T_Symbol where
  show s = ss s
    where
      ss T_NewLine    = "\n"
      ss T_LParen     = "("
      ss T_RParen     = ")"
      ss T_LSquare    = "["
      ss T_RSquare    = "]"
      ss T_LBrace     = "{"
      ss T_RBrace     = "}"
      ss T_Comma      = ","
      ss T_Dot        = "."
      ss T_SemiCol    = ";"
      ss T_Plus       = "+"
      ss T_Minus      = "-"
      ss T_Prod       = "*"
      ss T_Div        = "/"
      ss T_Exp        = "??"
      ss T_Mod        = "%"
      ss T_BAnd       = "&"
      ss T_BOr        = "|"
      ss T_BXor       = "^"
      ss T_BComp      = "~"
      ss T_LShift     = "<<"
      ss T_RShift     = ">>"
      ss T_LAnd       = "&&"
      ss T_LOr        = "||"
      ss T_Equals     = "=="
      ss T_Not        = "!"
      ss T_NEq        = "!="
      ss T_LAngle     = "<"
      ss T_RAngle     = ">"
      ss T_LAngleEq   = "<="
      ss T_RAngleEq   = ">="
      ss T_Eq         = "="
      ss T_PlusEq     = "+="
      ss T_MinusEq    = "-="
      ss T_MulEq      = "*="
      ss T_DivEq      = "/="
      ss T_ExpEq      = "??="
      ss T_ModEq      = "%="
      ss T_BAndEq     = "&="
      ss T_BOrEq      = "|="
      ss T_BXorEq     = "^="
      ss T_LShiftEq   = "<<="
      ss T_RShiftEq   = ">>="
      ss T_LAndEq     = "&&="
      ss T_LOrEq      = "||="
      ss T_RArrow     = "->"
      ss T_PlusPlus   = "++"
      ss T_MinusMinus = "--"
      ss T_Range      = ".."
      ss T_ThenE      = "?"
      ss T_ElseE      = ":"


data T_Keyword
  -- Basic Types
  = K_Int          -- int
  | K_Float        -- float
  | K_False        -- false
  | K_Char         -- char
  | K_String       -- string
  | K_Nil          -- nil
  -- Boolean Values
  | K_Bool         -- bool
  | K_True         -- true
  -- Control Constructs
  | K_For          -- for
  | K_While        -- while
  | K_If           -- if
  | K_Then         -- then
  | K_Else         -- else
  | K_Elsif        -- elsif
  | K_Unless       -- unless
  | K_Return       -- return
  | K_Break        -- break
  | K_Continue     -- next
  | K_Repeat       -- repeat
  | K_Until        -- until
  | K_Loop         -- loop
  | K_In           -- in
  -- Block Delimiters
  | K_Do           -- do
  | K_Begin        -- begin
  | K_End          -- end
  -- Function Declaration
  | K_Def          -- def
  -- Catch
  | K_Catch        -- rescue
  -- Parameter Passing Modalities
  | K_Val          -- val
  | K_Const        -- const
  | K_ValeRes      -- valres
  | K_Ref          -- ref
  | K_Res          -- res
  | K_Name         -- name
  deriving Eq

instance Show T_Keyword where
  show s = ss s
    where
      -- Basic Types
      ss K_Int          = "int"
      ss K_Float        = "float"
      ss K_False        = "false"
      ss K_Char         = "char"
      ss K_String       = "string"
      ss K_Nil          = "nil"
      -- Boolean Values
      ss K_Bool         = "bool"
      ss K_True         = "true"
      -- Control Constructs
      ss K_For          = "for"
      ss K_While        = "while"
      ss K_If           = "if"
      ss K_Then         = "then"
      ss K_Else         = "else"
      ss K_Elsif        = "elsif"
      ss K_Unless       = "unless"
      ss K_Return       = "return"
      ss K_Break        = "break"
      ss K_Continue     = "next"
      ss K_Repeat       = "repeat"
      ss K_Until        = "until"
      ss K_Loop         = "loop"
      ss K_In           = "in"
      -- Block Delimiters
      ss K_Do           = "do"
      ss K_Begin        = "begin"
      ss K_End          = "end"
      -- Function Declaration
      ss K_Def          = "def"
      -- Catch
      ss K_Catch        = "rescue"
      -- Parameter Passing Modalities
      ss K_Val          = "val"
      ss K_Const        = "const"
      ss K_ValeRes      = "valres"
      ss K_Ref          = "ref"
      ss K_Res          = "res"
      ss K_Name         = "name"

-- ALEXINPUT DEFINITION

-- Posn records the location of a token in the input text.
-- It has two fields: the line and column number of a token within the file.

data AlexPosn = AlexPn {line :: !Int, column :: !Int}
    deriving(Eq, Show, Ord)

type Byte = Word8

data AlexState = AlexState {
    alex_pos :: [AlexPosn],
    alex_inp :: String,     -- the current input
    alex_chr :: !Char,      -- the character before the input
    alex_bytes :: [Byte],   -- rest of the bytes for the current char
    alex_scd :: !Int,       -- the current startcode
    alex_file :: [String]   -- current file name
}
    deriving(Show, Eq, Ord)

type AlexInput = AlexState

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (AlexState pos inp chr bs sc fn) = AlexState pos inp chr [] sc fn

-- Encode a Haskell Char to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

alexStartPos :: AlexPosn
alexStartPos = AlexPn 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn l c) '\t' = AlexPn  l     (((c+3) `div` 4)*4+1)    -- assuming that a tab consists of 4 spaces
alexMove (AlexPn l c) '\n' = AlexPn (l+1)   1
alexMove (AlexPn l c) _    = AlexPn  l     (c+1)

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (AlexState pos inp chr (b:bs) sc fn) = Just ( b , (AlexState pos inp chr (bs) sc fn) )
alexGetByte (AlexState pos [] chr [] sc fn) = Nothing
alexGetByte (AlexState ~(p:pos) (c:st) chr [] sc fn) = let p' = alexMove p c
                                                           (b:bs) = utf8Encode c
                                                       in p' `seq` Just (b, (AlexState (p':pos) st c bs sc fn))

-- returns the previous char
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexState pos inp chr (b:bs) sc fn) = chr


-- MONAD DEFINITION

-- This is the monad defined by alex with the %wrapper "monad" declaration

-- newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

-- This newtype declaration reminds the State Monad with the exception that
-- it wraps an Either type.
-- The Either type is used to represent a value which is either correct or an error
-- And the state monad is used to carry on a state, that, in our case, is the lexer's state
-- So we could define our Monad as follow

-- newtype Alex a = State AlexState

-- This would lead into a State {runState :: AlexState -> (a, AlexState)} definition of
-- the monad.
-- But this doesn't fit our requirements because we need to rapresent a computation
-- which may also fail with an error.
-- What is needed is a monad that combines the features of the State Monad and the
-- Except Monad into a single computation.
-- Quoting the haskell wiki https://wiki.haskell.org/All_About_Monads#Introduction_3
-- It is inefficient and poor practice to write a new monad instance with the required
-- characteristics each time a new combination is desired. Instead, we would prefer to
-- develop a way to combine the standard monads to produce the needed hybrids.
-- The technique that lets us do this is called:

-- *** MONAD TRANSFORMERS *** --

-- We want to istantiate a transformer that does what the alex monad does:

-- Given the definition of the StateT Monad and the ExceptT Monad:
-- StateT:
-- newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
-- ExceptT:
-- newtype ExceptT e m a = ExceptT (m (Either e a))
-- we try to combine them to obtain a new monad

-- StateT (s -> m1 (a, s))
-- let b = (a, s) in StateT (s -> m1 b)
-- let m1 be our Except Monad
-- m1 b = Except e m2 b = m2 (Either e b)
-- StateT (s -> ExceptT e m2 b)
-- StateT (s -> m2 (Either e b))
-- StateT (s -> m2 (Either e (a, s)))

-- This is a StateT s ExcpetT e m Monad transformer

newtype ParserT m a = ParserT {unParserT :: StateT AlexInput (ExceptT String m) a}
  deriving(Monad, MonadState AlexState, MonadError String, Functor, Applicative, MonadIO)

type Parser = ParserT IO

-- Monad runner
runParserT :: (Monad m) => ParserT m a -> AlexState -> m (Either String (a, AlexState))
runParserT p s = runExceptT (runStateT (unParserT p)  s)

-- Start the parsing on a String
run :: (Monad m) => String -> ParserT m a -> m (Either String a)
run input parser = runExceptT (evalStateT (unParserT parser) (getStartState input))

runOnFile :: String -> Parser a -> IO (Either String a)
runOnFile filename parser = do
  input <- readFile filename
  runExceptT (evalStateT (unParserT parser) (getStartStateFile filename input))

-- Given a string, returns the initial AlexState
getStartState :: String -> AlexState
getStartState inp = AlexState { alex_pos = alexStartPos:[],
                                alex_inp = inp,
                                alex_chr = '\n',
                                alex_bytes = [],
                                alex_scd = 0 ,
                                alex_file = ["<stdin>"]}

getStartStateFile :: String -> String -> AlexState
getStartStateFile filename inp = AlexState {
                                alex_pos = alexStartPos:[],
                                alex_inp = inp,
                                alex_chr = '\n',
                                alex_bytes = [],
                                alex_scd = 0 ,
                                alex_file = [filename]}

alexGetInput :: (Monad m) => ParserT m AlexInput
alexGetInput = get

alexSetInput :: (Monad m) => AlexInput -> ParserT m ()
alexSetInput i = put i

alexGetStartCode :: (Monad m) => ParserT m Int
alexGetStartCode =  gets alex_scd

alexSetStartCode :: (Monad m) => Int -> ParserT m ()
alexSetStartCode sc = modify $ \s -> s{alex_scd=sc}

-- Improved lexing function
-- Handles Source Locations for tokens
lexToken :: Parser AnToken
lexToken = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> return $ A noAnn T_EOF
    AlexError (AlexState pos _ _ _ _ f) -> throwError
                                        $  "Lexical error in file:"
                                        ++ (head f)
                                        ++ " at line "
                                        ++ (show $ line $ head pos)
                                        ++ ", column " ++ (show $ column $ head pos)
    AlexSkip  inp' len -> do
        alexSetInput inp'
        lexToken
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

alexMonadScan = lexToken

type LexerAction result = AlexInput -> -- Start location of the matched token
                          Int ->       -- Length of the match
                          Parser result


-- builds a RealSrcSpan from a SrcLoc
-- with the assumption that every token lies within a line
srcSpan:: LexerAction SrcSpan
srcSpan old_s len = let
  fn = head . alex_file $ old_s -- file name
  pos@(AlexPn l c) = head . alex_pos $ old_s -- begin of token
  in return . RealSrcSpan $ RealSrcSpan' fn l c l (c+len-1)

-- Builds a Token from a matched Symbol
symbolToken:: T_Symbol -> LexerAction AnToken
symbolToken t old_s len = do
  srcspan <- srcSpan old_s len
  return $ A noAnn{_loc=srcspan} . T_RSymb $ t

-- Builds a Token from a matched Keyword
keywordToken:: T_Keyword -> LexerAction AnToken
keywordToken t old_s len = do
  srcspan <- srcSpan old_s len
  return $ A noAnn{_loc=srcspan} . T_RWrds $ t

-- Builds a Token using the function given as argument
readToken:: (String -> TokenType) -> -- specifies how to build the token from the matched input string
            LexerAction AnToken
readToken f old_s len = do
  srcspan <- srcSpan old_s len
  return $ A noAnn{_loc=srcspan} tok
  where
    tok = f . take len . alex_inp $ old_s

-- just ignore this token and scan another one
skip :: LexerAction AnToken
skip input len = alexMonadScan

-- ignore this token, but set the start code to a new value
begin :: Int -> LexerAction AnToken
begin code input len = do alexSetStartCode code; alexMonadScan
}

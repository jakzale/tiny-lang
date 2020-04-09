{-|
= Raw Parser

A simple parser for language involving booleans, field elements, and
vectors.

Given that the language is simple enough we are mixing lexical
analysis and grammar a bit.  To avoid any ambiguities between lexical
analysis and grammar we will explicitly mark all lexical tokens.

Please note that expressions are the designated start production for
the parser.

As convention we enclose /tokens/ in parentheses @""@.

== Literals

Literals are defined as follows

@
bool-literal ::= "T" | "F"
vec-literal  ::= "{" (bool-literal ("," bool-literal)*)? "}"
int-literal  ::= [0-9]+
@

== Keywords

Here is a list of keywords used in the language.

@
keyword ::=
    "and"
    "assert"
    "do"
    "else"
    "end"
    "for"
    "if"
    "inv"
    "let"
    "neq0"
    "not"
    "or"
    "then"
    "to"
    "unpack"
    "xor"
    "field"
    "bool"
    "vector"
@

== Types/Universes

Currently we only support 3 types/universes:

* booleans,
* fields, and
* vectors.

We use them to annotate expressions with their desired type.

@
uni ::=
    "bool"
    "field"
    "vector"
@

== Identifiers

To avoid a syntactic clash with /bool-literals/, identifiers for field
variables start with a lower-case letter.  Identifiers for boolean
variables are prefiex by @?@ and identifiers for vector variables are
prefixed by @#@.  We will check if an identifier is not a keyword.

@
ident ::= ( "?" | "#" ) [a-z] ([a-z0-9_'])*
@

== Constants

At the moment constants consist of literals only.

@
const ::=
    bool-literal
    field-literal
    vec-literal
@

Parsing rules for @field-literal@ are determined by the type class
instance of @TextField@.

In addition, we also permit @int-literals@ in for loops in statements
below.


== Expressions

As mentioned above @expr@ is the designated start production for the
parser.

@
expr ::=
    const
    "(" expr ")"
    ident
    expr infix-op expr
    prefix-op expr
    expr "[" expr "]"
    statement ";" expr
    "if" expr "then" expr "else" expr
    expr ":" uni

infix-op ::=
    "and"
    "or"
    "xor"
    "=="
    "<="
    "<"
    ">="
    ">"
    "+"
    "-"
    "*"
    "/"

prefix-op ::=
    "not"
    "neq0"
    "neg"
    "inv"
    "unpack"
@

== Statement

Program are a bit odd at the moment, as they are neither
expressions nor the usual statements.

@
statement ::=
    "let" var "=" expr
    "assert" expr
    "for" var "=" int-literal "to" int-literal "do" statements "end"

statements ::=
    (statement (";" statement)*)?
@

== Operator Precedence

We use the following operator precedence:

- unary operators,
- vector element access ([]),
- @*, /@,
- @+, -@,
- arithmetic comparison operators, and
- boolean operators.

The code is based on the tutorial at
https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

See also https://markkarpov.com/megaparsec/megaparsec.html
-}


module TinyLang.Field.Raw.Parser
    ( pTop
    , pTopExpr
    ) where

import           TinyLang.Prelude               hiding (many, option, try)

import           Data.Field
import           TinyLang.Field.Existential
import           TinyLang.Field.Raw.Core
import           TinyLang.Field.UniConst
import           TinyLang.ParseUtils

import qualified Control.Monad.Combinators.Expr as Comb
import           Data.Set                       (fromList, member)
import qualified Data.Vector                    as Vector
import           Text.Megaparsec
import           Text.Megaparsec.Char

type ParserT = ParsecT Void String

{-| == Lexer
-}

-- Identifier Character
isIdentifierChar :: Char -> Bool
isIdentifierChar c =
    any (\f -> f c) [isLower , isDigit, (=='_'), (=='\'')]

-- Parser label for identifier charactersr
identifierCharLabel :: String
identifierCharLabel = "identifier character [a-z0-9_']"

identifierChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
identifierChar = satisfy isIdentifierChar <?> identifierCharLabel

keyword :: String -> ParserT m ()
keyword kwd = lexeme (string kwd *> notFollowedBy identifierChar)

keywords :: Set String
keywords =
    fromList
    [ "T", "F"
    , "not", "and", "or", "xor"
    , "neq0", "neg", "inv"
    , "unpack"
    , "let"
    , "assert"
    , "for", "do", "end"
    , "if", "then", "else"
    , "bool", "field", "vector"
    ]

isKeyword :: String -> Bool
isKeyword = (`member` keywords)

pIdentifier :: ParserT m Identifier
pIdentifier =
    lexeme $ do
        prefix     <- option "" (string "?" <|> string "#")
        identifier <- (:) <$> lowerChar
                          <*> takeWhileP (Just identifierCharLabel)
                              isIdentifierChar
        pure $ prefix ++ identifier

pBoolLiteral :: ParserT m Bool
pBoolLiteral =
    lexeme $ charToBool <$> (satisfy isTF <?> "T or F")
    where
        isTF x = x == 'T' || x == 'F'
        --
        charToBool 'T' = True
        charToBool 'F' = False
        charToBool _   = error "impossible"

pVecLiteral :: ParserT m (Vector Bool)
pVecLiteral =
    lexeme $
        Vector.fromList <$>
        between
            (symbol "{")
            (symbol "}")
            (pBoolLiteral `sepBy` (symbol ","))

pIntLiteral :: ParserT m Integer
pIntLiteral = signedDecimal

-- variable is an identifier that is not a keyword
pVar :: ParserT m Var
pVar = do
    ident <- pIdentifier
    when (isKeyword ident)
         (fail ("keyword " ++ show ident ++ " cannot be an identifier"))
    pure $ Var ident


{-| == Parser
-}

unary :: ParserT m a -> UnOp -> Comb.Operator (ParserT m) (RawExpr f)
unary pName op = Comb.Prefix (EAppUnOp op <$ pName)

binary :: ParserT m a -> BinOp -> Comb.Operator (ParserT m) (RawExpr f)
binary pName op = Comb.InfixL (EAppBinOp op <$ pName)

pIndex :: TextField f => ParserT m (RawExpr f)
pIndex = brackets pExpr

operatorTable :: TextField f => [[Comb.Operator (ParserT m) (RawExpr f)]]
operatorTable =
    [ [ unary  (keyword "not")    $ Not
      , unary  (keyword "neg")    $ Neg
      , unary  (keyword "inv")    $ Inv
      , unary  (keyword "neq0")   $ Neq0
      , unary  (keyword "unpack") $ Unp
      ]
      -- expr [ expr ]
    , [ Comb.Postfix (EAppBinOp BAt <$> pIndex)
      ]
    , [ binary (symbol  "*")      $ Mul
      , binary (symbol  "/")      $ Div
      ]
    , [ binary (symbol  "+")      $ Add
      , binary (symbol  "-")      $ Sub
      ]
    , [ binary (symbol  "==")     $ FEq
      , binary (symbol  "<=")     $ FLe
      , binary (symbol  "<")      $ FLt
      , binary (symbol  ">=")     $ FGe
      , binary (symbol  ">")      $ FGt
      ]
    , [ binary (keyword "xor")    $ Xor
      , binary (keyword "and")    $ And
      , binary (keyword "or")     $ Or
      ]
      -- : uni
    , [ Comb.Postfix (ETypeAnn <$> pAnn)
      ]
    ]

vBool :: ParserT m (SomeUniConst f)
vBool  = Some . (UniConst Bool)   <$> pBoolLiteral

vVec :: ParserT m (SomeUniConst f)
vVec   = Some . (UniConst Vector) <$> pVecLiteral

vField :: TextField f => ParserT m (SomeUniConst f)
vField = Some . (UniConst Field)  <$> parseField

pConst :: TextField f => ParserT m (SomeUniConst f)
pConst = choice
    [ vBool
    , vVec
    , vField
    ]


pAnn :: ParserT m (SomeUni f)
pAnn = symbol ":" *> pSomeUni

pSomeUni :: ParserT m (SomeUni f)
pSomeUni = choice
    [ Some Bool   <$ keyword "bool"
    , Some Field  <$ keyword "field"
    , Some Vector <$ keyword "vector"
    ]

pTerm :: TextField f => ParserT m (RawExpr f)
pTerm =
    choice
    -- This can backtrack for parentheses
    [ try (EConst   <$> pConst)
    -- This can backtrack for keywords
    , try (EVar     <$> pVar)
    , EIf           <$> (keyword "if"   *> pExpr)
                    <*> (keyword "then" *> pExpr)
                    <*> (keyword "else" *> pExpr)
    , parens pExpr
    ]

pExpr :: TextField f => ParserT m (RawExpr f)
pExpr = Comb.makeExprParser pTerm operatorTable

pStatement :: TextField f => ParserT m (RawStatement f)
pStatement =
    choice
    -- This can backtrack for expr starting with a "("
    [ try (parens pStatement)
    , ELet    <$> (keyword "let"    *> pVar)
              <*> (symbol  "="      *> pExpr)
    , EAssert <$> (keyword "assert" *> pExpr)
    , EFor    <$> (keyword "for"    *> pVar)
              <*> (symbol  "="      *> pIntLiteral)
              <*> (keyword "to"     *> pIntLiteral)
              <*> (keyword "do"     *> pStatements)
              <*   keyword "end"
    ]

pStatements :: TextField f => ParserT m (RawStatements f)
pStatements =
    choice
    -- This can backtrack for statement starting with a "("
    [ try (parens pStatements)
    , Statements <$> many (pStatement <* symbol ";")
    ]

pProgram :: TextField f => ParserT m (RawProgram f)
pProgram = Program <$> pStatements

pTop :: TextField f => ParserT m (RawProgram f)
pTop = top pProgram

pTopExpr :: TextField f => ParserT m (RawExpr f)
pTopExpr = top pExpr
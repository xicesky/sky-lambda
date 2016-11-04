
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

module Sky.Lambda.Parser where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Sky.Lambda.AST

----------------------------------------------------------------------------------------------------
-- Definitons

-- reservedWords :: [String]
-- reservedWords = ["def"]

----------------------------------------------------------------------------------------------------
-- "Lexer"

-- | Consume spaces
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"


-- | 'lexeme' parses something that might be followed by spaces
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | 'symbol' parses a fixed string
symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme $ try $ (:) <$> letterChar <*> many alphaNumChar

----------------------------------------------------------------------------------------------------
-- "Parser"

type ExprParser a = Parser a -> Parser a

--ps :: Parser a -> P a -> Parser a
--ps expr primary = expr primary <|> primary

couldBe :: ExprParser a -> ExprParser a
couldBe p base = p base <|> base

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest where
    rest x = (do
        f <- op
        y <- p
        rest (f x y)
        ) <|> return x

moduleParser :: Parser [LDefinition String]
moduleParser = sc *> some definition <* eof

definition :: Parser (LDefinition String)
definition = label "Definition" $ do
    symbol "def"
    name <- identifier
    params <- many identifier <* symbol "="
    body <- expression <* symbol ";"
    return $ mkDef name params body

mkDef :: String -> [String] -> LExpression String -> LDefinition String
mkDef name [] body = LDefinition name body
mkDef name ps body = LDefinition name (LLambda ps body)

expression :: Parser (LExpression String)
expression = foldr id primaryExpression [lambdaExpression, appExpression]

lambdaExpression :: ExprParser (LExpression String)
lambdaExpression = couldBe $ \base -> label "Lambda expression" $ do
    symbol "\\"
    params <- some identifier <* symbol "."
    body <- base
    return $ LLambda params body

appExpression :: ExprParser (LExpression String)
appExpression base = chainl1 base $ return LApplication

primaryExpression :: Parser (LExpression String)
primaryExpression = choice
    [ parens expression
    , variable
    ]

variable :: Parser (LExpression String)
variable = LVariable <$> identifier

----------------------------------------------------------------------------------------------------
-- Helpers (for testing)

testParser :: ( ShowErrorComponent e
             , Ord (Token s)
             , ShowToken (Token s)
             , Show a )
    => Parsec e s a
    -> s
    -> a
testParser p input = case parse p "" input of
    Left  e -> error $ parseErrorPretty e
    Right x -> x


{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

module Sky.Lambda.PrettyPrinter
    ( Doc
    , PrettyPrec (..)
    , Pretty (..)
    ) where

import Text.PrettyPrint.Leijen

import Sky.Lambda.AST

----------------------------------------------------------------------------------------------------
-- Prettyprinting with precedence

class PrettyPrec a where
    prettyPrec :: a -> Int -> Doc

prec :: Int -> Doc -> Int -> Doc
prec p inner actual = (if actual > p then parens else id) inner

anyPrec :: Doc -> Int -> Doc
anyPrec = const

----------------------------------------------------------------------------------------------------
-- Prettyprinter

instance PrettyPrec (LExpression String) where
    prettyPrec :: LExpression String -> Int -> Doc
    prettyPrec (LVariable v)        = anyPrec   $ text v
    prettyPrec (LApplication f x)   = prec 1    $ prettyPrec f 1 <+> prettyPrec x 0
    prettyPrec (LLambda ps body)    = prec 2    $ text "\\" <> hsep (map text ps) <> text "." <+> prettyPrec body 1

instance Pretty (LExpression String) where
    pretty :: LExpression String -> Doc
    pretty a = prettyPrec a 9999

instance Pretty (LDefinition String) where
    pretty (LDefinition name body) = text "def" <+> text name <+> text "=" <+> pretty body <> text ";"

instance Pretty [LDefinition String] where
    pretty xs = vsep $ fmap pretty xs


{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

module Main where

import Sky.Lambda.AST
import Sky.Lambda.Parser
import Sky.Lambda.PrettyPrinter
import Sky.Lambda.Eval

main :: IO ()
main = putStrLn "WIP"

parse :: String -> LExpression String
parse = testParser expression

test_parser_1 :: [LDefinition String]
test_parser_1 = testParser moduleParser "def a y = \\x.x y ; def b = a ;"

test_parser_2 :: LExpression String
test_parser_2 = testParser expression "\\x.x y"

test_pretty_1 :: Doc
test_pretty_1 = pretty test_parser_1

test_pretty_2 :: Doc
test_pretty_2 = pretty test_parser_2

test_substitute_1 :: LExpression String
test_substitute_1 = substitute "x" (LVariable "r") test_parser_2

test_substitute_2 :: LExpression String
test_substitute_2 = substitute "y" (LVariable "r") test_parser_2

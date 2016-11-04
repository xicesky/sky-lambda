
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
--{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Sky.Lambda.AST where

import Sky.Util.Container

----------------------------------------------------------------------------------------------------
-- AST definiton

data LExpression i
    = LVariable i
    | LLambda [i] (LExpression i)
    | LApplication (LExpression i) (LExpression i)
    -- LambdaRef r       -- Reference 
    deriving (Eq, Show)

data LDefinition i
    = LDefinition i (LExpression i)
    deriving (Eq, Show)

-- type LModule i
--     = HashMap i (LExpression i)

----------------------------------------------------------------------------------------------------
-- AST tools

llambda :: [n] -> LExpression n -> LExpression n
llambda ns (LLambda ns' e) = llambda (ns ++ ns') e
llambda [] e = e
llambda ns e = LLambda ns e

{-
----------------------------------------------------------------------------------------------------

data UnpackedExpr name expr
    = LVariable name
    | LLambda name expr
    | LApplication expr expr

class LambdaExpr e where
    type NameT e :: *
    lVariable :: NameT e -> e
    lLambda :: NameT e -> e -> e
    lApplication :: e -> e -> e
    unpackExpr :: e -> UnpackedExpr (NameT e) e


LVariable :: Name -> Expression
LLambda :: Name -> Expression -> Expression
LApplication :: Expression -> Expression -> Expression

-}

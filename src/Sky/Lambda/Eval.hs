
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-} -- We need to compare KeyT and ValueT

module Sky.Lambda.Eval where

import Prelude hiding (lookup, (!!))

import Sky.Util.Container
import Sky.Lambda.AST

type Expr = LExpression String

--data LValue = -- ???
--type LValue = LScope -> LExpression String
--type LScope = HashMap String LValue

substituteMap :: forall map. (MapLike map, KeyT map ~ String, ValueT map ~ Expr) => map -> Expr -> Expr
substituteMap m e | isEmpty m = e
substituteMap m e@(LVariable name) = case lookup name m of
    Nothing     -> e
    Just value  -> value
substituteMap m e@(LLambda vns x) = LLambda vns (substituteMap m' x) where
    -- Delete all bound names from the map
    m' = foldr mapDelete m vns
substituteMap m e@(LApplication f x)
    = LApplication (substituteMap m f) (substituteMap m x)

substitute :: String -> Expr -> Expr -> Expr
substitute k v = substituteMap $ (singleton (k,v) :: HashMap String Expr)

-- type LazyExpr = (HashMap String Expr, Expr)


-- loadDefs :: [LDefinition String] -> HashMap String LazyExpr
-- loadDefs defs = 

{-
substitute v value e@(LVariable v')
    | v == v'   = value
    | otherwise = e
substitute v value e@(LLambda vns x)
    | vns `contains` v  = e
    | otherwise         = LLambda vns (substitute v value x)
substitute v value e@(LApplication f x)
    = LApplication (substitute v value f) (substitute v value x)
-}

evalExpr_step :: Expr -> Expr
evalExpr_step (LApplication (LLambda (v:vs) inner) x) = substitute v x $ llambda vs inner
evalExpr_step (LApplication f x) = LApplication (evalExpr_step f) x
evalExpr_step e = e

evalExpr :: Expr -> Expr
evalExpr e = case evalExpr_step e of
    e'  | e == e'   -> e
    e'              -> evalExpr e'

{-
evalInModule_step :: HashMap String Expr -> Expr -> Expr
evalInModule_step m (LApplication (LLambda (v:vs) inner) x) = substitute v x $ llambda vs inner
evalInModule_step m (LApplication f x) = LApplication (evalExpr_step f) x
evalInModule_step m e@(LVariable name) = case lookup name m of
    Nothing     -> e
    Just value  -> value
evalInModule_step m e = e
-}

{-
evalExpr :: LExpression String -> LScope -> LValue
evalExpr (LVariable v) scope = case lookup v scope of
    Nothing -> error $ "Variable not in scope: " ++ v
    Just x -> evalExpr scope x

eval (LApplication (LLambda [n] body) x) scope = 




evalExpr :: LScope -> LExpression String -> LValue
evalExpr scope (LVariable v) = case lookup v scope of
    Nothing -> error $ "Variable not in scope: " ++ v
    Just x -> evalExpr scope x
evalExpr scope (LApplication f x) = case evalExpr scope f of
    LLambda [n] body    -> evalExpr (mapInsert n x scope) body
    LLambda (n:ns) body -> LLambda ns $ evalExpr (mapInsert n x scope) body
evalExpr scope e@(LLambda _ _) = e  -- WRONG: Scope missing in e

eval

eval :: LExpression String -> LValue
eval = evalExpr empty
-}

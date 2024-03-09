module Mentat.ExpressionParser (parseExpr) where

import Mentat.ParseTypes
import Mentat.Tokenizer


-- | Parses TokTree list into expression
parseExpr :: [TokTree] -> Either Error Expr
parseExpr tokens = shuntingYard tokens [] []

-- | helper for parseExpr
shuntingYard :: [TokTree] -> [Expr] -> [BinOp] -> Either Error Expr
shuntingYard [] exprs ops
  -- if there are no more tokens
 = do
  (mergedExprs, mergedOps) <- combineExprs exprs ops Nothing -- merge the remaing exprs and ops
  case length mergedExprs == 1 && length mergedOps == 0 -- if there are extra exprs or ops throw error
        of
    True -> Right $ head mergedExprs -- if no extras return expr
    False -> Left EmptyExpr -- if there are extras throw error
shuntingYard (toT:restT) exprs ops =
  case toT of
    TLeaf (TNumber n) -> shuntingYard restT (LitE (RL n) : exprs) ops
    TLeaf (TId i) -> shuntingYard restT (VarE i : exprs) ops
    TLeaf (TOp op) -> do
      (mergedExprs, mergedOps) <- combineExprs exprs ops $ Just op
      shuntingYard restT mergedExprs mergedOps
    TNode _ innerTok -> do
      innerExpr <- shuntingYard innerTok [] []
      shuntingYard restT (innerExpr : exprs) ops
    TFxn name args -- if you encounter a fxn
     -> do
      argExprs <- mapM (\x -> shuntingYard x [] []) args
      shuntingYard restT (FxnE name argExprs : exprs) ops
    _ -> Left EmptyExpr

-- | Helper for shuntingYard
buildOpExpr :: BinOp -> Expr -> Expr -> Expr
buildOpExpr op e1 e2 =
  if opLeftAssoc op
    then BinOpE op e1 e2
    else BinOpE op e2 e1

-- | Helper for shuntingYard
-- | Takes in an expresssion stack operator stack and a maybe BinOp of the last operator hit.
-- | Returns the new Expression Stack and the new Operator Stack.
combineExprs ::
     [Expr] -> [BinOp] -> Maybe BinOp -> Either Error ([Expr], [BinOp])
combineExprs [] [] _ = Left EmptyExpr -- if both stacks are empty, and there is an op
combineExprs [] (op:rOps) _ = Left $ BadOp $ op -- If there are not enough expressions on the stack
combineExprs exprs [] op =
  case op -- If there are exprs left and no ops on the stack
        of
    Nothing ->
      case length exprs == 1 -- If op is Nothing, means all tokens are parsed
            of
        True -> Right (exprs, []) -- if tokens are constructed into a single tree return it
        False -> Left $ BadExpr $ exprs -- if not return error
    Just op -> Right (exprs, [op]) -- if op add to stack and return
combineExprs (exprL:exprR:rest) (op2:rops) maybeOp =
  case maybeOp -- if there are enought expression on the stack
        of
    Just op1 ->
      case popOp op1 op2 of
        True ->
          combineExprs (buildOpExpr op2 exprL exprR : rest) (rops) $ Just op1 -- If op1 has presidence over op2 combine the two expressions and put the new expression on the stack
        False -> Right ((exprL : exprR : rest), (op1 : op2 : rops))
    Nothing -> do
      combineExprs (buildOpExpr op2 exprL exprR : rest) rops Nothing
combineExprs _ _ _ = Left EmptyExpr


{-# LANGUAGE OverloadedStrings #-}

module Mentat.FunctionBuilder where

import Mentat.ParseTypes
import Mentat.ProgramTypes
import Mentat.Program
import Mentat.Evaluator
import Mentat.Validator
import Formatting
import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as HM

-- | by conventaion all names will start with "mentatFunc" then have the function name
-- | the hash map containing varriable values will always be the first argument of a fucntion and be called "mentatVars"

-- | The inLT.unpack $ formation needed to construct a javascript function 
-- | Takes the name,  n_args, arg_names, body


translateLit :: Literal -> String
translateLit (RL n) = show n
translateLit (BoolL True) = "true"
translateLit (BoolL False) = "false"


translateVar :: String -> String
translateVar s = LT.unpack $ format ("mentatVars.get(" % string % ")") s


translateOp :: BinOp -> String
translateOp Add = "+"
translateOp Sub = "-"
translateOp Mul = "*"
translateOp Div = "/"
translateOp Exp = "**"
translateOp (Comp Eql) = "==="
translateOp (Comp NEq) = "!="
translateOp (Comp GEq) = ">="
translateOp (Comp G) = ">"
translateOp (Comp LEq) = "<="
translateOp (Comp L) = "<"


translateArgString :: [String] -> String
translateArgString [] = ""
translateArgString (s: ss) = if null ss then s else s ++ ", " ++ translateArgString ss

translateExpr :: Expr -> [String] -> String
translateExpr (LitE lit) _ = translateLit lit
translateExpr (VarE var) args = if var `elem` args then var else translateVar var
translateExpr (BinOpE op (LitE left) (LitE right)) _ =
  case applyBinOp op left right of
    Right result -> translateLit result
    Left err -> error $ "unexpected error: " ++ show err
translateExpr (BinOpE op left right) args = do
  let leftText = translateExpr left args
  let rightText = translateExpr right args
  let opStr = translateOp op
  LT.unpack $ format ("(" % string % ")" % string % "(" % string % ")") leftText opStr rightText
translateExpr (FxnE name argExprs) args = do
  let transArgs = map (\x -> translateExpr x []) argExprs
  let argStr = translateArgString transArgs
  LT.unpack $ format (" mentatFunc" % string % "(mentatVars, " % string % ")") name argStr


translateFunction :: Function -> String
translateFunction (Function name args expr) = do
  let transExpr = translateExpr expr args
  let transArgs = LT.unpack $ format ("(mentatVars, " % string % ")") $ translateArgString args
  let body = LT.unpack $ format ("return (" % string % ")") transExpr
  let nameJS = LT.unpack $ format ("mentatFunc" % string) name
  LT.unpack $ format ("function " % string % " { " % string % " }") (nameJS ++ transArgs) body


data ConstraintTransed = ConstraintTransed String String CompOp deriving(Show)


-- | Takes in a program and outputs translated functions, constraints and expressions
translateProgram :: Program -> [String] -> Either Error ([String], [ConstraintTransed], [String])
translateProgram pg domainVars = do

  let funcs = getPgFxns pg
  let transFuncs = map translateFunction $ HM.elems funcs

  let cstrs = getPgCstrs pg
  
  let transCstrs = map (\(Constraint left right comp) -> ConstraintTransed (translateExpr left domainVars) (translateExpr right domainVars) comp) cstrs
  
  let transExpr = map (\x -> translateExpr x []) $ getPgExprs pg

  Right (transFuncs, transCstrs, transExpr)


module Mentat.Validator where

import Mentat.ParseTypes
import Mentat.SyntaxParser
import Mentat.Evaluator
import qualified Data.Map.Strict as HM
import Data.Traversable (sequence)


data EvalType
  = RealT
  | BoolT
  | Any
  deriving (Show, Eq)


getLitType :: Literal -> EvalType
getLitType (RL _) = RealT
getLitType (BoolL _) = BoolT


validateVars :: Program -> Either Error (HM.Map String EvalType)
validateVars pg = do
  vars <- parseVariables pg
  fxns <- parseFxns pg
  let varEvals = sequence $ HM.map (\x -> evalExpr x vars fxns 1000) vars
  case varEvals of
    Left err -> Left err
    Right evals -> Right $ HM.map getLitType evals

-- In future do type checks

validateProgram :: Program -> Either Error ()
validateProgram pg = do
  varTypes <- validateVars pg
  let pgStat = getProgramStatments pg

  vars <- parseVariables pg
  fxns <- parseFxns pg

  exprVals <- sequence $ map (\x -> evalExpr x vars fxns 1000) $ filterExprs pgStat
  cstrEval <- sequence $ map (\x -> evalExpr x vars fxns 1000) $ filterConstraints pgStat

  pure ()


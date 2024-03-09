module Mentat.Validator (validateProgram) where

import Mentat.ParseTypes
import Mentat.ProgramTypes
import Mentat.Evaluator
import qualified Data.Map.Strict as HM
import Data.Traversable (sequence)


getLitType :: Literal -> EvalType
getLitType (RL _) = RealT
getLitType (BoolL _) = BoolT


validateVars :: [Statment] -> Either Error (HM.Map String EvalType)
validateVars pg = do
  let varStats = filterVarStats pg
  let vars = HM.fromList $ map (\(PgVar i val) -> (i, val)) varStats

  let fxnStats = filterFxnStats pg
  let fxns = HM.fromList $ map (\(PgFxn i val) -> (i, val)) varStats
  
  let varEvals = sequence $ HM.map (\x -> evalExpr x vars fxns 1000) vars
  case varEvals of
    Left err -> Left err
    Right evals -> Right $ HM.map getLitType evals

-- In future do type checks


validateProgram :: [Statment] -> Either Error ()
validateProgram pg = do
  varTypes <- validateVars pg

  let varStats = filterVarStats pg
  let vars = HM.fromList $ map (\(PgVar i val) -> (i, val)) varStats

  let fxnStats = filterFxnStats pg
  let fxns = HM.fromList $ map (\(PgFxn i val) -> (i, val)) fxnStats

  exprVals <- sequence $ map (\(PgExpr x) -> evalExpr x vars fxns 1000) $ filterExprStats pg
  -- TODO: build system for testsing cstrs
  pure ()


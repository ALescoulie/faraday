module FunctionBuilderSpec where

import Mentat.ParseTypes
import Mentat.ProgramTypes
import Mentat.Program
import Mentat.FunctionBuilder
import Test.Hspec

spec :: Spec

spec = do
  describe "Test translate Expr" $ do
    let pg1 = parseProgram [ "a := 1", "b := 2", "f(x) := (2a + 2b)^x", "2x + y", "y = 2x", "f(2) + 1" ] ["x", "y"]
    let pg1Trans = pg1 >>= \x -> translateProgram x ["x", "y"]
    
    case pg1Trans of
      Left err -> error $ "unexpected error" ++ show err
      Right (fxns, cstrs, exprs) -> do
        it "Parses functions" $ do
          fxns `shouldBe` [(TransFunciton "mentatFuncf" ["mentatVars", "mentatFuncs", "x"] "(((mentatVars.get(b))*(2.0))+((mentatVars.get(a))*(2.0)))**(x)")]
        it "Parses constraints" $ do
          cstrs `shouldBe` [TransConstraint (TransFunciton "MentatExprLeft" ["mentatVars", "MentatFuncs", "x", "y"] "(x)*(2.0)") (TransFunciton "MentatExprLeft" ["mentatVars", "MentatFuncs", "x", "y"] "y") Eql]
          
          
          



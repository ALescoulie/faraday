module FunctionBuilderSpec where

import Mentat.ParseTypes
import Mentat.ProgramTypes
import Mentat.Program
import Mentat.FunctionBuilder
import Test.Hspec

spec :: Spec

spec = do
  describe "Test translate Expr" $ do
    let pg1 = parseProgram [ "y := 1", "x := 2", "f(x) := 2x + 2", "2x + y", "y = 2x", "f(2) + 1" ] []
    let pg1Trans = pg1 >>= \x -> translateProgram x []
    
    case pg1Trans of
      Left err -> error $ "unexpected error" ++ show err
      Right (fxns, cstrs, exprs) -> do
        it "Parses functions" $ do
          fxns `shouldBe` [(TransFunciton "mentatFuncf" ["mentatVars", "mentatFuncs", "x"] "(2.0)+((x)*(2.0))")]
          
          
          



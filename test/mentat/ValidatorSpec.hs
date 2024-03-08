module ValidatorSpec where

import Control.Monad (forM_)
import Data.Foldable (toList)
import qualified Data.Map.Strict as HM
import Mentat.ParseTypes
import Mentat.SyntaxParser
import Prelude hiding (lex)
import Mentat.Validator
import Test.Hspec


spec :: Spec

spec = do
  describe "Testing varriable validation" $ do
    let pg1Input =
          [ "var1 := 1"
          , "var2 := var1 + 3"
          , "var3 := var2 = 4"
          ]

    let pg1 = parseProgram pg1Input

    let pg1ExpectedTypes = HM.fromList [("var1", RealT), ("var2", RealT), ("var3", BoolT)]

    let pg1VarTypes = pg1 >>= validateVars

    case pg1VarTypes of
      Left err -> error $ "unexpected error: " ++ show err
      Right varTypes -> it ("Types: " ++ show pg1Input) $ do
        varTypes `shouldBe` pg1ExpectedTypes



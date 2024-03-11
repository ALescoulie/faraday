module FunctionBuilderSpec where

import Data.Aeson
import Mentat.FunctionBuilder
import Mentat.ParseTypes
import Mentat.Program
import Mentat.ProgramTypes
import Test.Hspec

spec :: Spec
spec = do
  describe "Test translate Expr" $ do
    let pg1 =
          parseProgram
            [ "a := 1"
            , "b := 2"
            , "f(x) := (2a + 2b)^x"
            , "2x + y"
            , "y = 2x"
            , "f(2) + 1"
            ]
            ["x", "y"]
    let pg1Trans = pg1 >>= \x -> translateProgram x ["x", "y"]
    case pg1Trans of
      Left err -> error $ "unexpected error" ++ show err
      Right (TransProgram vars fxns cstrs exprs) -> do
        it "Parses functions" $ do
          fxns `shouldBe`
            [ (TransFunction
                 "f"
                 ["mentatVars", "mentatFuncs", "x"]
                 "(((mentatVars.get(b))*(2.0))+((mentatVars.get(a))*(2.0)))**(x)")
            ]
        it "Parses constraints" $ do
          cstrs `shouldBe`
            [ TransConstraint
                (TransFunction
                   "MentatExprLeft"
                   ["mentatVars", "MentatFuncs", "x", "y"]
                   "(x)*(2.0)")
                (TransFunction
                   "MentatExprLeft"
                   ["mentatVars", "MentatFuncs", "x", "y"]
                   "y")
                "Eql"
            ]
        it "Translates to JSON" $ do
          let pgJSON = toJSON $ TransProgram vars fxns cstrs exprs
          let pgExpected =
                object
                  (fromList
                     [ ( "constraints"
                       , Array
                           [ Object
                               (fromList
                                  [ ("comparison", String "Eql")
                                  , ( "left"
                                    , Object
                                        (fromList
                                           [ ( "args"
                                             , Array
                                                 [ String "mentatVars"
                                                 , String "MentatFuncs"
                                                 , String "x"
                                                 , String "y"
                                                 ])
                                           , ("body", String "(x)*(2.0)")
                                           , ("name", String "MentatExprLeft")
                                           ]))
                                  , ( "right"
                                    , Object
                                        (fromList
                                           [ ( "args"
                                             , Array
                                                 [ String "mentatVars"
                                                 , String "MentatFuncs"
                                                 , String "x"
                                                 , String "y"
                                                 ])
                                           , ("body", String "y")
                                           , ("name", String "MentatExprLeft")
                                           ]))
                                  ])
                           ])
                     , ( "expressions"
                       , Array
                           [ String
                               "(mentatVars.get(y))+((mentatVars.get(x))*(2.0))"
                           , String
                               "(1.0)+( mentatFunc.get(f)(mentatArgs, mentatFuncs, 2.0))"
                           ])
                     , ( "functions"
                       , Array
                           [ Object
                               (fromList
                                  [ ( "args"
                                    , Array
                                        [ String "mentatVars"
                                        , String "mentatFuncs"
                                        , String "x"
                                        ])
                                  , ( "body"
                                    , String
                                        "(((mentatVars.get(b))*(2.0))+((mentatVars.get(a))*(2.0)))**(x)")
                                  , ("name", String "f")
                                  ])
                           ])
                     , ( "varriables"
                       , Array
                           [ Array
                               [ String "a"
                               , Object
                                   (fromList
                                      [ ("contents", Number 1.0)
                                      , ("tag", String "TFloat")
                                      ])
                               ]
                           , Array
                               [ String "b"
                               , Object
                                   (fromList
                                      [ ("contents", Number 2.0)
                                      , ("tag", String "TFloat")
                                      ])
                               ]
                           ])
                     ])
          pgJson `shouldBe` pgExpected

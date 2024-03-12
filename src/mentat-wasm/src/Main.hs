--{-# LANGUAGE ForeignFunctionInterface #-}

module MentatInterface where

import Data.Aeson
import Data.Text.Encoding (decodeUtf8)
import Data.Either (fromRight)
import Data.Text as T
import GHC.Wasm.Prim

import Mentat.ParseTypes
import Mentat.ProgramTypes
import Mentat.Program
import Mentat.FunctionBuilder


translateMentatProgram :: [String] -> [String] -> Text
translateMentatProgram [] _ -> Right T.empty
translateMentatProgram pgText domainVars -> do
  let maybePg = ParseProgram pgText domainVars

  let transPg = maybePg >>= (\x -> translateProgram x domainVars)
  
  case transPg of
    Right pg -> decodeUtf8 $ encode pg
    Left err -> error $ "syntax error: " ++ show err

foreign export javascript "translateMentatProgram"
  translateMentatProgram :: [String] -> [String] -> Text




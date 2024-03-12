--{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import System.Environment (getArgs)

import Data.Aeson
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Either (fromRight)
import qualified Data.Text.Lazy as T

import Mentat.ParseTypes
import Mentat.ProgramTypes
import Mentat.Program
import Mentat.FunctionBuilder


translateMentatProgram :: [String] -> [String] -> T.Text
translateMentatProgram [] _ = T.empty
translateMentatProgram pgText domainVars = do
  let maybePg = parseProgram pgText domainVars

  let transPg = maybePg >>= (\x -> translateProgram x domainVars)
  
  case transPg of
    Right pg -> decodeUtf8 $ encode pg
    Left err -> error $ "syntax error: " ++ show err

--foreign export javascript "translateMentatProgram"
--  translateMentatProgram :: [String] -> [String] -> Text

main :: IO ()

main = do
  args <- getArgs

  case args of
    [fileName] -> do
      content <- readFile fileName
      let pgLines = lines content
      let pg = translateMentatProgram pgLines ["x", "y"]
      print pg


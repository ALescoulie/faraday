{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}

module MentatInterface where

import GHC.Generics
import GHC.IO.Encoding (utf8)
import GHC.Foreign


import Foreign.StablePtr
import Foreign (free)

import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.Either (fromRight)
import qualified Data.Text.Lazy as T

import Mentat.ParseTypes
import Mentat.ProgramTypes
import Mentat.Program
import Mentat.FunctionBuilder


translateMentatProgram :: [String] -> [String] -> String
translateMentatProgram [] _ = ""
translateMentatProgram pgText domainVars = do
  let maybePg = parseProgram pgText domainVars

  let transPg = maybePg >>= (\x -> translateProgram x domainVars)
  
  case transPg of
    Right pg -> toString $ encode pg
    Left err -> error $ "syntax error: " ++ show err

data MentatInputJson =
  MentatInputJson { pgLines :: [String]
                  , domVars :: [String]
                    } deriving(Show, Eq, Generic)

instance FromJSON MentatInputJson

--foreign export javascript "translateMentatProgram"
--  translateMentatProgram :: [String] -> [String] -> Text
-- Partially based on: https://github.com/bradrn/brassica/blob/master/gui/brassica-interop-wasm/src/BrassicaInterop.hs

getString :: StablePtr CStringLen -> IO CString
getString = fmap fst . deRefStablePtr

getStringLen :: StablePtr CStringLen -> IO Int
getStringLen = fmap snd . deRefStablePtr

freeStableCStringLen :: StablePtr CStringLen -> IO ()
freeStableCStringLen ptr = do
    (cstr, _) <- deRefStablePtr ptr
    free cstr
    freeStablePtr ptr

c_trans_mentat_program :: CString -> Int -> IO (StablePtr CStringLen)
c_trans_mentat_program pgData pgLen  = do
  pgStr <- peekCStringLen utf8 (pgData, pgLen)
  let inputJson = decode $ fromString pgStr
  case inputJson of
    Just (MentatInputJson pgLines domVars) -> do
      let transPgJson = translateMentatProgram pgLines domVars
      cTransPgJson <- newCStringLen utf8 transPgJson
      newStablePtr cTransPgJson 
    Nothing -> error "Failed to parse input into json"

foreign export ccall c_trans_mentat_program :: CString -> Int -> IO (StablePtr CStringLen)
foreign export ccall getString :: StablePtr CStringLen -> IO CString
foreign export ccall getStringLen :: StablePtr CStringLen -> IO Int
foreign export ccall freeStableCStringLen :: StablePtr CStringLen -> IO ()


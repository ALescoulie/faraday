{-# LANGUAGE ForeignFunctionInterface #-}

module MentatInterface where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array

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

foreign export ccall c_trans_mentat_program :: Ptr CString -> CInt -> Ptr CString -> CInt -> IO (CString)

c_trans_mentat_program :: Ptr CString -> CInt -> Ptr CString -> CInt -> IO (CString)
c_trans_mentat_program pgCLines pgLen pgCDomVars pgCVarsLen = do
  pgCStr <- peekArray (fromIntegral pgLen) pgCLines
  pgLines <- sequence $ map peekCString pgCStr
  pgCStrDomVars <-peekArray (fromIntegral pgCVarsLen) pgCDomVars
  pgDomVars <- sequence $ map peekCString pgCStrDomVars
  let pgTrans = T.unpack $ translateMentatProgram pgLines pgDomVars
  newCString pgTrans



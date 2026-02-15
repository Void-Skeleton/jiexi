{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Jiexi.TH(
  TypstException(..), 
  exportPluginFunction,
  exportPluginFunctions
) where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as X
import Data.Text.Encoding qualified as X
import Language.Haskell.TH

import Jiexi.Foreign

newtype TypstException = TypstException Text
  deriving (Eq, Ord, Show)
instance Exception TypstException where
  displayException (TypstException msg) = X.unpack (X.pack "TypstException: " <> msg)

exportPluginFunction :: Name -> String -> Q [Dec]
exportPluginFunction hsName foreignName = do
  Just arity <- checkFuncSignature hsName
  exportSig <- pluginFuncSignature arity
  let exportedHsName = mkName $ "typst_plugin_export_" ++ foreignName
  lenNames <- forM [0 .. arity - 1] $ \i -> newName ("len" ++ show i)
  offsetNames <- forM [0 .. arity] $ \i -> newName ("offset" ++ show i)
  let totalLenName = offsetNames !! arity
  rawName <- newName "raw"
  argNames <- forM [0 .. arity - 1] $ \i -> newName ("arg" ++ show i)
  exportedFuncBody <-
    addOffsetVars 0 arity lenNames offsetNames $
      addRawBind totalLenName rawName $
        addArgVars 0 arity lenNames offsetNames argNames rawName $
          pluginContent hsName argNames
  exportedFuncSigDecl <- sigD exportedHsName (pure exportSig)
  exportedFuncBodyDecl <- funD exportedHsName [clause (varP <$> lenNames) (normalB (pure exportedFuncBody)) []]
  let exportedFuncForeignDecl = ForeignD (ExportF CCall foreignName exportedHsName exportSig)
  return [exportedFuncSigDecl, exportedFuncBodyDecl, exportedFuncForeignDecl]
  where
  checkFuncSignature :: Name -> Q (Maybe Int)
  checkFuncSignature funcName = do
    VarI _ ty _ <- reify funcName
    resolveFuncTy ty
    where
    resolveFuncTy :: Type -> Q (Maybe Int)
    resolveFuncTy (AppT (AppT ArrowT paramTy) residualTy) = do
      byteStringType <- [t|ByteString|]
      if paramTy == byteStringType
        then fmap (+1) <$> resolveFuncTy residualTy
        else return Nothing
    resolveFuncTy ty = do
      ioBsType <- [t|IO ByteString|]
      if ty == ioBsType
        then return (Just 0)
        else return Nothing

  pluginFuncSignature :: Int -> Q Type
  pluginFuncSignature 0 = [t|IO Int|]
  pluginFuncSignature n = [t|Int -> $(pluginFuncSignature (n - 1))|]

  addOffsetVars :: Int -> Int -> [Name] -> [Name] -> Q Exp -> Q Exp
  addOffsetVars i arity lenNames offsetNames bodyExp
    | i > arity = bodyExp
    | i == 0 = [|let $(varP (head offsetNames)) = 0 in $(addOffsetVars (i + 1) arity lenNames offsetNames bodyExp)|]
    | otherwise = do
      let prevOffsetName = offsetNames !! (i - 1)
          currOffsetName = offsetNames !! i
          lenName = lenNames !! (i - 1)
      [|let $(bangP (varP currOffsetName)) = $(varE prevOffsetName) + $(varE lenName)
          in $(addOffsetVars (i + 1) arity lenNames offsetNames bodyExp)|]

  addRawBind :: Name -> Name -> Q Exp -> Q Exp
  addRawBind totalLenName rawName bodyExp = [|typstGetInput $(varE totalLenName) >>= \ $(varP rawName) -> $(bodyExp) |]

  addArgVars :: Int -> Int -> [Name] -> [Name] -> [Name] -> Name -> Q Exp -> Q Exp
  addArgVars i arity lenNames offsetNames argNames rawName bodyExp
    | i >= arity = bodyExp
    | otherwise = [|
      let 
        $(varP (argNames !! i)) = 
          BS.take $(varE (lenNames !! i)) 
          (BS.drop $(varE (offsetNames !! i)) $(varE rawName)) 
      in $(addArgVars (i + 1) arity lenNames offsetNames argNames rawName bodyExp)|]

  apFunc :: Q Exp -> [Name] -> Q Exp
  apFunc funcName [] = funcName
  apFunc funcName (argName:argNames) = apFunc [|$funcName $(varE argName)|] argNames

  pluginContent :: Name -> [Name] -> Q Exp
  pluginContent pluginFuncName argNames = 
    [|wrapPluginAction $(apFunc (varE pluginFuncName) argNames)|]

exportPluginFunctions :: [(Name, String)] -> Q [Dec]
exportPluginFunctions = (join <$>) . traverse (uncurry exportPluginFunction)

wrapPluginAction :: IO ByteString -> IO Int
wrapPluginAction pluginAction = do
  (result, retval) <- ((, 0) <$> pluginAction) 
    `catch` (\(TypstException msg) -> return (X.encodeUtf8 msg, 1))
    `catch` (\(ex :: SomeException) -> return (X.encodeUtf8 (X.pack "Unknown exception: " <> X.pack (displayException ex)), 1))
  typstSendOutput result
  return retval
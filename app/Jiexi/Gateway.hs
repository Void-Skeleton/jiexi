{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Jiexi.Gateway(
  TypstVal(..), 
  FromTypst(..), ToTypst(..), 
  encodeTypst, decodeTypst,
  PluginFunc(..)
) where

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
-- import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as LBS
import Data.Int
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as X
import Data.Text.Lazy qualified as LX
import GHC.Float (float2Double)

import Jiexi.CBOR qualified as C
import Jiexi.Gateway.Types
import Jiexi.Gateway.TH
import Jiexi.TH

typst2cbor :: TypstVal -> C.Term
typst2cbor TypNone = C.TNull
typst2cbor (TypBool b) = C.TBool b
typst2cbor (TypInt i) = C.TInteger (toInteger i)
typst2cbor (TypFloat f) = C.TDouble f
typst2cbor (TypString s) = C.TString s
typst2cbor (TypBytes b) = C.TBytes b
typst2cbor (TypList xs) = C.TList (map typst2cbor xs)
typst2cbor (TypMap m) = C.TMap (map (\(k, v) -> (C.TString k, typst2cbor v)) (M.assocs m))

cbor2Typst :: C.Term -> Maybe TypstVal
cbor2Typst term = do
  case term of
    C.TNull -> Just TypNone
    C.TBool b -> Just (TypBool b)
    C.TInt i -> Just (TypInt (toEnum i))
    C.TInteger i -> Just (TypInt (fromInteger i))
    C.THalf f -> Just (TypFloat (float2Double f))
    C.TFloat f -> Just (TypFloat (float2Double f))
    C.TDouble f -> Just (TypFloat f)
    C.TString s -> Just (TypString s)
    C.TStringI (LX.toStrict -> s) -> Just (TypString s)
    C.TBytes b -> Just (TypBytes b)
    C.TBytesI (LBS.toStrict -> b) -> Just (TypBytes b)
    C.TList xs -> TypList <$> mapM cbor2Typst xs
    C.TListI xs -> TypList <$> mapM cbor2Typst xs
    C.TMap kvs -> do
      pairs <-
        mapM (\(k, v) -> do
          k' <- case k of
            C.TString s -> Just s
            _ -> Nothing
          v' <- cbor2Typst v
          return (k', v')) kvs
      return $ TypMap (M.fromList pairs)
    C.TMapI kvs -> do
      pairs <-
        mapM (\(k, v) -> do
          k' <- case k of
            C.TString s -> Just s
            _ -> Nothing
          v' <- cbor2Typst v
          return (k', v')) kvs
      return $ TypMap (M.fromList pairs)
    C.TTagged _ _ -> Nothing
    C.TSimple _ -> Nothing

encodeTypst :: (ToTypst a) => a -> ByteString
encodeTypst = C.encodeCbor . typst2cbor . toTypst

decodeTypst :: (FromTypst a) => ByteString -> Maybe a
decodeTypst = fromTypst <=< cbor2Typst <$> decodeCborThrow
  where
  decodeCborThrow bs = case C.decodeCbor bs of
    Left msg -> throw $ TypstException $ X.pack msg
    Right term -> term


$(deriveTuplesFromToTypst [2..16])

class PluginFunc a where
  type PluginFuncRepr a
  makePluginFunc :: a -> PluginFuncRepr a

instance ToTypst a => PluginFunc (IO a) where
  type PluginFuncRepr (IO a) = IO ByteString
  makePluginFunc = fmap encodeTypst

instance (FromTypst a, PluginFunc b) => PluginFunc (a -> b) where
  type PluginFuncRepr (a -> b) = ByteString -> PluginFuncRepr b
  makePluginFunc f = makePluginFunc . f . fromJust . decodeTypst
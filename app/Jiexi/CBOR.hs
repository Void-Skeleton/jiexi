{-# LANGUAGE OverloadedStrings #-}

module Jiexi.CBOR
  ( Term (..),
    encodeCbor,
    decodeCbor,
  )
where

import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Word

-- | The AST, updated to use Lazy types for indefinite lengths.
data Term
  = TInt !Int
  | TInteger !Integer
  | TBytes !BS.ByteString
  | TBytesI !BSL.ByteString
  | TString !T.Text
  | TStringI !TL.Text
  | TList ![Term]
  | TListI ![Term]
  | TMap ![(Term, Term)]
  | TMapI ![(Term, Term)]
  | TTagged !Word64 !Term
  | TBool !Bool
  | TNull
  | TSimple !Word8
  | THalf !Float
  | TFloat !Float
  | TDouble !Double
  deriving (Show, Eq)

-- | Encodes a Term to a strict ByteString.
encodeCbor :: Term -> BS.ByteString
encodeCbor = BSL.toStrict . runPut . putTerm

-- | Decodes a strict ByteString to a Term.
decodeCbor :: BS.ByteString -> Either String Term
decodeCbor bs =
  case runGetOrFail getTerm (BSL.fromStrict bs) of
    Left (_, _, err) -> Left err
    Right (_, _, term) -> Right term

--------------------------------------------------------------------------------
-- Encoders (Put)
--------------------------------------------------------------------------------

putTerm :: Term -> Put
putTerm (TInt n)
  | n >= 0 = putTypeAndArg 0 (fromIntegral n)
  | otherwise = putTypeAndArg 1 (fromIntegral (-1 - n))
putTerm (TInteger n)
  | n >= 0 = putTypeAndArg 0 (fromInteger n)
  | otherwise = putTypeAndArg 1 (fromInteger (-1 - n))
-- Definite length bytes
putTerm (TBytes bs) = do
  putTypeAndArg 2 (fromIntegral $ BS.length bs)
  putByteString bs

-- Indefinite length bytes (Major type 2, Info 31)
putTerm (TBytesI lbs) = do
  putWord8 0x5F
  mapM_ (putTerm . TBytes) (BSL.toChunks lbs)
  putWord8 0xFF

-- Definite length string
putTerm (TString txt) = do
  let bs = TE.encodeUtf8 txt
  putTypeAndArg 3 (fromIntegral $ BS.length bs)
  putByteString bs

-- Indefinite length string (Major type 3, Info 31)
putTerm (TStringI ltxt) = do
  putWord8 0x7F
  mapM_ (putTerm . TString) (TL.toChunks ltxt)
  putWord8 0xFF
putTerm (TList terms) = do
  putTypeAndArg 4 (fromIntegral $ length terms)
  mapM_ putTerm terms
putTerm (TListI terms) = do
  putWord8 0x9F
  mapM_ putTerm terms
  putWord8 0xFF
putTerm (TMap pairs) = do
  putTypeAndArg 5 (fromIntegral $ length pairs)
  mapM_ (\(k, v) -> putTerm k >> putTerm v) pairs
putTerm (TMapI pairs) = do
  putWord8 0xBF
  mapM_ (\(k, v) -> putTerm k >> putTerm v) pairs
  putWord8 0xFF
putTerm (TTagged tag term) = do
  putTypeAndArg 6 tag
  putTerm term
putTerm (TBool False) = putWord8 0xF4
putTerm (TBool True) = putWord8 0xF5
putTerm TNull = putWord8 0xF6
putTerm (TSimple w) = putWord8 (0xE0 .|. (w .&. 0x1F))
-- Floating point numbers
putTerm (THalf f) = putWord8 0xF9 >> putFloat16be f
putTerm (TFloat f) = putWord8 0xFA >> putFloatbe f
putTerm (TDouble d) = putWord8 0xFB >> putDoublebe d

-- Helper for CBOR header bytes
putTypeAndArg :: Word8 -> Word64 -> Put
putTypeAndArg major arg
  | arg < 24 = putWord8 (majorType .|. fromIntegral arg)
  | arg <= 0xFF = putWord8 (majorType .|. 24) >> putWord8 (fromIntegral arg)
  | arg <= 0xFFFF = putWord8 (majorType .|. 25) >> putWord16be (fromIntegral arg)
  | arg <= 0xFFFFFFFF = putWord8 (majorType .|. 26) >> putWord32be (fromIntegral arg)
  | otherwise = putWord8 (majorType .|. 27) >> putWord64be arg
  where
    majorType = major `shiftL` 5

--------------------------------------------------------------------------------
-- Decoders (Get)
--------------------------------------------------------------------------------

getTerm :: Get Term
getTerm = do
  header <- getWord8
  let major = header `shiftR` 5
  let addInfo = header .&. 0x1F

  if addInfo == 31
    then case major of
      -- Reconstruct lazy sequences directly from definite-length chunks
      2 -> TBytesI . BSL.fromChunks <$> getIndefinite getChunkBytes
      3 -> TStringI . TL.fromChunks <$> getIndefinite getChunkString
      4 -> TListI <$> getIndefinite getTerm
      5 -> TMapI <$> getIndefinite getPair
      7 -> fail "Break code unexpected outside of indefinite arrays/maps"
      _ -> fail "Indefinite length not supported for this major type"
    else do
      arg <- getArg addInfo
      case major of
        0 -> return $ TInt (fromIntegral arg)
        1 -> return $ TInt (-1 - fromIntegral arg)
        2 -> TBytes <$> getByteString (fromIntegral arg)
        3 -> TString . TE.decodeUtf8 <$> getByteString (fromIntegral arg)
        4 -> TList <$> replicateM (fromIntegral arg) getTerm
        5 -> TMap <$> replicateM (fromIntegral arg) getPair
        6 -> TTagged arg <$> getTerm
        7 -> case addInfo of
          20 -> return $ TBool False
          21 -> return $ TBool True
          22 -> return TNull
          25 -> THalf <$> getFloat16
          26 -> TFloat <$> getFloatbe
          27 -> TDouble <$> getDoublebe
          _
            | addInfo < 24 -> return $ TSimple addInfo
            | otherwise -> fail "Unrecognized simple value"
        _ -> fail "Unknown major type"

getArg :: Word8 -> Get Word64
getArg info
  | info < 24 = return (fromIntegral info)
  | info == 24 = fromIntegral <$> getWord8
  | info == 25 = fromIntegral <$> getWord16be
  | info == 26 = fromIntegral <$> getWord32be
  | info == 27 = getWord64be
  | otherwise = fail "Invalid additional info for argument"

-- Reads items until the 0xFF break code is encountered
getIndefinite :: Get a -> Get [a]
getIndefinite parser = do
  w <- lookAhead getWord8
  if w == 0xFF
    then getWord8 >> return [] -- Consume the break code
    else do
      x <- parser
      xs <- getIndefinite parser
      return (x : xs)

-- CBOR spec dictates that indefinite strings must only contain definite strings
getChunkBytes :: Get BS.ByteString
getChunkBytes = do
  t <- getTerm
  case t of
    TBytes b -> return b
    _ -> fail "Expected definite-length bytes chunk inside indefinite bytes"

getChunkString :: Get T.Text
getChunkString = do
  t <- getTerm
  case t of
    TString s -> return s
    _ -> fail "Expected definite-length string chunk inside indefinite string"

getPair :: Get (Term, Term)
getPair = do
  k <- getTerm
  v <- getTerm
  return (k, v)

-- Stubs to keep WASM happy without C-FFI / manual bitwise decoding for Float16
getFloat16 :: Get Float
getFloat16 = getWord16be >> return 0.0

putFloat16be :: Float -> Put
putFloat16be _ = putWord16be 0
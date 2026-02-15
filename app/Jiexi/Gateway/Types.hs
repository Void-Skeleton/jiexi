module Jiexi.Gateway.Types where

-- import Codec.CBOR qualified as C
import Data.ByteString (ByteString)
-- import Data.ByteString qualified as BS
import Data.Int
import Data.Map (Map)
import Data.Text (Text)
-- import Data.Text qualified as X

data TypstVal
  = TypNone
  | TypBool !Bool
  | TypInt !Int64
  | TypFloat !Double
  | TypString !Text
  | TypBytes !ByteString
  | TypList ![TypstVal]
  | TypMap !(Map Text TypstVal)

class FromTypst a where
  fromTypst :: TypstVal -> Maybe a

class ToTypst a where
  toTypst :: a -> TypstVal

instance FromTypst TypstVal where
  fromTypst = Just

instance ToTypst TypstVal where
  toTypst = id

instance FromTypst Bool where
  fromTypst (TypBool b) = Just b
  fromTypst _ = Nothing

instance ToTypst Bool where
  toTypst = TypBool

instance FromTypst Int where
  fromTypst (TypInt i) = Just (fromEnum i)
  fromTypst _ = Nothing

instance ToTypst Int where
  toTypst i = TypInt (toEnum i)

instance FromTypst Int64 where
  fromTypst (TypInt i) = Just i
  fromTypst _ = Nothing

instance ToTypst Int64 where
  toTypst i = TypInt (fromIntegral i)

instance FromTypst Double where
  fromTypst (TypFloat f) = Just f
  fromTypst _ = Nothing

instance ToTypst Double where
  toTypst = TypFloat

instance FromTypst Text where
  fromTypst (TypString s) = Just s
  fromTypst _ = Nothing

instance ToTypst Text where
  toTypst = TypString

instance FromTypst ByteString where
  fromTypst (TypBytes b) = Just b
  fromTypst _ = Nothing

instance ToTypst ByteString where
  toTypst = TypBytes

instance (FromTypst a) => FromTypst [a] where
  fromTypst (TypList xs) = mapM fromTypst xs
  fromTypst _ = Nothing

instance (ToTypst a) => ToTypst [a] where
  toTypst xs = TypList (map toTypst xs)

instance (FromTypst a) => FromTypst (Map Text a) where
  fromTypst (TypMap m) = mapM fromTypst m
  fromTypst _ = Nothing

instance (ToTypst a) => ToTypst (Map Text a) where
  toTypst m = TypMap (fmap toTypst m)

instance (FromTypst a) => FromTypst (Maybe a) where
  fromTypst TypNone = Just Nothing
  fromTypst v = Just <$> fromTypst v

instance (ToTypst a) => ToTypst (Maybe a) where
  toTypst Nothing = TypNone
  toTypst (Just v) = toTypst v
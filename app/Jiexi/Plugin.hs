{-# LANGUAGE TemplateHaskell #-}
module Jiexi.Plugin where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Jiexi.CodeGen
import Jiexi.Gateway
import Jiexi.Parse
import Jiexi.TH

grammarToMetadata :: Text -> IO TableParseMetadata
grammarToMetadata input = do
  jiexiFile <- parseJiexiFile input
  let parseMetadata = jiexiFileToParseMetadata jiexiFile
  return parseMetadata

grammarToMetadataP :: ByteString -> IO ByteString
grammarToMetadataP = makePluginFunc grammarToMetadata

$(exportPluginFunction 'grammarToMetadataP "grammar_to_metadata")
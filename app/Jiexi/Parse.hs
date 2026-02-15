module Jiexi.Parse(
  parseJiexiFile, 
  module Jiexi.Parse.Types
) where

import Control.Exception
import Data.Text (Text)
import Data.Text qualified as X
import Jiexi.Parse.Alex (alexScanTokens)
import Jiexi.Parse.Happy (happyParseJiexi)
import Jiexi.Parse.Types (
  JiexiFile(..), Directive(..), Production(..), Rule(..), RuleDirective(..), 
  TokenName, Eliminator)
import Jiexi.TH

parseJiexiFile :: Text -> IO JiexiFile
parseJiexiFile input = 
  evaluate (happyParseJiexi $ alexScanTokens input) `catch` \(e :: SomeException) ->
    throwIO (TypstException $ X.pack "bad parse: " <> X.pack (show e))
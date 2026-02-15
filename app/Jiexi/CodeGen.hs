module Jiexi.CodeGen(
  module Jiexi.CodeGen.Intermediary, 
  module Jiexi.CodeGen.Metadata, 
  jiexiFileToParseMetadata
) where

import Jiexi.CodeGen.Intermediary
import Jiexi.CodeGen.Metadata
import Jiexi.Parse

jiexiFileToParseMetadata :: JiexiFile -> TableParseMetadata
jiexiFileToParseMetadata = generateMetadata . jiexiInfoToCodeGenCtx . normalizeJiexiFile
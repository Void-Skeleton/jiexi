module Jiexi.Parse.Types where

import Data.Sequence (Seq(..), (<|), (|>))
import Data.Sequence qualified as Q
import Data.Text (Text)

type TokenName = Text
type Eliminator = Text

data JiexiToken
  = DirectiveT !DirectiveT -- %start, %token, etc
  | SeparatorT -- %%
  | NumberT !Int -- <number>
  | TokenNameT !TokenName -- <identifier>
  | CodeT !Text -- { <code> #}
  | ColonT -- :
  | PipeT -- |
  deriving (Eq, Ord, Show)

data DirectiveT
  = StartTD | TokenTD | LeftTD | RightTD | NonassocTD | PrecTD | ShiftTD
  deriving (Eq, Ord, Show)

data JiexiFile = JiexiFile { 
  jiexiDirectives :: !(Seq Directive), 
  jiexiProductions :: !(Seq Production)
} deriving (Eq, Ord, Show)

data Directive
  = StartD !TokenName | TokenD !TokenName !Int | LeftD !(Seq TokenName) | RightD !(Seq TokenName) | NonassocD !(Seq TokenName)
  deriving (Eq, Ord, Show)

data Production = Production { 
  productionToken :: !TokenName, 
  productionRules :: !(Seq Rule)
} deriving (Eq, Ord, Show)

data Rule = Rule {
  ruleTokens :: !(Seq TokenName), 
  ruleDirective :: !RuleDirective, 
  ruleEliminator :: !Eliminator
} deriving (Eq, Ord, Show)

data RuleDirective
  = NoRD | PrecRD !TokenName | ShiftRD
  deriving (Eq, Ord, Show)


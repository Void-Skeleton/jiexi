{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Jiexi.CodeGen.Metadata(
  TableParseMetadata(TableParseMetadata), 
  generateMetadata
) where

import Control.Exception
import Data.Array (Array)
import Data.Array qualified as A
import Data.Coerce
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Q
import Data.Text (Text)
import Happy.Grammar qualified as H
import Happy.Tabular qualified as H
import Happy.Tabular.LALR qualified as H

import Jiexi.CodeGen.Intermediary
import Jiexi.Gateway
import Jiexi.Parse
import Jiexi.TH

newtype TableParseMetadata = TableParseMetadataNT ([[Int]], [[Int]], [Int], [Int], [Text], [Int], Int)
  deriving newtype (FromTypst, ToTypst)

{-# COMPLETE TableParseMetadata #-}
pattern TableParseMetadata :: [[Int]] -> [[Int]] -> [Int] -> [Int] -> [Text] -> [Int] -> Int -> TableParseMetadata
pattern TableParseMetadata {
  shiftReduceTable, 
  gotoTable, 
  prodArityTable, 
  prodTokenTable, 
  prodWorkerCodeTable, 
  tagToTokenTable, 
  eofToken
} = TableParseMetadataNT (shiftReduceTable, gotoTable, prodArityTable, prodTokenTable, prodWorkerCodeTable, tagToTokenTable, eofToken)

{- 
We generate Typst code for a given grammar, using a shift-reduce parser with a generated table. 
The main tables participating in the parsing are: 
- Action table: It is a 2-dimensional array indexed by (state, token). Its values are: 
  - 0: error
  - n > 0: shift to state (n - 1)
  - n < 0: reduce by production (-n - 1). Note that accept is reduce by 0, and represented by -1. 
- Goto table: It is a 2-dimensional array indexed by (state, non-terminal). Its values are states to go to after a reduction. 
- Worker table: It is an array indexed by (production). Its values are lambda expressions to apply after a reduction. 
- Tag to token table: It is an array indexed by (tag) with values being token index represented by the tag. 
-}

generateMetadata :: CodeGenContext -> TableParseMetadata
generateMetadata ctx = TableParseMetadata {
  shiftReduceTable = processHappySRTable (H.actionTable (happyTable ctx)), 
  gotoTable = processHappyGotoTable n m (H.gotoTable (happyTable ctx)), 
  prodArityTable = [length toks | H.Production _ toks (_, _) _ <- joinedProdsList], 
  prodTokenTable = [coerce target | H.Production target _ (_, _) _ <- joinedProdsList], 
  prodWorkerCodeTable = [elim | H.Production _ _ (elim, _) _ <- joinedProdsList], 
  tagToTokenTable = undefined, 
  eofToken = m
} where
  info = jiexiInfo ctx
  n = 4 + Q.length (nonterminalTokens info)
  m = n + Q.length (terminalTokens info)
  joinedProds = joinedProductionArray ctx
  joinedProdsList = toListAssertStart 0 joinedProds

tableDummyValue :: Int
tableDummyValue = -2147483648

encodeLRAction :: H.LRAction -> Int
encodeLRAction (H.LR'Shift s _) = s + 1
encodeLRAction (H.LR'Reduce r _) = -(r + 1)
encodeLRAction H.LR'Accept = -1
encodeLRAction H.LR'Fail = 0
encodeLRAction H.LR'MustFail = 0
encodeLRAction (H.LR'Multiple _ action) = encodeLRAction action

processHappySRTable :: H.ActionTable -> [[Int]]
processHappySRTable tab = [
  [ encodeLRAction action
  | action <- toListAssertStart (H.MkName 0) stateRow]
  | stateRow <- toListAssertStart 0 tab]


toListAssertStart :: A.Ix i => i -> Array i e -> [e]
toListAssertStart requiredStart arr
  | (start, _) <- A.bounds arr, start == requiredStart = toList arr
  | otherwise = throw $ TypstException "toListAssertStart: assertion failed"

encodeGoto :: H.Goto -> Int
encodeGoto (H.Goto s) = s
encodeGoto H.NoGoto = tableDummyValue

processHappyGotoTable :: Int -> Int -> H.GotoTable -> [[Int]]
processHappyGotoTable n m tab = [
  [ if 4 <= t && t < n then encodeGoto (stateRow A.! coerce t) else tableDummyValue
  | t <- [0 .. m]]
  | stateRow <- toListAssertStart 0 tab]

processTagToTokenTable :: Seq (TokenName, TokenTag) -> Map TokenName Int -> [Int]
processTagToTokenTable tagToTokenMap indexByToken
  | minTag < 0 = throw $ TypstException "processTagToTokenTable: The current implementation uses a lookup table for tag to token mapping, and requires tokens to have non-negative tags that are ideally dense."
  | otherwise = [fromMaybe tableDummyValue (indexByTag M.!? tag) | tag <- [0 .. maxTag]]
  where
  indexTagAssocs = map (\(tokenName, tag) -> (tag, indexByToken M.! tokenName)) (toList tagToTokenMap)
  minTag = minimum (fst <$> indexTagAssocs)
  maxTag = maximum (fst <$> indexTagAssocs)
  indexByTag = M.fromList indexTagAssocs
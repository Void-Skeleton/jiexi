{-# LANGUAGE OverloadedStrings #-}
module Jiexi.CodeGen.Intermediary where

import Control.Exception
import Data.Array qualified as A
import Data.Coerce
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Sequence qualified as Q
import Data.Text (Text)
import Data.Text qualified as X
import Happy.Grammar qualified as H
import Happy.Tabular qualified as H

import Jiexi.Parse
import Jiexi.TH

type TokenTag = Int
data BareRule = BareRule (Seq TokenName) Eliminator
  deriving (Eq, Ord, Show)
data JiexiInfo = JiexiInfo {
  terminalTokens :: Seq (TokenName, TokenTag),
  nonterminalTokens :: Seq TokenName,
  startToken :: TokenName,
  terminalPriorities :: Map TokenName H.Priority,
  rulesByName :: Map TokenName (Seq (BareRule, Maybe H.Priority))
} deriving (Eq, Show)

normalizeJiexiFile :: JiexiFile -> JiexiInfo
normalizeJiexiFile (JiexiFile directives productions) = const JiexiInfo {
  terminalTokens = infoTerminals,
  nonterminalTokens = infoNonterminals,
  startToken = infoStartToken,
  terminalPriorities = infoTerminalPriorities,
  rulesByName = infoRulesByName
} $! sanityChecks
  where
  infoTerminals = foldl' accTerminals Q.empty directives
    where
    accTerminals acc (TokenD name tag) = acc |> (name, tag)
    accTerminals acc _ = acc

  startTokenM = foldl' accStartToken Nothing directives
    where
    accStartToken Nothing (StartD name) = Just name
    accStartToken (Just start) (StartD name) = throw $ TypstException $ "Multiple %start directives: " <> start <> " and " <> name
    accStartToken acc _ = acc
  infoStartToken = case startTokenM of
    Just start -> start
    Nothing -> throw $ TypstException "No %start directive found"

  infoTerminalPriorities = snd $ foldl accTerminalPriority (1, M.empty) directives
    where
    accTerminalPriority (!currPrec, !acc) (LeftD names) =
      (currPrec + 1, foldl' (\m name -> M.insert name (H.Prio H.LeftAssoc currPrec) m) acc names)
    accTerminalPriority (!currPrec, !acc) (RightD names) =
      (currPrec + 1, foldl' (\m name -> M.insert name (H.Prio H.RightAssoc currPrec) m) acc names)
    accTerminalPriority (!currPrec, !acc) (NonassocD names) =
      (currPrec + 1, foldl' (\m name -> M.insert name (H.Prio H.None currPrec) m) acc names)
    accTerminalPriority acc _ = acc

  infoNonterminals = productionToken <$> productions

  infoRulesByName = M.fromList (prodToRules <$> toList productions)
    where
    prodToRules (Production name rules) = (name, go <$> rules)
      where
      go (Rule tokens ruleDir elim) = (BareRule tokens elim, getPrio ruleDir)

      getPrio NoRD = Nothing
      getPrio ShiftRD = Just H.PrioLowest
      getPrio (PrecRD precTok) = infoTerminalPriorities M.!? precTok

  sanityChecks
    | inter <- S.intersection terminalSet nonterminalSet, not (S.null inter) = throw $ TypstException $ "Some symbol is both a terminal and a non-terminal: " <> X.pack (show inter)
    | not (infoStartToken `S.member` nonterminalSet) = throw $ TypstException $ "Start token is not a non-terminal: " <> X.pack (show infoStartToken)
    | not (M.keysSet infoTerminalPriorities `S.isSubsetOf` terminalSet) = throw $ TypstException $ "Some token in precedence declarations is not a terminal: " <> X.pack (show (S.difference (M.keysSet infoTerminalPriorities) terminalSet))
    | not (M.keysSet infoRulesByName `S.isSubsetOf` nonterminalSet) = throw $ TypstException $ "Some production rule refers to a symbol that is not a non-terminal: " <> X.pack (show (S.difference (M.keysSet infoRulesByName) nonterminalSet))
    | otherwise = foldr seq () (sanityCheckRule <$> map snd (M.assocs infoRulesByName))
    
  terminalSet = S.fromList (fst <$> toList infoTerminals)
  nonterminalSet = S.fromList (toList infoNonterminals)
  tokenSet = terminalSet `S.union` nonterminalSet
  sanityCheckRule ruleSeq
    | appearedTokens <- S.fromList (concatMap (\(BareRule toks _, _) -> toList toks) ruleSeq), not (appearedTokens `S.isSubsetOf` tokenSet) = throw $ TypstException $ "Some production rule refers to a symbol that is not a terminal or non-terminal: " <> X.pack (show (S.difference appearedTokens tokenSet))
    | otherwise = ()

data CodeGenContext = CodeGenContext {
  jiexiInfo :: JiexiInfo, 
  happyGrammar :: H.Grammar Eliminator, 
  happyTable :: H.Tables, 
  joinedProductionArray :: A.Array Int (H.Production Eliminator), 
  indexByTokenName :: Map TokenName Int
}

jiexiInfoToCodeGenCtx :: JiexiInfo -> CodeGenContext
jiexiInfoToCodeGenCtx info = let grammar = H.Grammar {
  H.token_names = A.listArray (H.MkName 0, coerce m)
    (["%epsilon", "%error", "%dummy", "%start"] ++
     (X.unpack <$> toList (nonterminalTokens info)) ++
     (X.unpack . fst <$> toList (terminalTokens info)) ++
     ["%eof"]),

  H.productions = joinedProdsList,
  H.lookupProdNo = (joinedProdsArray A.!),
  H.lookupProdsOfName = \(H.MkName index) ->
    if index == 3 then [0]
    else if 4 <= index && index < n then 
      let (start, end) = nameToProdsRangeArray A.! index in [start .. end - 1]
    else [], 

  H.first_nonterm = H.MkName 3,
  H.first_term = coerce n,
  H.eof_term = coerce m,

  H.non_terminals = coerce [3 .. n - 1], -- start, nonterminals
  H.terminals = coerce (1 : [n .. m]), -- error, terminals, eof

  H.starts = [("start", H.MkName 3, startTokenName, False)],
  H.types = A.listArray (H.MkName 0, coerce m) (repeat Nothing),
  H.priorities = [(coerce $ indexByToken M.! tokenName, prio) 
    | (tokenName, prio) <- M.assocs (terminalPriorities info)],
  H.token_specs = []
} in CodeGenContext {
  jiexiInfo = info,
  happyGrammar = grammar,
  happyTable = H.genTables H.select_first_reduction grammar, 
  joinedProductionArray = joinedProdsArray, 
  indexByTokenName = indexByToken
} where
  -- 0 = %epsilon, 1 = %error, 2 = %dummy, 3 = %start
  -- [4, n) = <non-terminals>, [n, m) = <terminals>, m = %eof
  n = 4 + Q.length (nonterminalTokens info)
  m = n + Q.length (terminalTokens info)

  indexByToken = M.fromList $ 
    zip (toList (nonterminalTokens info)) [4..]
    ++ zip (fst <$> toList (terminalTokens info)) [n..]

  startTokenName = H.MkName (indexByToken M.! startToken info)

  startProduction = H.Production (H.MkName 3) [startTokenName, coerce m] 
    (" panic(\"start production should not be used\") " :: Eliminator, [1, 2]) H.No

  nameToProdsArray = A.listArray (4, n - 1) [
    [ H.Production 
      (coerce (indexByToken M.! tokenName)) 
      (coerce ((indexByToken M.!) <$> toList tokens))
      (elim, [1 .. Q.length tokens])
      -- (fromMaybe H.No prioM)
      (fromMaybe (inducedPrio tokens) prioM) -- The prio/assoc of a production is by default that of its last terminal
    | (BareRule tokens elim, prioM) <- toList prods]
    | tokenName <- toList (nonterminalTokens info),
      let prods = rulesByName info M.! tokenName]

  (joinedProdsList, joinedProdsArray) = 
    let 
      actualProductions = concat nameToProdsArray
      allProductions = startProduction : actualProductions
    in
      (allProductions, A.listArray (0, length allProductions - 1) allProductions)

  nameToProdsRangeArray = let nameToProdsList = toList nameToProdsArray
    in let lengthScan = scanl (+) 0 (map length nameToProdsList)
    in A.listArray (4, n - 1) $ 
      zipWith (\start end -> (1 + start, 1 + end)) lengthScan (tail lengthScan)

  terminalSet = S.fromList $ fst <$> toList (terminalTokens info)

  inducedPrio :: Seq TokenName -> H.Priority
  inducedPrio tokens = case lastTerminalM of
    Nothing -> H.No
    Just lastTerminal -> fromMaybe H.No (terminalPriorities info M.!? lastTerminal)
    where
    lastTerminalM = case Q.dropWhileR (not . (`S.member` terminalSet)) tokens of
      Q.Empty -> Nothing
      _ :|> x -> Just x
  
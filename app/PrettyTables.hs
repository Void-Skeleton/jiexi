module PrettyTables where

import Data.Array (assocs, bounds, (!))
import Data.Coerce
import Data.List (intercalate)
import Happy.Grammar
import Happy.Tabular
import Happy.Tabular.LALR

-- | Main entry point for pretty printing the Tables record.
prettyPrintTables :: Grammar a -> Tables -> String
prettyPrintTables g tbls =
  intercalate
    "\n"
    [ "========================================",
      "          HAPPY PARSER TABLES           ",
      "========================================",
      "",
      prettyStats tbls,
      prettyRedundancies g tbls,
      prettyStateMachine g tbls
    ]

-- | 1. Extract and format Conflict Statistics
prettyStats :: Tables -> String
prettyStats tbls =
  let (_, (totalSR, totalRR)) = conflicts tbls
   in unlines
        [ "--- Parser Statistics ---",
          "Shift/Reduce Conflicts:  " ++ show totalSR,
          "Reduce/Reduce Conflicts: " ++ show totalRR
        ]

-- | 2. Extract and format dead code (Redundancies)
prettyRedundancies :: Grammar a -> Tables -> String
prettyRedundancies g tbls =
  let (unusedRules, unusedTerms) = redundancies tbls

      formatRule rId = "Rule " ++ show rId
      formatTerm tId = "Token '" ++ (token_names g ! tId) ++ "'"

      rulesStr = if null unusedRules then "None" else intercalate ", " (map formatRule unusedRules)
      termsStr = if null unusedTerms then "None" else intercalate ", " unusedTerms
   in unlines
        [ "--- Redundancies ---",
          "Unused Rules:     " ++ rulesStr,
          "Unused Terminals: " ++ termsStr
        ]

-- | 3. Format the actual State Machine (Action and Goto Tables)
prettyStateMachine :: Grammar a -> Tables -> String
prettyStateMachine g tbls =
  let acts = actionTable tbls
      gotos = gotoTable tbls

      -- The number of states is dictated by the bounds of the Action Table
      (0, maxState) = bounds acts

      -- Filter terminal IDs and non-terminal IDs based on Grammar bounds
      termIds = [first_term g .. eof_term g]
      nonTermIds = [first_nonterm g .. (MkName $ getName (first_term g) - 1)] -- Based on Happy's namespace
   in unlines $ map (prettyState g acts gotos (coerce termIds) (coerce nonTermIds)) [0 .. maxState]

-- | Format a single state's actions and gotos
prettyState :: Grammar a -> ActionTable -> GotoTable -> [Int] -> [Int] -> Int -> String
prettyState g acts gotos termIds nonTermIds stateId =
  let -- Extract the row for this specific state
      actRow = acts ! stateId
      gotoRow = gotos ! stateId

      -- Format Actions (Pass 'g' to formatAction)
      actionLines =
        [ "\t" ++ padName (token_names g ! coerce tId) ++ "  " ++ formatAction (actRow ! coerce tId)
        | tId <- termIds,
          not (isError (actRow ! coerce tId))
        ]

      -- Format Gotos
      gotoLines =
        [ "\t" ++ padName (token_names g ! coerce ntId) ++ "  " ++ formatGoto (gotoRow ! coerce ntId)
        | ntId <- nonTermIds,
          not (isNoGoto (gotoRow ! coerce ntId))
        ]

      -- Handle empty blocks gracefully
      actionBlock = if null actionLines then ["\t(No actions)"] else actionLines
      gotoBlock = if null gotoLines then ["\t(No gotos)"] else gotoLines
   in unlines $
        [ "State " ++ show stateId,
          "",
          "\t-- Actions --"
        ]
          ++ actionBlock
          ++ ["", "\t-- Gotos --"]
          ++ gotoBlock
          ++ [""] -- Add a trailing newline for spacing between states

-- --- Helpers ---

-- | Translates Action data type to readable text
formatAction :: LRAction -> String
formatAction (LR'Shift n _) = "shift to state " ++ show n
formatAction (LR'Reduce n _) = "reduce using rule " ++ show n
formatAction LR'Accept = "accept"
formatAction LR'Fail = "fail"
formatAction LR'MustFail = "mustfail"
formatAction (LR'Multiple possibilities selected) = formatAction selected ++ " [" ++ intercalate ", " (map formatAction possibilities) ++ "]"

isError :: LRAction -> Bool
-- isError _ = False
isError LR'Fail = True
isError LR'MustFail = True
isError _ = False

-- | Translates Goto data type to readable text
formatGoto :: Goto -> String
formatGoto (Goto n) = "go to state " ++ show n
formatGoto NoGoto = ""

isNoGoto :: Goto -> Bool
isNoGoto NoGoto = True
isNoGoto _ = False

-- | Right-pads strings so columns align nicely
padName :: String -> String
padName str = str ++ replicate (15 - length str) ' '
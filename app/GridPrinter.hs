module GridPrinter where

import Data.Array (bounds, (!))
import Data.Coerce (coerce)
import Data.List (intercalate, transpose)
import Happy.Grammar
import Happy.Tabular
import Happy.Tabular.LALR

-- | Prints a perfectly aligned Markdown-style Shift-Reduce table
prettyPrintGrid :: Grammar a -> Tables -> String
prettyPrintGrid g tbls =
  let acts = actionTable tbls
      gotos = gotoTable tbls
      (0, maxState) = bounds acts

      -- 1. Filter out Happy's internal tokens to keep the table clean
      isCleanName n = n `notElem` ["error", "dummy", "epsilon", "%start", "%start_parse"]

      tIds = filter (\n -> isCleanName (token_names g ! coerce n)) (terminals g)
      ntIds = filter (\n -> isCleanName (token_names g ! coerce n)) (non_terminals g)

      -- 2. Create the Header Row
      header =
        "State"
          : map (\t -> "Action " ++ (token_names g ! coerce t)) tIds
          ++ map (\nt -> "Goto " ++ (token_names g ! coerce nt)) ntIds

      -- 3. Helpers for concise grid actions
      shortAction :: LRAction -> String
      shortAction (LR'Shift n _) = "S" ++ show n
      shortAction (LR'Reduce n _) = "R" ++ show n
      shortAction LR'Accept = "ACC"
      shortAction (LR'Multiple alts act) = shortAction act ++ "* [" ++ intercalate ", " (map shortAction alts) ++ "]" -- Append * for conflicts
      shortAction _ = "" -- Fail/MustFail become empty cells
      shortGoto :: Goto -> String
      shortGoto (Goto n) = show n
      shortGoto NoGoto = ""

      -- 4. Generate the Data Rows
      makeRow s =
        let actRow = acts ! s
            gotoRow = gotos ! s
            cols =
              show s
                : map (\t -> shortAction (actRow ! coerce t)) tIds
                ++ map (\nt -> shortGoto (gotoRow ! coerce nt)) ntIds
         in cols

      rows = map makeRow [0 .. maxState]

      -- 5. Calculate column widths for perfect alignment
      colWidths = [maximum (map length col) | col <- transpose (header : rows)]

      padRight w s = s ++ replicate (w - length s) ' '

      -- 6. Format the Markdown table
      formatRow cells = "| " ++ intercalate " | " (zipWith padRight colWidths cells) ++ " |"
      separator = "|-" ++ intercalate "-|-" [replicate w '-' | w <- colWidths] ++ "-|"
   in unlines $ formatRow header : separator : map formatRow rows
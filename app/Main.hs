{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE StandaloneDeriving #-}
module Main where

-- import Data.Array
import Data.Text (Text)
import Data.Text.IO qualified as X
-- import Happy.Grammar
-- import Happy.Tabular
import GridPrinter (prettyPrintGrid)

import Jiexi.CodeGen
import Jiexi.Parse (parseJiexiFile)
import Jiexi.Plugin

-- instance Num Name where
--   fromInteger = MkName . fromIntegral
-- deriving instance Show Tables

-- manualGrammar :: Grammar ()
-- manualGrammar =
--   Grammar
--     { -- 1. The Magic Boundaries
--       first_nonterm = 3, -- The first %start rule is at 3
--       first_term = 5, -- Boundary 'n', our first terminal 'int'
--       eof_term = 11, -- Boundary 'm', the %eof token
--       terminals = [1, 5, 6, 7, 8, 9, 10, 11], -- error(1), user terms, eof(11)
--       non_terminals = [3, 4], -- %start(3) and Exp(4)

--       -- 2. Symbol Names Array (Bounds are now exactly 0 to 11)
--       token_names =
--         listArray
--           (0, 11)
--           [ "epsilon",
--             "error",
--             "dummy",
--             "%start_parse",
--             "Exp",
--             "int",
--             "+",
--             "-",
--             "*",
--             "(",
--             ")",
--             "%eof"
--           ],
--       -- 3. The Rules (Productions)
--       -- Format: Production LHS [RHS] (Action, [Bindings]) Priority
--       productions =
--         [ -- Rule 0: The Hidden Start Rule (%start_parse -> Exp %eof)
--           -- This provides the anchor for lookahead propagation!
--           Production 3 [4, 11] ((), [1, 2]) No,
--           -- Rule 1: Exp -> Exp + Exp
--           Production 4 [4, 6, 4] ((), [1, 3]) No,
--           -- Rule 2: Exp -> Exp - Exp
--           Production 4 [4, 7, 4] ((), [1, 3]) No,
--           -- Rule 3: Exp -> Exp * Exp
--           Production 4 [4, 8, 4] ((), [1, 3]) No,
--           -- Rule 4: Exp -> ( Exp )
--           Production 4 [9, 4, 10] ((), [2]) No,
--           -- Rule 5: Exp -> int
--           Production 4 [5] ((), [1]) No
--         ],
--       -- 4. Fast Lookup Functions
--       lookupProdNo = \id -> (productions manualGrammar) !! id,
--       lookupProdsOfName = \name ->
--         if name == 3
--           then [0] -- %start is defined by Rule 0
--           else
--             if name == 4
--               then [1 .. 5] -- Exp is defined by Rules 1 through 5
--               else [],
--       -- 5. Parser Entry Points (Function Name, Start ID, Target ID, Partial)
--       starts = [("parseCalc", 3, 4, False)],
--       -- 6. Conflict Resolution (Precedence)
--       priorities =
--         [ (6, Prio LeftAssoc 1), -- '+'
--           (7, Prio LeftAssoc 1), -- '-'
--           (8, Prio LeftAssoc 2) -- '*' (Higher precedence)
--         ],
--       types = listArray (0, 11) (repeat Nothing),
--       token_specs = []
--     }

-- -- Grammar for handling parentheses
-- -- epsilon = 0, error = 1, dummy = 2
-- -- start = 3..s, non-term = s..n, term = n..m, eof = m
-- parensGrammar :: Grammar ()
-- parensGrammar = let {
--   epsilon = MkName 0 ; errorT = MkName 1 ; dummy = MkName 2 ; start = MkName 3 ; expr = MkName 4 ; lparen = MkName 5 ; rparen = MkName 6 ; eof = MkName 7 ;
-- } in Grammar {
--   token_names = listArray (0, 7) [
--     "epsilon", "error", "dummy", "%start", "Expr", "(", ")", "%eof"
--   ], 
--   non_terminals = [start, expr], 
--   terminals = [errorT, lparen, rparen, eof], 
--   productions = [
--     Production start [expr, eof] ((), [1, 2]) No,
--     Production expr [lparen, expr, rparen, expr] ((), [2, 4]) No,
--     Production expr [] ((), []) No
--   ],
--   first_nonterm = start, 
--   first_term = lparen, 
--   eof_term = eof, 
--   lookupProdNo = (productions parensGrammar !!),
--   lookupProdsOfName = \name ->
--     if name == start then [0]
--     else if name == expr then [1, 2]
--     else [],
--   starts = [("parseParens", start, expr, False)],
--   types = listArray (0, 7) (repeat Nothing), 
--   priorities = [], 
--   token_specs = []
-- }

jiexiTest :: Text
jiexiTest = """%start Expr

%token int 0
%token '+' 1
%token '-' 2
%token '*' 3
%token '(' 4
%token ')' 5

%left '+' '-'
%left '*'

%%

Expr
  : int { t1 #}
  | Expr '+' Expr { t1 + t3 #}
  | Expr '-' Expr { t1 - t3 #}
  | Expr '*' Expr { t1 * t3 #}
  | '(' Expr ')' { t2 #}"""

main :: IO ()
main = do
  testFile <- X.readFile "test.jiexi"
  parsed <- parseJiexiFile testFile
  print parsed
  putStrLn $ replicate 60 '='
  print (normalizeJiexiFile parsed)
  putStrLn $ replicate 60 '='
  let context = jiexiInfoToCodeGenCtx (normalizeJiexiFile parsed)
  -- print context
  putStrLn $ replicate 60 '='
  putStrLn $ prettyPrintGrid (happyGrammar context) (happyTable context)
  putStrLn $ replicate 60 '='
  print =<< grammarToMetadata testFile

-- main1 :: IO ()
-- main1 = do
--   let tables = genTables select_first_reduction manualGrammar
--   putStrLn $ prettyPrintGrid manualGrammar tables

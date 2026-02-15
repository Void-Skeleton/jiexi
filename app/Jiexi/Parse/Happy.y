{
module Jiexi.Parse.Happy where

import Data.Sequence (Seq(..), (<|), (|>))
import Data.Sequence qualified as Q
import Jiexi.Parse.Types
}
{-
The syntax file for Jiexi's grammar specification files
Jiexi wraps Haskell's Happy parser generator, and hence follows a similar syntax: 
  <directives>
  %%
  <grammar>
Jiexi takes a grammar file, and generates Typst code for a shift reduce parser. 
This Typst code is then passed as a string to Typst and reified into a Typst value by `eval()`. 

Now, the directives may call some user-defined Typst functions. 
These functions is specified at the Typst level and passed to `eval()` as the `locals` argument, 
  and they are semantically available within the grammar rules. However, they are never passed
  to Haskell. 
This eliminates the need of a user-defined module header/trailer, as present in Happy files. 

Directives: 
- %start <non-terminal>
    Defines the non-terminal to obtain as the parse result. If not present, the first non-terminal in the grammar will be used.
- %token <token-name> <token-tag>
    Defines a token with the given name and index. 
    In Haskell, we have types and all tokens belong to a given type. In Typst, we don't have types. 
    The token stream is given as two arrays, an array for token tags, and an array for token values. 
    The token tags are used to identify the type of each token, while the token values hold the actual content of the tokens.
- %left <token-name>, %right <token-name>, %nonassoc <token-name>
    Defines the associativity of the given token. Any declaration of this type is assumed to have higher precedence than those above it and lower than those below it. 
  Associativity is used to resolve shift-reduce conflicts. More specifically: 
  - The precedence and associativity of a production is that of its last terminal symbol. 
  - In case of a shift-reduce conflict, we compare the precedence of the shifted token against
      that of the production rule to reduce with: 
    - token precedence > production precedence: SHIFT
    - token precedence < production precedence: REDUCE
    - token precedence == production precedence: 
      Since it is forced that terminals/rules with the same precedence have the same associativity, 
        we look at the associativity of either the rule or the terminal token: 
      - SHIFT if left associative; 
      - REDUCE if right associative; 
      - ERROR if non-associative. 

Grammar: 
    The grammar consists of a list of production rules. Each production consists of a non-terminal symbol on the left, followed by a colon, followed by one or more expansions on the right, separated by |. Each expansion has some Typst code associated with it, enclosed in braces as usual. 

    In Happy, in the associated Haskell code, we use $1, $2, .. to refer to the values of tokens in the production. In Typst, we use t1, t2, .. to refer to them in the Typst code in curly braces. Also, since Typst code might contain {} pairs itself, the code block is terminated using the sequence `#}`. 
    - Example: Exp : Exp '+' Exp { t1 + t2 #}

    A production rule may be optionally annotated with a directive: 
    - %prec <token-name>: This makes the rule inherit the precedence of the specified token.
    - %shift: This gives the rule the lowest precedence. Any shift-reduce conflict involving this
        rule is always resolved as SHIFT. 
-}

%tokentype { JiexiToken }
%name happyParse File
%token startD { DirectiveT StartTD }
%token tokenD { DirectiveT TokenTD }
%token leftD { DirectiveT LeftTD }
%token rightD { DirectiveT RightTD }
%token nonassocD { DirectiveT NonassocTD }
%token precD { DirectiveT PrecTD }
%token shiftD { DirectiveT ShiftTD }
%token int { NumberT $$ }
%token tokname { TokenNameT $$ }
%token code { CodeT $$ }
%token '%%' { SeparatorT }
%token ':' { ColonT }
%token '|' { PipeT }

%%

File
  : Directives '%%' Productions { JiexiFile $1 $3 }

Directive
  : startD tokname { StartD $2 }
  | tokenD tokname int { TokenD $2 $3 }
  | leftD TokNames { LeftD $2 }
  | rightD TokNames { RightD $2 }
  | nonassocD TokNames { NonassocD $2 }

Directives
  : {- empty -} { Q.empty :: Seq Directive }
  | Directives Directive { $1 |>! $2 }

Production
  : tokname ':' Rules { Production $1 $3 }

Productions
  : {- empty -} { Q.empty :: Seq Production }
  | Productions Production { $1 |>! $2 }

Rule
  : TokNames code { Rule $1 NoRD $2 }
  | TokNames precD tokname code { Rule $1 (PrecRD $3) $4 }
  | TokNames shiftD code { Rule $1 ShiftRD $3 }

Rules
  : Rule { Q.singleton $1 :: Seq Rule }
  | Rules '|' Rule { $1 |>! $3 }

TokNames
  : {- empty -} { Q.empty :: Seq TokenName }
  | TokNames tokname { $1 |>! $2 }

{
happyError :: [JiexiToken] -> a
happyError tokens = error ("Parse error with remaining tokens: " ++ show tokens)

(|>!) :: Seq a -> a -> Seq a
xs |>! !x = xs |> x
}
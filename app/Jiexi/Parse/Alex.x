{
module Jiexi.Parse.Alex where

import Data.Text (Text)
import Data.Text qualified as X
import Jiexi.Parse.Types
}

%wrapper "posn-strict-text"

$digit = [0-9]
$alpha = [a-zA-Z]
$idchar = [$alpha$digit\_]
@number = [\-\+]?($digit)+
@directive = \%[^$white]+
@separator = \%\%
@tokenname = $alpha$idchar*|\'([^\']|\\\')*\'|\"([^\"]|\\\")*\"

@not_hash = [^\#]|\n
$not_brace = [^\}]
@codeblock = \{(@not_hash|\#+$not_brace)*\#+\}

tokens :-
  $white+ ;
  @separator { \_ _ -> SeparatorT }
  @directive { \_ s -> DirectiveT (nameDirective s) }
  @number { \_ s -> NumberT (read $ X.unpack s) }
  @tokenname { \_ s -> TokenNameT s }
  @codeblock { \_ s -> CodeT (prepareCodeBlock s) }
  \: { \_ _ -> ColonT }
  \| { \_ _ -> PipeT }
  . { \(AlexPn _ line col) s -> error $ "Lexical error: unexpected " ++ X.unpack s ++ " at line " ++ show line ++ ", col " ++ show col }

{
nameDirective :: Text -> DirectiveT
nameDirective s = case X.unpack s of
  "%start"    -> StartTD
  "%token"    -> TokenTD
  "%left"     -> LeftTD
  "%right"    -> RightTD
  "%nonassoc" -> NonassocTD
  "%prec"     -> PrecTD
  "%shift"    -> ShiftTD
  dir        -> error $ "Unknown directive: " ++ dir

prepareCodeBlock :: Text -> Text
prepareCodeBlock s = X.take (X.length s - 3) $ X.drop 1 s -- Drop leading { and trailing #}
}
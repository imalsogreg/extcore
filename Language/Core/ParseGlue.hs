module Language.Core.ParseGlue where

import Language.Core.Encoding
import Data.List

data ParseResult a = OkP a | FailP String
type P a = String -> Int -> ParseResult a

instance Show a => Show (ParseResult a)
  where show (OkP r) = show r
        show (FailP s) = s

thenP :: P a -> (a -> P b) -> P b
m `thenP`  k = \ s l -> 
  case m s l of 
    OkP a -> k a s l
    FailP s1 -> FailP s1

returnP :: a -> P a
returnP m _ _ = OkP m

failP :: String -> P a
failP s s' _ = FailP (s ++ ":" ++ s')

data Token =
   TKmodule 
 | TKdata 
 | TKnewtype 
 | TKforall 
 | TKrec 
 | TKlet 
 | TKin 
 | TKcase 
 | TKof 
 | TKcast
 | TKnote 
 | TKexternal
 | TKdynexternal
 | TKlabel
 | TKwild
 | TKoparen 
 | TKcparen 
 | TKobrace
 | TKcbrace
 | TKhash
 | TKeq 
 | TKcoloncolon 
 | TKstar 
 | TKrarrow 
 | TKlambda
 | TKbiglambda
 | TKat 
 | TKdot
 | TKcolon
 | TKquestion
 | TKsemicolon
 | TKname String 
 | TKcname String
 | TKinteger Integer 
 | TKrational Rational
 | TKstring String 
 | TKchar Char 
 | TKsym
 | TKtrans
 | TKunsafe
 | TKleft
 | TKright
 | TKinst
 | TKpercent
 | TKEOF

----------------
-- ugh
splitModuleName :: EncodedString -> ([String], String)
splitModuleName mn = 
   let decoded = zDecodeString mn
       -- Triple ugh.
       -- We re-encode the individual parts so that:
       -- main:Foo_Bar.Quux.baz
       -- prints as:
       -- main:FoozuBarziQuux.baz
       -- and not:
       -- main:Foo_BarziQuux.baz
       parts   = map zEncodeString $ filter (notElem '.') $ groupBy 
                   (\ c1 c2 -> c1 /= '.' && c2 /= '.') 
                 decoded in
     (take (length parts - 1) parts, last parts)
----------------

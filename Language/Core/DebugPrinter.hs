{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}
module Language.Core.DebugPrinter(removeTypeBinds, pmodule, PrintOpts(..),
  defOpts) where

import Data.Data (Data)
import Data.Generics.Aliases (mkT)
import Data.Generics.Schemes (everywhere)

import Language.Core.Core
import Language.Core.Encoding (EncodedString, zDecodeString, zEncodeString)
import Language.Core.Printer (escape)

import Text.PrettyPrint.HughesPJ

{-
****************
Contributed by Neil Brown, nccb2@kent.ac.uk
****************
-}

{- Uncomment the following code to yield a simple command-line wrapper for 
   the debug printer. -}

{-
import Language.Core.Parser
import Language.Core.ParseGlue
import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

-- ============================================================
-- Command-line utility bits:
-- ============================================================

cmdName :: String
cmdName = "coreprint"

instance Monad (Either String) where
  return = Right
  (>>=) m k = either Left k m

options :: [OptDescr (PrintOpts -> Either String PrintOpts)]
options = [Option [] ["decode-names"] (onOff $ \b p -> p { decodeNames = b })
             "Decode names (show unescaped characters)"
          ,Option [] ["qual-names"] (onOff $ \b p -> p { ignoreQuals = not b })
             "Show fully qualified names"
          ,Option [] ["kinds"] (onOff $ \b p -> p { ignoreKinds = not b })
             "Show kinds"
          ,Option [] ["types"] (onOff $ \b p -> p { ignoreTypeBinds = not b })
             "Show types"
          ,Option [] ["dicts"] (onOff $ \b p -> p { ignoreDicts = not b })
             "Show dictionary parameters"
          ,Option [] ["infix"] (onOff $ \b p -> p { infixOperators = b })
             "Show operators in infix form"
          ,Option ['h'] ["help"] (NoArg $ const $ Left $ usageInfo cmdName options)
             "Show help"
          ]
  where
    onOff :: (Bool -> PrintOpts -> PrintOpts) -> ArgDescr (PrintOpts -> Either String PrintOpts)
    onOff f = OptArg toBool "'on' or 'off'"
      where
        toBool (Just "on") = Right . f True
        toBool (Just "off") = Right . f False
        toBool (Just s) = const $ Left $ "Unknown setting: '" ++ s ++ "'"
        toBool Nothing = Right . f True -- no arg means on

main :: IO ()
main = do (confMod, fileNames, err) <- getOpt Permute options <$> getArgs
          if not $ null err
            then hPutStrLn stderr $ unlines err
            else case (foldr (>=>) return confMod $ defOpts) of
              -- TODO stop help text ending up on stderr:
              Left err -> hPutStrLn stderr err
              Right conf -> mapM_ (parseAndPrint conf) fileNames

-- | Parses the given filename and prints it out again on stdout
-- using the given printing options
parseAndPrint :: PrintOpts -> FilePath -> IO ()
parseAndPrint conf fileName
  = do s1 <- readFile fileName
       let r <- parse s1 1
       case r of
         FailP err -> hPutStrLn stderr $ "Error parsing core file: " ++ show err
         OkP m -> let m' | ignoreTypeBinds conf = removeTypeBinds m
                           | otherwise = m
                    in putStrLn $ render $ pmodule conf m'

-}

-- ============================================================
-- Printing bits:
-- ============================================================

-- Removes all uses of the Appt constructor of Exp from the tree.
--
-- Easiest to do this as a separate pass before printing, and a quick
-- bit of SYB makes it nice and short
removeTypeBinds :: Data a => a -> a
removeTypeBinds = everywhere (mkT removeTypeBind)
  where
    removeTypeBind :: Exp -> Exp
    removeTypeBind (Appt e _) = e
    removeTypeBind e = e

type Print a = PrintOpts -> a -> Doc
type Print2 a b = PrintOpts -> a -> b -> Doc

-- | Options controlling the printing of external core.
data PrintOpts = PrintOpts
  { decodeNames :: Bool
    -- ^ Whether to decode names into their recognisable form, i.e. Data.List.nub
    -- instead of DataziListzinub.
  , ignoreQuals :: Bool
    -- ^ Whether to ignore module names on fully qualified names, i.e. nub instead
    -- of Data.List.nub.
  , ignoreKinds :: Bool
    -- ^ Whether to ignore kinds when printing out.
  , ignoreTypeBinds :: Bool
    -- ^ Whether to ignore type annotations when printing out.
  , ignoreDicts :: Bool
    -- ^ Whether to ignore things that look like dictionary parameters (ignores
    -- all parameters where the name begins with a dollar sign).
  , infixOperators :: Bool
    -- ^ Whether to write Haskell operators in infix form, i.e. p + x rather than
    -- (+) p x.
  }

-- | The default printing options.  Everything is True except ignoreQuals.
defOpts :: PrintOpts
defOpts = PrintOpts
  {decodeNames = True
  ,ignoreQuals = False
  ,ignoreKinds = True
  ,ignoreTypeBinds = True
  ,ignoreDicts = True
  ,infixOperators = True
  }

-- Checks if a function name is a parameter by examining the first character.
-- Core notation often includes an extra (alphanumeric) suffix, so the presence
-- of letters later on doesn't preclude it being an operator; the first character
-- is the determinant.
isOperator :: String -> Bool
isOperator (x:_) = x `elem` "!#$%&*+./<=>?@\\^|-~"
isOperator _ = False

-- Checks if the expression is a dictionary parameter.  See isDictionaryName
isDictionary :: Exp -> Bool
isDictionary (Var (_, n)) = isDictionaryName (zDecodeString n)
isDictionary (Appt e _) = isDictionary e
isDictionary _ = False

-- Checks if the String could be a dictionary parameter.  This is a heuristic,
-- and says yes if the parameter begins with a $ and the rest of the string is
-- non-empty and is not an operator by itself.  (Dictionary parameters in core
-- seem to begin with a dollar then are followed by letters.)
isDictionaryName :: String -> Bool
isDictionaryName ('$':n) | not (null n || isOperator n) = True
isDictionaryName _ = False

-- Indents the given document by two spaces
indent :: Doc -> Doc
indent = nest 2

pmodule :: Print Module
pmodule o (Module mname tdefs vdefgs) =
  (text "%module" <+> panmname o mname)
  $$ indent ((vcat (map ((<> char ';') . ptdef o) tdefs))
	     $$ (vcat (map ((<> char ';') . pvdefg o) vdefgs)))
  <> (if ((not.null) tdefs) || ((not.null) vdefgs) then char '\n' else empty)
         -- add final newline; sigh.

ptdef :: Print Tdef
ptdef o (Data qtcon tbinds cdefs) =
  (text "%data" <+> pqname o True qtcon <+> (hsep (map (ptbind o) tbinds)) <+> char '=')
  $$ indent (braces ((vcat (punctuate (char ';') (map (pcdef o) cdefs)))))
ptdef o (Newtype qtcon coercion tbinds tyopt) =
  text "%newtype" <+> pqname o True qtcon <+> pqname o True coercion 
    <+> (hsep (map (ptbind o) tbinds)) $$ indent repclause
       where repclause = char '=' <+> pty o tyopt

pcdef :: Print Cdef
pcdef o (Constr qdcon tbinds tys)  =
  (pqname o True qdcon) <+> (sep [hsep (map (pattbind o) tbinds),sep (map (paty o) tys)])

pname :: Print String
pname o
  | decodeNames o = text . zDecodeString
  | otherwise = text

pqname :: Print2 Bool (Qual EncodedString)
pqname o prefix (m,v)
  | ignoreQuals o = maybeParen $ pname o v
  | otherwise     = maybeParen $ pmname o m <> pname o v
  where
    maybeParen
      | decodeNames o && isOperator (zDecodeString v) && prefix = parens
      | otherwise = id

-- be sure to print the '.' here so we don't print out
-- ".foo" for unqualified foo...
pmname :: Print (Maybe AnMname)
pmname _ Nothing = empty
pmname o (Just m) = panmname o m <> char '.'

panmname :: Print AnMname
panmname o (M (P pkgName, parents, name)) =
  let parentStrs = map (pname o) parents in
         pname o pkgName <> char ':' <>
         -- This is to be sure to not print out:
         -- main:.Main for when there's a single module name
         -- with no parents.
             (case parentStrs of
                [] -> empty
                _  -> hcat (punctuate modSep
                        (map (pname o) parents)) 
                      <> modSep)
             <> pname o name
  where
    modSep = if decodeNames o then char '.' else hierModuleSeparator

    -- note that this is not a '.' but a Z-encoded '.':
    -- GHCziIOBase.IO, not GHC.IOBase.IO.
    hierModuleSeparator = text (zEncodeString ".")

ptbind :: Print Tbind
ptbind o (t,Klifted) = pname o t
ptbind o (t,k)
  | ignoreKinds o = pname o t
  | otherwise = parens (pname o t <> text "::" <> pkind o k)

pattbind :: Print (Tvar, Kind)
pattbind o (t,k) = char '@' <> ptbind o (t,k)

pakind :: Print Kind
pakind _ (Klifted) = char '*'
pakind _ (Kunlifted) = char '#'
pakind _ (Kopen) = char '?'
pakind o k = parens (pkind o k)

pkind :: Print Kind
pkind o (Karrow k1 k2) = parens (pakind o k1 <> text "->" <> pkind o k2)
pkind o (Keq from to) = peqkind o (from,to)
pkind o k = pakind o k

peqkind :: Print (Ty, Ty)
peqkind o (t1, t2) = parens (parens (pty o t1) <+> text ":=:" <+> parens (pty o t2)) 

paty :: Print Ty
paty o (Tvar n) = pname o n
paty o (Tcon c) = pqname o True c
paty o t = parens (pty o t)

pbty :: Print Ty
pbty o (Tapp(Tapp(Tcon tc) t1) t2) | tc == tcArrow = parens(fsep [pbty o t1, text "->",pty o t2])
pbty o (Tapp t1 t2) = pappty o t1 [t2] 
pbty o t = paty o t

pty :: Print Ty
pty o (Tapp(Tapp(Tcon tc) t1) t2) | tc == tcArrow = fsep [pbty o t1, text "->",pty o t2]
pty o (Tforall tb t) = text "%forall" <+> pforall o [tb] t
pty o (TransCoercion t1 t2) = 
    (sep ([text "%trans", paty o t1, paty o t2]))
pty o (SymCoercion t) = 
    (sep [text "%sym", paty o t])
pty o (UnsafeCoercion t1 t2) = 
    (sep [text "%unsafe", paty o t1, paty o t2])
pty o (LeftCoercion t) = 
    (text "%left" <+> paty o t)
pty o (RightCoercion t) = 
    (text "%right" <+> paty o t)
pty o (InstCoercion t1 t2) = 
    (sep [text "%inst", paty o t1, paty o t2])
pty o t = pbty o t

pappty :: Print2 Ty [Ty]
pappty o (Tapp t1 t2) ts = pappty o t1 (t2:ts)
pappty o t ts = sep (map (paty o) (t:ts))

pforall :: Print2 [Tbind] Ty
pforall o tbs (Tforall tb t) = pforall o (tbs ++ [tb]) t
pforall o tbs t = hsep (map (ptbind o) tbs) <+> char '.' <+> pty o t

pvdefg :: Print Vdefg
pvdefg o (Rec vdefs) = text "%rec" $$ braces (indent (vcat (punctuate (char ';') (map (pvdef o) vdefs))))
pvdefg o (Nonrec vdef) = pvdef o vdef

pvdef :: Print Vdef
pvdef o (Vdef (qv,t,e))
  | ignoreTypeBinds o = sep [pqname o True qv <+> char '=', indent (pexp o e)]
  | otherwise  = sep [pqname o True qv <+> text "::" <+> pty o t <+> char '=',
		     indent (pexp o e)]

paexp :: Print Exp
paexp o (Var x) = pqname o True x
paexp o (Dcon x) = pqname o True x
paexp o (Lit l) = plit o l
paexp o e = parens (pexp o e)

plamexp :: Print2 [Bind] Exp
plamexp o bs (Lam b e) = plamexp o (bs ++ [b]) e
plamexp o bs e = sep [sep (map (pbind o) bs) <+> text "->",
		    indent (pexp o e)]

pbind :: Print Bind
pbind o (Tb tb)
  | ignoreTypeBinds o = empty
  | otherwise = char '@' <+> ptbind o tb
pbind o (Vb vb) = pvbind o vb

pappexp :: Print2 Exp [Either Exp Ty]
pappexp o (App e1 e2) as
  | isDictionary e2 && ignoreDicts o = pappexp o e1 as
  | otherwise = pappexp o e1 (Left e2:as)
pappexp o (Appt e t) as = pappexp o e (Right t:as)
pappexp o e as
  = case (e, as) of
      (Var qn@(_, n), [Left l, Left r])
         | decodeNames o && isOperator (zDecodeString n) && infixOperators o
            -> sep [paexp o l, pqname o False qn, paexp o r]
      _ -> fsep (paexp o e : map pa as)
           where pa (Left ex) = paexp o ex
		 pa (Right t)
                  | ignoreTypeBinds o = empty
                  | otherwise = char '@' <+> paty o t

pexp :: Print Exp
pexp o (Lam b e) = char '\\' <+> plamexp o [b] e
pexp o (Let vd e) = (text "%let" <+> pvdefg o vd) $$ (text "%in" <+> pexp o e)
pexp o (Case e vb t alts) = sep [text "%case" <+> paty o t <+> paexp o e,
			     text "%of" <+> pvbind o vb]
			$$ (indent (braces (vcat (punctuate (char ';') (map (palt o) alts)))))
pexp o (Cast e t) = (text "%cast" <+> parens (pexp o e)) $$ paty o t
pexp o (Note s e) = (text "%note" <+> pstring o s) $$ pexp o e
-- TODO: ccall shouldn't really be there
pexp o (External n t) = (text "%external ccall" <+> pstring o n) $$ paty o t
pexp o e = pappexp o e []

pvbind :: Print (String, Ty)
pvbind o (x,t)
 | ignoreTypeBinds o = pname o x
 | otherwise = parens(pname o x <> text "::" <> pty o t)

palt :: Print Alt
palt o (Acon c tbs vbs e) =
	sep [pqname o True c, 
	     sep (map (pattbind o) tbs),
	     sep (map (pvbind o) vbs) <+> text "->"]
        $$ indent (pexp o e)
palt o (Alit l e) = 
	(plit o l <+>  text "->")
	$$ indent (pexp o e)
palt o (Adefault e) = 
	(text "%_ ->")
	$$ indent (pexp o e)

plit :: Print Lit
plit o (Literal cl t)
  | ignoreTypeBinds o = pclit o cl
  | otherwise = parens (pclit o cl <> text "::" <> pty o t)

pclit :: Print CoreLit
pclit _ (Lint i) = integer i
-- makes sure to print it out as n % d
pclit _ (Lrational r) = text (show r)
pclit _ (Lchar c) = text ("\'" ++ escape [c] ++ "\'")
pclit o (Lstring s) = pstring o s

pstring :: Print String
pstring _ s = doubleQuotes(text (escape s))

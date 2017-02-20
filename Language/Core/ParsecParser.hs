-- | This module currently is buggy. (Patches welcome.)
--   Use "Language.Core.Parser" instead. See also "Language.Core.ParseGlue".

{-# LANGUAGE FlexibleContexts #-}

module Language.Core.ParsecParser (parseCore, coreModuleName, coreTcon,
  coreQualifiedName, upperName, identifier, coreType, coreKind,
  coreTbinds, parens, braces, topVbind, pt) where

import Language.Core.Core
import Language.Core.Check
import Language.Core.PrimCoercions
import Language.Core.ParseGlue

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.Char
import Data.List
import Data.Maybe
import Data.Ratio

parseCore :: FilePath -> IO (Either ParseError Module)
parseCore = parseFromFile coreModule

coreModule :: Parser Module
coreModule = do
   whiteSpace
   reserved "module"
   mName      <- coreModuleName
   whiteSpace
   tdefs      <- option [] coreTdefs
-- Need to keep factoring! With the backtracking removed from reserved,
-- there's now a conflict between %newtype and %rec. But this is do-able.
   vdefGroups <- coreVdefGroups
   eof
   return $ Module mName tdefs vdefGroups

coreModuleName :: Parser AnMname
coreModuleName = do
   pkgName      <- corePackageName
   char ':'
   (modHierarchy,baseName) <- coreHierModuleNames
   return $ M (pkgName, modHierarchy, baseName)

corePackageName :: Parser Pname
-- Package names can be lowercase or uppercase!
corePackageName = (identifier <|> upperName) >>= (return . P)

coreHierModuleNames :: Parser ([Id], Id)
coreHierModuleNames = do
   parentName <- upperName
   return $ splitModuleName parentName

upperName :: Parser String
upperName = do
   firstChar <- upper
   if isUpper firstChar
     then do
        rest <- many (P.identLetter extCoreDef)
        whiteSpace
        return $ firstChar:rest
     else
       unexpected "expected an uppercase name here"

coreTdefs :: Parser [Tdef]
coreTdefs = many coreTdef 

coreTdef :: Parser Tdef
coreTdef = withSemi (char '%' >> (coreDataDecl <|> coreNewtypeDecl))

withSemi :: Parser a -> Parser a
withSemi p = p `withTerminator` ";"

withTerminator :: Parser a -> String -> Parser a
withTerminator p term = do
   x <- p
   symbol term
   return x

coreDataDecl :: Parser Tdef
coreDataDecl = do
  reserved' "data"
  tyCon  <- coreQualifiedCon
  whiteSpace -- important
  tBinds <- coreTbinds
  symbol "="
  cDefs  <- braces coreCdefs
  return $ Data tyCon tBinds cDefs

coreNewtypeDecl :: Parser Tdef
coreNewtypeDecl = do
  reserved' "newtype"
  tyCon  <- coreQualifiedCon
  whiteSpace
  coercionName <- coreQualifiedCon
  tBinds <- coreTbinds
  tyRep  <- coreTRep
  return $ Newtype tyCon coercionName tBinds tyRep

coreQualifiedCon :: Parser (Mname, Id)
coreQualifiedCon = do
  (P pkgId) <- corePackageName
  maybeRest <- optionMaybe (char ':')
  case maybeRest of
    Just _ -> do
       -- It's qualified
       (modHierarchy, baseName) <- coreHierModuleNames
       char '.'
       conName <- upperName
       return (Just $ M (P pkgId, modHierarchy, baseName), conName)
    Nothing -> do
       -- It's unqualified
       if isUpperName pkgId
          then return (Nothing,pkgId)
          else (fail $ "Expected a constructor name, got: " ++ pkgId)

isUpperName :: String -> Bool
isUpperName "" = False
isUpperName (c:_) = isUpper c
 
coreQualifiedName :: Parser (Mname, Id)
coreQualifiedName = do
   (P packageIdOrVarName) <- corePackageName
   maybeRest <- optionMaybe (char ':' >> coreHierModuleNames)
   case maybeRest of
     Nothing ->  return (Nothing, packageIdOrVarName)
     Just (modHierarchy, baseName) -> do
       -- qualified
               char '.'
               theId <- identifier
               return
                 (Just $ M (P packageIdOrVarName, modHierarchy, baseName),
                  theId)

coreTbinds :: Parser [Tbind]
coreTbinds = many coreTbind 

coreTbindsOrTyGen :: CharParser () String -> Parser ([Tbind],[Ty])
coreTbindsOrTyGen separator = do
  res <- optionMaybe ((do
           symbol "("
           sep <- optionMaybe separator
           case sep of
             Nothing -> do t <- coreType
                           symbol ")"
                           return ([], [t])
             Just _ -> do tb <- coreTbindGen separator 
                          symbol ")"
                          (tbs,tys) <- coreTbindsOrTyGen separator
                          return (tb:tbs,tys)) <|> (do separator
                                                       b <- coreTbindGen'
                                                       (tbs,tys) <- coreTbindsOrTyGen separator
                                                       return (b:tbs,tys)))
                                    
  return $ fromMaybe ([],[]) res

coreTbind :: Parser Tbind
coreTbind = (coreTbindGen pzero) <|> parens coreTbind

coreTbindGen :: CharParser () a -> Parser Tbind
coreTbindGen sep = do
  optionMaybe sep
  coreTbindGen'

coreTbindGen' :: Parser (String,Kind)
coreTbindGen' = do
  tyVar <- identifier
  kdecl <- optionMaybe (symbol "::" >> coreKind)
  return (tyVar, fromMaybe Klifted kdecl)

coreCdefs :: Parser [Cdef]
coreCdefs = sepBy coreCdef (symbol ";")

coreCdef :: Parser Cdef
coreCdef = do
  dataConName <- coreQualifiedCon
  whiteSpace -- important!
  (tbs,tys1)   <- coreTbindsOrTyGen (symbol "@")
  tys2         <- many coreAtySaturated
  return $ Constr dataConName tbs (tys1++tys2)

coreTRep :: Parser Ty
-- note that the "=" is inside here since if there's
-- no rhs for the newtype, there's no "="
coreTRep = symbol "=" >> coreType

coreType :: Parser Ty
coreType = coreForallTy <|> (do
             hd <- coreBty
             -- whiteSpace is important!
        --     whiteSpace
             -- This says: If there is at least one ("-> ty"..) thing,
             -- use it. If not, don't consume any input.
             rest <- many (symbol "->" >> coreType)
             return $ case rest of
                        [] -> hd 
                        _  -> foldl' Tapp (Tcon tcArrow) (hd:rest))

coreBty :: Parser Ty
coreBty = do
  hd <- coreAty
  whiteSpace
                         -- The "try" is necessary:
                         -- otherwise, parsing "T " fails rather
                         -- than returning "T".
                        -- again, not sure I think that makes sense
  maybeRest <- many coreAtySaturated
  return $ (case hd of
             -- so I'm not sure I like this... it's basically doing
             -- typechecking (kind-checking?) in the parser.
             -- However, the type syntax as defined in Core.hs sort of
             -- forces it.
             ATy t     -> foldl' Tapp t maybeRest
             Trans k   -> app k 2 maybeRest "trans"
             Sym k     -> app k 1 maybeRest "sym"
             Unsafe k  -> app k 2 maybeRest "unsafe"
             LeftCo k  -> app k 1 maybeRest "left"
             RightCo k -> app k 1 maybeRest "right"
             InstCo k  -> app k 2 maybeRest "inst")
                 where app k arity args _ | length args == arity = k args
                       app _ _ args err = 
                           primCoercionError (err ++ 
                             ("Args were: " ++ show args))

coreAtySaturated :: Parser Ty
coreAtySaturated = do
   t <- coreAty
   case t of
     ATy ty -> return ty
     _     -> unexpected "coercion ty"

coreAty :: Parser ATyOp
          -- which, by the way, also handles type vars
coreAty = coreTcon <|> ((parens coreType) >>= (return . ATy))

{-
coreTvar :: Parser Ty
coreTvar = identifier >>= (return . Tvar)
-}

coreTcon :: Parser ATyOp
-- TODO: Change the grammar
-- A Tcon can be an uppercase type constructor
-- or a lowercase (always qualified) coercion variable
coreTcon =  
        (do
           char '%'
           maybeCoercion <- choice [symCo, transCo, unsafeCo,
                                    instCo, leftCo, rightCo]
           return $ case maybeCoercion of
              TransC  -> Trans (\ [x,y] -> TransCoercion x y)
              SymC    -> Sym (\ [x] -> SymCoercion x)
              UnsafeC -> Unsafe (\ [x,y] -> UnsafeCoercion x y)
              LeftC   -> LeftCo (\ [x] -> LeftCoercion x)
              RightC  -> RightCo (\ [x] -> RightCoercion x)
              InstC   -> InstCo (\ [x,y] -> InstCoercion x y))
    <|> (coreTvarOrQualifiedCon >>= (return . ATy))

coreTvarOrQualifiedCon :: Parser Ty
coreTvarOrQualifiedCon = do
   (P packageIdOrVarName) <- corePackageName
   maybeRest <- optionMaybe (char ':' >> coreHierModuleNames)
   case maybeRest of
     Nothing ->  return (Tvar packageIdOrVarName)
     Just (modHierarchy, baseName) -> do
       -- qualified
               char '.'
               theId <- upperName
               return $ Tcon
                 (Just $ M (P packageIdOrVarName, modHierarchy, baseName),
                  theId)

data CoercionTy = TransC | InstC | SymC | UnsafeC | LeftC | RightC

symCo, transCo, unsafeCo, instCo, leftCo, rightCo :: Parser CoercionTy
symCo    = string "sym"    >> return SymC
transCo  = string "trans"  >> return TransC
unsafeCo = string "unsafe" >> return UnsafeC
leftCo   = string "left"   >> return LeftC
rightCo  = string "right"  >> return RightC
instCo   = string "inst"   >> return InstC

coreForallTy :: Parser Ty
coreForallTy = do
  reserved "forall"
  tBinds <- many1 coreTbind
  symbol "."
  bodyTy <- coreType
  return $ foldr Tforall bodyTy tBinds

-- TODO: similar to coreType. should refactor
coreKind :: Parser Kind
coreKind = do
  hd <- coreAtomicKind 
  maybeRest <- option [] (many1 (symbol "->" >> coreKind))
  return $ foldl Karrow hd maybeRest

coreAtomicKind :: Parser Kind
coreAtomicKind = liftedKind <|> unliftedKind 
       <|> openKind <|> parens (coreKind <|> do
                                  (from,to) <- equalityKind
                                  return $ Keq from to)

liftedKind :: Parser Kind
liftedKind = do
  symbol "*"
  return Klifted

unliftedKind :: Parser Kind
unliftedKind = do
  symbol "#"
  return Kunlifted

openKind :: Parser Kind
openKind = do
  symbol "?"
  return Kopen

equalityKind :: Parser (Ty,Ty)
equalityKind = do
  ty1 <- coreBty
  symbol ":=:"
  ty2 <- coreBty
  return (ty1, ty2)

-- Only used internally within the parser:
-- represents either a Tcon, or a continuation
-- for a primitive coercion
data ATyOp = 
   ATy Ty
 | Trans ([Ty] -> Ty)
 | Sym ([Ty] -> Ty)
 | Unsafe ([Ty] -> Ty)
 | LeftCo ([Ty] -> Ty)
 | RightCo ([Ty] -> Ty)
 | InstCo ([Ty] -> Ty)

coreVdefGroups :: Parser [Vdefg]
coreVdefGroups = option [] (do
  theFirstVdef <- coreVdefg
  symbol ";"
  others <- coreVdefGroups
  return $ theFirstVdef:others)

coreVdefg :: Parser Vdefg
coreVdefg = coreRecVdef <|> coreNonrecVdef

coreRecVdef :: Parser Vdefg
coreRecVdef = do
  reserved "rec"
  braces (sepBy1 coreVdef (symbol ";")) >>= (return . Rec)

coreNonrecVdef :: Parser Vdefg
coreNonrecVdef = coreVdef >>= (return . Nonrec)

coreVdef :: Parser Vdef
coreVdef = do
                    -- Same sort of thing as the qualifiedName refactoring (could I use that code?)
  (vdefLhs, vdefTy) <- try topVbind <|> (do
                        (v, ty) <- lambdaBind
                        return (unqual v, ty))
  whiteSpace
  symbol "="
  whiteSpace
  vdefRhs  <- coreFullExp
  return $ Vdef (vdefLhs, vdefTy, vdefRhs) 

coreAtomicExp :: Parser Exp
coreAtomicExp = do
-- For stupid reasons, the whiteSpace is necessary.
-- Without it, (pt coreAppExp "w a:B.C ") doesn't work.
-- comment should be out of date now
                                -- should rewrite so no backtracking is required
  res <- choice [coreDconOrVar, parens (coreLit <|> coreFullExp)]
  whiteSpace
  return res

coreFullExp :: Parser Exp
coreFullExp = choice [coreLam, coreLet,
  coreCase, coreCast, coreNote, coreExternal, coreLabel, coreAppExp]
-- delete this comment
-- The "try" is necessary so that we backtrack
-- when we see a var (that is not an app)
-- actually, why not just fold the latter case into the coreAppExp case?
--    <|> coreAtomicExp

coreAppExp :: Parser Exp
coreAppExp = do
-- notes:
-- it's important to have a separate coreAtomicExp (that any app exp
-- begins with) and to define the args in terms of many1.
-- previously, coreAppExp could parse either an atomic exp (an app with
-- 0 arguments) or an app with >= 1 arguments, but that led to ambiguity.
    oper <- coreAtomicExp
    args <- many (whiteSpace >> ((coreAtomicExp >>= (return . Left)) <|>
             -- note this MUST be coreAty, not coreType, because otherwise:
             -- "A @ B c" gets parsed as "A @ (B c)"
             ((symbol "@" >> coreAtySaturated) >>= (return . Right))))
    return $ foldl' (\ op ->
                     either (App op) (Appt op)) oper args

-- Could refactor this (and the other qualified-name parsers)
coreDconOrVar :: Parser Exp
coreDconOrVar = do
  (P firstPart) <- corePackageName
  maybeRest <- optionMaybe (char ':' >> coreHierModuleNames)
  case maybeRest of
    Nothing | (c:_) <- firstPart, isUpper c -> return (Dcon (Nothing, firstPart))
    Nothing -> return (Var (Nothing, firstPart))
    Just (modHierarchy, baseName) -> do
       char '.'
       theId <- upperName <|> identifier
       return (case theId of
                 (c:_) | isUpper c -> Dcon (Just (M (P firstPart, modHierarchy, baseName)), theId)
                 _ -> Var (Just (M (P firstPart, modHierarchy, baseName)), theId))
      
coreLit :: Parser Exp
coreLit = coreLiteral >>= (return . Lit)

coreLiteral :: Parser Lit
coreLiteral = do
  l <- aLit
  symbol "::"
  t <- coreType
  return $ Literal l t

coreLam :: Parser Exp
coreLam = do
  symbol "\\"
  binds <- coreLambdaBinds
  symbol "->"
  body <- coreFullExp
  return $ foldr Lam body binds
coreLet :: Parser Exp
coreLet = do
  reserved "let"
  vdefg <- coreVdefg
  whiteSpace
  reserved "in"
  body <- coreFullExp
  return $ Let vdefg body 
coreCase :: Parser Exp
coreCase = do
  reserved "case"
  ty <- coreAtySaturated
  scrut <- coreAtomicExp
  reserved "of"
  vBind <- parens lambdaBind
  alts <- coreAlts
  return $ Case scrut vBind ty alts
coreCast :: Parser Exp
coreCast = do
  reserved "cast"
  whiteSpace
-- The parens are CRUCIAL, o/w it's ambiguous
  body <- parens coreFullExp
  ty <- coreAtySaturated
  return $ Cast body ty
coreNote :: Parser Exp
coreNote = do
  reserved "note"
  s <- stringLiteral
  e <- coreFullExp
  return $ Note s e
coreExternal :: Parser Exp
coreExternal = (do
  reserved "external"
  -- TODO: This isn't in the grammar, but GHC
  -- always prints "external ccall". investigate...
  symbol "ccall"
  s <- stringLiteral
  t <- coreAtySaturated
  return $ External s t) <|>
    -- TODO: I don't really understand what this does
                (do
    reserved "dynexternal"
    symbol "ccall"
    t <- coreAtySaturated
    return $ External "[dynamic]" t)
coreLabel :: Parser Exp
coreLabel = do
-- TODO: Totally punting this, but it needs to go in the grammar
-- or not at all
  reserved "label"
  s <- stringLiteral
  return $ External s tAddrzh

coreLambdaBinds :: Parser [Bind]
coreLambdaBinds = many1 coreBind

coreBind :: Parser Bind
coreBind = coreTbinding <|> coreVbind

coreTbinding, coreVbind :: Parser Bind
coreTbinding = coreAtTbind >>= (return . Tb)
coreVbind = parens (lambdaBind >>= (return . Vb))

coreAtTbind :: Parser Tbind
coreAtTbind = (symbol "@") >> coreTbind

topVbind :: Parser (Qual Var, Ty)
topVbind   = aCoreVbind coreQualifiedName
lambdaBind :: Parser (Var, Ty)
lambdaBind = aCoreVbind identifier

aCoreVbind :: Parser a -> Parser (a, Ty)
aCoreVbind idP =  do
  nm <- idP
  symbol "::"
  t <- coreType
  return (nm, t)


aLit :: Parser CoreLit
aLit = intOrRatLit <|> charLit <|> stringLit

intOrRatLit :: Parser CoreLit
intOrRatLit = do
 -- Int and lit combined into one to avoid ambiguity.
 -- Argh....
  lhs <- intLit
  maybeRhs <- optionMaybe (symbol "%" >> intLit)
  case maybeRhs of
    Nothing  -> return $ Lint lhs
    Just rhs -> return $ Lrational (lhs % rhs)

intLit :: Parser Integer
intLit = do
  sign <- option 1 (symbol "-" >> return (-1)) 
  n <- natural
  return (sign * n)

charLit :: Parser CoreLit
charLit = charLiteral >>= (return . Lchar)
 -- make sure this is right
   
stringLit :: Parser CoreLit
stringLit = stringLiteral >>= (return . Lstring)
 -- make sure this is right

coreAlts :: Parser [Alt]
coreAlts = braces $ sepBy1 coreAlt (symbol ";")

coreAlt :: Parser Alt
coreAlt = conAlt <|> litAlt <|> defaultAlt

conAlt :: Parser Alt
conAlt = do
  conName <- coreQualifiedCon
  whiteSpace
  (tBinds, vBinds) <- caseVarBinds
  symbol "->"
  rhs     <- coreFullExp
  return $ Acon conName tBinds vBinds rhs

caseVarBinds :: Parser ([Tbind], [Vbind])
caseVarBinds = do
     maybeFirstTbind <- optionMaybe coreAtTbind
     case maybeFirstTbind of
        Just tb -> do
           (tbs,vbs) <- caseVarBinds
           return (tb:tbs, vbs)
        Nothing -> do
           vbs <- many (parens lambdaBind)
           return ([], vbs)

litAlt :: Parser Alt
litAlt = do
  l <- parens coreLiteral
  symbol "->"
  rhs <- coreFullExp
  return $ Alit l rhs

defaultAlt :: Parser Alt
defaultAlt = do
  reserved "_"
  symbol "->"
  rhs <- coreFullExp
  return $ Adefault rhs

extCore :: P.TokenParser a
extCore = P.makeTokenParser extCoreDef

parens, braces :: CharParser st a -> CharParser st a
parens          = P.parens extCore    
braces          = P.braces extCore  
whiteSpace :: Parser ()  
-- newlines are allowed anywhere
whiteSpace      = P.whiteSpace extCore <|> (newline >> return ())
symbol :: String -> CharParser st String
symbol          = P.symbol extCore
identifier :: Parser String    
identifier      = -- P.identifier extCore
                   do c <- identStart extCoreDef
                      cs <- many (identLetter extCoreDef)
                      whiteSpace
                      return (c:cs)

reserved' :: String -> Parser ()
-- Keywords all begin with '%'
reserved' s     = -- P.reserved extCore ('%':s)
                   do caseString s
                      notFollowedBy (identLetter extCoreDef)
                      whiteSpace

reserved :: String -> Parser ()
reserved s     = -- P.reserved extCore ('%':s)
                   do char '%'
                      caseString s
                      notFollowedBy (identLetter extCoreDef)
                      whiteSpace
 
caseString :: String -> Parser String
-- ripped from the Parsec library
caseString name
        | caseSensitive extCoreDef  = string name
        | otherwise               = do{ walk name; return name }
        where
          walk []     = return ()
          walk (c:cs) = do{ caseChar c <?> msg; walk cs }

          caseChar c  | isAlpha c  = char (toLower c) <|> char (toUpper c)
                      | otherwise  = char c

          msg         = show name

natural :: CharParser st Integer
natural         = P.natural extCore
charLiteral :: CharParser st Char    
charLiteral     = P.charLiteral extCore
stringLiteral :: CharParser st String    
stringLiteral   = P.stringLiteral extCore    

-- dodgy since Core doesn't really allow comments,
-- but we'll pretend...
extCoreDef :: LanguageDef st
extCoreDef = LanguageDef { 
      commentStart    = "{-"
    , commentEnd      = "-}"
    , commentLine     = "--"
    , nestedComments  = True
    , identStart      = lower
    , identLetter     = lower <|> upper <|> digit <|> (char '\'')
    , opStart         = opLetter extCoreDef
    , opLetter        = oneOf ";=@:\\%_.*#?%"
    , reservedNames   = map ('%' :)
                          ["module", "data", "newtype", "rec",
                           "let", "in", "case", "of", "cast",
                           "note", "external", "forall"]
    , reservedOpNames = [";", "=", "@", "::", "\\", "%_",
                          ".", "*", "#", "?"]
    , caseSensitive   = True
    }       


pt :: Show a => CharParser () a -> String -> IO ()
pt pr s = do
  x <- parseTest pr s
  print x

{-
-- Stuff to help with testing in ghci.
pTest (Left a) = error (show a)
pTest (Right t) = print t

pTest1 :: Show a => CharParser () a -> String -> IO ()
pTest1 pr s = do
  let res = parse pr "" s
  pTest res

try_ = try
many_ = many
option_ = option
many1_ = many1
il = identLetter

andThenSym a b = do
  p <- a
  symbol b
  return p
-}

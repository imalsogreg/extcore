{-
   This module combines multiple External Core modules into
   a single module, including both datatype and value definitions. 
-}

{-# LANGUAGE FlexibleContexts#-}

module Language.Core.Merge(merge,uniqueNamesIn,nonUniqueNamesIn) where

import Language.Core.Core
import Language.Core.CoreUtils
import Language.Core.Utils

import Control.Monad.State
import Data.Char
import Data.Generics
import Data.List
import qualified Data.Map as M

-- import Debug.Trace

{-
   merge turns a group of (possibly mutually recursive) modules
   into a single module, which should be called main:Main. 

   This doesn't handle dependency-finding; you have to hand it all
   the modules that your main module depends on (transitively).
   Language.Core.Dependencies does automatic dependency-finding,
   but that code is a bit moldy.

   merge takes an extra argument that is a variable substitution.
   This is because you may want to treat some defined names specially
   rather than dumping their definitions into the Main module. For
   example, if my back-end tool defines a new primop that has
   the type IO (), it's easiest for me if I can consider IO and () as
   primitive type constructors, though they are not. Thus, I pass in
   a substitution that says to replace GHC.IOBase.IO with GHC.Prim.IO,
   and GHC.Base.() with GHC.Prim.(). Of course, I am responsible for
   providing a type environment defining those names if I want to be
   able to type the resulting program.

   You can pass in the empty list if you don't understand what the
   purpose of the substitution is.
-}

merge    :: M.Map (Qual Var) (Qual Var) -> [Module] -> Module
merge subst ms = 
  let new =
        zapNames (subst `M.union` subst1) topNames (Module mainMname newTdefs topBinds) in
--        trace ("merge: old = " ++ show ms ++ " and new = " ++ show new) $
          new
     where -- note: dead code elimination will later remove any names
           -- that were in the domain of the substitution
           newTdefs = finishTdefs deadIds $ concat allTdefs
           (renamedVdefgs,(_,subst1)) = runState
               (mapM (\ (Module _ _ vdefgs) -> rnTopVdefgs vdefgs) ms) (0,M.empty)
           allTdefs = map (\ (Module _ tds _) -> tds) ms 
           deadIds  = M.keys subst
           topNames    = uniqueNamesIn topBinds (concat allTdefs)
           (topBinds::[Vdefg])    = finishVdefs deadIds $ concat renamedVdefgs

{-
   This looks at each top-level *unqualified* var and renames it if it's not unique across
   all modules. Each renamed var gets recorded in a state-threaded substitution.

   This doesn't fix up occurrences of the renamed vars. zapNames does that.
-}

rnTopVdefgs :: [Vdefg] -> State (Int,M.Map (Qual Var) (Qual Var)) [Vdefg]
rnTopVdefgs vds = mapM (mapVdefg doOneVdef) vds
  where doOneVdef (Vdef (qv@(Nothing,v),t,e)) = do
           newUnique <- gets fst
           let newV = unqual $ v ++ "zu" ++ show newUnique
           modify (\ (i,m) -> (newUnique `seq`i+1,M.insert qv newV m))
           return (Vdef (newV,t,e))
        doOneVdef vd = return vd


{-
   This function finds all of the names in the given group of vdefs and
   tdefs that are only defined by one module. This is because if function
   quux is only defined in module foo:Bar.Blat, we want to call it
   main:Main.quux in the final module, and not main:Main.foo_Bar_Blat_quux,
   for file size and readability's sake.

   Possible improvement:
   * restructure the whole thing to shorten names *after* dead code elim.        
   (Would allow for more names to be shortened, but aren't
   strictly necessary.)
-}
uniqueNamesIn :: [Vdefg] -> [Tdef] -> ([Qual Var],[Qual Dcon],[Qual Tcon])
uniqueNamesIn topBinds allTdefs = -- trace ("uniqueDcons = " ++ show uniqueDcons ++ " dcons = " ++ show dcons ++ "\n\n" ++ "uniqueTcons = " ++ show uniqueTcons) $ 
    res
  where vars  = vdefNamesQ (flattenBinds topBinds)
        dcons = tdefDcons allTdefs
        tcons = tdefTcons allTdefs
        uniqueVars  = vars \\ dupsUnqual vars
        uniqueDcons = dcons \\ dupsUnqual dcons
        uniqueTcons = tcons \\ dupsUnqual tcons
        res = (uniqueVars, uniqueDcons, uniqueTcons)

nonUniqueNamesIn :: [Vdef] -> [Tdef] -> [Qual Var]
nonUniqueNamesIn topBinds allTdefs = dupsUnqual allNames
  where allNames = vdefNamesQ topBinds ++ tdefNames allTdefs
        
-- This takes each top-level name of the form Foo.Bar.blah and
-- renames it to FoozuBarzublah (note we *don't* make it exported!
-- This is so we know which names were in the original program and
-- which were dumped in from other modules, and thus can eliminate
-- dead code.)
zapNames :: Data a => M.Map (Qual Var) (Qual Var) -> ([Qual Var],[Qual Dcon],[Qual Tcon]) -> a -> a
zapNames subst qvs = everywhereBut (mkQ False (\ (_::String) -> True))
             (mkT (fixupName subst qvs))

-- AAAArgh this is the problem. This assumption is wrong...
-- have to rename uniquely.

-- also need version for type and data constructors
-- don't forget to *not* zap if something has the primitive module name
-- We hope and pray there are no top-level unqualified names that are used in
-- more than one module. (Can we assume this?) (I think so, b/c -fext-core
-- attaches uniques to things. But could still perhaps go wrong if we fed
-- in .hcr files that were generated in diff. compilation sessions...)
-- (This wouldn't be too hard to fix, but should state the assumption,
-- and how to remove it.)

fixupName :: M.Map (Qual Var) (Qual Var) -> ([Qual Var],[Qual Dcon],[Qual Tcon]) -> Qual Var -> Qual Var
-- For a variable in the domain of the substitution, just
-- apply the substitution.
fixupName subst _ oldVar | Just newVar <- M.lookup oldVar subst = newVar
-- We don't alter unqualified names, since we just need to make sure
-- everything can go in the Main module.
fixupName _ _ vr@(Nothing,_) = vr
-- Nor do we alter anything defined in the Main module,
-- in the primitive module, or the Bool module (this is because primops
-- have no definitions to insert into the new module, and the Bool module
-- is depended on by primops, so it can't be combined with the new module
-- either).
fixupName _ _ vr@(Just mn@(M (pname,_,leaf)), _) | mn == mainMname || mn == wrapperMainMname ||
                            (pname == primPkg && (leaf == "Prim" || leaf == "Bool")) = vr
-- For a variable that is defined by only one module in scope, we 
-- give it a name that is just its unqualified name, without the original
-- module and package names.

-- TODO: Not shortening names anywhere.
-- Have to consider vars, dcons, tcons differently.
-- But then we get awful code duplication.

fixupName _ _ _old@(Just (M (P pname, hierNames, leafName)), varName) =
  let new =
        (mkMname varName, -- see comment for zapNames 
          (if isUpperStr varName then capitalize else id) $
             intercalate "zu" (pname:(hierNames ++ [leafName, varName]))) in
--          trace ("old = " ++ show old ++ " new = " ++ show new) $
             new
  where capitalize (ch:rest) = (toUpper ch):rest
        capitalize ""        = ""

mkMname :: Var -> Mname
-- icky hack :-(
-- necessary b/c tycons and datacons have to be qualified,
-- but we want to write fixupName as a generic transformation on vars.
mkMname v = if isUpperStr v then Just mainMname else Nothing

isUpperStr :: String -> Bool
isUpperStr (c:_)     = isUpper c
isUpperStr []        = False

dupsUnqual :: [Qual Var] -> [Qual Var]
dupsUnqual = dupsBy (\ (_,v1) (_,v2) -> v1 == v2)

-- We remove any declarations for tcons/dcons that are in
-- the domain of the substitution. Why? Because we assume that
-- the substitution maps anything in its domain onto something
-- with a different module name from the main one. If you want
-- to substitute Main-module-defined things for Main-module-defined
-- things, you can do that before merging modules.
finishTdefs :: [Qual Var] -> [Tdef] -> [Tdef]
finishTdefs namesToDrop = filter isOkay
  where isOkay (Newtype qtc qtc1 _ _) = 
               qtc `notElem` namesToDrop 
            && qtc1 `notElem` namesToDrop
        isOkay (Data qtc _ cdefs) = 
               qtc `notElem` namesToDrop 
            && cdefsOkay cdefs
        cdefsOkay = all cdefOkay
        cdefOkay (Constr qdc _ _) = qdc `notElem` namesToDrop
finishVdefs :: [Qual Var] -> [Vdefg] -> [Vdefg]
finishVdefs namesToDrop = filterVdefgs
  (\ (Vdef (qv,_,_)) -> qv `notElem` namesToDrop)

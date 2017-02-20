module Language.Core.Dependencies(getDependencies) where

import Language.Core.Core
import Language.Core.Encoding
import Language.Core.Parser
import Language.Core.ParseGlue

import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as B
-- import Data.Generics
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO

type DepM a = StateT DepState IO a

data DepState = DS { mainPath :: FilePath,
                     packageRoots :: [FilePath],
                     moduleDeps :: M.Map (Either AnMname FilePath) [AnMname],
                     moduleCache ::  M.Map (Either AnMname FilePath) (FilePath, Module)
                    }

-- Given a module, return all the modules it
-- depends on (directly or indirectly).
getDependencies :: [FilePath] -> [FilePath] -> IO [(FilePath, Module)]
getDependencies pkgRoots ms =
  evalStateT (do
    (mapM_ (\ f -> do
              when debug $ liftIO $ putStrLn $ "==== Finding deps for " ++ show f ++ "====="
              -- Every module depends on itself anyway,
              -- so we ignore the FilePath deps.
              ds <- go getDeps lefts (map Left) (map Right ms)
              return (f, ds)) ms)
    t <- gets moduleDeps
    c <- gets moduleCache
    let modNames = nub $ concat (snd (unzip (leftsPairs (M.toList t))))
                       
    res1 <- (liftM catMaybes) $ mapM findModuleP (map Left modNames)
    return $ res1 `unionByFirst`
               (snd (unzip (M.toList c))))
   (DS (last ms) pkgRoots M.empty M.empty)
      where unionByFirst = unionBy (\ (f,_) (g,_) -> f == g)

go :: (Show a, Show b, Eq b, MonadIO m) =>
  (a -> m [b]) -> ([a] -> [b]) -> ([b] -> [a]) -> [a] -> m [b]
go getMore p fixUp start = do
  next <- concatMapM getMore start
  let more = nub $ (p start) ++ next
  if (length start == length more)
    then return more
    else go getMore p fixUp (fixUp more)

{-
varRef :: Exp -> [AnMname]
varRef (Var v) | Just m' <- getModule v = [m']
varRef (Dcon dc) | Just m' <- getModule dc = [m']
varRef _ = []

tyRef :: Ty -> [AnMname]
tyRef (Tcon tc) | Just m' <- getModule tc = [m']
tyRef  _ = []
-}

getDeps :: Either AnMname FilePath -> DepM [AnMname]
getDeps mn = do
          t <- gets moduleDeps
          case M.lookup mn t of
            Just ds -> return ds
            Nothing -> do
              maybeM <- findModule mn
              case maybeM of
                Nothing -> return []
                Just m@(Module mname _ _) -> do
                  let ds = S.toList $ usedModules m in do
                  -- liftIO $ putStrLn (show mn ++ " : " ++ show ds)
                  -- in case we were given a filepath, register the
                  -- module name too
                  modify (\s -> s{moduleDeps=(M.insert mn ds (M.insert (Left mname) ds (moduleDeps s)))})
                  return ds

findModule :: Either AnMname FilePath -> DepM (Maybe Module)
findModule x = do
 maybeRes <- findModuleP x
 case maybeRes of
   Just (_,m) -> return $ Just m
   _          -> return Nothing

findModuleP :: Either AnMname FilePath -> DepM (Maybe (FilePath, Module))
findModuleP (Left mn) | mn == wrapperMainMname || mn == mainMname = do
  f <- gets mainPath
  findModuleP (Right f)
findModuleP (Left mn) | mn == primMname = return Nothing
  -- Nothing means that this module is valid; it just doesn't have
  -- an implementation
findModuleP m = tryFindModule m

tryFindModule :: Either AnMname FilePath -> DepM (Maybe (FilePath, Module))
tryFindModule k = do
  mCache <- gets moduleCache
  liftM Just $ case M.lookup k mCache of
    Just p -> return p
    Nothing -> findModuleNotCached k

findModuleNotCached :: Either AnMname FilePath -> DepM (FilePath, Module)
findModuleNotCached (Left m@(M (P pkgName, encHier, encLeafName))) = do
      searchPath <- gets packageRoots
      let hier = map zDecodeString encHier
          leafName = zDecodeString encLeafName
          possibleFiles = (map (dirs hier leafName) searchPath)
                     ++ map (dirs (zDecodeString pkgName:hier) leafName) searchPath 
                     ++ map (dirs ((takeWhile (/= '-') (zDecodeString pkgName)):hier) leafName) searchPath in do
      match <- liftIO $ findM doesFileExist possibleFiles
      case match of
         Just fp -> findModule' fp
         Nothing -> error ("findModule: failed to find dependency " ++ show m
                      ++ " tried " ++ show possibleFiles)
findModuleNotCached (Right fp) = findModule' fp

dirs :: [String] -> String -> FilePath -> FilePath
dirs modulePath leafName dir = dir </> 
                 (foldr (</>) (addExtension leafName "hcr") modulePath)

findModule' :: FilePath -> DepM (FilePath, Module)
findModule' fp = do
          stuff <- liftIO $ B.readFile fp
          let s = B.unpack stuff
          let parseRes = parse s 0
          let parsedMod@(Module mn _ _) = requireOK fp parseRes 
          when debug $ liftIO $ putStrLn ("Parsed " ++ fp)
          cacheModule mn fp parsedMod
          
-- Maybe can comment this out
          mods <- getDeps (Left mn) 
          when debug $ liftIO $ print mods

          forcePrint parsedMod -- we get a space leak if we don't do this
          return (fp, parsedMod)

forcePrint :: Module -> DepM ()
forcePrint a = liftIO $ do
  hdl <- openFile "/dev/null" WriteMode
  hPutStrLn hdl (show a)
  hClose hdl
 
requireOK :: FilePath -> ParseResult Module -> Module
requireOK s (FailP err) = error ("error in " ++ s ++ " " ++ err)
requireOK _ (OkP m)     = m

cacheModule :: AnMname -> FilePath -> Module -> DepM ()
cacheModule mn fp m = modify (\ s -> s{moduleCache=(M.insert (Left mn) (fp, m)
                                    (M.insert (Right fp) (fp, m)
                                    (moduleCache s)))})

{-
searchPath :: [FilePath]
searchPath = overriddenDir:["../../libraries/",
             -- kludgy: we wouldn't need these if we parsed the
             -- package.conf file, but for now, we are too lazy
              "../../libraries/integer-gmp/",
              "../../libraries/array/"]

overriddenDir :: FilePath
overriddenDir = "./lib/"
-}

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p = liftM listToMaybe . filterM p

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = (liftM concat) . (mapM f)

lefts :: [Either a b] -> [a]
lefts = foldr lefts' []
  where lefts' (Left a) xs = a:xs
        lefts' _        xs = xs

leftsPairs :: [(Either a b, c)] -> [(a, c)]
leftsPairs = foldr leftsPairs' []
  where leftsPairs' ((Left x), y) xs = (x, y):xs
        leftsPairs' _             xs = xs

{-
rightsPairs :: [(Either a b, c)] -> [(b, c)]
rightsPairs = foldr rightsPairs' []
  where rightsPairs' ((Right x), y) xs = (x, y):xs
        rightsPairs' _             xs = xs
-}

usedModules :: Module -> S.Set AnMname
usedModules (Module _ tds vds) = usedModuleTds tds `S.union` usedModuleVds vds

usedModuleTds :: [Tdef] -> S.Set AnMname
usedModuleTds = some usedModuleTd

usedModuleTd :: Tdef -> S.Set AnMname
usedModuleTd (Data _ _ cds) = S.unions
  (map (\ (Constr _ _ ts) -> some usedModuleTy ts) cds)
usedModuleTd (Newtype _ _ _ t) = usedModuleTy t

usedModuleVds :: [Vdefg] -> S.Set AnMname
usedModuleVds = some usedModuleVdefg

usedModuleVdefg :: Vdefg -> S.Set AnMname
usedModuleVdefg (Rec vds) = some usedModuleVdef vds
usedModuleVdefg (Nonrec vdef) = usedModuleVdef vdef

usedModuleVdef :: Vdef -> S.Set AnMname
usedModuleVdef (Vdef (_,t,e)) = usedModuleTy t `S.union` usedModuleExp e

usedModuleExp :: Exp -> S.Set AnMname
usedModuleExp (Var v) | Just m' <- getModule v = S.singleton m'
usedModuleExp (Dcon d) | Just m' <- getModule d = S.singleton m'
usedModuleExp (Var _) = S.empty
usedModuleExp (Dcon _) = S.empty
usedModuleExp (Lit _) = S.empty
usedModuleExp (App a b) = someExps [a,b]
usedModuleExp (Appt e t) = usedModuleExp e `S.union` usedModuleTy t
usedModuleExp (Lam _ e) = usedModuleExp e
usedModuleExp (Let vd e) = usedModuleVdefg vd `S.union` usedModuleExp e
usedModuleExp (Case e _ t alts) = usedModuleExp e `S.union`
  usedModuleTy t `S.union` usedModuleAlts alts
usedModuleExp (Cast e t) = usedModuleExp e `S.union` usedModuleTy t
usedModuleExp (Note _ e) = usedModuleExp e
usedModuleExp (External _ t) = usedModuleTy t

usedModuleTy :: Ty -> S.Set AnMname
usedModuleTy (Tvar _) = S.empty
usedModuleTy (Tcon t) | Just m' <- getModule t = S.singleton m'
usedModuleTy (Tcon _) = S.empty
usedModuleTy (Tapp t u) = usedModuleTy t `S.union` usedModuleTy u
usedModuleTy (Tforall _ t) = usedModuleTy t
usedModuleTy (TransCoercion t u) = usedModuleTy t `S.union` usedModuleTy u
usedModuleTy (SymCoercion t) = usedModuleTy t
usedModuleTy (UnsafeCoercion t u) = usedModuleTy t `S.union` usedModuleTy u
usedModuleTy (InstCoercion t u) = usedModuleTy t `S.union` usedModuleTy u
usedModuleTy (LeftCoercion t) = usedModuleTy t
usedModuleTy (RightCoercion t) = usedModuleTy t

usedModuleAlts :: [Alt] -> S.Set AnMname
usedModuleAlts = some go'
  where go' (Acon dc _ _ e) = case getModule dc of
           Just m' -> S.insert m' (usedModuleExp e)
           _       -> usedModuleExp e
        go' (Alit _ e) = usedModuleExp e
        go' (Adefault e) = usedModuleExp e

some :: (a -> S.Set AnMname) -> [a] -> S.Set AnMname 
some f = S.unions . map f

someExps :: [Exp] -> S.Set AnMname
someExps = some usedModuleExp

debug :: Bool
debug = False

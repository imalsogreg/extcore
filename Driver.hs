{-# OPTIONS -Wall -Werror #-}

{- 
    This driver illustrates the use of the External Core library.

    It takes a list of modules on the command line and loads, typechecks,
    preprocesses, re-typechecks, and evaluates the list of modules.

    The modules should be self-contained. That is, the only free variables
    they contain should be defined in the GHC primitive module, as defined
    in Language.Core.PrimEnv.

    I've actually only tested this driver with a single self-contained
    module, but multiple modules may work.
-}

import Data.Maybe
import Monad
import Prelude hiding (catch)
import System.Environment
import System.FilePath

import Language.Core.Core
import Language.Core.Prims hiding (mkInitialEnv, mkTypeEnv)
import Language.Core.Check
import Language.Core.Prep
import Language.Core.Interp
import Language.Core.ParsecParser

process :: (Menv,[Module]) -> (FilePath, Module)
             -> IO (Menv,[Module])
process (senv,modules) (f, m@(Module mn _ _)) = do
        (case checkModule senv m of
           OkC senv' ->
	     do putStrLn $ "Check succeeded for " ++ show mn
                m' <- prepM senv' m f
		case checkModule senv' m' of
                  OkC senv'' ->
		      do putStrLn "Recheck succeeded"
                         return (senv'',modules ++ [m'])
		  FailC s -> 
		      do putStrLn ("Recheck failed: " ++ s)
			 error "quit"
	   FailC s -> error ("Typechecking failed: " ++ s))
  
prepM :: Menv -> Module -> FilePath -> IO Module
prepM senv' m _f = do
  let m' = prepModule senv' m
  --writeFile (f </> ".prepped") (show m')
  return m'

main :: IO ()
main = do
  args <- getArgs
  case args of
    (_:_) -> doOneProgram args
    _ -> error "usage: ./Driver [filenames]"
  where  doOneProgram :: [FilePath] -> IO ()
         doOneProgram fns = do
               putStrLn $ "========== Program " ++ (show fns) ++ " ============="
               deps <- (liftM catMaybes) (mapM readModule fns)

               topEnv <- mkInitialEnv (snd (unzip deps))

               (_,modules) <- foldM process (topEnv,[]) deps
               let succeeded = length modules

               putStrLn ("Finished typechecking. Successfully checked " 
                          ++ show succeeded)

               result <- evalProgram modules
               putStrLn ("Result = " ++ show result)
               putStrLn "All done\n============================================="

         mkInitialEnv :: [Module] -> IO Menv
         mkInitialEnv libs = foldM mkTypeEnv initialEnv libs

         mkTypeEnv :: Menv -> Module -> IO Menv
         mkTypeEnv globalEnv m = return (envsModule globalEnv m)

readModule :: FilePath -> IO (Maybe (FilePath, Module))
readModule fn = do
  putStrLn $ "Finding " ++ show fn
  res <- parseCore fn
  case res of
    Left err -> error (show err)
    Right m -> return $ Just (fn,m)
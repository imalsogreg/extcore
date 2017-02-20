module Language.Core.Environments where

import qualified Data.Map as M

import Language.Core.Core
import Language.Core.Printer()

{- Environments. -}
type Tvenv = M.Map Tvar Kind                    -- type variables  (local only)
type Tcenv = M.Map Tcon KindOrCoercion          -- type constructors
type Cenv = M.Map Dcon Ty 		      -- data constructors
type Venv = M.Map Var Ty 			      -- values
type Menv = M.Map AnMname Envs		      -- modules
data Envs = Envs {tcenv_::Tcenv,cenv_::Cenv,venv_::Venv} -- all the exportable envs
  deriving Show

{- Extend an environment, checking for illegal shadowing of identifiers (for term
   variables -- shadowing type variables is allowed.) -}
data EnvType = Tv | NotTv
  deriving Eq

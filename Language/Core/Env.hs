{- Environments.
  The original version used lists. I changed it to use Data.Map.
  Sadly it doesn't seem to matter much. --tjc
-}

module Language.Core.Env (eempty,
	    elookup,
	    eextend,
            edomain,
	    efromlist,
            etolist,
	    efilter,
	    eremove)
where

import qualified Data.Map as M

eempty :: M.Map a b 
eempty = M.empty

{- In case of duplicates, returns most recently added entry. -}
elookup :: (Eq a, Ord a) => M.Map a b -> a -> Maybe b
elookup l k = M.lookup k l 

{- May hide existing entries. -}
eextend :: Ord a => M.Map a b -> (a,b) -> M.Map a b
eextend l (k,d) = (M.insert k d l)

edomain :: (Eq a) => M.Map a b -> [a]
edomain l = M.keys l

{- In case of duplicates, first entry hides others. -}
efromlist :: Ord a => [(a,b)] -> M.Map a b
efromlist = M.fromList

etolist :: M.Map a b -> [(a,b)]
etolist l = M.toList l

eremove :: (Eq a, Ord a)  => M.Map a b -> a -> M.Map a b
eremove l k = (M.delete k l)

efilter :: Ord a => M.Map a b -> (a -> Bool) -> M.Map a b
efilter l p = (M.filterWithKey (\ k _ -> p k) l)


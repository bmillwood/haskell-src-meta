{-# OPTIONS_GHC -fglasgow-exts #-}

{- |
  Module      :  Language.Haskell.Meta.Syntax.Vars
  Copyright   :  (c) Matt Morrow 2008
  License     :  BSD3
  Maintainer  :  Matt Morrow <mjm2002@gmail.com>
  Stability   :  experimental
  Portability :  portable (template-haskell)
-}

module Language.Haskell.Meta.Syntax.Vars (
    Vars(..)
) where

import Data.Set (Set)
import qualified Data.Set as S
import Language.Haskell.TH.Syntax

-----------------------------------------------------------------------------


class Vars e v where
  vars :: (Ord v) => e -> Set v
  fvs  :: (Ord v) => e -> Set v
  bvs  :: (Ord v) => e -> Set v
  vars e = fvs e `S.union` bvs e
  fvs e = vars e `S.difference` bvs e
  bvs e = vars e `S.difference` fvs e


instance (Vars e v) => Vars [e] v where
  vars  = S.unions . fmap vars
  fvs   = S.unions . fmap fvs
  bvs   = S.unions . fmap bvs


-----------------------------------------------------------------------------


instance Vars Pat Name where
  vars (LitP _) = S.empty
  vars (VarP n) = S.singleton n
  vars (TupP ps) = vars ps
  vars (ConP n ps) = n `S.insert` vars ps
  vars (InfixP p n q) = n `S.insert` vars [p,q]
  vars (TildeP p) = vars p
  vars (AsP n p) = n `S.insert` vars p
  vars (WildP) = S.empty
  vars (RecP n pfs) = (n `S.insert`) . vars . fmap snd $ pfs
  vars (ListP ps) = vars ps
  vars (SigP p _) = vars p
  bvs (LitP _) = S.empty
  bvs (VarP n) = S.singleton n
  bvs (TupP ps) = bvs ps
  bvs (ConP _ ps) = bvs ps
  bvs (InfixP p _ q) = bvs [p,q]
  bvs (TildeP p) = bvs p
  bvs (AsP n p) = n `S.insert` bvs p
  bvs (WildP) = S.empty
  bvs (RecP _ pfs) = bvs . fmap snd $ pfs
  bvs (ListP ps) = bvs ps
  bvs (SigP p _)  = bvs p


instance Vars Range Name where
  vars (FromR e) = vars e
  vars (FromThenR e f) = vars [e,f]
  vars (FromToR e f) = vars [e,f]
  vars (FromThenToR e f g) = vars [e,f,g]
  fvs (FromR e) = fvs e
  fvs (FromThenR e f) = fvs [e,f]
  fvs (FromToR e f) = fvs [e,f]
  fvs (FromThenToR e f g) = fvs [e,f,g]


instance Vars Exp Name where
  vars (LamE ps e) = fvs ps `S.union` vars e
  vars (LetE ds e) = fvs e `S.union` vars ds
  vars e = fvs e
  fvs (VarE n) = S.singleton n
  fvs (ConE n) = S.singleton n
  fvs (LitE _) = S.empty
  fvs (AppE a b) = fvs [a,b]
  fvs (InfixE aM b cM) = fvs (b : concatMap (maybe [] (:[])) [aM,cM])
  fvs (LamE ps e) = fvs e `S.difference` bvs ps
  fvs (TupE es) = fvs es
  fvs (CondE e f g) = fvs [e,f,g]
  fvs (LetE ds e) = (fvs e `S.union` fvs ds) `S.difference` bvs ds
  fvs (CaseE e ms) = fvs e `S.union` fvs ms
  fvs (DoE ss) = fvs ss
  fvs (CompE ss) = fvs ss
  fvs (ArithSeqE r) = fvs r
  fvs (ListE xs) = fvs xs
  fvs (SigE e _) = fvs e
  fvs (RecConE n xs) = (n `S.insert`) . fvs . fmap snd $ xs
  fvs (RecUpdE e xs) = fvs . (e:) . fmap snd $ xs


instance Vars Match Name where
  fvs (Match p b decs) = (fvs b `S.union` fvs decs)
          `S.difference` (bvs decs `S.union` bvs p)


instance Vars Dec Name where
  vars (FunD n cs) = n `S.insert` vars cs
  vars (ValD p bdy decs) =
    vars p `S.union` vars bdy `S.union` vars decs
  vars (ClassD _ _ _ _ decs) = vars decs
  vars (InstanceD _ _ decs) = vars decs
  vars _ = S.empty
  bvs (FunD n _) = S.singleton n
  bvs (ValD p _ _) = bvs p
  bvs (ClassD _ _ _ _ decs) = bvs decs
  bvs (InstanceD _ _ decs) = bvs decs
  bvs (SigD n _) = S.singleton n
  bvs _ = S.empty


-- data Clause = Clause [Pat] Body [Dec]
instance Vars Clause Name where
  vars (Clause ps bdy decs) =
    vars ps `S.union` vars bdy `S.union` vars decs
  fvs (Clause ps bdy decs) =
    fvs bdy `S.difference` (bvs ps `S.union` bvs decs)


-- data Body = GuardedB [(Guard, Exp)] | NormalB Exp
instance Vars Body Name where
  vars (NormalB e) = vars e
  vars (GuardedB xs) = S.unions
      . fmap (\(g,e) -> vars g `S.union` vars e)
        $ xs
  fvs (NormalB e) = fvs e
  fvs (GuardedB xs) = S.unions
      . fmap (\(g,e) -> fvs e `S.difference` bvs g)
        $ xs


-- data Guard = NormalG Exp | PatG [Stmt]
instance Vars Guard Name where
  vars (NormalG e) = vars e
  vars (PatG ss) = vars ss
  fvs (NormalG e) = fvs e
  fvs (PatG ss) = fvs ss
  bvs (NormalG e) = bvs e
  bvs (PatG ss) = bvs ss


-- data Stmt = BindS Pat Exp | LetS [Dec] | NoBindS Exp | ParS [[Stmt]]
instance Vars Stmt Name where
  vars (BindS p e) = vars p `S.union` vars e
  vars (LetS decs) = vars decs
  vars (NoBindS e) = vars e
  vars (ParS sss) = vars . concat $ sss
  fvs (BindS p e) = fvs e `S.difference` bvs p
  fvs (LetS decs) = fvs decs
  fvs (NoBindS e) = fvs e
  fvs (ParS sss) = fvs . concat $ sss
  bvs (BindS p e) = bvs p
  bvs (LetS decs) = bvs decs
  bvs (NoBindS e) = bvs e
  bvs (ParS sss) = bvs . concat $ sss


-----------------------------------------------------------------------------

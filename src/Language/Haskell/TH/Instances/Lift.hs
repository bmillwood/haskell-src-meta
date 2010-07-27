{-# LANGUAGE StandaloneDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
  Module      :  Language.Haskell.TH.Instances.Lift
  Copyright   :  (c) Matt Morrow 2008
  License     :  BSD3
  Maintainer  :  Matt Morrow <mjm2002@gmail.com>
  Stability   :  experimental
  Portability :  portable (template-haskell)
-}

module Language.Haskell.TH.Instances.Lift () where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Lift

deriving instance Ord Exp
deriving instance Ord Dec
deriving instance Ord Stmt
deriving instance Ord Type
deriving instance Ord TyVarBndr
deriving instance Ord Pred
deriving instance Ord Kind
deriving instance Ord FamFlavour
deriving instance Ord Foreign
deriving instance Ord InlineSpec
deriving instance Ord Pragma
deriving instance Ord FunDep
deriving instance Ord Con
deriving instance Ord Body
deriving instance Ord Clause
deriving instance Ord Strict
deriving instance Ord Safety
deriving instance Ord Callconv
deriving instance Ord Guard
deriving instance Ord Range
deriving instance Ord Match
deriving instance Ord Pat
deriving instance Ord Lit

deriving instance Show Loc
deriving instance Eq Loc

-- TODO: make this better
instance Ppr Loc where
  ppr = showtextl . show

instance Ppr Lit where
  ppr l = ppr (LitE l)

-- comic relief from HERA
instance Lift Rational where lift _ = error "Rational.. what are you doing!"

deriveLift ''()

fmap concat $ mapM deriveLift [''Body, ''Callconv, ''Clause, ''Con, ''Dec,
  ''Exp, ''FamFlavour, ''Fixity, ''FixityDirection, ''Foreign, ''FunDep,
  ''Guard, ''Info, ''InlineSpec, ''Kind, ''Lit, ''Match, ''Pat, ''Pragma,
  ''Pred, ''Range, ''Safety, ''Stmt, ''Strict, ''Type, ''TyVarBndr]


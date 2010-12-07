{-# LANGUAGE CPP #-}
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
import Language.Haskell.TH.Lift (deriveLiftMany)

deriving instance Ord Exp
deriving instance Ord Dec
deriving instance Ord Stmt
deriving instance Ord Type
deriving instance Ord Foreign
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

#if MIN_VERSION_template_haskell(2,4,0)
deriving instance Ord TyVarBndr
deriving instance Ord Pred
deriving instance Ord Kind
deriving instance Ord FamFlavour
deriving instance Ord InlineSpec
deriving instance Ord Pragma
#endif /* MIN_VERSION_template_haskell(2,4,0) */

deriving instance Show Loc
deriving instance Eq Loc

-- TODO: make this better
instance Ppr Loc where
  ppr = showtextl . show

instance Ppr Lit where
  ppr l = ppr (LitE l)

$(deriveLiftMany [ ''Body
                 , ''Callconv
                 , ''Clause
                 , ''Con
                 , ''Dec
                 , ''Exp
                 , ''Fixity
                 , ''FixityDirection
                 , ''Foreign
                 , ''FunDep
                 , ''Guard
                 , ''Info
                 , ''Lit
                 , ''Match
                 , ''Pat
                 , ''Range
                 , ''Safety
                 , ''Stmt
                 , ''Strict
                 , ''Type
#if MIN_VERSION_template_haskell(2,4,0)
                 , ''FamFlavour
                 , ''InlineSpec
                 , ''Kind
                 , ''Pragma
                 , ''Pred
                 , ''TyVarBndr
#if MIN_VERSION_template_haskell(2,5,0)
                 , ''ClassInstance
#endif /* MIN_VERSION_template_haskell(2,5,0) */
#endif /* MIN_VERSION_template_haskell(2,4,0) */
                 ])


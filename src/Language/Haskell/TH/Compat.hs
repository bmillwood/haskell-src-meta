{-# LANGUAGE CPP #-}

-- | This module defines type synonyms for compatibility between template
-- haskell versions.

module Language.Haskell.TH.Compat where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

#if MIN_VERSION_template_haskell(2,4,0)
#else /* !MIN_VERSION_template_haskell(2,4,0) */
type TyVarBndr = Name

type Pred = Type

classP :: Name -> [TypeQ] -> TypeQ
classP n typs = appsT (conT n : typs)

appsT :: [TypeQ] -> TypeQ
appsT []       = error "appsT []"
appsT [x]      = x
appsT (x:y:zs) = appsT ((appT x y) : zs)
#endif /* !MIN_VERSION_template_haskell(2,4,0) */

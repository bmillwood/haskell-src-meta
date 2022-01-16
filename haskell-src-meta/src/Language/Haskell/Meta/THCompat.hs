{-# LANGUAGE CPP #-}
module Language.Haskell.Meta.THCompat (
    module Language.Haskell.Meta.THCompat
) where

import Language.Haskell.TH.Syntax


conP :: Name -> [Pat] -> Pat
#if MIN_VERSION_template_haskell(2,18,0)
conP name = ConP name []
#else
conP = ConP
#endif


#if MIN_VERSION_template_haskell(2,17,0)
plainTV :: Name -> TyVarBndr Specificity
plainTV n = PlainTV n SpecifiedSpec
#else
plainTV :: Name -> TyVarBndr
plainTV = PlainTV
#endif


#if MIN_VERSION_template_haskell(2,17,0)
type TyVarBndr_ flag = TyVarBndr flag
#else
type TyVarBndr_ flag = TyVarBndr
#endif

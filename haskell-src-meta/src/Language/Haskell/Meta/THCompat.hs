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


dataDCons :: Dec -> [Con]
#if MIN_VERSION_template_haskell(2,11,0)
dataDCons (DataD _ _ _ _ cons _) = cons
#else
dataDCons (DataD _ _ _ cons _)   = cons
#endif
dataDCons _                      = []


#if MIN_VERSION_template_haskell(2,17,0)
plainTV :: Name -> TyVarBndr Specificity
plainTV n = PlainTV n SpecifiedSpec
#else
plainTV :: Name -> TyVarBndr
plainTV = PlainTV
#endif


decCons :: Dec -> [Con]
#if MIN_VERSION_template_haskell(2,11,0)
decCons (DataD _ _ _ _ cons _)   = cons
decCons (NewtypeD _ _ _ _ con _) = [con]
#else
decCons (DataD _ _ _ cons _)     = cons
decCons (NewtypeD _ _ _ con _)   = [con]
#endif
decCons _                        = []


decTyVars :: Dec -> [TyVarBndr_ ()]
#if MIN_VERSION_template_haskell(2,11,0)
decTyVars (DataD _ _ ns _ _ _)    = ns
decTyVars (NewtypeD _ _ ns _ _ _) = ns
#else
decTyVars (DataD _ _ ns _ _)      = ns
decTyVars (NewtypeD _ _ ns _ _)   = ns
#endif
decTyVars (TySynD _ ns _)         = ns
decTyVars (ClassD _ _ ns _ _)     = ns
decTyVars _                       = []


decName :: Dec -> Maybe Name
decName (FunD n _)             = Just n
#if MIN_VERSION_template_haskell(2,11,0)
decName (DataD _ n _ _ _ _)    = Just n
decName (NewtypeD _ n _ _ _ _) = Just n
#else
decName (DataD _ n _ _ _)      = Just n
decName (NewtypeD _ n _ _ _)   = Just n
#endif
decName (TySynD n _ _)         = Just n
decName (ClassD _ n _ _ _)     = Just n
decName (SigD n _)             = Just n
decName (ForeignD fgn)         = Just (foreignName fgn)
decName _                      = Nothing


#if MIN_VERSION_template_haskell(2,17,0)
type TyVarBndr_ flag = TyVarBndr flag
#else
type TyVarBndr_ flag = TyVarBndr
#endif


foreignName :: Foreign -> Name
foreignName (ImportF _ _ _ n _) = n
foreignName (ExportF _ _ n _)   = n

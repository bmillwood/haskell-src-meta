{-# LANGUAGE TemplateHaskell, CPP, MagicHash, TypeSynonymInstances #-}

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
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import Data.PackedString
import Data.List(intercalate)
import GHC.Base

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

instance Lift () where
  lift () = [|()|]

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 609
instance Show Loc where
  show (Loc f p m s e) =
    intercalate " " $
      ("Loc":fmap show [f,p,m]++[show s,show e])

instance Eq Loc where
  (Loc a b c d e) == (Loc v w x y z)
    = and $ [d==y,e==z] ++
        (zipWith (==) [a,b,c] [v,w,x])

-- TODO: make this better
instance Ppr Loc where
  ppr = showtextl . show
#endif

instance Ppr Lit where
  ppr l = ppr (LitE l)

-- comic relief from HERA
instance Lift Rational where lift _ = error "Rational.. what are you doing!"

instance Lift Name where
  lift (Name occName nameFlavour) = [| Name occName nameFlavour |]

instance Lift PackedString where
  lift ps = [| packString $(lift $ unpackPS ps) |]

instance Lift NameFlavour where
  lift NameS = [| NameS |]
  lift (NameQ modName) = [| NameQ modName |]
  lift (NameU i) = [| case $( lift (I# i) ) of
                          I# i' -> NameU i' |]
  lift (NameL i) = [| case $( lift (I# i) ) of
                          I# i' -> NameL i' |]
  lift (NameG nameSpace pkgName modName)
    = [| NameG nameSpace pkgName modName |]

instance Lift NameSpace where
  lift VarName = [| VarName |]
  lift DataName = [| DataName |]
  lift TcClsName = [| TcClsName |]


instance Lift Dec where
        lift (FunD x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "FunD")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (ValD x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "ValD")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (DataD x0 x1 x2 x3 x4)
          = appE
              (appE
                 (appE
                    (appE
                       (appE
                          (conE
                             (Name (packString "DataD")
                                (NameG DataName (packString "template-haskell")
                                   (packString "Language.Haskell.TH.Syntax"))))
                          (lift x0))
                       (lift x1))
                    (lift x2))
                 (lift x3))
              (lift x4)
        lift (NewtypeD x0 x1 x2 x3 x4)
          = appE
              (appE
                 (appE
                    (appE
                       (appE
                          (conE
                             (Name (packString "NewtypeD")
                                (NameG DataName (packString "template-haskell")
                                   (packString "Language.Haskell.TH.Syntax"))))
                          (lift x0))
                       (lift x1))
                    (lift x2))
                 (lift x3))
              (lift x4)
        lift (TySynD x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "TySynD")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (ClassD x0 x1 x2 x3 x4)
          = appE
              (appE
                 (appE
                    (appE
                       (appE
                          (conE
                             (Name (packString "ClassD")
                                (NameG DataName (packString "template-haskell")
                                   (packString "Language.Haskell.TH.Syntax"))))
                          (lift x0))
                       (lift x1))
                    (lift x2))
                 (lift x3))
              (lift x4)
        lift (InstanceD x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "InstanceD")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (SigD x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "SigD")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (ForeignD x0)
          = appE
              (conE
                 (Name (packString "ForeignD")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)

instance Lift Exp where
        lift (VarE x0)
          = appE
              (conE
                 (Name (packString "VarE")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (ConE x0)
          = appE
              (conE
                 (Name (packString "ConE")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (LitE x0)
          = appE
              (conE
                 (Name (packString "LitE")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (AppE x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "AppE")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (InfixE x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "InfixE")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (LamE x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "LamE")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (TupE x0)
          = appE
              (conE
                 (Name (packString "TupE")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (CondE x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "CondE")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (LetE x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "LetE")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (CaseE x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "CaseE")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (DoE x0)
          = appE
              (conE
                 (Name (packString "DoE")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (CompE x0)
          = appE
              (conE
                 (Name (packString "CompE")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (ArithSeqE x0)
          = appE
              (conE
                 (Name (packString "ArithSeqE")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (ListE x0)
          = appE
              (conE
                 (Name (packString "ListE")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (SigE x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "SigE")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (RecConE x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "RecConE")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (RecUpdE x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "RecUpdE")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)

instance Lift Lit where
        lift (CharL x0)
          = appE
              (conE
                 (Name (packString "CharL")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (StringL x0)
          = appE
              (conE
                 (Name (packString "StringL")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (IntegerL x0)
          = appE
              (conE
                 (Name (packString "IntegerL")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (RationalL x0)
          = appE
              (conE
                 (Name (packString "RationalL")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (IntPrimL x0)
          = appE
              (conE
                 (Name (packString "IntPrimL")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 609
        lift (WordPrimL x0)
          = appE
              (conE
                 (Name (packString "WordPrimL")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
#endif
        lift (FloatPrimL x0)
          = appE
              (conE
                 (Name (packString "FloatPrimL")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (DoublePrimL x0)
          = appE
              (conE
                 (Name (packString "DoublePrimL")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)

instance Lift Pat where
        lift (LitP x0)
          = appE
              (conE
                 (Name (packString "LitP")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (VarP x0)
          = appE
              (conE
                 (Name (packString "VarP")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (TupP x0)
          = appE
              (conE
                 (Name (packString "TupP")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (ConP x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "ConP")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (InfixP x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "InfixP")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (TildeP x0)
          = appE
              (conE
                 (Name (packString "TildeP")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (AsP x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "AsP")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (WildP)
          = conE
              (Name (packString "WildP")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))
        lift (RecP x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "RecP")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (ListP x0)
          = appE
              (conE
                 (Name (packString "ListP")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (SigP x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "SigP")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)

instance Lift Body where
        lift (GuardedB x0)
          = appE
              (conE
                 (Name (packString "GuardedB")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (NormalB x0)
          = appE
              (conE
                 (Name (packString "NormalB")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)

instance Lift Con where
        lift (NormalC x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "NormalC")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (RecC x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "RecC")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (InfixC x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "InfixC")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (ForallC x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "ForallC")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)

instance Lift Clause where
        lift (Clause x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "Clause")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)

instance Lift Guard where
        lift (NormalG x0)
          = appE
              (conE
                 (Name (packString "NormalG")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (PatG x0)
          = appE
              (conE
                 (Name (packString "PatG")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)

instance Lift Strict where
        lift (IsStrict)
          = conE
              (Name (packString "IsStrict")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))
        lift (NotStrict)
          = conE
              (Name (packString "NotStrict")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))

instance Lift FunDep where
        lift (FunDep x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "FunDep")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)

instance Lift Foreign where
        lift (ImportF x0 x1 x2 x3 x4)
          = appE
              (appE
                 (appE
                    (appE
                       (appE
                          (conE
                             (Name (packString "ImportF")
                                (NameG DataName (packString "template-haskell")
                                   (packString "Language.Haskell.TH.Syntax"))))
                          (lift x0))
                       (lift x1))
                    (lift x2))
                 (lift x3))
              (lift x4)
        lift (ExportF x0 x1 x2 x3)
          = appE
              (appE
                 (appE
                    (appE
                       (conE
                          (Name (packString "ExportF")
                             (NameG DataName (packString "template-haskell")
                                (packString "Language.Haskell.TH.Syntax"))))
                       (lift x0))
                    (lift x1))
                 (lift x2))
              (lift x3)

instance Lift Callconv where
        lift (CCall)
          = conE
              (Name (packString "CCall")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))
        lift (StdCall)
          = conE
              (Name (packString "StdCall")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))

instance Lift Safety where
        lift (Unsafe)
          = conE
              (Name (packString "Unsafe")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))
        lift (Safe)
          = conE
              (Name (packString "Safe")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))
        lift (Threadsafe)
          = conE
              (Name (packString "Threadsafe")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))

instance Lift Match where
        lift (Match x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "Match")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)

instance Lift Stmt where
        lift (BindS x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "BindS")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (LetS x0)
          = appE
              (conE
                 (Name (packString "LetS")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (NoBindS x0)
          = appE
              (conE
                 (Name (packString "NoBindS")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (ParS x0)
          = appE
              (conE
                 (Name (packString "ParS")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)

instance Lift Range where
        lift (FromR x0)
          = appE
              (conE
                 (Name (packString "FromR")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (FromThenR x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "FromThenR")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (FromToR x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "FromToR")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)
        lift (FromThenToR x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "FromThenToR")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)

instance Lift Type where
        lift (ForallT x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "ForallT")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (VarT x0)
          = appE
              (conE
                 (Name (packString "VarT")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (ConT x0)
          = appE
              (conE
                 (Name (packString "ConT")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (TupleT x0)
          = appE
              (conE
                 (Name (packString "TupleT")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (ArrowT)
          = conE
              (Name (packString "ArrowT")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))
        lift (ListT)
          = conE
              (Name (packString "ListT")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))
        lift (AppT x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "AppT")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)

instance Lift Info where
        lift (ClassI x0)
          = appE
              (conE
                 (Name (packString "ClassI")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (ClassOpI x0 x1 x2 x3)
          = appE
              (appE
                 (appE
                    (appE
                       (conE
                          (Name (packString "ClassOpI")
                             (NameG DataName (packString "template-haskell")
                                (packString "Language.Haskell.TH.Syntax"))))
                       (lift x0))
                    (lift x1))
                 (lift x2))
              (lift x3)
        lift (TyConI x0)
          = appE
              (conE
                 (Name (packString "TyConI")
                    (NameG DataName (packString "template-haskell")
                       (packString "Language.Haskell.TH.Syntax"))))
              (lift x0)
        lift (PrimTyConI x0 x1 x2)
          = appE
              (appE
                 (appE
                    (conE
                       (Name (packString "PrimTyConI")
                          (NameG DataName (packString "template-haskell")
                             (packString "Language.Haskell.TH.Syntax"))))
                    (lift x0))
                 (lift x1))
              (lift x2)
        lift (DataConI x0 x1 x2 x3)
          = appE
              (appE
                 (appE
                    (appE
                       (conE
                          (Name (packString "DataConI")
                             (NameG DataName (packString "template-haskell")
                                (packString "Language.Haskell.TH.Syntax"))))
                       (lift x0))
                    (lift x1))
                 (lift x2))
              (lift x3)
        lift (VarI x0 x1 x2 x3)
          = appE
              (appE
                 (appE
                    (appE
                       (conE
                          (Name (packString "VarI")
                             (NameG DataName (packString "template-haskell")
                                (packString "Language.Haskell.TH.Syntax"))))
                       (lift x0))
                    (lift x1))
                 (lift x2))
              (lift x3)
        lift (TyVarI x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "TyVarI")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)

instance Lift Fixity where
        lift (Fixity x0 x1)
          = appE
              (appE
                 (conE
                    (Name (packString "Fixity")
                       (NameG DataName (packString "template-haskell")
                          (packString "Language.Haskell.TH.Syntax"))))
                 (lift x0))
              (lift x1)

instance Lift FixityDirection where
        lift (InfixL)
          = conE
              (Name (packString "InfixL")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))
        lift (InfixR)
          = conE
              (Name (packString "InfixR")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))
        lift (InfixN)
          = conE
              (Name (packString "InfixN")
                 (NameG DataName (packString "template-haskell")
                    (packString "Language.Haskell.TH.Syntax")))

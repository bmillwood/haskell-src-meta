{-# LANGUAGE CPP #-}
{- |
  Module      :  Language.Haskell.Meta.Parse
  Copyright   :  (c) Serokell 2022, Adam Bergmark 2022
  License     :  BSD3
  Maintainer  :  Adam Bergmark <adam@bergmark.nl>
  Stability   :  experimental
  Portability :  portable (template-haskell)
-}

module Language.Haskell.Meta.Extensions (
  toExtension,
  fromExtension
 ) where

import qualified Language.Haskell.Exts.Extension as Exts
import qualified Language.Haskell.TH.Syntax      as TH


-----------------------------------------------------------------------------

-- * To template-haskell

-- | Returns @Nothing@ when the extension is not supported by template-haskell.
toExtension :: Exts.KnownExtension -> Maybe TH.Extension
toExtension e = case e of
  Exts.OverlappingInstances       -> Just TH.OverlappingInstances
  Exts.UndecidableInstances       -> Just TH.UndecidableInstances
  Exts.IncoherentInstances        -> Just TH.IncoherentInstances
  Exts.InstanceSigs               -> Just TH.InstanceSigs
  Exts.DoRec                      -> Nothing
  Exts.RecursiveDo                -> Just TH.RecursiveDo
  Exts.ParallelListComp           -> Just TH.ParallelListComp
  Exts.MultiParamTypeClasses      -> Just TH.MultiParamTypeClasses
  Exts.MonomorphismRestriction    -> Just TH.MonomorphismRestriction
  Exts.FunctionalDependencies     -> Just TH.FunctionalDependencies
  Exts.Rank2Types                 -> Nothing
  Exts.RankNTypes                 -> Just TH.RankNTypes
  Exts.PolymorphicComponents      -> Nothing
  Exts.ExistentialQuantification  -> Just TH.ExistentialQuantification
  Exts.ScopedTypeVariables        -> Just TH.ScopedTypeVariables
  Exts.PatternSignatures          -> Nothing
  Exts.ImplicitParams             -> Just TH.ImplicitParams
  Exts.FlexibleContexts           -> Just TH.FlexibleContexts
  Exts.FlexibleInstances          -> Just TH.FlexibleInstances
  Exts.EmptyDataDecls             -> Just TH.EmptyDataDecls
  Exts.CPP                        -> Just TH.Cpp
  Exts.KindSignatures             -> Just TH.KindSignatures
  Exts.BangPatterns               -> Just TH.BangPatterns
  Exts.TypeSynonymInstances       -> Just TH.TypeSynonymInstances
  Exts.TemplateHaskell            -> Just TH.TemplateHaskell
  Exts.ForeignFunctionInterface   -> Just TH.ForeignFunctionInterface
  Exts.Arrows                     -> Just TH.Arrows
  Exts.Generics                   -> Nothing
  Exts.ImplicitPrelude            -> Just TH.ImplicitPrelude
  Exts.NamedFieldPuns             -> Nothing
  Exts.PatternGuards              -> Just TH.PatternGuards
  Exts.GeneralizedNewtypeDeriving -> Just TH.GeneralizedNewtypeDeriving
  Exts.DeriveAnyClass             -> Just TH.DeriveAnyClass
  Exts.ExtensibleRecords          -> Nothing
  Exts.RestrictedTypeSynonyms     -> Nothing
  Exts.HereDocuments              -> Nothing
  Exts.MagicHash                  -> Just TH.MagicHash
  Exts.BinaryLiterals             -> Just TH.BinaryLiterals
  Exts.TypeFamilies               -> Just TH.TypeFamilies
  Exts.StandaloneDeriving         -> Just TH.StandaloneDeriving
  Exts.UnicodeSyntax              -> Just TH.UnicodeSyntax
  Exts.UnliftedFFITypes           -> Just TH.UnliftedFFITypes
  Exts.LiberalTypeSynonyms        -> Just TH.LiberalTypeSynonyms
  Exts.TypeOperators              -> Just TH.TypeOperators
  Exts.ParallelArrays             -> Just TH.ParallelArrays
  Exts.RecordWildCards            -> Just TH.RecordWildCards
#if __GLASGOW_HASKELL__ >= 904
  Exts.RecordPuns                 -> Just TH.NamedFieldPuns
#else
  Exts.RecordPuns                 -> Just TH.RecordPuns
#endif
  Exts.DisambiguateRecordFields   -> Just TH.DisambiguateRecordFields
  Exts.OverloadedStrings          -> Just TH.OverloadedStrings
  Exts.GADTs                      -> Just TH.GADTs
  Exts.MonoPatBinds               ->
#if !MIN_VERSION_template_haskell(2,18,0)
    Just TH.MonoPatBinds
#else
    Nothing
#endif
  Exts.RelaxedPolyRec             -> Just TH.RelaxedPolyRec
  Exts.ExtendedDefaultRules       -> Just TH.ExtendedDefaultRules
  Exts.UnboxedTuples              -> Just TH.UnboxedTuples
  Exts.DeriveDataTypeable         -> Just TH.DeriveDataTypeable
  Exts.ConstrainedClassMethods    -> Just TH.ConstrainedClassMethods
  Exts.PackageImports             -> Just TH.PackageImports
  Exts.LambdaCase                 -> Just TH.LambdaCase
  Exts.EmptyCase                  -> Just TH.EmptyCase
  Exts.ImpredicativeTypes         -> Just TH.ImpredicativeTypes
  Exts.NewQualifiedOperators      -> Nothing
  Exts.PostfixOperators           -> Just TH.PostfixOperators
  Exts.QuasiQuotes                -> Just TH.QuasiQuotes
  Exts.TransformListComp          -> Just TH.TransformListComp
  Exts.ViewPatterns               -> Just TH.ViewPatterns
  Exts.XmlSyntax                  -> Nothing
  Exts.RegularPatterns            -> Nothing
  Exts.TupleSections              -> Just TH.TupleSections
  Exts.GHCForeignImportPrim       -> Just TH.GHCForeignImportPrim
  Exts.NPlusKPatterns             -> Just TH.NPlusKPatterns
  Exts.DoAndIfThenElse            -> Just TH.DoAndIfThenElse
  Exts.RebindableSyntax           -> Just TH.RebindableSyntax
  Exts.ExplicitForAll             -> Just TH.ExplicitForAll
  Exts.DatatypeContexts           -> Just TH.DatatypeContexts
  Exts.MonoLocalBinds             -> Just TH.MonoLocalBinds
  Exts.DeriveFunctor              -> Just TH.DeriveFunctor
  Exts.DeriveGeneric              -> Just TH.DeriveGeneric
  Exts.DeriveTraversable          -> Just TH.DeriveTraversable
  Exts.DeriveFoldable             -> Just TH.DeriveFoldable
  Exts.NondecreasingIndentation   -> Just TH.NondecreasingIndentation
  Exts.InterruptibleFFI           -> Just TH.InterruptibleFFI
  Exts.CApiFFI                    -> Just TH.CApiFFI
  Exts.JavaScriptFFI              -> Just TH.JavaScriptFFI
  Exts.ExplicitNamespaces         -> Just TH.ExplicitNamespaces
  Exts.DataKinds                  -> Just TH.DataKinds
  Exts.PolyKinds                  -> Just TH.PolyKinds
  Exts.MultiWayIf                 -> Just TH.MultiWayIf
  Exts.SafeImports                -> Nothing
  Exts.Safe                       -> Nothing
  Exts.Trustworthy                -> Nothing
  Exts.DefaultSignatures          -> Just TH.DefaultSignatures
  Exts.ConstraintKinds            -> Just TH.ConstraintKinds
  Exts.RoleAnnotations            -> Just TH.RoleAnnotations
  Exts.PatternSynonyms            -> Just TH.PatternSynonyms
  Exts.PartialTypeSignatures      -> Just TH.PartialTypeSignatures
  Exts.NamedWildCards             -> Just TH.NamedWildCards
  Exts.TypeApplications           -> Just TH.TypeApplications
  Exts.TypeFamilyDependencies     -> Just TH.TypeFamilyDependencies
  Exts.OverloadedLabels           -> Just TH.OverloadedLabels
  Exts.DerivingStrategies         -> Just TH.DerivingStrategies
  Exts.UnboxedSums                -> Just TH.UnboxedSums
#if MIN_VERSION_haskell_src_exts(1,21,0)
  Exts.TypeInType                 -> Just TH.TypeInType
#endif
  Exts.Strict                     -> Just TH.Strict
  Exts.StrictData                 -> Just TH.StrictData

#if MIN_VERSION_haskell_src_exts(1,21,0)
  Exts.DerivingVia                ->
#if MIN_VERSION_template_haskell(2,14,0)
    Just TH.DerivingVia
#else
    Nothing
#endif
#endif

#if MIN_VERSION_haskell_src_exts(1,22,0)
  Exts.QuantifiedConstraints      ->
#if MIN_VERSION_template_haskell(2,14,0)
    Just TH.QuantifiedConstraints
#else
    Nothing
#endif
#endif

#if MIN_VERSION_haskell_src_exts(1,23,0)
  Exts.BlockArguments             ->
#if MIN_VERSION_template_haskell(2,14,0)
    Just TH.BlockArguments
#else
    Nothing
#endif
#endif

  -- NB: when adding a case here, you may also need to update `fromExtension`


-----------------------------------------------------------------------------

-- * From template-haskell

-- | Returns @Nothing@ when the extension is not supported by haskell-src-exts.
fromExtension :: TH.Extension -> Maybe Exts.KnownExtension
fromExtension e = case e of
  TH.Cpp                               -> Just Exts.CPP
  TH.OverlappingInstances              -> Just Exts.OverlappingInstances
  TH.UndecidableInstances              -> Just Exts.UndecidableInstances
  TH.IncoherentInstances               -> Just Exts.IncoherentInstances
  TH.UndecidableSuperClasses           -> Nothing
  TH.MonomorphismRestriction           -> Just Exts.MonomorphismRestriction
#if !MIN_VERSION_template_haskell(2,18,0)
  TH.MonoPatBinds                      -> Just Exts.MonoPatBinds
#endif
  TH.MonoLocalBinds                    -> Just Exts.MonoLocalBinds
  TH.RelaxedPolyRec                    -> Just Exts.RelaxedPolyRec
  TH.ExtendedDefaultRules              -> Just Exts.ExtendedDefaultRules
  TH.ForeignFunctionInterface          -> Just Exts.ForeignFunctionInterface
  TH.UnliftedFFITypes                  -> Just Exts.UnliftedFFITypes
  TH.InterruptibleFFI                  -> Just Exts.InterruptibleFFI
  TH.CApiFFI                           -> Just Exts.CApiFFI
  TH.GHCForeignImportPrim              -> Just Exts.GHCForeignImportPrim
  TH.JavaScriptFFI                     -> Just Exts.JavaScriptFFI
  TH.ParallelArrays                    -> Just Exts.ParallelArrays
  TH.Arrows                            -> Just Exts.Arrows
  TH.TemplateHaskell                   -> Just Exts.TemplateHaskell
  TH.TemplateHaskellQuotes             -> Nothing
  TH.QuasiQuotes                       -> Just Exts.QuasiQuotes
  TH.ImplicitParams                    -> Just Exts.ImplicitParams
  TH.ImplicitPrelude                   -> Just Exts.ImplicitPrelude
  TH.ScopedTypeVariables               -> Just Exts.ScopedTypeVariables
  TH.AllowAmbiguousTypes               -> Nothing
  TH.UnboxedTuples                     -> Just Exts.UnboxedTuples
  TH.UnboxedSums                       -> Just Exts.UnboxedSums
  TH.BangPatterns                      -> Just Exts.BangPatterns
  TH.TypeFamilies                      -> Just Exts.TypeFamilies
  TH.TypeFamilyDependencies            -> Just Exts.TypeFamilyDependencies
  TH.TypeInType                        ->
#if MIN_VERSION_haskell_src_exts(1,21,0)
    Just Exts.TypeInType
#else
    Nothing
#endif
  TH.OverloadedStrings                 -> Just Exts.OverloadedStrings
  TH.OverloadedLists                   -> Nothing
  TH.NumDecimals                       -> Nothing
  TH.DisambiguateRecordFields          -> Just Exts.DisambiguateRecordFields
  TH.RecordWildCards                   -> Just Exts.RecordWildCards
#if __GLASGOW_HASKELL__ >= 904
  TH.NamedFieldPuns                    -> Just Exts.RecordPuns
  TH.QualifiedDo                       -> Nothing
  TH.UnliftedDatatypes -> Nothing
  TH.LinearTypes -> Nothing
  TH.LexicalNegation -> Nothing
  TH.FieldSelectors -> Nothing
  TH.OverloadedRecordDot -> Nothing
  TH.OverloadedRecordUpdate -> Nothing
#else
  TH.RecordPuns                        -> Just Exts.RecordPuns
#endif
  TH.ViewPatterns                      -> Just Exts.ViewPatterns
  TH.GADTs                             -> Just Exts.GADTs
  TH.GADTSyntax                        -> Nothing
  TH.NPlusKPatterns                    -> Just Exts.NPlusKPatterns
  TH.DoAndIfThenElse                   -> Just Exts.DoAndIfThenElse
  TH.RebindableSyntax                  -> Just Exts.RebindableSyntax
  TH.ConstraintKinds                   -> Just Exts.ConstraintKinds
  TH.PolyKinds                         -> Just Exts.PolyKinds
  TH.DataKinds                         -> Just Exts.DataKinds
  TH.InstanceSigs                      -> Just Exts.InstanceSigs
  TH.ApplicativeDo                     -> Nothing
  TH.StandaloneDeriving                -> Just Exts.StandaloneDeriving
  TH.DeriveDataTypeable                -> Just Exts.DeriveDataTypeable
  TH.AutoDeriveTypeable                -> Nothing
  TH.DeriveFunctor                     -> Just Exts.DeriveFunctor
  TH.DeriveTraversable                 -> Just Exts.DeriveTraversable
  TH.DeriveFoldable                    -> Just Exts.DeriveFoldable
  TH.DeriveGeneric                     -> Just Exts.DeriveGeneric
  TH.DefaultSignatures                 -> Just Exts.DefaultSignatures
  TH.DeriveAnyClass                    -> Just Exts.DeriveAnyClass
  TH.DeriveLift                        -> Nothing
  TH.DerivingStrategies                -> Just Exts.DerivingStrategies
  TH.TypeSynonymInstances              -> Just Exts.TypeSynonymInstances
  TH.FlexibleContexts                  -> Just Exts.FlexibleContexts
  TH.FlexibleInstances                 -> Just Exts.FlexibleInstances
  TH.ConstrainedClassMethods           -> Just Exts.ConstrainedClassMethods
  TH.MultiParamTypeClasses             -> Just Exts.MultiParamTypeClasses
  TH.NullaryTypeClasses                -> Nothing
  TH.FunctionalDependencies            -> Just Exts.FunctionalDependencies
  TH.UnicodeSyntax                     -> Just Exts.UnicodeSyntax
  TH.ExistentialQuantification         -> Just Exts.ExistentialQuantification
  TH.MagicHash                         -> Just Exts.MagicHash
  TH.EmptyDataDecls                    -> Just Exts.EmptyDataDecls
  TH.KindSignatures                    -> Just Exts.KindSignatures
  TH.RoleAnnotations                   -> Just Exts.RoleAnnotations
  TH.ParallelListComp                  -> Just Exts.ParallelListComp
  TH.TransformListComp                 -> Just Exts.TransformListComp
  TH.MonadComprehensions               -> Nothing
  TH.GeneralizedNewtypeDeriving        -> Just Exts.GeneralizedNewtypeDeriving
  TH.RecursiveDo                       -> Just Exts.RecursiveDo
  TH.PostfixOperators                  -> Just Exts.PostfixOperators
  TH.TupleSections                     -> Just Exts.TupleSections
  TH.PatternGuards                     -> Just Exts.PatternGuards
  TH.LiberalTypeSynonyms               -> Just Exts.LiberalTypeSynonyms
  TH.RankNTypes                        -> Just Exts.RankNTypes
  TH.ImpredicativeTypes                -> Just Exts.ImpredicativeTypes
  TH.TypeOperators                     -> Just Exts.TypeOperators
  TH.ExplicitNamespaces                -> Just Exts.ExplicitNamespaces
  TH.PackageImports                    -> Just Exts.PackageImports
  TH.ExplicitForAll                    -> Just Exts.ExplicitForAll
  TH.AlternativeLayoutRule             -> Nothing
  TH.AlternativeLayoutRuleTransitional -> Nothing
  TH.DatatypeContexts                  -> Just Exts.DatatypeContexts
  TH.NondecreasingIndentation          -> Just Exts.NondecreasingIndentation
  TH.RelaxedLayout                     -> Nothing
  TH.TraditionalRecordSyntax           -> Nothing
  TH.LambdaCase                        -> Just Exts.LambdaCase
  TH.MultiWayIf                        -> Just Exts.MultiWayIf
  TH.BinaryLiterals                    -> Just Exts.BinaryLiterals
  TH.NegativeLiterals                  -> Nothing
  TH.DuplicateRecordFields             -> Nothing
  TH.OverloadedLabels                  -> Just Exts.OverloadedLabels
  TH.EmptyCase                         -> Just Exts.EmptyCase
  TH.PatternSynonyms                   -> Just Exts.PatternSynonyms
  TH.PartialTypeSignatures             -> Just Exts.PartialTypeSignatures
  TH.NamedWildCards                    -> Just Exts.NamedWildCards
  TH.StaticPointers                    -> Nothing
  TH.TypeApplications                  -> Just Exts.TypeApplications
  TH.Strict                            -> Just Exts.Strict
  TH.StrictData                        -> Just Exts.StrictData
#if !MIN_VERSION_template_haskell(2,18,0)
  TH.MonadFailDesugaring               -> Nothing
#endif

-- 2.13.0 ----------------------------------------
#if MIN_VERSION_template_haskell(2,13,0)
  TH.HexFloatLiterals                  -> Nothing
  TH.EmptyDataDeriving                 -> Nothing
#endif

-- 2.14.0 ----------------------------------------
#if MIN_VERSION_template_haskell(2,14,0)

  TH.DerivingVia                       ->
#if MIN_VERSION_haskell_src_exts(1,21,0)
    Just Exts.DerivingVia
#else
    Nothing
#endif

  TH.QuantifiedConstraints             ->
#if MIN_VERSION_haskell_src_exts(1,22,0)
    Just Exts.QuantifiedConstraints
#else
    Nothing
#endif

  TH.BlockArguments                    ->
#if MIN_VERSION_haskell_src_exts(1,23,0)
    Just Exts.BlockArguments
#else
    Nothing
#endif

  TH.NumericUnderscores                -> Nothing
  TH.StarIsType                        -> Nothing
#endif

-- 2.16.0 ----------------------------------------
#if MIN_VERSION_template_haskell(2,16,0)
  TH.UnliftedNewtypes                  -> Nothing
  TH.ImportQualifiedPost               -> Nothing
  TH.CUSKs                             -> Nothing
  TH.StandaloneKindSignatures          -> Nothing
#endif

-- 2.19.0 ---------------------------------------

#if MIN_VERSION_template_haskell(2,19,0)
  TH.DeepSubsumption                   -> Nothing
#endif

  -- NB: when adding a case here, you may also need to update `toExtension`


-----------------------------------------------------------------------------

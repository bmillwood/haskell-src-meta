{- |
  Module      :  Language.Haskell.Meta.Parse
  Copyright   :  (c) Matt Morrow 2008
  License     :  BSD3
  Maintainer  :  Matt Morrow <mjm2002@gmail.com>
  Stability   :  experimental
  Portability :  portable (template-haskell)
-}

module Language.Haskell.Meta.Parse (
  parsePat,
  parseExp,
  parseType,
  parseDecs,
  parsePatWithMode,
  parseExpWithMode,
  parseTypeWithMode,
  parseDecsWithMode,
  myDefaultParseMode,
  myDefaultExtensions,
  parseResultToEither,
  parseHsModule,
  parseHsDecls,
  parseHsType,
  parseHsExp,
  parseHsPat,
  parseHsModuleWithMode,
  parseHsDeclsWithMode,
  parseHsTypeWithMode,
  parseHsExpWithMode,
  parseHsPatWithMode,
  pprHsModule,
  moduleDecls,
  emptySrcLoc,
  emptyHsModule,
  defaultFixities
 ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Syntax.Translate
import qualified Language.Haskell.Exts.Syntax as Hs
import Language.Haskell.Exts.Annotated.Fixity as Fix
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Parser hiding (
  parseExp, parseType, parsePat, parseTypeWithMode,
  parseExpWithMode, parsePatWithMode)
import qualified Language.Haskell.Exts.Parser as PA
import Language.Haskell.Exts.Pretty

-----------------------------------------------------------------------------

-- * template-haskell

parsePat :: String -> Either String Pat
parsePat = fmap toPat . parseHsPat

parseExp :: String -> Either String Exp
parseExp = fmap toExp . parseHsExp

parseType :: String -> Either String Type
parseType = fmap toType . parseHsType

parseDecs :: String -> Either String [Dec]
parseDecs  = fmap toDecs . parseHsDecls

-- | Compiles a string as Haskell code, retruning a Template Haskell 'Pat' data
-- type.  Takes a mode dictating what options (extensions etc) to apply.
parsePatWithMode :: ParseMode -> String -> Either String Pat
parsePatWithMode m = fmap toPat . parseHsPatWithMode m

-- | Compiles a string as Haskell code, retruning a Template Haskell 'Exp' data
-- type.  Takes a mode dictating what options (extensions etc) to apply.
parseExpWithMode :: ParseMode -> String -> Either String Exp
parseExpWithMode m = fmap toExp . parseHsExpWithMode m

-- | Compiles a string as Haskell code, retruning a Template Haskell 'Type' data
-- type.  Takes a mode dictating what options (extensions etc) to apply.
parseTypeWithMode  :: ParseMode -> String -> Either String Type
parseTypeWithMode m = fmap toType . parseHsTypeWithMode m

-- | Compiles a string as Haskell code, retruning a Template Haskell '[dec]'
-- data type.  Takes a mode dictating what options (extensions etc) to apply.
parseDecsWithMode  :: ParseMode -> String -> Either String [Dec]
parseDecsWithMode m = fmap toDecs . parseHsDeclsWithMode m

-----------------------------------------------------------------------------

{-# DEPRECATED myDefaultParseMode, myDefaultExtensions
  "The provided ParseModes aren't very meaningful, use your own instead" #-}
myDefaultParseMode :: ParseMode
myDefaultParseMode = ParseMode
  {parseFilename = []
  ,extensions = myDefaultExtensions
  ,ignoreLinePragmas = False
  ,ignoreLanguagePragmas = False
  ,fixities = defaultFixities}

-- This is a silly hack to make things work on haskell-src-exts versions
-- 1.10 and 1.11 simultaneously. I justify it because myDefaultParseMode is
-- deprecated anyway.
--
-- Essentially we want defaultFixities to be baseFixities or Just baseFixities
-- as appropriate. We do this without requiring FlexibleInstances using the
-- same trick as Show on lists does.
class DefaultFixities a where
  defaultFixities :: a
  defaultFixities =
    error "Language.Haskell.Meta.Parse.defaultFixities undefined"
  defaultFixityList :: [a]
  defaultFixityList =
    error "Language.Haskell.Meta.Parse.defaultFixityList undefined"

instance DefaultFixities Fix.Fixity where
  defaultFixityList = baseFixities

instance DefaultFixities a => DefaultFixities [a] where
  defaultFixities = defaultFixityList

instance DefaultFixities a => DefaultFixities (Maybe a) where
  defaultFixities = Just defaultFixities

myDefaultExtensions :: [Extension]
myDefaultExtensions = [PostfixOperators
                      ,QuasiQuotes
                      ,UnicodeSyntax
                      ,PatternSignatures
                      ,MagicHash
                      ,ForeignFunctionInterface
                      ,TemplateHaskell
                      ,RankNTypes
                      ,MultiParamTypeClasses
                      ,RecursiveDo]

parseResultToEither :: ParseResult a -> Either String a
parseResultToEither (ParseOk a) = Right a
parseResultToEither (ParseFailed loc e)
  = let line = Hs.srcLine loc - 1
    in Left (unlines [show line,show loc,e])



parseHsUsing parser mode = parseResultToEither . parser mode
parseHsUsingDefault parser = parseHsUsing parser myDefaultParseMode



parseHsModule :: String -> Either String Hs.Module
parseHsModule = parseHsUsingDefault parseModuleWithMode

parseHsDecls :: String -> Either String [Hs.Decl]
parseHsDecls = fmap moduleDecls . parseHsModule

parseHsType :: String -> Either String Hs.Type
parseHsType = parseHsUsingDefault PA.parseTypeWithMode

parseHsExp :: String -> Either String Hs.Exp
parseHsExp = parseHsUsingDefault PA.parseExpWithMode

parseHsPat :: String -> Either String Hs.Pat
parseHsPat = parseHsUsingDefault PA.parsePatWithMode



parseHsModuleWithMode :: ParseMode -> String -> Either String Hs.Module
parseHsModuleWithMode = parseHsUsing parseModuleWithMode

parseHsDeclsWithMode :: ParseMode -> String -> Either String [Hs.Decl]
parseHsDeclsWithMode mode = fmap moduleDecls . parseHsModuleWithMode mode

parseHsTypeWithMode :: ParseMode -> String -> Either String Hs.Type
parseHsTypeWithMode = parseHsUsing PA.parseTypeWithMode

parseHsExpWithMode :: ParseMode -> String -> Either String Hs.Exp
parseHsExpWithMode = parseHsUsing PA.parseExpWithMode

parseHsPatWithMode :: ParseMode -> String -> Either String Hs.Pat
parseHsPatWithMode =  parseHsUsing PA.parsePatWithMode



pprHsModule :: Hs.Module -> String
pprHsModule = prettyPrint


moduleDecls :: Hs.Module -> [Hs.Decl]
moduleDecls (Hs.Module _ _ _ _ _ _ x) = x

-- mkModule :: String -> Hs.Module
-- mkModule s = Hs.Module undefined (Hs.ModuleName s) Nothing [] []

emptySrcLoc :: Hs.SrcLoc
emptySrcLoc = (Hs.SrcLoc [] 0 0)

emptyHsModule :: String -> Hs.Module
emptyHsModule n =
    (Hs.Module
        emptySrcLoc
        (Hs.ModuleName n)
        []
        Nothing
        Nothing
        []
        [])

{-
ghci> :i Module
data Module
  = Module SrcLoc
           ModuleName
           [OptionPragma]
           (Maybe WarningText)
           (Maybe [ExportSpec])
           [ImportDecl]
           [Decl]
        -- Defined in Language.Haskell.Exts.Syntax
instance Show Module -- Defined in Language.Haskell.Exts.Syntax
-}

-----------------------------------------------------------------------------


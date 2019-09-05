{-# LANGUAGE CPP #-}
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
  parseDecsWithMode,
  myDefaultParseMode,
  myDefaultExtensions,
  parseResultToEither,
  parseHsModule,
  parseHsDecls,
  parseHsDeclsWithMode,
  parseHsType,
  parseHsExp,
  parseHsPat,
  pprHsModule,
  moduleDecls,
  noSrcSpanInfo,
  emptyHsModule
 ) where

#if MIN_VERSION_template_haskell(2,11,0)
import Language.Haskell.TH.Syntax hiding (Extension (..))
#else
import Language.Haskell.TH.Syntax
#endif
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Parser           hiding
  (parseExp, parsePat, parseType)
import           Language.Haskell.Exts.Pretty
import qualified Language.Haskell.Exts.SrcLoc           as Hs
import qualified Language.Haskell.Exts.Syntax           as Hs
import           Language.Haskell.Meta.Syntax.Translate

-----------------------------------------------------------------------------

-- * template-haskell

parsePat :: String -> Either String Pat
parsePat = either Left (Right . toPat) . parseHsPat

parseExp :: String -> Either String Exp
parseExp = either Left (Right . toExp) . parseHsExp

parseType :: String -> Either String Type
parseType = either Left (Right . toType) . parseHsType

parseDecs :: String -> Either String [Dec]
parseDecs  = either Left (Right . toDecs) . parseHsDecls

-- | @since 0.8.2
parseDecsWithMode :: ParseMode -> String -> Either String [Dec]
parseDecsWithMode parseMode = either Left (Right . toDecs)
  . parseHsDeclsWithMode parseMode

-----------------------------------------------------------------------------

{-# DEPRECATED myDefaultParseMode, myDefaultExtensions
  "The provided ParseModes aren't very meaningful, use your own instead" #-}
myDefaultParseMode :: ParseMode
myDefaultParseMode = defaultParseMode
  {parseFilename = []
  ,baseLanguage = Haskell2010
  ,extensions = map EnableExtension myDefaultExtensions
  }

myDefaultExtensions :: [KnownExtension]
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

parseHsModule :: String -> Either String (Hs.Module Hs.SrcSpanInfo)
parseHsModule = parseResultToEither . parseModuleWithMode myDefaultParseMode

parseHsDecls :: String -> Either String [Hs.Decl Hs.SrcSpanInfo]
parseHsDecls = either Left (Right . moduleDecls)
  . parseResultToEither . parseModuleWithMode myDefaultParseMode

-- | @since 0.8.2
parseHsDeclsWithMode :: ParseMode -> String -> Either String [Hs.Decl Hs.SrcSpanInfo]
parseHsDeclsWithMode parseMode = either Left (Right . moduleDecls)
  . parseResultToEither . parseModuleWithMode parseMode


parseHsType :: String -> Either String (Hs.Type Hs.SrcSpanInfo)
parseHsType = parseResultToEither . parseTypeWithMode myDefaultParseMode


parseHsExp :: String -> Either String (Hs.Exp Hs.SrcSpanInfo)
parseHsExp = parseResultToEither . parseExpWithMode myDefaultParseMode

parseHsPat :: String -> Either String (Hs.Pat Hs.SrcSpanInfo)
parseHsPat = parseResultToEither . parsePatWithMode myDefaultParseMode

pprHsModule :: Hs.Module Hs.SrcSpanInfo -> String
pprHsModule = prettyPrint


moduleDecls :: Hs.Module Hs.SrcSpanInfo -> [Hs.Decl Hs.SrcSpanInfo]
moduleDecls (Hs.Module _ _ _ _ x) = x
moduleDecls m                     = todo "" m
-- TODO
--             (Hs.XmlPage _ _ _ _ _ _ _)
--          (Hs.XmlHybrid _ _ _ _ _ _ _ _ _)

-- mkModule :: String -> Hs.Module
-- mkModule s = Hs.Module undefined (Hs.ModuleName s) Nothing [] []

emptyHsModule :: String -> Hs.Module Hs.SrcSpanInfo
emptyHsModule n =
    (Hs.Module
        noSrcSpanInfo
        (Just (Hs.ModuleHead noSrcSpanInfo (Hs.ModuleName noSrcSpanInfo n) Nothing Nothing))
        []
        []
        [])

noSrcSpanInfo :: Hs.SrcSpanInfo
noSrcSpanInfo = Hs.noInfoSpan (Hs.mkSrcSpan Hs.noLoc Hs.noLoc)

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

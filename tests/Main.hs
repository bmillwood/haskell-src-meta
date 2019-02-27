{-# LANGUAGE CPP #-}

module Main where

import Language.Haskell.Meta.Parse
#if MIN_VERSION_haskell_src_exts(1,18,0)
import qualified Language.Haskell.Exts as Exts
#else
import qualified Language.Haskell.Exts.Annotated as Exts
#endif
import qualified Language.Haskell.Exts.Extension as Extension
import qualified Language.Haskell.Exts.Parser as Parser
import qualified Language.Haskell.TH as TH
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, (@?=))

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ derivingClausesTest
        , typeAppTest
        ]

derivingClausesTest :: Test
derivingClausesTest = testCase "Deriving clauses preserved" $
    roundTripDecls "data Foo = Foo deriving (A, B, C)"

typeAppMode :: Exts.ParseMode
typeAppMode = Parser.defaultParseMode { Parser.extensions = [Extension.EnableExtension Extension.TypeApplications] }

typeAppTest :: Test
typeAppTest = testCase "Type app preserved, or something" $
  roundTripDeclsWithMode typeAppMode "tenStr = show @Int 10"

roundTripDecls :: String -> Assertion
roundTripDecls s = do
    declsExts  <- liftEither $ parseHsDecls s
    declsExts' <- liftEither $ parseDecs s >>= parseHsDecls . TH.pprint
    declsExts' @?= declsExts

roundTripDeclsWithMode :: Exts.ParseMode -> String -> Assertion
roundTripDeclsWithMode mode s = do
  declsExts <- liftEither $ parseHsDeclsWithMode mode s
  declsExts' <- liftEither $ parseDecsWithMode mode s >>= parseHsDeclsWithMode mode . TH.pprint
  declsExts' @?= declsExts

liftEither :: Monad m => Either String a -> m a
liftEither = either fail return

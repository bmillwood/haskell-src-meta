{-# LANGUAGE CPP #-}

module Main where

import qualified Control.Monad.Fail              as Fail
import qualified Language.Haskell.Exts           as Exts
import qualified Language.Haskell.Exts.Extension as Extension
import qualified Language.Haskell.Exts.Parser    as Parser
import           Language.Haskell.Meta.Parse
import qualified Language.Haskell.TH             as TH
-- import           Test.Framework
-- import           Test.Framework.Providers.HUnit
import Test.HUnit       (Assertion, (@?=))
import Test.Tasty       (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

type Test = TestTree

main :: IO ()
main = defaultMain (testGroup "unit" tests)

tests :: [Test]
tests = [ derivingClausesTest
#if MIN_VERSION_template_haskell(2,12,0)
        , typeAppTest
#endif
        ]

derivingClausesTest :: Test
derivingClausesTest = testCase "Deriving clauses preserved" $
    roundTripDecls "data Foo = Foo deriving (A, B, C)"

typeAppMode :: Exts.ParseMode
typeAppMode = Parser.defaultParseMode { Parser.extensions = [Extension.EnableExtension Extension.TypeApplications] }

typeAppTest :: Test
typeAppTest = testCase "Type app preserved" $
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

liftEither :: Fail.MonadFail m => Either String a -> m a
liftEither = either fail return

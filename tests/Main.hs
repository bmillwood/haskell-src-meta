{-# LANGUAGE CPP #-}

module Main where

import Language.Haskell.Meta.Parse
#if MIN_VERSION_haskell_src_exts(1,18,0)
import qualified Language.Haskell.Exts as Exts
#else
import qualified Language.Haskell.Exts.Annotated as Exts
#endif
import qualified Language.Haskell.TH as TH
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, (@?=))

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [derivingClausesTest]

derivingClausesTest :: Test
derivingClausesTest = testCase "Deriving clauses preserved" $
    roundTripDecls "data Foo = Foo deriving (A, B, C)"

roundTripDecls :: String -> Assertion
roundTripDecls s = do
    declsExts  <- liftEither $ parseHsDecls s
    declsExts' <- liftEither $ parseDecs s >>= parseHsDecls . TH.pprint
    declsExts' @?= declsExts

liftEither :: Monad m => Either String a -> m a
liftEither = either fail return

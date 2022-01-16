{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Monad.Fail                     as Fail
import           Data.Data                              (Data, cast, gfoldl)
import           Data.Functor.Const
  (Const (Const, getConst))
import qualified Language.Haskell.Exts                  as Exts
import qualified Language.Haskell.Exts.Extension        as Extension
import qualified Language.Haskell.Exts.Parser           as Parser
import           Language.Haskell.Meta.Parse
import           Language.Haskell.Meta.Syntax.Translate
import qualified Language.Haskell.TH                    as TH
import           Test.HUnit                             (Assertion, (@?=))
import           Test.Tasty
  (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit                       (testCase)

type Test = TestTree

main :: IO ()
main = defaultMain (testGroup "unit" tests)

tests :: [Test]
tests = [ derivingClausesTest
        , typeAppTest
        , orderInTypeTuples
        ]

derivingClausesTest :: Test
derivingClausesTest = testCase "Deriving clauses preserved" $
    roundTripDecls "data Foo = Foo deriving (A, B, C)"

orderInTypeTuples :: Test
orderInTypeTuples =
  testCase "Ensure that type tuples reconstructed in proper order" $ do
    expected @?= actual
  where
    expected :: [TH.TyLit]
    expected = collectAll (toExp parsed)
    actual   = [TH.StrTyLit "a", TH.StrTyLit "b"]

    parsed :: Exts.Exp Exts.SrcSpanInfo
    parsed = case Exts.parseExpWithMode mode "foo @'(\"a\", \"b\")" of
      Exts.ParseOk v -> v
      e              -> error $ show e
    mode :: Exts.ParseMode
    mode = Exts.defaultParseMode {
          Exts.extensions = [
              Exts.EnableExtension Exts.TypeApplications
            , Exts.EnableExtension Exts.DataKinds
            ]
        }

collectAll :: (Data a, Data b) => a -> [b]
collectAll = ($ []) . go
  where
    go :: forall a b. (Data a, Data b) => a -> [b] -> [b]
    go = \x ->
        case cast x of
          Just x' -> (x' :)
          Nothing -> getConst $ gfoldl ap (const $ Const id) x
      where
        ap :: Data x => Const ([b] -> [b]) (x -> y) -> x -> Const ([b] -> [b]) y
        ap (Const acc) x = Const $ acc . go x

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

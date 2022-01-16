module Main where

import qualified Control.Monad.Fail              as Fail
import qualified Language.Haskell.Exts           as Exts
import qualified Language.Haskell.Exts.Extension as Extension
import qualified Language.Haskell.Exts.Parser    as Parser
import           Language.Haskell.Meta.Parse
import           Language.Haskell.Meta.Syntax.Translate
import qualified Language.Haskell.TH             as TH
import           Test.HUnit                      (Assertion, (@?=))
import           Test.Tasty
  (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit                (testCase)

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

-- Very ugly test, illustrating incorrect order of promoted type tuples reconstructed into TH
orderInTypeTuples :: Test
orderInTypeTuples = testCase "Ensure that type tuples reconstructed in proper order" $ do
  let Parser.ParseOk parsed = show . toExp <$> Parser.parseExpWithMode mode "getField @'(\"a\", \"b\", \"c\") localVar"
  parsed @?= expected
  where
    mode = Parser.defaultParseMode { Parser.extensions = [Extension.EnableExtension Extension.TypeApplications, Extension.EnableExtension Extension.DataKinds  ] }
    expected = "(AppE (AppTypeE (VarE getField) (AppT (AppT (AppT (PromotedTupleT 3) (LitT (StrTyLit \"a\"))) (LitT (StrTyLit \"b\"))) (LitT (StrTyLit \"c\")))) (VarE localVar))"

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

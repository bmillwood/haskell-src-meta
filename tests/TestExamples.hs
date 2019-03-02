{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified BF
import qualified Hs
import qualified HsHere
import qualified SKI

import SKI (SKI ((:$), I, K, S))

-- Very dumb test framework
shouldBe :: (Show a, Eq a) => a -> a -> IO ()
actual `shouldBe` expected = case actual == expected of
  True -> return ()
  False -> do
    putStr "Expected: "
    print expected
    putStr "Actual: "
    print actual
    fail "Expectation failure"


a :: Int -> String
a x = [HsHere.here| random "text" $(x + 1)
  something else|]

hereTest :: IO ()
hereTest = do
 a 3 `shouldBe` (" random \"text\" "++ show (3 + 1 :: Int) ++"\n  something else")

-- TODO: better test exercising the bf quasiquoter

bfTest :: IO ()
bfTest = do
  BF.eval_ (BF.parse BF.bfHelloWorld) "" `shouldBe` "Hello World!\n"

hsTest :: IO ()
hsTest = do
  (\ [Hs.hs|b@(x,_)|] -> [Hs.hs|(b,x)|]) (42 :: Int,88 :: Int) `shouldBe` ((42,88),42)

-- TODO: better test exercising the ski quasiquoter

skiTest :: IO ()
skiTest = do
  SKI.parse "S(SS)IK(SK)" `shouldBe` ([(((S :$ (S :$ S)) :$ I) :$ K) :$ (S :$ K)],"")

main :: IO ()
main = do
  putStrLn ""
  hereTest
  bfTest
  hsTest
  skiTest

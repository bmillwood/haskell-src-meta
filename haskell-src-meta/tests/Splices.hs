{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE CPP                   #-}
#if __GLASGOW_HASKELL__ >= 904
{-# LANGUAGE TypeOperators         #-}
#endif
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

#if MIN_VERSION_template_haskell(2,14,0)
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE QuantifiedConstraints #-}
#endif

-- | Tests stuff mostly by just compiling correctly
import qualified Language.Haskell.Exts.Extension as Extension
import qualified Language.Haskell.Exts.Parser    as Parser
import qualified Language.Haskell.Meta           as Meta

----- Testing names -----

-- Test that the unit constructor works
$(either error return $ Meta.parseDecs
      "unit :: IO ()\nunit = return ()")

-- Testing that the [] constructor works in types,
$(either error return $ Meta.parseDecs
      "nilp :: [a] -> ([] a)\nnilp [] = []")

$(either error return $ Meta.parseDecs
      "pair :: (,) Int Int\npair = (,) 1 2")


----- Testing classes and instances -----
$(either error return $ Meta.parseDecs $ unlines
   ["class MyClass a where mymethod :: a -> b -> (a,b)"
   ,"instance MyClass Bool where mymethod a b = (a,b)"
   ])

$(either error return $ Meta.parseDecsWithMode (Parser.defaultParseMode { Parser.extensions = [Extension.EnableExtension Extension.TypeApplications] }) $ unlines
   ["tenStr :: String"
   ,"tenStr = show @Int 10"])

#if MIN_VERSION_template_haskell(2,14,0)
$(either error return $ Meta.parseDecsWithMode
  (Parser.defaultParseMode { Parser.extensions = [Extension.EnableExtension Extension.QuantifiedConstraints, Extension.EnableExtension Extension.ExplicitForAll] })
  $ unlines
  ["class (forall a. Eq a => Eq (f a)) => Eq1 f where"
  ,"  eq1 :: f Int -> f Int -> Bool"
  ,"  eq1 = (==)"
  ,""
  ,"instance Eq1 []"])
#else
$(either error return $ Meta.parseDecs $ unlines
  ["eq1 :: [Int] -> [Int] -> Bool"
  ,"eq1 = (==)"])
#endif

$(either error return $ Meta.parseDecsWithMode
  (Parser.defaultParseMode { Parser.extensions = [Extension.EnableExtension Extension.GADTs] })
  $ unlines
   [
-- Not sure why but ghc 7.10 complains that "type var a is not in scope"
   "intConstraint :: (a ~ Int) => a"
   ,"intConstraint = 3"])

-- Just to check that it works as intended
main :: IO ()
main = do
  -9 <- return $(either error return $ Meta.parseExp "-3^2 :: Int") :: IO Int
  () <- unit
  [] <- return (nilp [])
  (1,2) <- return pair
  (True,1) <- return $ mymethod True 1
  "10" <- return tenStr
  3 <- return intConstraint
  True <- return $ eq1 [1] [1]
  return ()

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TemplateHaskell       #-}

#if MIN_VERSION_template_haskell(2,12,0)
{-# LANGUAGE TypeApplications      #-}
#endif

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
#if MIN_VERSION_base(4,9,0)
$(either error return $ Meta.parseDecs
      "nilp :: [a] -> ([] a)\nnilp [] = []")
#else
-- CPP Note: Apparently ghc < 7 doesn't parse this correctly w/o the forall.
-- https://github.com/DanBurton/haskell-src-meta/issues/2
$(either error return $ Meta.parseDecs
      "nilp :: forall a. [a] -> ([] a)\nnilp [] = []")
#endif

$(either error return $ Meta.parseDecs
      "pair :: (,) Int Int\npair = (,) 1 2")


----- Testing classes and instances -----
#if MIN_VERSION_base(4,9,0)
$(either error return $ Meta.parseDecs $ unlines
   ["class MyClass a where mymethod :: a -> b -> (a,b)"
   ,"instance MyClass Bool where mymethod a b = (a,b)"
   ])
#else
-- CPP Note: Apparently ghc < 7 doesn't parse this correctly w/o the forall.
-- https://github.com/DanBurton/haskell-src-meta/issues/2
$(either error return $ Meta.parseDecs $ unlines
   ["class MyClass a where mymethod :: forall b. a -> b -> (a,b)"
   ,"instance MyClass Bool where mymethod a b = (a,b)"
   ])
#endif

#if MIN_VERSION_template_haskell(2,12,0)
$(either error return $ Meta.parseDecsWithMode (Parser.defaultParseMode { Parser.extensions = [Extension.EnableExtension Extension.TypeApplications] }) $ unlines
   ["tenStr :: String"
   ,"tenStr = show @Int 10"])
#else
-- Type Application not supported by template-haskell < 2.12
$(either error return $ Meta.parseDecs $ unlines
   ["tenStr :: String"
   ,"tenStr = show (10 :: Int)"])
#endif

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
#if MIN_VERSION_template_haskell(2,11,0)
   "intConstraint :: (a ~ Int) => a"
#else
   "intConstraint :: Int"
#endif
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

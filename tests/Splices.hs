{-# LANGUAGE CPP, TemplateHaskell #-}
-- | Tests stuff mostly by just compiling correctly
import Language.Haskell.Meta

----- Testing names -----

-- Test that the unit constructor works
$(either error return $ parseDecs
      "unit :: IO ()\nunit = return ()")

-- Testing that the [] constructor works in types,
#if MIN_VERSION_base(4,9,0)
$(either error return $ parseDecs
      "nilp :: [a] -> ([] a)\nnilp [] = []")
#else
-- CPP Note: Apparently ghc < 7 doesn't parse this correctly w/o the forall.
-- https://github.com/DanBurton/haskell-src-meta/issues/2
$(either error return $ parseDecs
      "nilp :: forall a. [a] -> ([] a)\nnilp [] = []")
#endif

$(either error return $ parseDecs
      "pair :: (,) Int Int\npair = (,) 1 2")


----- Testing classes and instances -----
#if MIN_VERSION_base(4,9,0)
$(either error return $ parseDecs $ unlines
   ["class MyClass a where mymethod :: a -> b -> (a,b)"
   ,"instance MyClass Bool where mymethod a b = (a,b)"
   ])
#else
-- CPP Note: Apparently ghc < 7 doesn't parse this correctly w/o the forall.
-- https://github.com/DanBurton/haskell-src-meta/issues/2
$(either error return $ parseDecs $ unlines
   ["class MyClass a where mymethod :: forall b. a -> b -> (a,b)"
   ,"instance MyClass Bool where mymethod a b = (a,b)"
   ])
#endif



-- Just to check that it works as intended
main = do
  -9 <- return $(either error return $ parseExp "-3^2") :: IO Int
  () <- unit
  [] <- return (nilp [])
  (1,2) <- return pair
  (True,1) <- return $ mymethod True 1
  return ()

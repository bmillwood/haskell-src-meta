{-#LANGUAGE TemplateHaskell#-}
-- | Tests stuff mostly by just compiling correctly
import Language.Haskell.Meta


----- Testing names -----

-- Test that the unit constructor works
$(either error return $ parseDecs 
      "unit :: IO ()\nunit = return ()")

-- Testing that the [] constructor works in types, 
$(either error return $ parseDecs 
      "nilp :: [a] -> ([] a)\nnilp [] = []")

$(either error return $ parseDecs 
      "pair :: (,) Int Int\npair = (,) 1 2")


----- Testing classes and instances -----
$(either error return $ parseDecs $ unlines
   ["class MyClass a where mymethod :: a -> b -> (a,b)"
   ,"instance MyClass Bool where mymethod a b = (a,b)"
   ])

-- Just to check that it works as intended
main = do
  () <- unit
  [] <- return (nilp [])
  (1,2) <- return pair
  (True,1) <- return $ mymethod True 1
  return ()
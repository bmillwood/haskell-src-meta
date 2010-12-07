{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- | Idiom brackets. Vixey's idea.

module Language.Haskell.Meta.QQ.Idiom (i) where

import Data.Generics
  (Data,everywhere,mkT)
import Control.Applicative
import Language.Haskell.Meta
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- ghci> [$i| (,) "foo" "bar" |]
-- [('f','b'),('f','a'),('f','r'),('o','b'),('o','a'),('o','r'),('o','b'),('o','a'),('o','r')]
i :: QuasiQuoter
i = QuasiQuoter
    { quoteExp = (cleanNames <$>) . applicateQ
    , quotePat = either fail return . parsePat
    }

applicateQ :: String -> ExpQ
applicateQ s = case either fail unwindE (parseExp s) of
                  x:y:xs -> foldl
                              (\e e' -> [|$e <*> $e'|])
                              [|$(return x) <$> $(return y)|]
                              (fmap return xs)
                  _ -> fail "applicateQ fails."

unwindE :: Exp -> [Exp]
unwindE = go []
  where go acc (e `AppE` e') = go (e':acc) e
        go acc e = e:acc

cleanNames :: (Data a) => a -> a
cleanNames = everywhere (mkT cleanName)
  where cleanName :: Name -> Name
        cleanName n
          | isNameU n = n
          | otherwise = (mkName . nameBase) n
        isNameU :: Name -> Bool
        isNameU (Name _ (NameU _)) = True
        isNameU _ = False



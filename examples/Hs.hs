

-- | Eat your face!

module Language.Haskell.Meta.QQ.Hs (hs, pat) where

import Language.Haskell.Meta (parseExp, parsePat)
import Language.Haskell.Meta.Utils (pretty)
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- |
-- > ghci> [$hs|\x -> (x,x)|] 42
-- > (42,42)
-- > ghci> (\[$hs|a@(x,_)|] -> (a,x)) (42,88)
-- > ((42,88),42)
hs :: QuasiQuoter
hs = QuasiQuoter
      { quoteExp = either fail transformE . parseExp
      , quotePat = either fail transformP . parsePat
      }

transformE :: Exp -> ExpQ
transformE = return

transformP :: Pat -> PatQ
transformP = return

pat :: QuasiQuoter
pat = QuasiQuoter
        { quoteExp = quoteExp hs
        , quotePat = \s -> case parseExp s of
                Left err -> fail err
                Right e -> either fail return (parsePat . pretty $ e)
        }

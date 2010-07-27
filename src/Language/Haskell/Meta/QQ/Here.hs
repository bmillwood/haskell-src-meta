
-- | Here!

module Language.Haskell.Meta.QQ.Here (here) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

here :: QuasiQuoter
here = QuasiQuoter (litE . stringL) (litP . stringL)

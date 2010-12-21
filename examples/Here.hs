
-- | Here!

module Language.Haskell.Meta.QQ.Here (here) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib

here :: QuasiQuoter
here = QuasiQuoter
    { quoteExp = litE . stringL
    , quotePat = litP . stringL
    }

{- |
  Module      :  Language.Haskell.Meta
  Copyright   :  (c) Matt Morrow 2008
  License     :  BSD3
  Maintainer  :  Matt Morrow <mjm2002@gmail.com>
  Stability   :  experimental
  Portability :  portable (template-haskell)
-}

module Language.Haskell.Meta (
    module Language.Haskell.Meta.Parse,
    module Language.Haskell.Meta.Syntax.Translate
) where

import Language.Haskell.Meta.Parse
import Language.Haskell.Meta.Syntax.Translate
import Language.Haskell.TH.Instances ()

{- |
  Module      :  Language.Haskell.TH.Instances.Lift
  Copyright   :  (c) Matt Morrow 2008
  License     :  BSD3
  Maintainer  :  Matt Morrow <mjm2002@gmail.com>
  Stability   :  experimental
  Portability :  portable (template-haskell)

  This module is exported for backwards-compatibility purposes.
  All it does is re-export the instances defined in
  "Language.Haskell.TH.Instances", from the th-orphans package.
-}
module Language.Haskell.TH.Instances.Lift
  {-# DEPRECATED "Use the th-orphans package instead." #-} () where

import Language.Haskell.TH.Instances

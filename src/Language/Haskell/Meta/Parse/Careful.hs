{- |
DEPRECATED: haskell-src-meta now requires GHC >= 7.4, so this module is no longer necessary. It will be GHC-warning deprecated soon.

This module provides the tools to handle operator fixities in infix expressions correctly.

The problem we solve is the following. Consider making a quasiquoter which antiquotes to Haskell - for instance, the quasiquoter in <http://hackage.haskell.org/package/hmatrix-static> allows me to write

> myVec :: Vector Double
> myVec = [vec| 2+3*4, 5-4-3 |]

To correctly parse such expressions, we need to know the fixities and precedences of the operators, so that the above is parsed the same way as

> myVec = [vec| 2+(3*4), (5-4)-3 |]

There is a danger, if we are not careful in parsing, that the above expression instead parses as

> myVec = [vec| (2+3)*4, 5-(4-3) |]

which is a surprising bug, and would only be detected through testing at runtime, rather than at compile time.

When this danger arises, we use this \"careful\" module. It handles \"unresolved infix\" expressions such as @2+3*4@ in two ways, depending on the version of GHC:

  * in GHC 7.4 and above (where support for \"unresolved infix\" was added in Template Haskell), resolution of the infix expression is deferred to the compiler, which has all fixities available to it.

  * prior to GHC 7.4, any ambiguous infix expression is flagged as a parse error at compile time, and the user is advised to resolve the ambiguity by adding parentheses.

-}
module Language.Haskell.Meta.Parse.Careful(
  parsePat,
  parseExp,
  parseType,
  parseDecs
 ) where

import qualified Language.Haskell.Meta.Parse as Sloppy
import qualified Language.Haskell.Meta.Syntax.Translate as Translate
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.Exts.Syntax as Hs

doChecked parser translater p =
  case parser p of
    Left s -> Left s
    Right p' | amb p' -> Left "Infix expression could not be resolved as operator fixities are not known. Resolve ambiguity by adding parentheses"
             | otherwise -> Right (translater p')

parsePat :: String -> Either String TH.Pat
parsePat = doChecked Sloppy.parseHsPat Translate.toPat

parseExp :: String -> Either String TH.Exp
parseExp = doChecked Sloppy.parseHsExp Translate.toExp

parseType :: String -> Either String TH.Type
parseType = doChecked Sloppy.parseHsType Translate.toType

parseDecs :: String -> Either String [TH.Dec]
parseDecs = doChecked Sloppy.parseHsDecls Translate.toDecs

-- This was more complicated, but since support for GHC pre-7.4 was dropped,
-- it's no longer necessary
amb = const False

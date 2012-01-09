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
#ifdef NEW_TH
#else
import Data.Generics.Uniplate.Data
#endif

doChecked parser translater p = 
  case parser p of 
    Left s -> Left s
    Right p' | amb p' -> Left "Infix expression could not be resolved as operator fixities are not known. Resolve ambiguity byadding parentheses"
             | otherwise -> Right (translater p')

parsePat :: String -> Either String TH.Pat
parsePat = doChecked Sloppy.parseHsPat Translate.toPat

parseExp :: String -> Either String TH.Exp
parseExp = doChecked Sloppy.parseHsExp Translate.toExp

parseType :: String -> Either String TH.Type
parseType = doChecked Sloppy.parseHsType Translate.toType

parseDecs :: String -> Either String [TH.Dec]
parseDecs = doChecked Sloppy.parseHsDecls Translate.toDecs

#ifdef NEW_TH
amb = const False
#else
amb syn = any isAmbExp (universeBi syn) || any isAmbPat (universeBi syn)
  where
    isAmbExp (Hs.InfixApp Hs.InfixApp{} _ _) = True
    isAmbExp (Hs.InfixApp _ _ Hs.InfixApp{}) = True
    isAmbExp (Hs.InfixApp Hs.RightSection{} _ _) = True
    isAmbExp (Hs.InfixApp _ _ Hs.LeftSection{}) = True
    isAmbExp _ = False
    
    isAmbPat (Hs.PInfixApp Hs.PInfixApp{} _ _) = True
    isAmbPat (Hs.PInfixApp _ _ Hs.PInfixApp{}) = True
    isAmbPat _ = False
#endif

-- TODO: knock out these warnings
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards      #-}
{-# LANGUAGE TemplateHaskell    #-}

module HsHere
  ( here
  , lexemeP
  , nestedP
  , parensP
  , bracksP
  , oparenP
  , obrackP
  , cbrackP
  ) where

import qualified Control.Monad.Fail           as Fail
import           Data.Generics                (Data)
import           Data.Typeable                (Typeable)
import           Language.Haskell.Meta        (parseExp, parsePat)
import           Language.Haskell.Meta.Utils  (cleanNames)
import           Language.Haskell.TH.Lib      hiding (parensP)
import           Language.Haskell.TH.Ppr
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.ParserCombinators.ReadP

-- TODO: narrow type & move to shared module
quoteTypeNotImplemented :: Fail.MonadFail m => String -> m a
quoteTypeNotImplemented = fail . ("type quoter not implemented: " ++)

-- TODO: narrow type & move to shared module
quoteDecNotImplemented :: Fail.MonadFail m => String -> m a
quoteDecNotImplemented = fail . ("dec quoter not implemented: " ++ )


data Here
  = CodeH Exp
  | TextH String
  | ManyH [Here]
  deriving (Eq,Show,Data,Typeable)

-- | Example:
--
-- > a x = [here| random "text" $(x + 1)
-- >  something else|]
--
-- Is like:
--
-- > a x = " random \"text\" "++ show (x + 1) ++"\n  something else"
here :: QuasiQuoter
here = QuasiQuoter
        {quoteType = quoteTypeNotImplemented
        ,quoteDec = quoteDecNotImplemented
        ,quoteExp = hereExpQ
        ,quotePat = herePatQ}

instance Lift Here
  where lift = liftHere

liftHere :: Here -> ExpQ
liftHere (TextH s)  = (litE . stringL) s
liftHere (CodeH e)  = [|show $(return e)|]
liftHere (ManyH hs) = [|concat $(listE (fmap liftHere hs))|]


hereExpQ :: String -> ExpQ
hereExpQ s = case run s of
              []  -> fail "here: parse error"
              e:_ -> lift (cleanNames e)

herePatQ :: String -> PatQ
herePatQ s = do
  e <- hereExpQ s
  let p = (parsePat
            . pprint
              . cleanNames) e
  case p of
    Left e  -> fail e
    Right p -> return p

run :: String -> [Here]
run = fst . parse

parse :: String -> ([Here], String)
parse = runP hereP

hereP :: ReadP Here
hereP = (ManyH . mergeTexts)
  `fmap` many (oneP =<< look)

mergeTexts :: [Here] -> [Here]
mergeTexts [] = []
mergeTexts (TextH s:TextH t:hs)
  = mergeTexts (TextH (s++t):hs)
mergeTexts (h:hs) = h : mergeTexts hs

oneP :: String -> ReadP Here
oneP s
  | [] <- s         = pfail
  | '\\':'$':s <- s = do skip 2
                         (TextH . ("\\$"++))
                          `fmap` munch (/='\\')
  | '$':'(':s <- s  = skip 2 >> go 1 [] s
  | c:s <- s        = do skip 1
                         (TextH . (c:))
                          `fmap` munch (not.(`elem`"\\$"))
  where go :: Int -> String -> String -> ReadP Here
        go _ acc  []         = return (TextH (reverse acc))
        go 1 []  (')':_) = skip 1 >> return (TextH "$()")
        go 1 acc (')':_) = do skip (1 + length acc)
                              let s = reverse acc
                              either (const (return
                                              (TextH s)))
                                     (return . CodeH)
                                     (parseExp s)
        go n acc ('(':s)     = go (n+1) ('(':acc) s
        go n acc (')':s)     = go (n-1) (')':acc) s
        go n acc (c:s)       = go n (c:acc) s


runP :: ReadP a -> String -> ([a], String)
runP p s = case readP_to_S p s of
              [] -> ([],[])
              xs -> mapfst (:[]) (last xs)
  where mapfst f (a,b) = (f a,b)
skip :: Int -> ReadP ()
skip n = count n get >> return ()
lexemeP :: ReadP a -> ReadP a
lexemeP p = p >>= \x -> skipSpaces >> return x
nestedP :: (ReadP a -> ReadP a) -> (ReadP a -> ReadP a)
nestedP nest p = p <++ nest (skipSpaces >> nestedP nest p)
parensP, bracksP :: ReadP a -> ReadP a
parensP  = between oparenP cparenP
bracksP  = between oparenP cparenP
oparenP, cparenP, obrackP, cbrackP :: ReadP Char
oparenP  = char '('
cparenP  = char ')'
obrackP  = char '['
cbrackP  = char ']'

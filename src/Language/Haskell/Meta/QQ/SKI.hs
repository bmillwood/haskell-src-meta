{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Language.Haskell.Meta.QQ.SKI (SKI(..),ski) where

import Language.Haskell.Meta
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Utils
import Text.ParserCombinators.ReadP
import Data.Typeable(Typeable)
import Data.Generics(Data)
import Text.PrettyPrint(render)

data SKI = S | K | I | E Exp | SKI :$ SKI
  deriving (Eq,Data,Typeable)

run :: String -> [SKI]
run = fmap eval . fst . parse

-- I x = x
-- K x y = x
-- S x y z = (x z) (y z)
eval :: SKI -> SKI
eval (I :$ x)               = eval x
eval ((K :$ x) :$ y)        = eval x
eval (((S :$ x) :$ y :$ z)) = eval (eval (x :$ z) :$ eval (y :$ z))
eval (E e :$ E e')          = E (unQ[|$(return e) $(return e')|])
eval (x :$ y)               = eval0 ((eval x) :$ (eval y))
eval  x                     = x
eval0 (I :$ x)               = eval x
eval0 ((K :$ x) :$ y)        = eval x
eval0 (((S :$ x) :$ y :$ z)) = eval (eval (x :$ z) :$ eval (y :$ z))
eval0 (E e :$ E e')          = E (unQ[|$(return e) $(return e')|])
eval0  x                     = x

ski :: QuasiQuoter
ski = QuasiQuoter
        {quoteExp = skiExpQ
        ,quotePat = skiPatQ}

instance Lift SKI where
  lift = liftSKI

liftSKI (E e) = return e
liftSKI a     = go a
  where go S = [|S|]
        go K = [|K|]
        go I = [|I|]
        go (E e) = [|E e|]
        go (x:$y) = [|$(go x) :$ $(go y)|]

instance Show SKI where
        showsPrec p (S) = showString "S"
        showsPrec p (K) = showString "K"
        showsPrec p (I) = showString "I"
        showsPrec p (E x1)
          = showParen (p > 10)
              (showString (render (ppDoc x1)))
        showsPrec p ((:$) x1 x2)
          = showParen (p > 10)
              (showsPrec 11 x1 . (showString " :$ " . showsPrec 10 x2))

skiExpQ :: String -> ExpQ
skiExpQ s = case run s of
              [] -> fail "ski: parse error"
              e:_ -> lift (cleanNames e)

skiPatQ :: String -> PatQ
skiPatQ s = do
  e <- skiExpQ s
  let p = (parsePat
            . pprint
              . cleanNames) e
  case p of
    Left e -> fail e
    Right p -> return p

-- ghci> parse "S(SS)IK(SK)"
-- ([(((S :$ (S :$ S)) :$ I) :$ K) :$ (S :$ K)],"")
parse :: String -> ([SKI], String)
parse = runP skiP

skiP :: ReadP SKI
skiP = nestedP parensP
  (let go a = (do b <- lexemeP (oneP <++ skiP)
                  go (a:$b)) <++ return a
    in lexemeP (go =<< lexemeP oneP))

oneP :: ReadP SKI
oneP = nestedP parensP
  (lexemeP (choice [sP
                   ,kP
                   ,iP
                   ,spliceP =<< look
                   ]))

spliceP :: String -> ReadP SKI
spliceP s
  | '[':s <- s = skip 1 >> go 1 [] s
  | otherwise  = pfail
  where go _ _   []         = pfail
        go 1 acc (']':_) = do skip (1 + length acc)
                              either (const pfail)
                                     (return . E)
                                     (parseExp (reverse acc))
        go n acc ('[':s)     = go (n+1) ('[':acc) s
        go n acc (']':s)     = go (n-1) (']':acc) s
        go n acc (c:s)       = go n (c:acc) s



sP = (char 's' +++ char 'S') >> return S
kP = (char 'k' +++ char 'K') >> return K
iP = (char 'i' +++ char 'I') >> return I

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
parensP  = between oparenP cparenP
bracksP  = between oparenP cparenP
oparenP  = char '('
cparenP  = char ')'
obrackP  = char '['
cbrackP  = char ']'




{-
import Prelude hiding (($))
data Komb = S (Maybe (Komb, Maybe Komb)) | K (Maybe Komb) deriving Show
S Nothing $ x = S (Just (x, Nothing))
S (Just (x, Nothing)) $ y = S (Just (x, Just y))
S (Just (x, Just y)) $ z = x $ z $ (y $ z)
K Nothing $ x = K (Just x)
K (Just x) $ y = y
q x = x $ (c $ k) $ k $ k $ s
 where s = S Nothing
       k = K Nothing
       c = s $ (b $ b $ s) $ k $ k
       b = s $ (k $ s) $ k
-}




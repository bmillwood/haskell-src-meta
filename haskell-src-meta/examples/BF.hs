-- TODO: knock out these warnings
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}

module BF (
   bf,bf2,bfHelloWorld,eval_,parse, exec, test0
) where

import Language.Haskell.Meta      (parsePat)
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import qualified Control.Monad.Fail as Fail
import           Data.Char
import           Data.IntMap        (IntMap)
import qualified Data.IntMap        as IM

-- TODO: narrow type & move to shared module
quoteTypeNotImplemented :: Fail.MonadFail m => String -> m a
quoteTypeNotImplemented = fail . ("type quoter not implemented: " ++)

-- TODO: narrow type & move to shared module
quoteDecNotImplemented :: Fail.MonadFail m => String -> m a
quoteDecNotImplemented = fail . ("dec quoter not implemented: " ++ )

bf :: QuasiQuoter
bf = QuasiQuoter
  { quoteExp = bfExpQ
  , quotePat = bfPatQ
  , quoteType = quoteTypeNotImplemented
  , quoteDec = quoteDecNotImplemented
  }

bf2 :: QuasiQuoter
bf2 = QuasiQuoter
  { quoteExp = bf2ExpQ
  , quotePat = bfPatQ
  , quoteType = quoteTypeNotImplemented
  , quoteDec = quoteDecNotImplemented
  }

bf2ExpQ :: String -> ExpQ
bf2ExpQ s = [|eval (parse s)|]

bfExpQ :: String -> ExpQ
bfExpQ s = [|eval_ (parse s)|]

bfPatQ :: String -> PatQ
bfPatQ s = do
  let p = (parsePat
            . show
              . parse) s
  case p of
    Left e  -> fail e
    Right p -> return p

instance Lift Bf where
  lift Inp        = [|Inp|]
  lift Out        = [|Out|]
  lift Inc        = [|Inc|]
  lift Dec        = [|Dec|]
  lift MovL       = [|MovL|]
  lift MovR       = [|MovR|]
  lift (While xs) = [|While $(lift xs)|]

type Ptr = Int
newtype Mem = Mem (IntMap Int) deriving (Show)

data Bf = Inp
        | Out
        | Inc
        | Dec
        | MovL
        | MovR
        | While [Bf]
  deriving (Eq,Ord,Read,Show)

data Status = D Ptr Mem
            | W Int Status
            | R (Int -> Status)

-- ghci> exec (parse helloWorld)
-- Hello World!
-- (4,Mem (fromList [(0,0),(1,87),(2,100),(3,33),(4,10)]))
bfHelloWorld :: String
bfHelloWorld = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

eval_ :: [Bf] -> (String -> String)
eval_ is = go (run 0 initMem is)
  where go (D p m)    _    = []
        go (W n s)    cs   = chr n : go s cs
        go (R cont)   []   = "*** Exception: bf blocked on input"
        go (R cont) (c:cs) = go ((cont . ord) c) cs

eval :: [Bf] -> String -> (String, (Ptr, Mem))
eval is = go [] (run 0 initMem is)
  where go acc (D p m)    _    = (reverse acc, (p, m))
        go acc (W n s)    cs   = go (chr n:acc) s cs
        go _   (R cont)   []   = ("*** Exception: bf blocked on input",(-1, Mem IM.empty))
        go acc (R cont) (c:cs) = go acc ((cont . ord) c) cs

exec :: [Bf] -> IO (Ptr, Mem)
exec is = go (run 0 initMem is)
  where go (D p m)  = return (p, m)
        go (W n s)  = putChar (chr n) >> go s
        go (R cont) = go . cont . ord =<< getChar

run :: Ptr -> Mem -> [Bf] -> Status
run dp m is = step dp m is (\dp m -> D dp m)

step :: Ptr -> Mem -> [Bf] -> (Ptr -> Mem -> Status) -> Status
step dp m   []          k = k dp m
step dp m (Inc:is)      k = step dp (inc dp m) is k
step dp m (Dec:is)      k = step dp (dec dp m) is k
step dp m (MovL:is)     k = step (dp-1) m is k
step dp m (MovR:is)     k = step (dp+1) m is k
step dp m (Inp:is)      k = R (\c -> step dp (wr m dp c) is k)
step dp m (Out:is)      k = W (rd m dp) (step dp m is k)
step dp m (While xs:is) k = let go dp m = if rd m dp == 0
                                            then step dp m is k
                                            else step dp m xs go
                            in go dp m

initMem :: Mem
initMem = Mem IM.empty

inc :: Ptr -> (Mem -> Mem)
dec :: Ptr -> (Mem -> Mem)
rd :: Mem -> Ptr -> Int
wr :: Mem -> Ptr -> Int -> Mem
upd :: Mem -> Ptr -> (Int -> Int) -> Mem
inc p m = upd m p (+1)
dec p m = upd m p (subtract 1)
rd (Mem m) p = maybe 0 id (IM.lookup p m)
wr (Mem m) p n = Mem (IM.insert p n m)
upd m p f = wr m p (f (rd m p))

parse :: String -> [Bf]
parse s = go 0 [] s (\_ xs _ -> xs)
  where go :: Int -> [Bf] -> String
          -> (Int -> [Bf] -> String -> o) -> o
        go !n acc   []     k = k n (reverse acc) []
        go !n acc (',':cs) k = go (n+1) (Inp:acc) cs k
        go !n acc ('.':cs) k = go (n+1) (Out:acc) cs k
        go !n acc ('+':cs) k = go (n+1) (Inc:acc) cs k
        go !n acc ('-':cs) k = go (n+1) (Dec:acc) cs k
        go !n acc ('<':cs) k = go (n+1) (MovL:acc) cs k
        go !n acc ('>':cs) k = go (n+1) (MovR:acc) cs k
        go !n acc ('[':cs) k = go (n+1) [] cs (\n xs cs ->
                                go n (While xs:acc) cs k)
        go !n acc (']':cs) k = k (n+1) (reverse acc) cs
        go !n acc (c  :cs) k = go n acc cs k


test0 :: IO [Bf]
test0 = do
  a <- readFile "prime.bf"
  return (parse a)







{-
data Bf = Inp
        | Out
        | Inc
        | Dec
        | MovL
        | MovR
        | While [Bf]
        | Error String
  deriving (Eq,Ord,Read,Show)

parse :: String -> [Bf]
parse s = let p n s = case go n [] s of
                        (_,xs,[]) -> xs
                        (n,xs, s) -> xs ++ p n s
          in p 0 s
  where go :: Int -> [Bf] -> [Char] -> (Int, [Bf], String)
        go !n acc   []     = (n, reverse acc, [])
        go !n acc (',':cs) = go (n+1) (Inp:acc) cs
        go !n acc ('.':cs) = go (n+1) (Out:acc) cs
        go !n acc ('+':cs) = go (n+1) (Inc:acc) cs
        go !n acc ('-':cs) = go (n+1) (Dec:acc) cs
        go !n acc ('<':cs) = go (n+1) (MovL:acc) cs
        go !n acc ('>':cs) = go (n+1) (MovR:acc) cs
        go !n acc ('[':cs) = case go (n+1) [] cs of
                                (n,xs,cs) -> go n (While xs:acc) cs
        go !n acc (']':cs) = (n+1, reverse acc, cs)
        go !n acc (c  :cs) = (n+1, [Error ("go error: char "++show n
                                    ++" illegal character: "++show c)], [])
-}

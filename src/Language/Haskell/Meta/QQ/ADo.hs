{-# LANGUAGE PatternGuards, TemplateHaskell, QuasiQuotes #-}

-- | Applicative do. Phillipa Cowderoy's idea, some explanations due Edward
-- Kmett
--
-- Pointful version of "Language.Haskell.Meta.QQ.Idiom". Note the only
-- expression which has the bound variables in scope is the last one.
--
-- This lets you work with applicatives without the order of fields in an data
-- constructor becoming such a burden.
--
-- In a similar role as 'fail' in do notation, if match failures can be
-- expected, the result is an @Applicative f => f (Maybe a)@, rather than
-- @Applicative f => f a@, where @a@ may be partially defined.
module Language.Haskell.Meta.QQ.ADo (

    ado,
    ado'

    -- * Desugaring
    -- $desugaring
    ) where

import Control.Applicative
import Language.Haskell.Meta
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Control.Monad
import qualified Data.Set as S

import Language.Haskell.Meta.Utils (cleanNames)

import Data.Generics

-- $desugaring
--
-- If you use patterns that may fail:
--
-- > foo :: Applicative f => f (Maybe T)
-- > foo = [$ado|
-- >    x:xs <- foo bar baz
-- >    Just y <- quux quaffle
-- >    T x y |]
--
-- 'ado' desugars to:
--
-- > foo = (\x y -> case (x,y) of
-- >                    (x:xs,Just y) -> Just $ T x y
-- >                    _ -> Nothing
-- >        ) <$> foo bar baz <*> quux quaffle
--
-- While 'ado'' desugars to the less safe:
--
-- > foo = (\(x:xs) (Just y) -> T x y) <$> foo bar baz <*> quux quaffle
--
-- If the simple patterns cannot fail, there is no 'Maybe' for the 'ado' quote,
-- just like 'ado'':
--
-- > newtype A = A Int
-- > foo :: Applicative f => f T
-- > foo = [$ado|
-- >    ~(x:xs) <- foo bar baz
-- >    A y <- quux quaffle
-- >    T x y |]
--
-- Becomes:
--
-- > foo = (\ ~(x:xs) (A y) -> T x y) <$> foo bar baz <*> quux quaffle

-- | Usage:
--
-- > ghci> [$ado| a <- "foo"; b <- "bar"; (a,b) |]
-- > [('f','b'),('f','a'),('f','r'),('o','b'),('o','a'),('o','r'),('o','b'),('o','a'),('o','r')]
--
-- > ghci> [$ado| Just a <- [Just 1,Nothing,Just 2]; b <- "fo"; (a,b) |]
-- > [Just (1,'f'),Just (1,'o'),Nothing,Nothing,Just (2,'f'),Just (2,'o')]
ado :: QuasiQuoter
ado = ado'' False

-- | Variant of 'ado' that does not implicitly add a Maybe when patterns may fail:
--
-- > ghci> [$ado'| Just a <- [Just 1,Nothing,Just 2]; b <- "fo"; (a,b) |]
-- > [(1,'f'),(1,'o'),*** Exception: <interactive>:...
--
ado' :: QuasiQuoter
ado' = ado'' True

ado'' ::  Bool -> QuasiQuoter
ado'' b = QuasiQuoter
    { quoteExp = (\str -> fmap cleanNames $ applicate b =<< parseDo str)
    , quotePat = either fail return . parsePat
    }

parseDo ::  (Monad m) => String -> m [Stmt]
parseDo str =
    let prefix = "do\n" in
    case parseExp $ prefix ++ str of
      Right (DoE stmts) -> return stmts
      Right a -> fail $ "ado can't handle:\n" ++ show a
      Left a -> fail a

applicate :: Bool -> [Stmt] -> ExpQ
applicate rawPatterns stmt = do
    (_:ps,f:es) <- fmap (unzip . reverse) $
            flip mapM stmt $ \s ->
            case s of
                BindS p e -> return (p,e)
                NoBindS e   -> return (WildP,e)
                LetS _ -> fail $ "LetS not supported"
                ParS _ -> fail $ "ParS not supported"

    fps <- failingPatterns ps
    f' <- case filter (not . snd) $ zip ps fps of
        [] -> return $ LamE ps f
        _ | rawPatterns -> return $ LamE ps f
          | otherwise -> do
            xs <- mapM (const $ newName "x") ps
            return $ LamE (map VarP xs) $ CaseE (TupE (map VarE xs))
                [Match (TupP ps) (NormalB $ ConE 'Just `AppE` f) []
                ,Match WildP (NormalB $ ConE 'Nothing) []
                ]

    return $ foldl (\g e -> VarE '(<**>) `AppE` e `AppE` g)
                    (VarE 'pure `AppE` f')
                    es

failingPatterns ::  (Data a) => [a] -> Q [Bool]
failingPatterns ps = flip mapM ps $ \p ->
    let couldFail x = liftM not (singleCon x)

        constrs :: Data a => a -> S.Set Name
        constrs = S.fromList . map (\(ConP n _) -> n)
            . listify (\x -> case x of ConP {} -> True; _ -> False)
        irrefutables = constrs
                    . listify (\x -> case x of TildeP {} -> True; _ -> False)

    in liftM null $ filterM couldFail $ S.elems $
        constrs p S.\\ irrefutables p

singleCon ::  Name -> Q Bool
singleCon n = do
    DataConI _ _ tn _ <- reify n
    TyConI dec <- reify tn
    case dec of
        DataD _ _ _ [_] _ -> return True
        NewtypeD {} -> return True
        DataD _ _ _ (_:_) _ -> return False
        _ -> fail $ "Bad dec: "++show dec


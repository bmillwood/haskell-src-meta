{-# OPTIONS_GHC -fno-warn-orphans #-} -- TODO
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This module is a staging ground
-- for to-be-organized-and-merged-nicely code.

module Language.Haskell.Meta.Utils where

import Control.Monad
import Data.Generics                hiding (Fixity)
import Data.List                    (findIndex)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Meta
import Language.Haskell.TH.Lib      hiding (cxt)
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.Syntax
import System.IO.Unsafe             (unsafePerformIO)
import Text.PrettyPrint

-----------------------------------------------------------------------------


cleanNames :: (Data a) => a -> a
cleanNames = everywhere (mkT cleanName)
  where cleanName :: Name -> Name
        cleanName n
          | isNameU n = n
          | otherwise = (mkName . nameBase) n
        isNameU :: Name -> Bool
        isNameU (Name _ (NameU _)) = True
        isNameU _                  = False


-- | The type passed in must have a @Show@ instance which
--  produces a valid Haskell expression. Returns an empty
--  @String@ if this is not the case. This is not TH-specific,
--  but useful in general.
pretty :: (Show a) => a -> String
pretty a = case parseHsExp (show a) of
            Left _  -> []
            Right e -> prettyPrint e


pp :: (Data a, Ppr a) => a -> String
pp = pprint . cleanNames

ppDoc :: (Data a, Ppr a) => a -> Doc
ppDoc = text . pp


gpretty :: (Data a) => a -> String
gpretty = either (const []) prettyPrint . parseHsExp . gshow


instance Show ExpQ where show = show . cleanNames . unsafeRunQ
instance Show (Q [Dec]) where show = unlines . fmap (show . cleanNames) . unsafeRunQ
instance Show DecQ where show = show . cleanNames . unsafeRunQ
instance Show TypeQ where show = show . cleanNames . unsafeRunQ
instance Show (Q String) where show = unsafeRunQ
instance Show (Q Doc) where show = show . unsafeRunQ

-- | @unsafeRunQ = unsafePerformIO . runQ@
unsafeRunQ :: Q a -> a
unsafeRunQ = unsafePerformIO . runQ


nameToRawCodeStr :: Name -> String
nameToRawCodeStr n =
  let s = showNameParens n
  in case nameSpaceOf n of
      Just VarName   -> "'"++s
      Just DataName  -> "'"++s
      Just TcClsName -> "''"++s
      _              -> concat ["(mkName \"", filter (/='"') s, "\")"]
  where showNameParens :: Name -> String
        showNameParens n' =
          let nb = nameBase n'
          in case nb of
            (c:_) | isSym c -> concat ["(",nb,")"]
            _               -> nb
        isSym :: Char -> Bool
        isSym = (`elem` ("><.\\/!@#$%^&*-+?:|" :: [Char]))


-----------------------------------------------------------------------------


(|$|) :: ExpQ -> ExpQ -> ExpQ
infixr 0 |$|
f |$| x = [|$f $x|]

(|.|) :: ExpQ -> ExpQ -> ExpQ
infixr 9 |.|
g |.| f = [|$g . $f|]

(|->|) :: TypeQ -> TypeQ -> TypeQ
infixr 9 |->|
a |->| b = appT (appT arrowT a) b



unForall :: Type -> Type
unForall (ForallT _ _ t) = t
unForall t               = t

functionT :: [TypeQ] -> TypeQ
functionT = foldl1 (|->|)

mkVarT :: String -> TypeQ
mkVarT = varT . mkName


-- | Infinite list of names composed of lowercase letters
myNames :: [Name]
myNames = let xs = fmap (:[]) ['a'..'z']
              ys = iterate (join (zipWith (++))) xs
           in fmap mkName (concat ys)

-- | Generalisation of renameTs
renameThings :: (t1 -> t2 -> a1 -> (a2, t1, t2))
             -> t1 -> t2 -> [a2] -> [a1] -> ([a2], t1, t2)
renameThings _ env new acc [] = (reverse acc, env, new)
renameThings f env new acc (t:ts) =
  let (t', env', new') = f env new t
  in renameThings f env' new' (t':acc) ts

-- | renameT applied to a list of types
renameTs :: [(Name, Name)] -> [Name] -> [Type] -> [Type]
  -> ([Type], [(Name,Name)], [Name])
renameTs = renameThings renameT

-- | Rename type variables in the Type according to the given association
-- list. Normalise constructor names (remove qualification, etc.)
-- If a name is not found in the association list, replace it with one from
-- the fresh names list, and add this translation to the returned list.
-- The fresh names list should be infinite; myNames is a good example.
renameT :: [(Name, Name)] -> [Name] -> Type -> (Type, [(Name,Name)], [Name])
renameT _env [] _ = error "renameT: ran out of names!"
renameT env (x:new) (VarT n)
 | Just n' <- lookup n env = (VarT n',env,x:new)
 | otherwise = (VarT x, (n,x):env, new)
renameT env new (ConT n) = (ConT (normaliseName n), env, new)
renameT env new t@(TupleT {}) = (t,env,new)
renameT env new ArrowT = (ArrowT,env,new)
renameT env new ListT = (ListT,env,new)
renameT env new (AppT t t') = let (s,env',new') = renameT env new t
                                  (s',env'',new'') = renameT env' new' t'
                              in (AppT s s', env'', new'')
renameT env new (ForallT ns cxt t) =
    let (ns',env2,new2) = renameTs env new [] (fmap (VarT . toName) ns)
        ns'' = fmap unVarT ns'
        (cxt',env3,new3) = renamePreds env2 new2 [] cxt
        (t',env4,new4) = renameT env3 new3 t
    in (ForallT ns'' cxt' t', env4, new4)
  where
    unVarT (VarT n) = PlainTV n
    unVarT ty       = error $ "renameT: unVarT: TODO for" ++ show ty
    renamePreds = renameThings renamePred
    renamePred = renameT
renameT _ _ t = error $ "renameT: TODO for " ++ show t

-- | Remove qualification, etc.
normaliseName :: Name -> Name
normaliseName = mkName . nameBase

applyT :: Type -> Type -> Type
applyT (ForallT [] _ t) t' = t `AppT` t'
applyT (ForallT (n:ns) cxt t) t' = ForallT ns cxt
  (substT [(toName n,t')] (fmap toName ns) t)
applyT t t' = t `AppT` t'



substT :: [(Name, Type)] -> [Name] -> Type -> Type
substT env bnd (ForallT ns _ t) = substT env (fmap toName ns++bnd) t
substT env bnd t@(VarT n)
  | n `elem` bnd = t
  | otherwise = maybe t id (lookup n env)
substT env bnd (AppT t t') = AppT (substT env bnd t)
                                  (substT env bnd t')
substT _ _ t = t





splitCon :: Con -> (Name,[Type])
splitCon c = (conName c, conTypes c)


strictTypeTy :: StrictType -> Type
strictTypeTy (_,t) = t

varStrictTypeTy :: VarStrictType -> Type
varStrictTypeTy (_,_,t) = t


conTypes :: Con -> [Type]
conTypes (NormalC _ sts) = fmap strictTypeTy sts
conTypes (RecC    _ vts) = fmap varStrictTypeTy vts
conTypes (InfixC t _ t') = fmap strictTypeTy [t,t']
conTypes (ForallC _ _ c) = conTypes c
conTypes c               = error $ "conTypes: TODO for " ++ show c
-- TODO
            -- (GadtC _ _ _)
            -- (RecGadtC _ _ _)


conToConType :: Type -> Con -> Type
conToConType ofType con = foldr (\a b -> AppT (AppT ArrowT a) b) ofType (conTypes con)



decCons :: Dec -> [Con]
#if MIN_VERSION_template_haskell(2,11,0)
decCons (DataD _ _ _ _ cons _)   = cons
decCons (NewtypeD _ _ _ _ con _) = [con]
#else
decCons (DataD _ _ _ cons _)     = cons
decCons (NewtypeD _ _ _ con _)   = [con]
#endif
decCons _                        = []


decTyVars :: Dec -> [TyVarBndr]
#if MIN_VERSION_template_haskell(2,11,0)
decTyVars (DataD _ _ ns _ _ _)    = ns
decTyVars (NewtypeD _ _ ns _ _ _) = ns
#else
decTyVars (DataD _ _ ns _ _)      = ns
decTyVars (NewtypeD _ _ ns _ _)   = ns
#endif
decTyVars (TySynD _ ns _)         = ns
decTyVars (ClassD _ _ ns _ _)     = ns
decTyVars _                       = []


decName :: Dec -> Maybe Name
decName (FunD n _)             = Just n
#if MIN_VERSION_template_haskell(2,11,0)
decName (DataD _ n _ _ _ _)    = Just n
decName (NewtypeD _ n _ _ _ _) = Just n
#else
decName (DataD _ n _ _ _)      = Just n
decName (NewtypeD _ n _ _ _)   = Just n
#endif
decName (TySynD n _ _)         = Just n
decName (ClassD _ n _ _ _)     = Just n
decName (SigD n _)             = Just n
decName (ForeignD fgn)         = Just (foreignName fgn)
decName _                      = Nothing


foreignName :: Foreign -> Name
foreignName (ImportF _ _ _ n _) = n
foreignName (ExportF _ _ n _)   = n


unwindT :: Type -> [Type]
unwindT = go
  where go :: Type -> [Type]
        go (ForallT _ _ t)           = go t
        go (AppT (AppT ArrowT t) t') = t : go t'
        go _                         = []


unwindE :: Exp -> [Exp]
unwindE = go []
  where go acc (e `AppE` e') = go (e':acc) e
        go acc e             = e:acc


-- | The arity of a Type.
arityT :: Type -> Int
arityT = go 0
  where go :: Int -> Type -> Int
        go n (ForallT _ _ t) = go n t
        go n (AppT (AppT ArrowT _) t) =
          let n' = n+1 in n' `seq` go n' t
        go n _ = n

typeToName :: Type -> Maybe Name
typeToName t
  | ConT n <- t = Just n
  | ArrowT <- t = Just ''(->)
  | ListT  <- t = Just ''[]
  | TupleT n <- t = Just $ tupleTypeName n
  | ForallT _ _ t' <- t = typeToName t'
  | otherwise = Nothing

-- | Randomly useful.
nameSpaceOf :: Name -> Maybe NameSpace
nameSpaceOf (Name _ (NameG ns _ _)) = Just ns
nameSpaceOf _                       = Nothing

conName :: Con -> Name
conName (RecC n _)        = n
conName (NormalC n _)     = n
conName (InfixC _ n _)    = n
conName (ForallC _ _ con) = conName con
conName c                 = error $ "conName: TODO for" ++ show c
-- TODO
            -- (GadtC _ _ _)
            -- (RecGadtC _ _ _)

recCName :: Con -> Maybe Name
recCName (RecC n _) = Just n
recCName _          = Nothing

dataDCons :: Dec -> [Con]
#if MIN_VERSION_template_haskell(2,11,0)
dataDCons (DataD _ _ _ _ cons _) = cons
#else
dataDCons (DataD _ _ _ cons _)   = cons
#endif
dataDCons _                      = []

fromDataConI :: Info -> Q (Maybe Exp)
#if MIN_VERSION_template_haskell(2,11,0)
fromDataConI (DataConI dConN ty _tyConN) =
  let n = arityT ty
  in replicateM n (newName "a")
      >>= \ns -> return (Just (LamE
                    [ConP dConN (fmap VarP ns)]
#if MIN_VERSION_template_haskell(2,16,0)
                    (TupE $ fmap (Just . VarE) ns)
#else
                    (TupE $ fmap VarE ns)
#endif
                    ))
#else
fromDataConI (DataConI dConN ty _tyConN _fxty) =
  let n = arityT ty
  in replicateM n (newName "a")
      >>= \ns -> return (Just (LamE
                    [ConP dConN (fmap VarP ns)]
                    (TupE $ fmap VarE ns)))

#endif
fromDataConI _ = return Nothing

fromTyConI :: Info -> Maybe Dec
fromTyConI (TyConI dec) = Just dec
fromTyConI _            = Nothing

mkFunD :: Name -> [Pat] -> Exp -> Dec
mkFunD f xs e = FunD f [Clause xs (NormalB e) []]

mkClauseQ :: [PatQ] -> ExpQ -> ClauseQ
mkClauseQ ps e = clause ps (normalB e) []

-----------------------------------------------------------------------------

-- | The strategy for producing QuasiQuoters which
--  this datatype aims to facilitate is as follows.
--  Given a collection of datatypes which make up
--  the to-be-quasiquoted languages AST, make each
--  type in this collection an instance of at least
--  @Show@ and @Lift@. Now, assuming @parsePat@ and
--  @parseExp@, both of type @String -> Q a@ (where @a@
--  is the top level type of the AST), are the pair of
--  functions you wish to use for parsing in pattern and
--  expression context respectively, put them inside
--  a @Quoter@ datatype and pass this to quasify.
{-
data Quoter a = Quoter
  { expQ :: (Lift a) => String -> Q a
  , patQ :: (Show a) => String -> Q a }

quasify :: (Show a, Lift a) => Quoter a -> QuasiQuoter
quasify q = QuasiQuoter
              (toExpQ (expQ q))
              (toPatQ (patQ q))
              -}

toExpQ :: (Lift a) => (String -> Q a) -> (String -> ExpQ)
toExpQ parseQ = (lift =<<) . parseQ

toPatQ :: (Show a) => (String -> Q a) -> (String -> PatQ)
toPatQ parseQ = (showToPatQ =<<) . parseQ

showToPatQ :: (Show a) => a -> PatQ
showToPatQ = either fail return . parsePat . show

-----------------------------------------------------------------------------

eitherQ :: (e -> String) -> Either e a -> Q a
eitherQ toStr = either (fail . toStr) return

-----------------------------------------------------------------------------




normalizeT :: (Data a) => a -> a
normalizeT = everywhere (mkT go)
  where go :: Type -> Type
        go (ConT n) | n == ''[] = ListT
        go (AppT (TupleT 1) t) = t
        go (ConT n)
          | Just m <- findIndex (== n) tupleNames = TupleT (m + 2)
         where
          tupleNames = map tupleTypeName [2 .. 64]
        go t = t



-----------------------------------------------------------------------------

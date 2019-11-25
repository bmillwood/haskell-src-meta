{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
  Module      :  Language.Haskell.Meta.Syntax.Translate
  Copyright   :  (c) Matt Morrow 2008
  License     :  BSD3
  Maintainer  :  Matt Morrow <mjm2002@gmail.com>
  Stability   :  experimental
  Portability :  portable (template-haskell)
-}

module Language.Haskell.Meta.Syntax.Translate (
    module Language.Haskell.Meta.Syntax.Translate
) where

import qualified Data.Char                    as Char
import qualified Data.List                    as List
import qualified Language.Haskell.Exts.SrcLoc as Exts.SrcLoc
import qualified Language.Haskell.Exts.Syntax as Exts
import qualified Language.Haskell.TH.Syntax   as TH

-----------------------------------------------------------------------------


class ToName a where toName :: a -> TH.Name
class ToNames a where toNames :: a -> [TH.Name]
class ToLit  a where toLit  :: a -> TH.Lit
class ToType a where toType :: a -> TH.Type
class ToPat  a where toPat  :: a -> TH.Pat
class ToExp  a where toExp  :: a -> TH.Exp
class ToDecs a where toDecs :: a -> [TH.Dec]
class ToDec  a where toDec  :: a -> TH.Dec
class ToStmt a where toStmt :: a -> TH.Stmt
class ToLoc  a where toLoc  :: a -> TH.Loc
class ToCxt  a where toCxt  :: a -> TH.Cxt
class ToPred a where toPred :: a -> TH.Pred
class ToTyVars a where toTyVars :: a -> [TH.TyVarBndr]
class ToMaybeKind a where toMaybeKind :: a -> Maybe TH.Kind
#if MIN_VERSION_template_haskell(2,11,0)
class ToInjectivityAnn a where toInjectivityAnn :: a -> TH.InjectivityAnn
#endif

#if MIN_VERSION_template_haskell(2,12,0)
type DerivClause = TH.DerivClause
#elif MIN_VERSION_template_haskell(2,11,0)
type DerivClause = TH.Pred
#else
type DerivClause = TH.Name
#endif

class ToDerivClauses a where toDerivClauses :: a -> [DerivClause]

-- for error messages
moduleName :: String
moduleName = "Language.Haskell.Meta.Syntax.Translate"

-- When to use each of these isn't always clear: prefer 'todo' if unsure.
noTH :: (Functor f, Show (f ())) => String -> f e -> a
noTH fun thing = error . concat $ [moduleName, ".", fun,
  ": template-haskell has no representation for: ", show (fmap (const ()) thing)]

noTHyet :: (Functor f, Show (f ())) => String -> String -> f e -> a
noTHyet fun minVersion thing = error . concat $ [moduleName, ".", fun,
  ": template-haskell-", VERSION_template_haskell, " (< ", minVersion, ")",
  " has no representation for: ", show (fmap (const ()) thing)]

todo :: (Functor f, Show (f ())) => String -> f e -> a
todo fun thing = error . concat $ [moduleName, ".", fun,
  ": not implemented: ", show (fmap (const ()) thing)]

nonsense :: (Functor f, Show (f ())) => String -> String -> f e -> a
nonsense fun inparticular thing = error . concat $ [moduleName, ".", fun,
  ": nonsensical: ", inparticular, ": ", show (fmap (const ()) thing)]

#if MIN_VERSION_template_haskell(2,16,0)
toTupEl :: ToExp a => a -> Maybe TH.Exp
toTupEl = Just . toExp
#else
toTupEl :: ToExp a => a -> TH.Exp
toTupEl = toExp
#endif

-----------------------------------------------------------------------------


instance ToExp TH.Lit where
  toExp = TH.LitE
instance (ToExp a) => ToExp [a] where
  toExp = TH.ListE . fmap toExp
instance (ToExp a, ToExp b) => ToExp (a,b) where
  toExp (a,b) = TH.TupE [toTupEl a, toTupEl b]
instance (ToExp a, ToExp b, ToExp c) => ToExp (a,b,c) where
  toExp (a,b,c) = TH.TupE [toTupEl a, toTupEl b, toTupEl c]
instance (ToExp a, ToExp b, ToExp c, ToExp d) => ToExp (a,b,c,d) where
  toExp (a,b,c,d) = TH.TupE [toTupEl a, toTupEl b, toTupEl c, toTupEl d]


instance ToPat TH.Lit where
  toPat = TH.LitP
instance (ToPat a) => ToPat [a] where
  toPat = TH.ListP . fmap toPat
instance (ToPat a, ToPat b) => ToPat (a,b) where
  toPat (a,b) = TH.TupP [toPat a, toPat b]
instance (ToPat a, ToPat b, ToPat c) => ToPat (a,b,c) where
  toPat (a,b,c) = TH.TupP [toPat a, toPat b, toPat c]
instance (ToPat a, ToPat b, ToPat c, ToPat d) => ToPat (a,b,c,d) where
  toPat (a,b,c,d) = TH.TupP [toPat a, toPat b, toPat c, toPat d]


instance ToLit Char where
  toLit = TH.CharL
instance ToLit String where
  toLit = TH.StringL
instance ToLit Integer where
  toLit = TH.IntegerL
instance ToLit Int where
  toLit = TH.IntegerL . toInteger
instance ToLit Float where
  toLit = TH.RationalL . toRational
instance ToLit Double where
  toLit = TH.RationalL . toRational


-----------------------------------------------------------------------------


-- * ToName {String,HsName,Module,HsSpecialCon,HsQName}


instance ToName String where
  toName = TH.mkName

instance ToName (Exts.Name l) where
  toName (Exts.Ident _ s)  = toName s
  toName (Exts.Symbol _ s) = toName s

instance ToName (Exts.SpecialCon l) where
  toName (Exts.UnitCon _) = TH.mkName "()" -- TODO LumiGuide: '()
  toName (Exts.ListCon _) = ''[] -- Parser only uses this in types -- TODO LumiGuide: '[]
  toName (Exts.FunCon _)  = ''(->)
  toName (Exts.TupleCon _ _ n) =
    TH.mkName $ concat ["(",replicate (n-1) ',',")"]
    -- TODO LumiGuide:
    -- .
    -- .| n<2 = '()
    -- .| otherwise =
    -- .  let x = maybe [] (++".") (nameModule '(,))
    -- .  in TH.mkName . concat $ x : ["(",replicate (n-1) ',',")"]
  toName (Exts.Cons _)    = '(:)
  toName h = todo "toName not implemented" h
  -- TODO
  -- toName (Exts.UnboxedSingleCon _) = ''
  -- toName (Exts.ExprHole _) = ''_


instance ToName (Exts.QName l) where
-- TODO: why is this commented out?
--  toName (Exts.Qual (Exts.Module []) n) = toName n
  toName (Exts.Qual _ (Exts.ModuleName _ []) n) = toName n
  toName (Exts.Qual _ (Exts.ModuleName _ m) n) =
    let m' = show . toName $ m
        n' = show . toName $ n
    in toName . concat $ [m',".",n']
  toName (Exts.UnQual _ n) = toName n
  toName (Exts.Special _ s) = toName s

#if MIN_VERSION_haskell_src_exts(1,20,1)
instance ToName (Exts.MaybePromotedName l) where
  toName (Exts.PromotedName   _ qn) = toName qn
  toName (Exts.UnpromotedName _ qn) = toName qn
#endif

instance ToName (Exts.Op l) where
  toName (Exts.VarOp _ n) = toName n
  toName (Exts.ConOp _ n) = toName n


-----------------------------------------------------------------------------

-- * ToLit HsLiteral


instance ToLit (Exts.Literal l) where
  toLit (Exts.Char _ a _) = TH.CharL a
  toLit (Exts.String _ a _) = TH.StringL a
  toLit (Exts.Int _ a _) = TH.IntegerL a
  toLit (Exts.Frac _ a _) = TH.RationalL a
  toLit l@Exts.PrimChar{} = noTH "toLit" l
  toLit (Exts.PrimString _ a _) = TH.StringPrimL (map toWord8 a)
   where
    toWord8 = fromIntegral . Char.ord
  toLit (Exts.PrimInt _ a _) = TH.IntPrimL a
  toLit (Exts.PrimFloat _ a _) = TH.FloatPrimL a
  toLit (Exts.PrimDouble _ a _) = TH.DoublePrimL a
  toLit (Exts.PrimWord _ a _) = TH.WordPrimL a


-----------------------------------------------------------------------------

-- * ToPat HsPat


instance ToPat (Exts.Pat l) where
  toPat (Exts.PVar _ n)
    = TH.VarP (toName n)
  toPat (Exts.PLit _ (Exts.Signless _) l)
    = TH.LitP (toLit l)
  toPat (Exts.PLit _ (Exts.Negative _) l) = TH.LitP $ case toLit l of
    TH.IntegerL z      -> TH.IntegerL (negate z)
    TH.RationalL q     -> TH.RationalL (negate q)
    TH.IntPrimL z'     -> TH.IntPrimL (negate z')
    TH.FloatPrimL r'   -> TH.FloatPrimL (negate r')
    TH.DoublePrimL r'' -> TH.DoublePrimL (negate r'')
    _                  -> nonsense "toPat" "negating wrong kind of literal" l
  toPat (Exts.PInfixApp _ p n q) = TH.UInfixP (toPat p) (toName n) (toPat q)
  toPat (Exts.PApp _ n ps) = TH.ConP (toName n) (fmap toPat ps)
  toPat (Exts.PTuple _ Exts.Boxed ps) = TH.TupP (fmap toPat ps)
  toPat (Exts.PTuple _ Exts.Unboxed ps) = TH.UnboxedTupP (fmap toPat ps)
  toPat (Exts.PList _ ps) = TH.ListP (fmap toPat ps)
  toPat (Exts.PParen _ p) = TH.ParensP (toPat p)
  -- TODO: move toFieldPat to top level defn
  toPat (Exts.PRec _ n pfs) = let toFieldPat (Exts.PFieldPat _ n' p) = (toName n', toPat p)
                                  toFieldPat h = todo "toFieldPat" h
                            in TH.RecP (toName n) (fmap toFieldPat pfs)
  toPat (Exts.PAsPat _ n p) = TH.AsP (toName n) (toPat p)
  toPat (Exts.PWildCard _) = TH.WildP
  toPat (Exts.PIrrPat _ p) = TH.TildeP (toPat p)
  toPat (Exts.PatTypeSig _ p t) = TH.SigP (toPat p) (toType t)
  toPat (Exts.PViewPat _ e p) = TH.ViewP (toExp e) (toPat p)
  -- regular pattern
  toPat p@Exts.PRPat{} = noTH "toPat" p
  -- XML stuff
  toPat p@Exts.PXTag{} = noTH "toPat" p
  toPat p@Exts.PXETag{} = noTH "toPat" p
  toPat p@Exts.PXPcdata{} = noTH "toPat" p
  toPat p@Exts.PXPatTag{} = noTH "toPat" p
  toPat (Exts.PBangPat _ p) = TH.BangP (toPat p)
  toPat p = todo "toPat" p
  -- TODO
            -- (Exts.PNPlusK _ _ _)
            -- (Exts.PUnboxedSum _ _ _ _)
            -- (Exts.PXRPats _ _)
            -- (Exts.PSplice _ _)
            -- ...

-----------------------------------------------------------------------------

-- * ToExp HsExp

instance ToExp (Exts.QOp l) where
  toExp (Exts.QVarOp _ n) = TH.VarE (toName n)
  toExp (Exts.QConOp _ n) = TH.ConE (toName n)

toFieldExp :: Exts.FieldUpdate l -> TH.FieldExp
toFieldExp (Exts.FieldUpdate _ n e) = (toName n, toExp e)
toFieldExp h                        = todo "toFieldExp" h




instance ToExp (Exts.Exp l) where
  toExp (Exts.Var _ n)                 = TH.VarE (toName n)
  toExp e@Exts.IPVar{}                 = noTH "toExp" e
  toExp (Exts.Con _ n)                 = TH.ConE (toName n)
  toExp (Exts.Lit _ l)                 = TH.LitE (toLit l)
  toExp (Exts.InfixApp _ e o f)        = TH.UInfixE (toExp e) (toExp o) (toExp f)
#if MIN_VERSION_template_haskell(2,12,0)
  toExp (Exts.App _ e (Exts.TypeApp _ t)) = TH.AppTypeE (toExp e) (toType t)
#else
  toExp (Exts.App _ _ e@Exts.TypeApp{}) = noTHyet "toExp" "2.12.0" e
#endif
  toExp (Exts.App _ e f)               = TH.AppE (toExp e) (toExp f)
  toExp (Exts.NegApp _ e)              = TH.AppE (TH.VarE 'negate) (toExp e)
  toExp (Exts.Lambda _ ps e)           = TH.LamE (fmap toPat ps) (toExp e)
  toExp (Exts.Let _ bs e)              = TH.LetE (toDecs bs) (toExp e)
  toExp (Exts.If _ a b c)              = TH.CondE (toExp a) (toExp b) (toExp c)
  toExp (Exts.MultiIf _ ifs)           = TH.MultiIfE (map toGuard ifs)
  toExp (Exts.Case _ e alts)           = TH.CaseE (toExp e) (map toMatch alts)
  toExp (Exts.Do _ ss)                 = TH.DoE (map toStmt ss)
  toExp e@Exts.MDo{}                   = noTH "toExp" e
  toExp (Exts.Tuple _ Exts.Boxed xs)   = TH.TupE (fmap toTupEl xs)
  toExp (Exts.Tuple _ Exts.Unboxed xs) = TH.UnboxedTupE (fmap toTupEl xs)
  toExp e@Exts.TupleSection{}          = noTH "toExp" e
  toExp (Exts.List _ xs)               = TH.ListE (fmap toExp xs)
  toExp (Exts.Paren _ e)               = TH.ParensE (toExp e)
  toExp (Exts.LeftSection _ e o)       = TH.InfixE (Just . toExp $ e) (toExp o) Nothing
  toExp (Exts.RightSection _ o f)      = TH.InfixE Nothing (toExp o) (Just . toExp $ f)
  toExp (Exts.RecConstr _ n xs)        = TH.RecConE (toName n) (fmap toFieldExp xs)
  toExp (Exts.RecUpdate _ e xs)        = TH.RecUpdE (toExp e) (fmap toFieldExp xs)
  toExp (Exts.EnumFrom _ e)            = TH.ArithSeqE $ TH.FromR (toExp e)
  toExp (Exts.EnumFromTo _ e f)        = TH.ArithSeqE $ TH.FromToR (toExp e) (toExp f)
  toExp (Exts.EnumFromThen _ e f)      = TH.ArithSeqE $ TH.FromThenR (toExp e) (toExp f)
  toExp (Exts.EnumFromThenTo _ e f g)  = TH.ArithSeqE $ TH.FromThenToR (toExp e) (toExp f) (toExp g)
  toExp (Exts.ListComp _ e ss)         = TH.CompE $ map convert ss ++ [TH.NoBindS (toExp e)]
   where
    convert (Exts.QualStmt _ st) = toStmt st
    convert s                    = noTH "toExp ListComp" s
  toExp (Exts.ExpTypeSig _ e t)      = TH.SigE (toExp e) (toType t)
  toExp e = todo "toExp" e


toMatch :: Exts.Alt l -> TH.Match
toMatch (Exts.Alt _ p rhs ds) = TH.Match (toPat p) (toBody rhs) (toDecs ds)

toBody :: Exts.Rhs l -> TH.Body
toBody (Exts.UnGuardedRhs _ e)   = TH.NormalB $ toExp e
toBody (Exts.GuardedRhss _ rhss) = TH.GuardedB $ map toGuard rhss

toGuard :: Exts.GuardedRhs l -> (TH.Guard, TH.Exp)
toGuard (Exts.GuardedRhs _ stmts e) = (g, toExp e)
  where
    g = case map toStmt stmts of
      [TH.NoBindS x] -> TH.NormalG x
      xs             -> TH.PatG xs

instance ToDecs a => ToDecs (Maybe a) where
    toDecs Nothing  = []
    toDecs (Just a) = toDecs a

instance ToDecs (Exts.Binds l) where
  toDecs (Exts.BDecls _ ds)  = toDecs ds
  toDecs a@(Exts.IPBinds {}) = noTH "ToDecs Exts.Binds" a

instance ToDecs (Exts.ClassDecl l) where
  toDecs (Exts.ClsDecl _ d) = toDecs d
  toDecs x                  = todo "classDecl" x

-----------------------------------------------------------------------------

-- * ToLoc SrcLoc

instance ToLoc Exts.SrcLoc.SrcLoc where
  toLoc (Exts.SrcLoc.SrcLoc fn l c) =
    TH.Loc fn [] [] (l,c) (-1,-1)

-----------------------------------------------------------------------------

-- * ToType HsType

instance ToName (Exts.TyVarBind l) where
  toName (Exts.KindedVar _ n _) = toName n
  toName (Exts.UnkindedVar _ n) = toName n

instance ToName TH.Name where
  toName = id

instance ToName TH.TyVarBndr where
  toName (TH.PlainTV n)    = n
  toName (TH.KindedTV n _) = n

#if !MIN_VERSION_haskell_src_exts(1,21,0)
instance ToType (Exts.Kind l) where
  toType (Exts.KindStar _)     = TH.StarT
  toType (Exts.KindFn _ k1 k2) = toType k1 .->. toType k2
  toType (Exts.KindParen _ kp) = toType kp
  toType (Exts.KindVar _ n)    = TH.VarT (toName n)
  -- TODO LumiGuide:
  -- toType (Hs.KindVar _ n)
  --    | isCon (nameBase th_n) = ConT th_n
  --    | otherwise             = VarT th_n
  --  where
  --    th_n = toName n
  --
  --    isCon :: String -> Bool
  --    isCon (c:_) = isUpper c || c == ':'
  --    isCon _ = nonsense "toType" "empty kind variable name" n
  toType (Exts.KindApp _ k1 k2) = toType k1 `TH.AppT` toType k2
  toType (Exts.KindTuple _ ks) = foldr (\k pt -> pt `TH.AppT` toType k) (TH.TupleT $ length ks) ks
  toType (Exts.KindList _ k) = TH.ListT `TH.AppT` toType k
#endif

toKind :: Exts.Kind l -> TH.Kind
toKind = toType

toTyVar :: Exts.TyVarBind l -> TH.TyVarBndr
toTyVar (Exts.KindedVar _ n k) = TH.KindedTV (toName n) (toKind k)
toTyVar (Exts.UnkindedVar _ n) = TH.PlainTV (toName n)

instance ToType (Exts.Type l) where
  toType (Exts.TyForall _ tvbM cxt t) = TH.ForallT (maybe [] (fmap toTyVar) tvbM) (toCxt cxt) (toType t)
  toType (Exts.TyFun _ a b) = toType a .->. toType b
  toType (Exts.TyList _ t) = TH.ListT `TH.AppT` toType t
  toType (Exts.TyTuple _ b ts) = foldAppT (tuple . length $ ts) (fmap toType ts)
   where
    tuple = case b of
      Exts.Boxed   -> TH.TupleT
      Exts.Unboxed -> TH.UnboxedTupleT
  toType (Exts.TyApp _ a b) = TH.AppT (toType a) (toType b)
  toType (Exts.TyVar _ n) = TH.VarT (toName n)
  toType (Exts.TyCon _ qn) = TH.ConT (toName qn)
  toType (Exts.TyParen _ t) = toType t
  -- XXX: need to wrap the name in parens!
#if MIN_VERSION_haskell_src_exts(1,20,0)
  -- TODO: why does this branch exist?
  -- Why fail toType if this is a promoted name?
  toType (Exts.TyInfix _ a (Exts.UnpromotedName _ o) b) =
    TH.AppT (TH.AppT (TH.ConT (toName o)) (toType a)) (toType b)
#else
  toType (Exts.TyInfix _ a o b) =
    TH.AppT (TH.AppT (TH.ConT (toName o)) (toType a)) (toType b)
#endif
  toType (Exts.TyKind _ t k) = TH.SigT (toType t) (toKind k)
  toType (Exts.TyPromoted _ p) = case p of
    Exts.PromotedInteger _ i _ -> TH.LitT $ TH.NumTyLit i
    Exts.PromotedString _ _ s -> TH.LitT $ TH.StrTyLit s
    Exts.PromotedCon _ _q n -> TH.PromotedT $ toName n
    Exts.PromotedList _ _q ts -> foldr (\t pl -> TH.PromotedConsT `TH.AppT` toType t `TH.AppT` pl) TH.PromotedNilT ts
    Exts.PromotedTuple _ ts -> foldr (\t pt -> pt `TH.AppT` toType t) (TH.PromotedTupleT $ length ts) ts
    Exts.PromotedUnit _ -> TH.PromotedT ''()
  toType (Exts.TyEquals _ t1 t2) = TH.EqualityT `TH.AppT` toType t1 `TH.AppT` toType t2
  toType t@Exts.TySplice{} = noTH "toType" t
  toType t@Exts.TyBang{} =
    nonsense "toType" "type cannot have strictness annotations in this context" t
  toType t@Exts.TyWildCard{} = noTH "toType" t
  toType t = todo "toType" t
  -- TODO
  -- toType (Exts.TyUnboxedSum _ _)
  -- toType (Exts.TyParArray _ _)
  -- toType (Exts.TyInfix _ _ (Exts.PromotedName _ _) _)

toStrictType :: Exts.Type l -> TH.StrictType
#if MIN_VERSION_template_haskell(2,11,0)
toStrictType (Exts.TyBang _ s u t) = (TH.Bang (toUnpack u) (toStrict s), toType t)
    where
      toStrict (Exts.LazyTy _)        = TH.SourceLazy
      toStrict (Exts.BangedTy _)      = TH.SourceStrict
      toStrict (Exts.NoStrictAnnot _) = TH.NoSourceStrictness
      toUnpack (Exts.Unpack _)         = TH.SourceUnpack
      toUnpack (Exts.NoUnpack _)       = TH.SourceNoUnpack
      toUnpack (Exts.NoUnpackPragma _) = TH.NoSourceUnpackedness
toStrictType x = (TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, toType x)
#else
-- TODO: what is this comment? Outdated?
-- TyBang l (BangType l) (Unpackedness l) (Type l)
-- data BangType l = BangedTy l        | LazyTy l | NoStrictAnnot l
-- data Unpackedness l = Unpack l | NoUnpack l | NoUnpackPragma l
toStrictType (Exts.TyBang _ b u t) = (toStrict b u, toType t)
    where
      toStrict :: Exts.BangType l -> Exts.Unpackedness l -> TH.Strict
      toStrict (Exts.BangedTy _) _ = TH.IsStrict
      toStrict _ (Exts.Unpack _)   = TH.Unpacked
      toStrict _ _                 = TH.NotStrict
toStrictType x = (TH.NotStrict, toType x)
#endif

(.->.) :: TH.Type -> TH.Type -> TH.Type
a .->. b = TH.AppT (TH.AppT TH.ArrowT a) b

instance ToPred (Exts.Asst l) where
#if MIN_VERSION_haskell_src_exts(1,22,0)
    toPred (Exts.TypeA _ t) = toType t
#else
    toPred (Exts.ClassA _ n ts) = List.foldl' TH.AppT (TH.ConT (toName n)) (fmap toType ts)
    toPred (Exts.InfixA _ t1 n t2) = List.foldl' TH.AppT (TH.ConT (toName n)) (fmap toType [t1,t2])
    toPred (Exts.EqualP _ t1 t2) = List.foldl' TH.AppT TH.EqualityT (fmap toType [t1,t2])
    toPred a@Exts.AppA{} = todo "toPred" a
    toPred a@Exts.WildCardA{} = todo "toPred" a
#endif
    toPred (Exts.ParenA _ asst) = toPred asst
    toPred a@Exts.IParam{} = noTH "toPred" a
    -- Pattern match is redundant.
    -- TODO: Is there a way to turn off this warn for catch-alls?
    -- would make the code more future-compat
    -- toPred p = todo "toPred" p

instance ToDerivClauses (Exts.Deriving l) where
#if MIN_VERSION_template_haskell(2,12,0)
#if MIN_VERSION_haskell_src_exts(1,20,0)
  toDerivClauses (Exts.Deriving _ strat irules) = [TH.DerivClause (fmap toDerivStrategy strat) (map toType irules)]
#else
  toDerivClauses (Exts.Deriving _ irules) = [TH.DerivClause Nothing (map toType irules)]
#endif
#elif MIN_VERSION_template_haskell(2,11,0)
#if MIN_VERSION_haskell_src_exts(1,20,0)
  toDerivClauses (Exts.Deriving _ _ irules) = map toType irules
#else
  toDerivClauses (Exts.Deriving _ irules) = map toType irules
#endif
#else
-- template-haskell < 2.11
#if MIN_VERSION_haskell_src_exts(1,20,0)
  toDerivClauses (Exts.Deriving _ _ irules) = concatMap toNames irules
#else
  toDerivClauses (Exts.Deriving _ irules) = concatMap toNames irules
#endif
#endif

instance ToDerivClauses a => ToDerivClauses (Maybe a) where
  toDerivClauses Nothing  = []
  toDerivClauses (Just a) = toDerivClauses a

instance ToDerivClauses a => ToDerivClauses [a] where
  toDerivClauses = concatMap toDerivClauses


#if MIN_VERSION_template_haskell(2,12,0) && MIN_VERSION_haskell_src_exts(1,20,0)
toDerivStrategy :: (Exts.DerivStrategy l) -> TH.DerivStrategy
toDerivStrategy (Exts.DerivStock _)    = TH.StockStrategy
toDerivStrategy (Exts.DerivAnyclass _) = TH.AnyclassStrategy
toDerivStrategy (Exts.DerivNewtype _)  = TH.NewtypeStrategy
#if MIN_VERSION_haskell_src_exts(1,21,0)
#if MIN_VERSION_template_haskell(2,14,0)
toDerivStrategy (Exts.DerivVia _ t)    = TH.ViaStrategy (toType t)
#else
toDerivStrategy d@Exts.DerivVia{}      = noTHyet "toDerivStrategy" "2.14" d
#endif
#endif

#endif


-- TODO LumiGuide
-- instance ToCxt (Hs.Deriving l) where
-- #if MIN_VERSION_haskell_src_exts(1,20,1)
--   toCxt (Hs.Deriving _ _ rule) = toCxt rule
-- #else
--   toCxt (Hs.Deriving _   rule) = toCxt rule
-- #endif

-- instance ToCxt [Hs.InstRule l] where
--   toCxt = concatMap toCxt

-- instance ToCxt a => ToCxt (Maybe a) where
--     toCxt Nothing = []
--     toCxt (Just a) = toCxt a


foldAppT :: TH.Type -> [TH.Type] -> TH.Type
foldAppT t ts = List.foldl' TH.AppT t ts

-----------------------------------------------------------------------------

-- * ToStmt HsStmt

instance ToStmt (Exts.Stmt l) where
  toStmt (Exts.Generator _ p e)   = TH.BindS (toPat p) (toExp e)
  toStmt (Exts.Qualifier _ e)     = TH.NoBindS (toExp e)
  toStmt _a@(Exts.LetStmt _ bnds) = TH.LetS (toDecs bnds)
  toStmt s@Exts.RecStmt{}         = noTH "toStmt" s


-----------------------------------------------------------------------------

-- * ToDec HsDecl

instance ToDec (Exts.Decl l) where
  toDec (Exts.TypeDecl _ h t)
    = TH.TySynD (toName h) (toTyVars h) (toType t)

  toDec a@(Exts.DataDecl  _ dOrN cxt h qcds qns)
    = case dOrN of
        Exts.DataType _ -> TH.DataD (toCxt cxt)
                             (toName h)
                             (toTyVars h)
#if MIN_VERSION_template_haskell(2,11,0)
                             Nothing
#endif
                             (fmap qualConDeclToCon qcds)
                             (toDerivClauses qns)
        Exts.NewType _  -> let qcd = case qcds of
                                     [x] -> x
                                     _   -> nonsense "toDec" ("newtype with " ++
                                                              "wrong number of constructors") a
                        in TH.NewtypeD (toCxt cxt)
                                    (toName h)
                                    (toTyVars h)
#if MIN_VERSION_template_haskell(2,11,0)
                                    Nothing
#endif
                                    (qualConDeclToCon qcd)
                                    (toDerivClauses qns)

  -- This type-signature conversion is just wrong.
  -- Type variables need to be dealt with. /Jonas
  toDec _a@(Exts.TypeSig _ ns t)
    -- XXXXXXXXXXXXXX: oh crap, we can't return a [Dec] from this class!
    = let xs = fmap (flip TH.SigD (toType t) . toName) ns
      in case xs of x:_ -> x; [] -> error "toDec: malformed TypeSig!"

  toDec (Exts.InlineConlikeSig _ act qn) = TH.PragmaD $
    TH.InlineP (toName qn) TH.Inline TH.ConLike (transAct act)
  toDec (Exts.InlineSig _ b act qn) = TH.PragmaD $
    TH.InlineP (toName qn) inline TH.FunLike (transAct act)
   where
    inline | b = TH.Inline | otherwise = TH.NoInline

#if MIN_VERSION_template_haskell(2,11,0)
  toDec (Exts.TypeFamDecl _ h sig inj)
    = TH.OpenTypeFamilyD $ TH.TypeFamilyHead (toName h)
                                       (toTyVars h)
                                       (maybe TH.NoSig TH.KindSig . toMaybeKind $ sig)
                                       (fmap toInjectivityAnn inj)
  toDec (Exts.DataFamDecl _ _ h sig)
    = TH.DataFamilyD (toName h) (toTyVars h) (toMaybeKind sig)
#else
  toDec (Exts.TypeFamDecl _ h sig inj)
    = TH.FamilyD TH.TypeFam (toName h) (toTyVars h) (toMaybeKind sig)
  toDec (Exts.DataFamDecl _ _ h sig)
    = TH.FamilyD TH.DataFam (toName h) (toTyVars h) (toMaybeKind sig)
#endif

  toDec _a@(Exts.FunBind _ mtchs)                           = hsMatchesToFunD mtchs
  toDec (Exts.PatBind _ p rhs bnds)                      = TH.ValD (toPat p)
                                                              (hsRhsToBody rhs)
                                                              (toDecs bnds)

  toDec i@(Exts.InstDecl _ (Just overlap) _ _) =
    noTH "toDec" (fmap (const ()) overlap, i)

  -- the 'vars' bit seems to be for: instance forall a. C (T a) where ...
  -- TH's own parser seems to flat-out ignore them, and honestly I can't see
  -- that it's obviously wrong to do so.
#if MIN_VERSION_template_haskell(2,11,0)
  toDec (Exts.InstDecl _ Nothing irule ids) = TH.InstanceD
    Nothing
    (toCxt irule)
    (toType irule)
    (toDecs ids)
#else
  toDec (Exts.InstDecl _ Nothing irule ids) = TH.InstanceD
    (toCxt irule)
    (toType irule)
    (toDecs ids)
#endif

  toDec (Exts.ClassDecl _ cxt h fds decls) = TH.ClassD
    (toCxt cxt)
    (toName h)
    (toTyVars h)
    (fmap toFunDep fds)
    (toDecs decls)
   where
    toFunDep (Exts.FunDep _ ls rs) = TH.FunDep (fmap toName ls) (fmap toName rs)

  toDec x = todo "toDec" x

instance ToMaybeKind (Exts.ResultSig l) where
    toMaybeKind (Exts.KindSig _ k)  = Just $ toKind k
    toMaybeKind (Exts.TyVarSig _ _) = Nothing

instance ToMaybeKind a => ToMaybeKind (Maybe a) where
    toMaybeKind Nothing  = Nothing
    toMaybeKind (Just a) = toMaybeKind a

#if MIN_VERSION_template_haskell(2,11,0)
instance ToInjectivityAnn (Exts.InjectivityInfo l) where
  toInjectivityAnn (Exts.InjectivityInfo _ n ns) = TH.InjectivityAnn (toName n) (fmap toName ns)
#endif

transAct :: Maybe (Exts.Activation l) -> TH.Phases
transAct Nothing                       = TH.AllPhases
transAct (Just (Exts.ActiveFrom _ n))  = TH.FromPhase n
transAct (Just (Exts.ActiveUntil _ n)) = TH.BeforePhase n

instance ToName (Exts.DeclHead l) where
  toName (Exts.DHead _ n)     = toName n
  toName (Exts.DHInfix _ _ n) = toName n
  toName (Exts.DHParen _ h)   = toName h
  toName (Exts.DHApp _ h _)   = toName h

instance ToTyVars (Exts.DeclHead l) where
  toTyVars (Exts.DHead _ _)       = []
  toTyVars (Exts.DHParen _ h)     = toTyVars h
  toTyVars (Exts.DHInfix _ tvb _) = [toTyVar tvb]
  toTyVars (Exts.DHApp _ h tvb)   = toTyVars h ++ [toTyVar tvb]

instance ToNames a => ToNames (Maybe a) where
  toNames Nothing  = []
  toNames (Just a) = toNames a

instance ToNames (Exts.Deriving l) where
#if MIN_VERSION_haskell_src_exts(1,20,0)
  toNames (Exts.Deriving _ _ irules) = concatMap toNames irules
#else
  toNames (Exts.Deriving _ irules)   = concatMap toNames irules
#endif

instance ToNames (Exts.InstRule l) where
  toNames (Exts.IParen _ irule)            = toNames irule
  toNames (Exts.IRule _ _mtvbs _mcxt mihd) = toNames mihd
instance ToNames (Exts.InstHead l) where
  toNames (Exts.IHCon _ n)     = [toName n]
  toNames (Exts.IHInfix _ _ n) = [toName n]
  toNames (Exts.IHParen _ h)   = toNames h
  toNames (Exts.IHApp _ h _)   = toNames h

instance ToCxt (Exts.InstRule l) where
  toCxt (Exts.IRule _ _ cxt _) = toCxt cxt
  toCxt (Exts.IParen _ irule)  = toCxt irule

instance ToCxt (Exts.Context l) where
  toCxt x = case x of
              Exts.CxEmpty _     -> []
              Exts.CxSingle _ x' -> [toPred x']
              Exts.CxTuple _ xs  -> fmap toPred xs

instance ToCxt a => ToCxt (Maybe a) where
    toCxt Nothing  = []
    toCxt (Just a) = toCxt a

instance ToType (Exts.InstRule l) where
    toType (Exts.IRule _ _ _ h)  = toType h
    toType (Exts.IParen _ irule) = toType irule

instance ToType (Exts.InstHead l) where
    toType (Exts.IHCon _ qn)       = toType qn
    toType (Exts.IHInfix _ typ qn) = TH.AppT (toType typ) (toType qn)
    toType (Exts.IHParen _ hd)     = toType hd
    toType (Exts.IHApp _ hd typ)   = TH.AppT (toType hd) (toType typ)

qualConDeclToCon :: Exts.QualConDecl l -> TH.Con
qualConDeclToCon (Exts.QualConDecl _ Nothing Nothing cdecl) = conDeclToCon cdecl
qualConDeclToCon (Exts.QualConDecl _ ns cxt cdecl) = TH.ForallC (toTyVars ns)
                                                    (toCxt cxt)
                                                    (conDeclToCon cdecl)

instance ToTyVars a => ToTyVars (Maybe a) where
  toTyVars Nothing  = []
  toTyVars (Just a) = toTyVars a

instance ToTyVars a => ToTyVars [a] where
  toTyVars = concatMap toTyVars

instance ToTyVars (Exts.TyVarBind l) where
  toTyVars tvb = [toTyVar tvb]

instance ToType (Exts.QName l) where
    toType = TH.ConT . toName

conDeclToCon :: Exts.ConDecl l -> TH.Con
conDeclToCon (Exts.ConDecl _ n tys)
  = TH.NormalC (toName n) (map toStrictType tys)
conDeclToCon (Exts.RecDecl _ n fieldDecls)
  = TH.RecC (toName n) (concatMap convField fieldDecls)
  where
    convField :: Exts.FieldDecl l -> [TH.VarStrictType]
    convField (Exts.FieldDecl _ ns t) =
      let (strict, ty) = toStrictType t
      in map (\n' -> (toName n', strict, ty)) ns
conDeclToCon h = todo "conDeclToCon" h
-- TODO
-- (Exts.InfixConDecl _ _ _ _)


hsMatchesToFunD :: [Exts.Match l] -> TH.Dec
hsMatchesToFunD [] = TH.FunD (TH.mkName []) []   -- errorish
hsMatchesToFunD xs@(Exts.Match _ n _ _ _ : _) = TH.FunD (toName n) (fmap hsMatchToClause xs)
hsMatchesToFunD xs@(Exts.InfixMatch _ _ n _ _ _ : _) = TH.FunD (toName n) (fmap hsMatchToClause xs)


hsMatchToClause :: Exts.Match l -> TH.Clause
hsMatchToClause (Exts.Match _ _ ps rhs bnds) = TH.Clause
                                                (fmap toPat ps)
                                                (hsRhsToBody rhs)
                                                (toDecs bnds)
hsMatchToClause (Exts.InfixMatch _ p _ ps rhs bnds) = TH.Clause
                                                        (fmap toPat (p:ps))
                                                        (hsRhsToBody rhs)
                                                        (toDecs bnds)



hsRhsToBody :: Exts.Rhs l -> TH.Body
hsRhsToBody (Exts.UnGuardedRhs _ e) = TH.NormalB (toExp e)
hsRhsToBody (Exts.GuardedRhss _ hsgrhs) =
  let fromGuardedB (TH.GuardedB a) = a
      fromGuardedB h               = todo "fromGuardedB" [h]
      -- TODO: (NormalB _)
  in TH.GuardedB . concat
     . fmap (fromGuardedB . hsGuardedRhsToBody)
     $ hsgrhs


hsGuardedRhsToBody :: Exts.GuardedRhs l -> TH.Body
hsGuardedRhsToBody (Exts.GuardedRhs _ [] e)  = TH.NormalB (toExp e)
hsGuardedRhsToBody (Exts.GuardedRhs _ [s] e) = TH.GuardedB [(hsStmtToGuard s, toExp e)]
hsGuardedRhsToBody (Exts.GuardedRhs _ ss e)  = let ss' = fmap hsStmtToGuard ss
                                                   (pgs,ngs) = unzip [(p,n)
                                                                     | (TH.PatG p) <- ss'
                                                                     , n@(TH.NormalG _) <- ss']
                                                   e' = toExp e
                                                   patg = TH.PatG (concat pgs)
                                               in TH.GuardedB $ (patg,e') : zip ngs (repeat e')



hsStmtToGuard :: Exts.Stmt l -> TH.Guard
hsStmtToGuard (Exts.Generator _ p e) = TH.PatG [TH.BindS (toPat p) (toExp e)]
hsStmtToGuard (Exts.Qualifier _ e)   = TH.NormalG (toExp e)
hsStmtToGuard (Exts.LetStmt _ bs)    = TH.PatG [TH.LetS (toDecs bs)]
hsStmtToGuard h                      = todo "hsStmtToGuard" h
-- TODO
-- (Exts.RecStmt _ _)


-----------------------------------------------------------------------------

-- * ToDecs InstDecl
instance ToDecs (Exts.InstDecl l) where
  toDecs (Exts.InsDecl _ decl) = toDecs decl
  toDecs d                     = todo "toDec" d

-- * ToDecs HsDecl HsBinds

instance ToDecs (Exts.Decl l) where
  toDecs _a@(Exts.TypeSig _ ns t)
    -- TODO: fixforall as before?
    -- = let xs = fmap (flip SigD (fixForall $ toType t) . toName) ns
    = let xs = fmap (flip TH.SigD (toType t) . toName) ns
       in xs

  toDecs (Exts.InfixDecl l assoc Nothing ops) =
      toDecs (Exts.InfixDecl l assoc (Just 9) ops)
  toDecs (Exts.InfixDecl _ assoc (Just fixity) ops) =
    map (\op -> TH.InfixD (TH.Fixity fixity dir) (toName op)) ops
   where
    dir = case assoc of
      Exts.AssocNone _  -> TH.InfixN
      Exts.AssocLeft _  -> TH.InfixL
      Exts.AssocRight _ -> TH.InfixR

  toDecs a = [toDec a]


-- TODO: see aboe re: fixforall
-- fixForall t@(TH.ForallT _ _ _) = t
-- fixForall t = case vs of
--   [] -> t
--   _  -> TH.ForallT vs [] t
--   where vs = collectVars t
-- collectVars e = case e of
--   VarT n -> [PlainTV n]
--   AppT t1 t2 -> nub $ collectVars t1 ++ collectVars t2
--   TH.ForallT ns _ t -> collectVars t \\ ns
--   _          -> []

instance ToDecs a => ToDecs [a] where
  toDecs a = concatMap toDecs a

-----------------------------------------------------------------------------

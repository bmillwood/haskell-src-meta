{-# LANGUAGE CPP, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}

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

import Data.Char (ord)
import Data.Typeable
import Data.List (foldl', nub, (\\))
import Language.Haskell.TH.Syntax
import qualified Language.Haskell.Exts.Syntax as Hs

-----------------------------------------------------------------------------


class ToName a where toName :: a -> Name
class ToLit  a where toLit  :: a -> Lit
class ToType a where toType :: a -> Type
class ToPat  a where toPat  :: a -> Pat
class ToExp  a where toExp  :: a -> Exp
class ToDecs a where toDecs :: a -> [Dec]
class ToDec  a where toDec  :: a -> Dec
class ToStmt a where toStmt :: a -> Stmt
class ToLoc  a where toLoc  :: a -> Loc

-- for error messages
moduleName = "Language.Haskell.Meta.Syntax.Translate"

-- When to use each of these isn't always clear: prefer 'todo' if unsure.
noTH :: Show e => String -> e -> a
noTH fun thing = error . concat $ [moduleName, ".", fun,
  ": no TH representation for: ", show thing]

todo :: Show e => String -> e -> a
todo fun thing = error . concat $ [moduleName, ".", fun,
  ": not implemented: ", show thing]

nonsense :: Show e => String -> String -> e -> a
nonsense fun inparticular thing = error . concat $ [moduleName, ".", fun,
  ": nonsensical: ", inparticular, ": ", show thing]

-----------------------------------------------------------------------------


instance ToExp Lit where
  toExp = LitE
instance (ToExp a) => ToExp [a] where
  toExp = ListE . fmap toExp
instance (ToExp a, ToExp b) => ToExp (a,b) where
  toExp (a,b) = TupE [toExp a, toExp b]
instance (ToExp a, ToExp b, ToExp c) => ToExp (a,b,c) where
  toExp (a,b,c) = TupE [toExp a, toExp b, toExp c]
instance (ToExp a, ToExp b, ToExp c, ToExp d) => ToExp (a,b,c,d) where
  toExp (a,b,c,d) = TupE [toExp a, toExp b, toExp c, toExp d]


instance ToPat Lit where
  toPat = LitP
instance (ToPat a) => ToPat [a] where
  toPat = ListP . fmap toPat
instance (ToPat a, ToPat b) => ToPat (a,b) where
  toPat (a,b) = TupP [toPat a, toPat b]
instance (ToPat a, ToPat b, ToPat c) => ToPat (a,b,c) where
  toPat (a,b,c) = TupP [toPat a, toPat b, toPat c]
instance (ToPat a, ToPat b, ToPat c, ToPat d) => ToPat (a,b,c,d) where
  toPat (a,b,c,d) = TupP [toPat a, toPat b, toPat c, toPat d]


instance ToLit Char where
  toLit = CharL
instance ToLit String where
  toLit = StringL
instance ToLit Integer where
  toLit = IntegerL
instance ToLit Int where
  toLit = IntegerL . toInteger
instance ToLit Float where
  toLit = RationalL . toRational
instance ToLit Double where
  toLit = RationalL . toRational


-----------------------------------------------------------------------------


-- * ToName {String,HsName,Module,HsSpecialCon,HsQName}


instance ToName String where
  toName = mkName

instance ToName Hs.Name where
  toName (Hs.Ident s) = toName s
  toName (Hs.Symbol s) = toName s

instance ToName Hs.Module where
  toName (Hs.Module _ (Hs.ModuleName s) _ _ _ _ _) = toName s


instance ToName Hs.SpecialCon where
  toName Hs.UnitCon = '()
  toName Hs.ListCon = '[]
  toName Hs.FunCon  = ''(->)
  toName (Hs.TupleCon _ n)
    | n<2 = '()
    | otherwise =
      let x = maybe [] (++".") (nameModule '(,))
      in mkName . concat $ x : ["(",replicate (n-1) ',',")"]
  toName Hs.Cons    = '(:)


instance ToName Hs.QName where
--  toName (Hs.Qual (Hs.Module []) n) = toName n
  toName (Hs.Qual (Hs.ModuleName []) n) = toName n
  toName (Hs.Qual (Hs.ModuleName m) n) =
    let m' = show . toName $ m
        n' = show . toName $ n
    in toName . concat $ [m',".",n']
  toName (Hs.UnQual n) = toName n
  toName (Hs.Special s) = toName s


instance ToName Hs.Op where
  toName (Hs.VarOp n) = toName n
  toName (Hs.ConOp n) = toName n


-----------------------------------------------------------------------------

-- * ToLit HsLiteral


instance ToLit Hs.Literal where
  toLit (Hs.Char a) = CharL a
  toLit (Hs.String a) = StringL a
  toLit (Hs.Int a) = IntegerL a
  toLit (Hs.Frac a) = RationalL a
  toLit l@Hs.PrimChar{} = noTH "toLit" l
#if MIN_VERSION_template_haskell(2,8,0)
  toLit (Hs.PrimString a) = StringPrimL (map toWord8 a)
   where
    toWord8 = fromIntegral . ord
#elif MIN_VERSION_template_haskell(2,5,0)
  toLit (Hs.PrimString a) = StringPrimL a
#else
  toLit l@Hs.PrimString{} = noTH "toLit" l
#endif
  toLit (Hs.PrimInt a) = IntPrimL a
  toLit (Hs.PrimFloat a) = FloatPrimL a
  toLit (Hs.PrimDouble a) = DoublePrimL a
#if MIN_VERSION_template_haskell(2,4,0)
  toLit (Hs.PrimWord a) = WordPrimL a
#endif


-----------------------------------------------------------------------------

-- * ToPat HsPat


instance ToPat Hs.Pat where
  toPat (Hs.PVar n)
    = VarP (toName n)
  toPat (Hs.PLit l)
    = LitP (toLit l)
  toPat (Hs.PNeg (Hs.PLit l)) = LitP $ case toLit l of
    IntegerL z -> IntegerL (negate z)
    RationalL q -> RationalL (negate q)
    IntPrimL z' -> IntPrimL (negate z')
    FloatPrimL r' -> FloatPrimL (negate r')
    DoublePrimL r'' -> DoublePrimL (negate r'')
    _ -> nonsense "toPat" "negating wrong kind of literal" l
  toPat (Hs.PNeg p) = nonsense "toPat" "negating non-literal" p
#if MIN_VERSION_template_haskell(2,7,0)
  toPat (Hs.PInfixApp p n q) = UInfixP (toPat p) (toName n) (toPat q)    
#else
  toPat (Hs.PInfixApp p n q)= InfixP (toPat p) (toName n) (toPat q)
#endif
  toPat (Hs.PApp n ps) = ConP (toName n) (fmap toPat ps)
  toPat (Hs.PTuple ps) = TupP (fmap toPat ps)
  toPat (Hs.PList ps) = ListP (fmap toPat ps)
#if MIN_VERSION_template_haskell(2,7,0)  
  toPat (Hs.PParen p) = ParensP (toPat p)  
#else
  toPat (Hs.PParen p) = toPat p
#endif
  toPat (Hs.PRec n pfs) = let toFieldPat (Hs.PFieldPat n p) = (toName n, toPat p)
                          in RecP (toName n) (fmap toFieldPat pfs)
  toPat (Hs.PAsPat n p) = AsP (toName n) (toPat p)
  toPat (Hs.PWildCard) = WildP
  toPat (Hs.PIrrPat p) = TildeP (toPat p)
  toPat (Hs.PatTypeSig _ p t) = SigP (toPat p) (toType t)
#if MIN_VERSION_template_haskell(2,5,0)
  toPat (Hs.PViewPat e p) = ViewP (toExp e) (toPat p)
#endif
  -- regular pattern
  toPat p@Hs.PRPat{} = noTH "toPat" p
  -- XML stuff
  toPat p@Hs.PXTag{} = noTH "toPat" p
  toPat p@Hs.PXETag{} = noTH "toPat" p
  toPat p@Hs.PXPcdata{} = noTH "toPat" p
  toPat p@Hs.PXPatTag{} = noTH "toPat" p
#if MIN_VERSION_template_haskell(2,4,0)
  toPat (Hs.PBangPat p) = BangP (toPat p)
#endif /* MIN_VERSION_template_haskell(2,4,0) */
  toPat p = todo "toPat" p

-----------------------------------------------------------------------------

-- * ToExp HsExp

instance ToExp Hs.QOp where
  toExp (Hs.QVarOp n) = VarE (toName n)
  toExp (Hs.QConOp n) = ConE (toName n)

toFieldExp :: Hs.FieldUpdate -> FieldExp
toFieldExp (Hs.FieldUpdate n e) = (toName n, toExp e)




instance ToExp Hs.Exp where
  toExp (Hs.Var n)                 = VarE (toName n)
  toExp (Hs.Con n)                 = ConE (toName n)
  toExp (Hs.Lit l)                 = LitE (toLit l)
#if MIN_VERSION_template_haskell(2,7,0)
  toExp (Hs.InfixApp e o f)        = UInfixE (toExp e) (toExp o) (toExp f)
#else
  toExp (Hs.InfixApp e o f)        = InfixE (Just . toExp $ e) (toExp o) (Just . toExp $ f)
#endif
  toExp (Hs.LeftSection e o)       = InfixE (Just . toExp $ e) (toExp o) Nothing
  toExp (Hs.RightSection o f)      = InfixE Nothing (toExp o) (Just . toExp $ f)
  toExp (Hs.App e f)               = AppE (toExp e) (toExp f)
  toExp (Hs.NegApp e)              = AppE (VarE 'negate) (toExp e)
  toExp (Hs.Lambda _ ps e)         = LamE (fmap toPat ps) (toExp e)
  toExp (Hs.Let bs e)              = LetE (hsBindsToDecs bs) (toExp e)
  toExp (Hs.If a b c)              = CondE (toExp a) (toExp b) (toExp c)
  toExp (Hs.Do ss)                 = DoE (map toStmt ss)
  toExp e@(Hs.MDo _)               = noTH "toExp" e
  toExp (Hs.Tuple xs)              = TupE (fmap toExp xs)
  toExp (Hs.List xs)               = ListE (fmap toExp xs)
#if MIN_VERSION_template_haskell(2,7,0)
  toExp (Hs.Paren e)               = ParensE (toExp e)
#else
  toExp (Hs.Paren e)               = toExp e
#endif
  toExp (Hs.RecConstr n xs)        = RecConE (toName n) (fmap toFieldExp xs)
  toExp (Hs.RecUpdate e xs)        = RecUpdE (toExp e) (fmap toFieldExp xs)
  toExp (Hs.EnumFrom e)            = ArithSeqE $ FromR (toExp e)
  toExp (Hs.EnumFromTo e f)        = ArithSeqE $ FromToR (toExp e) (toExp f)
  toExp (Hs.EnumFromThen e f)      = ArithSeqE $ FromThenR (toExp e) (toExp f)
  toExp (Hs.EnumFromThenTo e f g)  = ArithSeqE $ FromThenToR (toExp e) (toExp f) (toExp g)
  toExp (Hs.ExpTypeSig _ e t)      = SigE (toExp e) (toType t)
  toExp (Hs.ListComp e ss)         = CompE $ map convert ss ++ [NoBindS (toExp e)]
   where
    convert (Hs.QualStmt st) = toStmt st
    convert s = noTH "toExp ListComp" s
  toExp (Hs.Case e alts) = CaseE (toExp e) (map toMatch alts)
  toExp e = todo "toExp" e


toMatch :: Hs.Alt -> Match
toMatch (Hs.Alt _ p galts ds) = Match (toPat p) (toBody galts) (toDecs ds)

toBody :: Hs.GuardedAlts -> Body
toBody (Hs.UnGuardedAlt  e) = NormalB $ toExp e
toBody (Hs.GuardedAlts alts) = GuardedB $ do
  Hs.GuardedAlt _ stmts e <- alts
  let
    g = case map toStmt stmts of
      [NoBindS x] -> NormalG x
      xs -> PatG xs
  return (g, toExp e)

toGuard (Hs.GuardedAlt _ ([Hs.Qualifier e1]) e2) = (NormalG $ toExp e1,toExp e2)

-----------------------------------------------------------------------------

-- * ToLoc SrcLoc

instance ToLoc Hs.SrcLoc where
  toLoc (Hs.SrcLoc fn l c) =
    Loc fn [] [] (l,c) (-1,-1)

-----------------------------------------------------------------------------

-- * ToType HsType

instance ToName Hs.TyVarBind where
  toName (Hs.KindedVar n _) = toName n
  toName (Hs.UnkindedVar n) = toName n

instance ToName Name where
  toName = id

#if MIN_VERSION_template_haskell(2,4,0)
instance ToName TyVarBndr where
  toName (PlainTV n) = n
  toName (KindedTV n _) = n
#endif /* !MIN_VERSION_template_haskell(2,4,0) */

#if MIN_VERSION_template_haskell(2,4,0)
#if MIN_VERSION_template_haskell(2,8,0)

instance ToType Hs.Kind where
  toType Hs.KindStar = StarT
  toType (Hs.KindFn k1 k2) = toType k1 .->. toType k2
  toType (Hs.KindParen kp) = toType kp
  toType k@Hs.KindBang = noTH "toKind" k
  toType (Hs.KindVar n) = VarT (toName n)

toKind :: Hs.Kind -> Kind
toKind = toType

#else

toKind :: Hs.Kind -> Kind
toKind Hs.KindStar = StarK
toKind (Hs.KindFn k1 k2) = ArrowK (toKind k1) (toKind k2)
toKind (Hs.KindParen kp) = toKind kp
toKind k@Hs.KindBang = noTH "toKind" k
toKind k@Hs.KindVar{} = noTH "toKind" k

#endif /* !MIN_VERSION_template_haskell(2,8,0) */
#endif /* !MIN_VERSION_template_haskell(2,4,0) */

#if MIN_VERSION_template_haskell(2,4,0)
toTyVar :: Hs.TyVarBind -> TyVarBndr
toTyVar (Hs.KindedVar n k) = KindedTV (toName n) (toKind k)
toTyVar (Hs.UnkindedVar n) = PlainTV (toName n)
#else /* !MIN_VERSION_template_haskell(2,4,0) */
toTyVar :: Hs.TyVarBind -> Name
toTyVar (Hs.KindedVar n _) = toName n
toTyVar (Hs.UnkindedVar n) = toName n
#endif /* !MIN_VERSION_template_haskell(2,4,0) */

instance ToType Hs.Type where
  toType (Hs.TyForall tvbM cxt t) = ForallT (maybe [] (fmap toTyVar) tvbM) (toCxt cxt) (toType t)
  toType (Hs.TyFun a b) = toType a .->. toType b
  toType (Hs.TyList t) = ListT `AppT` toType t
  toType (Hs.TyTuple b ts) = foldAppT (tuple . length $ ts) (fmap toType ts)
   where
    tuple = case b of
      Hs.Boxed -> TupleT
#if MIN_VERSION_template_haskell(2,6,0)
      Hs.Unboxed -> UnboxedTupleT
#else
      Hs.Unboxed -> noTH "toType TyTuple" (Hs.TyTuple b ts)
#endif
  toType (Hs.TyApp a b) = AppT (toType a) (toType b)
  toType (Hs.TyVar n) = VarT (toName n)
  toType (Hs.TyCon qn) = ConT (toName qn)
  toType (Hs.TyParen t) = toType t
  -- XXX: need to wrap the name in parens!
  toType (Hs.TyInfix a o b) = AppT (AppT (ConT (toName o)) (toType a)) (toType b)
  toType (Hs.TyKind t k) = SigT (toType t) (toKind k)




(.->.) :: Type -> Type -> Type
a .->. b = AppT (AppT ArrowT a) b

toCxt :: Hs.Context -> Cxt
toCxt = fmap toPred
 where
#if MIN_VERSION_template_haskell(2,4,0)
  toPred (Hs.ClassA n ts) = ClassP (toName n) (fmap toType ts)
  toPred (Hs.InfixA t1 n t2) = ClassP (toName n) (fmap toType [t1, t2])
  toPred (Hs.EqualP t1 t2) = EqualP (toType t1) (toType t2)
  toPred a@Hs.IParam{} = noTH "toCxt" a
#else
  toPred (Hs.ClassA n ts) = foldAppT (ConT (toName n)) (fmap toType ts)
  toPred (Hs.InfixA t1 n t2) = foldAppT (ConT (toName n)) (fmap toType [t1, t2])
  toPred a@Hs.EqualP{} = noTH "toCxt" a
  toPred a@Hs.IParam{} = noTH "toCxt" a
#endif

foldAppT :: Type -> [Type] -> Type
foldAppT t ts = foldl' AppT t ts

-----------------------------------------------------------------------------

-- * ToStmt HsStmt

instance ToStmt Hs.Stmt where
  toStmt (Hs.Generator _ p e)  = BindS (toPat p) (toExp e)
  toStmt (Hs.Qualifier e)      = NoBindS (toExp e)
  toStmt a@(Hs.LetStmt bnds)   = LetS (hsBindsToDecs bnds)


-----------------------------------------------------------------------------

-- * ToDec HsDecl

hsBindsToDecs :: Hs.Binds -> [Dec]
hsBindsToDecs (Hs.BDecls ds) = fmap toDec ds
hsBindsToDecs a@Hs.IPBinds{} = noTH "hsBindsToDecs" a


hsBangTypeToStrictType :: Hs.BangType -> (Strict, Type)
hsBangTypeToStrictType (Hs.BangedTy t)   = (IsStrict, toType t)
hsBangTypeToStrictType (Hs.UnBangedTy t) = (NotStrict, toType t)


instance ToDec Hs.Decl where
  toDec (Hs.TypeDecl _ n ns t)
    = TySynD (toName n) (fmap toTyVar ns) (toType t)

  toDec a@(Hs.DataDecl  _ dOrN cxt n ns qcds qns)
    = case dOrN of
        Hs.DataType -> DataD (toCxt cxt)
                             (toName n)
                             (fmap toTyVar ns)
                             (fmap qualConDeclToCon qcds)
                             (fmap (toName . fst) qns)
        Hs.NewType  -> let qcd = case qcds of
                                  [x] -> x
                                  _   -> nonsense "toDec" ("newtype with " ++
                                           "wrong number of constructors") a
                        in NewtypeD (toCxt cxt)
                                    (toName n)
                                    (fmap toTyVar ns)
                                    (qualConDeclToCon qcd)
                                    (fmap (toName . fst) qns)

  -- This type-signature conversion is just wrong. 
  -- Type variables need to be dealt with. /Jonas
  toDec a@(Hs.TypeSig _ ns t)
    -- XXXXXXXXXXXXXX: oh crap, we can't return a [Dec] from this class!
    = let xs = fmap (flip SigD (toType t) . toName) ns
      in case xs of x:_ -> x; [] -> error "toDec: malformed TypeSig!"

#if MIN_VERSION_template_haskell(2,4,0)
#if MIN_VERSION_template_haskell(2,8,0)

  toDec (Hs.InlineConlikeSig _ act qn) = PragmaD $
    InlineP (toName qn) Inline ConLike (transAct act)
  toDec (Hs.InlineSig _ b act qn) = PragmaD $
    InlineP (toName qn) inline FunLike (transAct act)
   where
    inline | b = Inline | otherwise = NoInline

#else

  toDec (Hs.InlineConlikeSig _ act id)                 = PragmaD $ 
    InlineP (toName id) (InlineSpec True True $ transAct act)
  toDec (Hs.InlineSig _ b act id)                      = PragmaD $ 
    InlineP (toName id) (InlineSpec b False $ transAct act)

#endif /* MIN_VERSION_template_haskell(2,8,0) */

  toDec (Hs.TypeFamDecl _ n ns k)
    = FamilyD TypeFam (toName n) (fmap toTyVar ns) (fmap toKind k)

  -- TODO: do something with context?
  toDec (Hs.DataFamDecl _ _ n ns k)
    = FamilyD DataFam (toName n) (fmap toTyVar ns) (fmap toKind k)

#endif /* MIN_VERSION_template_haskell(2,4,0) */

  toDec a@(Hs.FunBind mtchs)                           = hsMatchesToFunD mtchs
  toDec (Hs.PatBind _ p tM rhs bnds)                   = ValD ((maybe id
                                                                      (flip SigP . toType)
                                                                      tM) (toPat p))
                                                              (hsRhsToBody rhs)
                                                              (hsBindsToDecs bnds)

  toDec (Hs.InstDecl _ cxt qname ts ids) = InstanceD 
    (toCxt cxt) 
    (foldl AppT (ConT (toName qname)) (map toType ts))
    (toDecs ids)

  toDec (Hs.ClassDecl _ cxt name ts fds decls) = ClassD
    (toCxt cxt)
    (toName name)
    (fmap toTyVar ts)
    (fmap toFunDep fds)
    (fmap classDeclToDec decls)
   where
    classDeclToDec cd = case cd of
      (Hs.ClsDecl d) -> toDec d
      x -> todo "classDecl" x
    toFunDep (Hs.FunDep ls rs) = FunDep (fmap toName ls) (fmap toName rs)

  toDec x = todo "toDec" x

#if MIN_VERSION_template_haskell(2,8,0)
transAct :: Hs.Activation -> Phases
transAct Hs.AlwaysActive = AllPhases
transAct (Hs.ActiveFrom n) = FromPhase n
transAct (Hs.ActiveUntil n) = BeforePhase n
#else
transAct act = case act of
  Hs.AlwaysActive    -> Nothing
  Hs.ActiveFrom n    -> Just (True,n)
  Hs.ActiveUntil n   -> Just (False,n)
#endif


qualConDeclToCon :: Hs.QualConDecl -> Con
qualConDeclToCon (Hs.QualConDecl _ [] [] cdecl) = conDeclToCon cdecl
qualConDeclToCon (Hs.QualConDecl _ ns cxt cdecl) = ForallC (fmap toTyVar ns)
                                                    (toCxt cxt)
                                                    (conDeclToCon cdecl)
conDeclToCon :: Hs.ConDecl -> Con
conDeclToCon (Hs.ConDecl n tys)
  = NormalC (toName n) (fmap bangToStrictType tys)
conDeclToCon (Hs.RecDecl n lbls)
  = RecC (toName n) (concatMap (uncurry bangToVarStrictTypes) lbls)


bangToVarStrictTypes :: [Hs.Name] -> Hs.BangType -> [VarStrictType]
bangToVarStrictTypes ns t = let (a,b) = bangToStrictType t
                            in fmap (\n->(toName n,a,b)) ns

bangToStrictType :: Hs.BangType -> StrictType
bangToStrictType (Hs.BangedTy   t) = (IsStrict, toType t)
bangToStrictType (Hs.UnBangedTy t) = (NotStrict, toType t)
bangToStrictType (Hs.UnpackedTy t) = (IsStrict, toType t)


hsMatchesToFunD :: [Hs.Match] -> Dec
hsMatchesToFunD [] = FunD (mkName []) []   -- errorish
hsMatchesToFunD xs@(Hs.Match _ n _ _ _ _:_) = FunD (toName n) (fmap hsMatchToClause xs)


hsMatchToClause :: Hs.Match -> Clause
hsMatchToClause (Hs.Match _ _ ps _ rhs bnds) = Clause
                                                (fmap toPat ps)
                                                (hsRhsToBody rhs)
                                                (hsBindsToDecs bnds)



hsRhsToBody :: Hs.Rhs -> Body
hsRhsToBody (Hs.UnGuardedRhs e) = NormalB (toExp e)
hsRhsToBody (Hs.GuardedRhss hsgrhs) = let fromGuardedB (GuardedB a) = a
                                      in GuardedB . concat
                                          . fmap (fromGuardedB . hsGuardedRhsToBody)
                                              $ hsgrhs



hsGuardedRhsToBody :: Hs.GuardedRhs -> Body
hsGuardedRhsToBody (Hs.GuardedRhs _ [] e)  = NormalB (toExp e)
hsGuardedRhsToBody (Hs.GuardedRhs _ [s] e) = GuardedB [(hsStmtToGuard s, toExp e)]
hsGuardedRhsToBody (Hs.GuardedRhs _ ss e)  = let ss' = fmap hsStmtToGuard ss
                                                 (pgs,ngs) = unzip [(p,n)
                                                               | (PatG p) <- ss'
                                                               , n@(NormalG _) <- ss']
                                                 e' = toExp e
                                                 patg = PatG (concat pgs)
                                            in GuardedB $ (patg,e') : zip ngs (repeat e')



hsStmtToGuard :: Hs.Stmt -> Guard
hsStmtToGuard (Hs.Generator _ p e) = PatG [BindS (toPat p) (toExp e)]
hsStmtToGuard (Hs.Qualifier e)     = NormalG (toExp e)
hsStmtToGuard (Hs.LetStmt bs)      = PatG [LetS (hsBindsToDecs bs)]


-----------------------------------------------------------------------------

-- * ToDecs InstDecl
instance ToDecs Hs.InstDecl where
  toDecs (Hs.InsDecl decl) = toDecs decl
  toDecs d              = todo "toDec" d

-- * ToDecs HsDecl HsBinds

instance ToDecs Hs.Decl where
  toDecs a@(Hs.TypeSig _ ns t)
    = let xs = fmap (flip SigD (fixForall $ toType t) . toName) ns
       in xs

#if MIN_VERSION_template_haskell(2,8,0)
  toDecs (Hs.InfixDecl _ assoc fixity ops) =
    map (\op -> InfixD (Fixity fixity dir) (toName op)) ops
   where
    dir = case assoc of
      Hs.AssocNone -> InfixN
      Hs.AssocLeft -> InfixL
      Hs.AssocRight -> InfixR
#endif

  toDecs a = [toDec a]

collectVars e = case e of
#if MIN_VERSION_template_haskell(2,4,0)
  VarT n -> [PlainTV n]
#else /* !MIN_VERSION_template_haskell(2,4,0) */
  VarT n -> [n]
#endif /* !MIN_VERSION_template_haskell(2,4,0) */
  AppT t1 t2 -> nub $ collectVars t1 ++ collectVars t2
  ForallT ns _ t -> collectVars t \\ ns
  _          -> []

fixForall t = case vs of
  [] -> t
  _  -> ForallT vs [] t
  where vs = collectVars t

instance ToDecs a => ToDecs [a] where
  toDecs a = concatMap toDecs a

instance ToDecs Hs.Binds where
  toDecs (Hs.BDecls ds) = toDecs ds


-----------------------------------------------------------------------------

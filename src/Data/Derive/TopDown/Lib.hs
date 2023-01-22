{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP, TypeFamilies,GADTs #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module Data.Derive.TopDown.Lib (
  isInstance'
 , generateClassContext
 , getTyVarCons,getTVBName
 , getCompositeTypeNames
 , ClassName
 , TypeName
 , decType
 , DecTyType(..)
 , getTypeConstructor
 , isTypeFamily
 ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Data.Generics
import GHC.Exts
import Language.Haskell.TH.ExpandSyns (expandSynsWith,noWarnTypeFamilies)
import Data.List (nub,intersect,foldr1)
import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative
import Control.Monad
import Data.List
import Data.Typeable
import Data.Data
import Debug.Trace

noWarnexpandSynsWith :: Type -> Q Type
noWarnexpandSynsWith = expandSynsWith noWarnTypeFamilies
-- `isInstance` in template library does not work with polymorphic types.
-- The follwoing is an isInstance function with polymorphic type replaced by Any in GHC.Exts so that it can work with polymorphic type.
-- This is inspired by Ryan Scott
-- see https://ghc.haskell.org/trac/ghc/ticket/10607
-- isInstance will not work with Typeable.
-- See https://ghc.haskell.org/trac/ghc/ticket/11251

-- For fixing deriving Typeable problem, I use Data type calss to replace Typeable since the are always pairing with each other.
-- So if the data type is already an instance of Typeable and not an instance of Data, this might not work.
isInstance' :: Name -> [Type] -> Q Bool
isInstance' className tys =
               if className == ''Typeable
                then
                 isInstance' ''Data tys
                else
                 isInstance className (map (removeExplicitForAllTrans. replacePolyTypeTrans) tys)

replacePolyType :: Type -> Type
replacePolyType (VarT t) = ConT ''Any
replacePolyType x = x

replacePolyTypeTrans = everywhere (mkT replacePolyType)

removeExplicitForAll :: Type -> Type
removeExplicitForAll (ForallT _ _ t) = t
removeExplicitForAll t = t

removeExplicitForAllTrans :: Type -> Type
removeExplicitForAllTrans = everywhere (mkT removeExplicitForAll)

getVarName :: Type -> [Name]
getVarName (VarT n) = [n]
getVarName _ = []

getAllVarNames :: Type -> [Name]
getAllVarNames = everything (++) (mkQ [] getVarName)

isLeftMostAppTTypeFamily :: Type -> Q Bool
isLeftMostAppTTypeFamily (ConT n) = isTypeFamily n
isLeftMostAppTTypeFamily t@(AppT t1 t2) = isLeftMostAppTTypeFamily t1
isLeftMostAppTTypeFamily _ = return False

getTypeFamilyType :: Type -> Q ([Type], Bool)
getTypeFamilyType ty = do 
                  isTF <- isLeftMostAppTTypeFamily ty
                  if isTF 
                    then return ([ty], True)
                    else return ([], False)

everythingMBut :: forall r m. Monad m => (m r -> m r -> m r)
                          -> (forall a. Data a => a -> m (r, Bool)) 
                          -> (forall a. Data a => a -> m r)
everythingMBut k f = go
      where
            go :: forall a. Data a => a -> m r
            go x = do 
                (res, stop) <- f x
                if stop 
                    then return res
                    else do 
                      let ls = gmapQ go x :: [m r]
                      foldl' k (return res) ls

getAllTypeFamilyTypes :: Data a => a -> Q [Type]
getAllTypeFamilyTypes = everythingMBut (liftA2 (++)) (mkQ (return ([], False)) getTypeFamilyType)

getAllTypeFamilyTypesFromName :: Name -> Q [Type]
getAllTypeFamilyTypesFromName nm = reify nm 
                               >>= everywhereM
                                     (mkM $ noWarnexpandSynsWith) 
                               >>= getAllTypeFamilyTypes
                               >>= return.nub

isLeftMostAppTypeVar :: Type -> Bool
isLeftMostAppTypeVar (AppT (VarT n) t2) = True
isLeftMostAppTypeVar (AppT t1 t2) = isLeftMostAppTypeVar t1
isLeftMostAppTypeVar _ = False

getTypeVarAppTypes :: Type -> ([Type], Bool)
getTypeVarAppTypes t = if isLeftMostAppTypeVar t
                          then ([t], True)
                          else ([], False)

getAllTypeVarAppTypes :: Data a => a -> [Type]
getAllTypeVarAppTypes = everythingBut (++) (mkQ ([], False) getTypeVarAppTypes)

test :: Name -> Q [Type]
test t = do
   a <- reify t
   return $ getAllTypeVarAppTypes a

-- When a type variable is both representational and nominal,
-- we still need to put it in the context
getRepresentionalTypeVar :: Type -> Q ([Type], Bool)
getRepresentionalTypeVar v@(VarT n) = return ([v], True)
getRepresentionalTypeVar t = do
                        isTF <- isLeftMostAppTTypeFamily t
                        if isTF || isLeftMostAppTypeVar t
                          then return ([], True)
                          else return ([], False)

getAllRepresentionalTypeVar :: Data a => a -> Q [Type]
getAllRepresentionalTypeVar = everythingMBut (liftA2 (++)) 
                                             (mkQ (return ([], False)) 
                                                  getRepresentionalTypeVar)

-- In the following case, a is representional.
-- data P a = P (F a) a
-- type family F x
getAllRepresentionalTypeVarFromName :: Name -> Q [Type]
getAllRepresentionalTypeVarFromName nm = do 
                                  info <- reify nm
                                  expandInfo <- everywhereM (mkM $ noWarnexpandSynsWith) info
                                  cons <- return $ listify (\(x :: Con) -> True) expandInfo
                                  vars <- getAllRepresentionalTypeVar cons
                                  return $ nub vars

#if __GLASGOW_HASKELL__ > 810
type TypeVarBind = TyVarBndr ()
#else
type TypeVarBind = TyVarBndr
#endif

constructorTypesVars :: [(Name, Role)] -> Type -> [Type]
-- get all free variablein a forall type expression.
#if __GLASGOW_HASKELL__ > 810
constructorTypesVars n2r  f@(ForallT tvbs _ t) = let scopedVarNames = map (getTVBName.voidTyVarBndrFlag) tvbs in
                                              filter (\x -> null $ intersect (getAllVarNames x) scopedVarNames)
                                              (constructorTypesVars n2r t)
#else
constructorTypesVars n2r  f@(ForallT tvbs _ t) = let scopedVarNames = map getTVBName tvbs in
                                              filter (\x -> null $ intersect (getAllVarNames x) scopedVarNames)
                                              (constructorTypesVars n2r t)

#endif

constructorTypesVars n2r a@(AppT (VarT tvn) t2) = [a]
constructorTypesVars n2r  c@(AppT (ConT name) t) = constructorTypesVars n2r t
constructorTypesVars n2r  c@(AppT t1 t2) = constructorTypesVars n2r  t1 ++ constructorTypesVars n2r t2
constructorTypesVars n2r  v@(VarT name) = case lookup name n2r of
                                               Just PhantomR -> []
                                               -- Note: there is not correct to filter out nominal type vars
                                               -- when a type var is both normal and representational it is 
                                               -- nominal by GHC, however, we need to fetch this type vars
                                               -- back when generating the context.
                                               Just NominalR -> []
                                               _ -> [v]
constructorTypesVars n2r  c@(ConT name) = []
constructorTypesVars n2r  (PromotedT name) = []
#if __GLASGOW_HASKELL__ > 710
constructorTypesVars n2r  (InfixT t1 name t2) = constructorTypesVars n2r t1 ++ constructorTypesVars n2r t2
constructorTypesVars n2r  (UInfixT t1 name t2) = constructorTypesVars n2r t1 ++ constructorTypesVars n2r t2
constructorTypesVars n2r  (ParensT t) = constructorTypesVars n2r t
#endif
constructorTypesVars n2r  (TupleT i) = []
constructorTypesVars n2r  (ListT ) = [] 
-- constructorTypesVars n2r  (UnboxedTupleT i) = undefined
-- constructorTypesVars n2r  (UnboxedSumT t) = undefined -- ghc 8.2.1
constructorTypesVars n2r  (EqualityT) = []
constructorTypesVars n2r  (PromotedTupleT i) = []
constructorTypesVars n2r  (PromotedNilT) = []
constructorTypesVars n2r  (PromotedConsT) = []
constructorTypesVars n2r  (LitT lit) = []
constructorTypesVars n2r  (ConstraintT) = []
-- constructorTypesVars n2r  (WildCardT lit) = undefined
constructorTypesVars n2r  (ArrowT) = []
constructorTypesVars n2r  t = error $ pprint t ++ " is not support"

expandSynsAndGetContextTypes :: [(TypeVarBind, Role)] -> Type -> Q [Type]
expandSynsAndGetContextTypes tvb2rs t = do
                             t' <- noWarnexpandSynsWith t
                             let n2r = map (\(tvb, r) -> (getTVBName tvb, r)) tvb2rs
                             return $ (constructorTypesVars n2r  t')

third (a,b,c) = c

getContextType :: [(TypeVarBind, Role)] -> Con -> Q [Type]
getContextType tvb2role (NormalC name bangtypes) = fmap concat $ mapM (expandSynsAndGetContextTypes tvb2role) (map snd bangtypes)
getContextType tvb2role (RecC name varbangtypes) = fmap concat $ mapM (expandSynsAndGetContextTypes tvb2role) (map third varbangtypes)
getContextType tvb2role (InfixC bangtype1 name bangtype2) = fmap concat $ mapM (expandSynsAndGetContextTypes tvb2role) (map snd [bangtype1, bangtype2])
-- need to remove types which contains scoped variables
#if __GLASGOW_HASKELL__ > 810

{-
There are 2 foralls for GADTs

data T4 k a b where
  T31 :: a -> k b -> T4 k a b

> putStrLn  $(reify ''T4  >>=  (\x -> return $ ppr x) >>= stringE . show)           
data Lib.T4 (k_0 :: * -> *) (a_1 :: *) (b_2 :: *) where
   T31 :: forall (a_1 :: *) (k_0 :: * -> *) (b_2 :: *) . a_1 -> (k_0 b_2) -> T4 k_0 a_1 b_2

data T1 k a b = T11 (k a) b | T12 (k (k a)) a b String

While for GFT1 the qualified 'a' should not be in the context of class instance

data GadtForall where
    GFT1 :: Show a => a -> GadtForall

That is why the intersect of type parameters in type constructor and type variables in data constructor needs to be null
-}

getContextType tvb2role (ForallC tvbs _ con) = let scopedVarNames = map (getTVBName.voidTyVarBndrFlag) tvbs in
                                         do
                                           types <- (getContextType tvb2role) con
                                           let ty_vars = filter (\ty -> (null $ intersect (getAllVarNames ty) scopedVarNames)) types
                                           fmap concat $ mapM (expandSynsAndGetContextTypes tvb2role) 
                                              (if (sort $ map (voidTyVarBndrFlag.fst) tvb2role) == sort (map voidTyVarBndrFlag tvbs) 
                                                then types else ty_vars) 
#else

getContextType tvb2role (ForallC tvbs _ con) = let scopedVarNames = map getTVBName tvbs in
                                         do
                                           types <- (getContextType tvb2role) con
                                           let ty_vars = filter (\ty -> (null $ intersect (getAllVarNames ty) scopedVarNames)) types
                                           fmap concat $ mapM (expandSynsAndGetContextTypes tvb2role) ty_vars
#endif
#if __GLASGOW_HASKELL__ > 710
getContextType tvb2role (GadtC name bangtypes result_type) = fmap concat $ mapM (expandSynsAndGetContextTypes tvb2role) (map snd bangtypes)
getContextType tvb2role (RecGadtC name bangtypes result_type) = fmap concat $ mapM (expandSynsAndGetContextTypes tvb2role) (map third bangtypes)
#endif

getTyVarCons :: ClassName -> TypeName -> StateT [Type] Q ([TypeVarBind], [Con])
getTyVarCons cn name = do
            info <- lift $ reify name
            case info of
              TyConI dec -> case dec of
#if __GLASGOW_HASKELL__ > 810
                              DataD _ _ tvbs _ cons _  -> return (map voidTyVarBndrFlag tvbs, cons)
                              NewtypeD _ _ tvbs _ con _-> return (map voidTyVarBndrFlag tvbs, [con])
#endif
#if __GLASGOW_HASKELL__ > 710 && __GLASGOW_HASKELL__ <= 810
                              DataD _ _ tvbs _ cons _  -> return (tvbs, cons)
                              NewtypeD _ _ tvbs _ con _-> return (tvbs, [con])
#endif
#if __GLASGOW_HASKELL__ <= 710 
                              DataD _ _ tvbs cons _  -> return (tvbs, cons)
                              NewtypeD _ _ tvbs con _-> return (tvbs, [con])
#endif
                              TySynD name tvbs t -> error $ show name ++ " is a type synonym and `TypeSynonymInstances' is not supported. "
                                  ++ "If you did not derive it then this is a bug, please report this bug to the author of `derive-topdown' package."
                              x -> do
                                 tys <- get
                                 error $ pprint x ++ " is not a data or newtype definition. " ++ show "Stack: " ++ show tys
              _ -> error $ "Cannot generate "++ show cn ++ " instances for " ++ show name

type ClassName = Name
type TypeName = Name

getDecTyVarBndrs :: Dec -> [TypeVarBind]
getDecTyVarBndrs (DataD _ _ bnds _ _ _)                            = bnds
getDecTyVarBndrs (NewtypeD _ _ bnds _ _ _)                         = bnds
getDecTyVarBndrs (TySynD _ bnds _)                                 = bnds
getDecTyVarBndrs (OpenTypeFamilyD (TypeFamilyHead _ bnds _ _))     = bnds
getDecTyVarBndrs (ClosedTypeFamilyD (TypeFamilyHead _ bnds _ _) _) = bnds
getDecTyVarBndrs _                                                 = []

getTypeNameTyVarBndrs :: TypeName -> Q [TypeVarBind]
getTypeNameTyVarBndrs tn = do
            info <- reify tn
            case info of
              TyConI dec    -> return (getDecTyVarBndrs dec)
              FamilyI dec _ -> return (getDecTyVarBndrs dec)

isVarBindPolyKinded :: TypeVarBind -> Bool
#if __GLASGOW_HASKELL__ > 810
isVarBindPolyKinded (KindedTV n f (VarT t)) = True
#else 
isVarBindPolyKinded (KindedTV n (VarT t)) = True
#endif  
isVarBindPolyKinded _ = False

getTypeNameRoles :: TypeName -> Q [Role]
getTypeNameRoles tn = do
              vars <- getTypeNameTyVarBndrs tn
              roles <- reifyRoles tn
              let polyKindedBndVar = filter isVarBindPolyKinded vars
              -- I write this due to a possible bug of GHC.
              -- With PolyKinds extension, GHC may return more 
              -- roles than expected with reifyRoles function.
              -- See: https://gitlab.haskell.org/ghc/ghc/-/issues/21046
              if length vars < length roles
                then return (drop (length polyKindedBndVar) roles)
                else return roles

isTypeFamily :: TypeName -> Q Bool
isTypeFamily tn = do
                info <- reify tn
                case info of
                  FamilyI (OpenTypeFamilyD hd) insts -> return True
                  FamilyI (ClosedTypeFamilyD hd eqs) insts -> return True 
                  _ -> return False

#if __GLASGOW_HASKELL__ > 810
voidTyVarBndrFlag :: TyVarBndr flag -> TyVarBndr ()
voidTyVarBndrFlag (PlainTV n f) = PlainTV n ()
voidTyVarBndrFlag (KindedTV n f k) = KindedTV n () k
#endif

-- In the future of GHC, this will be removed.
-- See https://ghc.haskell.org/trac/ghc/ticket/13324
-- This function is not correct need to be rewrite in a more formalized way
generateClassContext :: ClassName -> TypeName -> Q Cxt
generateClassContext classname typename = do
                            (tvbs, cons) <- (evalStateT $ getTyVarCons classname typename) []
                            -- Need to remove phantom types, needs GHC 7.2
                            roles <- getTypeNameRoles typename
                            let typeVarBind2Role = zip tvbs roles
                            traceShowM "typeVarBind2Role"
                            traceShowM typeVarBind2Role
                            let reTyVars = map (tvb2type . fst) (filter (\(t,r) -> r /= PhantomR && getTVBKind t == StarT) typeVarBind2Role)
                            traceShowM "reTyVars"
                            traceShowM reTyVars
                            tfs <- getAllTypeFamilyTypesFromName typename
                            types <- fmap (nub. concat) $ mapM (getContextType typeVarBind2Role) cons
                            let len = length $ types ++ tfs
                            if len == 0
                              then return []
                              else do
                                  -- Eq a, Eq b ...
                                  let contexts = map (AppT (ConT classname)) (nub (types ++ tfs ++ reTyVars))
                                  return $ contexts


#if __GLASGOW_HASKELL__ > 810
getTVBName :: TyVarBndr () -> Name
getTVBName (PlainTV name _)   = name
getTVBName (KindedTV name _ _) = name
#else
getTVBName :: TyVarBndr -> Name
getTVBName (PlainTV name)   = name
getTVBName (KindedTV name _) = name
#endif

#if __GLASGOW_HASKELL__ > 810
getTVBKind :: TyVarBndr () -> Kind
getTVBKind (PlainTV name f) = StarT -- This is not correct!
getTVBKind (KindedTV name _ k) = k
#else
getTVBKind :: TyVarBndr -> Name
getTVBKind (PlainTV f) = k
getTVBKind (KindedTV name _) = k
#endif

#if __GLASGOW_HASKELL__ > 810
tvb2type :: TyVarBndr () -> Kind
tvb2type (PlainTV name f) = VarT name
tvb2type (KindedTV name _ k) = VarT name
#else
tvb2type :: TyVarBndr -> Name
tvb2type (PlainTV f) = VarT name
tvb2type (KindedTV name _) = VarT name
#endif


getTypeNames :: Type -> [Name]
getTypeNames (ForallT tvbs cxt t) = getTypeNames t
getTypeNames (ConT n) = [n]
getTypeNames (AppT t1 t2) = getTypeNames t1 ++ getTypeNames t2
getTypeNames _ = []

expandSynsAndGetTypeNames :: [Type] -> Q [TypeName]
expandSynsAndGetTypeNames ts = do
                          ts' <- mapM noWarnexpandSynsWith ts
                          return $ concatMap getTypeNames ts'

getCompositeTypeNames :: Con -> Q [TypeName]
getCompositeTypeNames (NormalC n bts) = expandSynsAndGetTypeNames (map snd bts)
getCompositeTypeNames (RecC n vbts) = expandSynsAndGetTypeNames (map third vbts)
getCompositeTypeNames (InfixC st1 n st2) = expandSynsAndGetTypeNames (map snd [st1 , st2])
getCompositeTypeNames (ForallC bind context con) = getCompositeTypeNames con
#if __GLASGOW_HASKELL__ > 710
getCompositeTypeNames (GadtC name bangtype resulttype) = expandSynsAndGetTypeNames (map snd bangtype)
getCompositeTypeNames (RecGadtC name bangtypes result_type) = expandSynsAndGetTypeNames (map third bangtypes)
#endif

#if __GLASGOW_HASKELL__ > 810
tvb2kind :: TyVarBndr a -> Kind
tvb2kind (PlainTV n _) = StarT
tvb2kind (KindedTV n _ kind) = kind 
#else
tvb2kind :: TyVarBndr -> Kind
tvb2kind (PlainTV n) = StarT
tvb2kind (KindedTV n kind) = kind 
#endif


data DecTyType = Data | Newtype | TypeSyn | BuiltIn deriving (Show, Enum, Eq)

decType :: Name -> Q DecTyType
decType name = do
         info <- reify name
         case info of
           TyConI dec -> case dec of
#if __GLASGOW_HASKELL__ > 710
              DataD _ _ tvbs _ cons _   -> return Data
              NewtypeD _ _ tvbs _ con _ -> return Newtype
#else
              DataD _ _ tvbs cons _   -> return Data                                               
              NewtypeD _ _ tvbs con _ -> return Newtype
#endif
              TySynD name tvbs t -> return TypeSyn
           PrimTyConI name arity unlifted -> return BuiltIn


-- A function which is not used
getKind :: Name -> Q Kind
getKind name = do
         info <- reify name
         case info of
           TyConI dec -> case dec of
#if __GLASGOW_HASKELL__ > 710
              DataD _ _ tvbs _ cons _   -> case tvbs of
                                                [] -> return StarT
                                                xs -> return (foldr1 AppT (map tvb2kind xs))
              NewtypeD _ _ tvbs _ con _ -> case tvbs of
                                                [] -> return StarT
                                                xs -> return (foldr1 AppT (map tvb2kind xs))
#else
              DataD _ _ tvbs cons _   -> case tvbs of
                                                [] -> return StarT
                                                xs -> return (foldr1 AppT (map tvb2kind xs))
              NewtypeD _ _ tvbs con _ -> case tvbs of
                                                [] -> return StarT
                                                xs -> return (foldr1 AppT (map tvb2kind xs))
#endif
           PrimTyConI name arity unlifted -> case arity of
                                                 -- Unlifted types are not considered here.
                                                0 -> return StarT
                                                n -> return (foldr1 (\x y -> AppT (AppT ArrowT x) y) (replicate arity StarT))

getTypeConstructor :: Type -> Type
getTypeConstructor (AppT a1 a2) = getTypeConstructor a1
getTypeConstructor a = a
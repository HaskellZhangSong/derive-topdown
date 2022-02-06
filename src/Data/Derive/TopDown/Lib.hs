{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Data.Derive.TopDown.Lib (
  isInstance'
 , generateClassContext
 , getTyVarCons,getTVBName
 , getCompositeTypeNames
 , ClassName
 , TypeName
 , decType
 , DecTyType(..)
 ,getTypeConstructor
 ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Data.Generics (mkT,everywhere,mkQ,everything)
import GHC.Exts
import Language.Haskell.TH.ExpandSyns (expandSynsWith,noWarnTypeFamilies)
import Data.List (nub,intersect,foldr1)
import Control.Monad.State
import Control.Monad.Trans
#ifdef __GLASGOW_HASKELL__
import Data.Typeable
import Data.Data
#endif

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

expandSynsAndGetContextTypes :: [(Name, Role)] -> Type -> Q [Type]
expandSynsAndGetContextTypes n2r t = do
                             t' <- noWarnexpandSynsWith t
                             return $ (constructorTypesVars n2r  t')

third (a,b,c) = c

getContextType :: [(Name, Role)] -> Con -> Q [Type]
getContextType name2role (NormalC name bangtypes) = fmap concat $ mapM (expandSynsAndGetContextTypes name2role) (map snd bangtypes)
getContextType name2role (RecC name varbangtypes) = fmap concat $ mapM (expandSynsAndGetContextTypes name2role) (map third varbangtypes)
getContextType name2role (InfixC bangtype1 name bangtype2) = fmap concat $ mapM (expandSynsAndGetContextTypes name2role) (map snd [bangtype1, bangtype2])
-- need to remove types which contains scoped variables
#if __GLASGOW_HASKELL__>810
getContextType name2role (ForallC tvbs _ con) =  let scopedVarNames = map (getTVBName.voidTyVarBndrFlag) tvbs in
                                         do
                                           types <- (getContextType name2role) con
                                           let ty_vars = filter (\ty -> (null $ intersect (getAllVarNames ty) scopedVarNames)) types
                                           fmap concat $ mapM (expandSynsAndGetContextTypes name2role) ty_vars
#else

getContextType name2role (ForallC tvbs _ con) =  let scopedVarNames = map getTVBName tvbs in
                                         do
                                           types <- (getContextType name2role) con
                                           let ty_vars = filter (\ty -> (null $ intersect (getAllVarNames ty) scopedVarNames)) types
                                           fmap concat $ mapM (expandSynsAndGetContextTypes name2role) ty_vars
#endif
#if __GLASGOW_HASKELL__ > 710
getContextType name2role (GadtC name bangtypes result_type) = fmap concat $ mapM (expandSynsAndGetContextTypes name2role) (map snd bangtypes)
getContextType name2role (RecGadtC name bangtypes result_type) = fmap concat $ mapM (expandSynsAndGetContextTypes name2role) (map third bangtypes)
#endif

#if __GLASGOW_HASKELL__ > 810
getTyVarCons :: ClassName -> TypeName -> StateT [Type] Q ([TyVarBndr ()], [Con])
#else
getTyVarCons :: ClassName -> TypeName -> StateT [Type] Q ([TyVarBndr], [Con])
#endif
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
                              TySynD name tvbs t -> error $ show name ++ " is a type synonym and -XTypeSynonymInstances is not supported. If you did not derive it then This is a bug, please report this bug to the author of this package."
                              x -> do
                                 tys <- get
                                 error $ pprint x ++ " is not a data or newtype definition. " ++ show "Stack: " ++ show tys
              _ -> error $ "Cannot generate "++ show cn ++ " instances for " ++ show name

type ClassName = Name
type TypeName = Name

#if __GLASGOW_HASKELL__ > 810
voidTyVarBndrFlag :: TyVarBndr flag -> TyVarBndr ()
voidTyVarBndrFlag (PlainTV n f) = PlainTV n ()
voidTyVarBndrFlag (KindedTV n f k) = KindedTV n () k
#endif

-- In the future of GHC, this will be removed.
-- See https://ghc.haskell.org/trac/ghc/ticket/13324
generateClassContext :: ClassName -> TypeName -> Q Cxt
generateClassContext classname typename = do
                            (tvbs, cons) <- (evalStateT $ getTyVarCons classname typename) []
                            -- Need to remove phantom types
                            roles <- reifyRoles typename
                            let varName2Role = zip (map getTVBName tvbs) roles
                            types <- fmap nub $ fmap concat $ mapM (getContextType varName2Role) cons
                            let len = length types
                            if len == 0
                              then return []
                              else do
                                  -- Eq a, Eq b ...
                                  let contexts = map (AppT (ConT classname)) types
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
#if __GLASGOW_HASKELL__> 710
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
{-# LANGUAGE CPP, GADTs, ViewPatterns, MultiWayIf, DataKinds, BlockArguments, RankNTypes, ScopedTypeVariables, KindSignatures, TypeFamilies, DeriveLift, BangPatterns #-}

module Data.Derive.TopDown.CxtGen where

import Control.Monad
import Data.Data
import Data.Function
import Data.Generics
import Data.List
import Data.Derive.TopDown.Lib
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.TH.ExpandSyns (expandSynsWith,noWarnTypeFamilies)
import Debug.Trace

getCon g@(GadtC _ _ _) = [g]
getCon g@(RecGadtC _ _ _) = [g]
getCon g = []

-- ConT a1 a2 a3 .. -> [ConT a1, a2,a3]
unappTy :: Type -> [Type]
unappTy (AppT t1 t2) = unappTy t1 ++ [t2]
unappTy (AppKindT ty kind) = unappTy ty
unappTy t = [t]

getConstrFields = tail.unappTy

getAllGadtCons :: Dec -> [Con]
getAllGadtCons = everything (++) (mkQ [] getCon)  

isGadt :: TypeName -> Q Bool
isGadt tn = do 
        info <- reify tn
        case info of
              TyConI dec -> case dec of
#if __GLASGOW_HASKELL__ > 710
                              d@(DataD _ _ _ _ _ _) 
#endif
#if __GLASGOW_HASKELL__ <= 710 
                              d@(DataD _ _ _ _ _)  
#endif
                                -> return $ (not.null) $ getAllGadtCons d
                              _ -> return False
              _ -> error $ show tn ++ " is not a type"

getVarName :: Type -> [Name]
getVarName (VarT n) = [n]
getVarName _ = []

getAllVarNames :: Data a => a -> [Name]
getAllVarNames = everything (++) (mkQ [] getVarName)

replaceName :: [(Name, Name)] -> Name -> Name
replaceName m nm = case lookup nm m of
                    Nothing -> nm
                    Just n -> n

replaceAllNames :: Data a => [(Name, Name)] -> a -> a
replaceAllNames m = everywhere (mkT (replaceName m))

third :: (a, b, c) -> c
third (a, b, c) = c

#if __GLASGOW_HASKELL__ > 810
voidTyVarBndrFlag :: TyVarBndr flag -> TyVarBndr ()
voidTyVarBndrFlag (PlainTV n f) = PlainTV n ()
voidTyVarBndrFlag (KindedTV n f k) = KindedTV n () k
#endif

{- For GADTs the type variables will be qualified twice
   need to be careful with the difference of 

     data A a = forall b. A (Either a b)
     data A a where
        A :: a -> Int -> A a -- a will be qualifed again by 'template-haskell' api
  -}
getConArgs :: Bool -> Con -> [Type]
getConArgs gadt (GadtC nm bt t)    = map snd bt
getConArgs gadt (RecGadtC nm bt t) = map third bt
getConArgs gadt (NormalC nm bt)    = map snd bt
getConArgs gadt (RecC nm vbts)     = map third vbts
getConArgs gadt (InfixC bt nm bt2) = [snd bt, snd bt2]
-- need GHC version Macro > 810
-- must not GADT
getConArgs gadt (ForallC tvbs cxt con) = 
  if not gadt 
    -- Non-gadt qualified variable should be removed, since qualified vars should be in 
    -- class instances context such as @data Obj b = forall a. Show a => Obj a b@
    -- when Obj need to derive Show, only b should be in context.
    -- Need to be recursive since 'forall' can be 'forall a. forall b. DataCon a b'
    then
      let scopedVarNames = map (getTVBName.voidTyVarBndrFlag) tvbs
          constructorArgTypes = getConArgs False con
          isTypeContainScopedVars x = null $ intersect (getAllVarNames x) scopedVarNames
        in filter isTypeContainScopedVars constructorArgTypes 
    -- 
    else
      getConArgs True con

{-

instance context type role (ICT role) is different from type role

There are 5 situations for a type variable is used
1. Not used. ICT role phantom same as type role
2. If it's used as field a data constructor then ICT role is representational, same as type role
3. This type variable is a type constructor, then ICT role is representational, different with type role
4. This type variable is used as type arguments of a type variable, then ICT role is nominal
5. This type variable is used as type arguments of a type Contructor, then ICT role need to be inferred recursively

data ICTRole = P | N | R

+ is a idemponent and communitive operator when the type variable has two ICT roles

P + N = N
P + R = R
N + R = R

In most cases, the roles are representation. 
-}

data ICTRole = P | N | R
    deriving (Show, Eq, Ord, Enum)

roleListToExp :: Name -> Q Exp
roleListToExp nm = do
                roles <- inferICTRoles nm
                listE $ map (\r -> conE $ mkName $ show r) roles
                

ictRoleToRole P = PhantomR
ictRoleToRole N = NominalR
ictRoleToRole R = RepresentationalR

roleToIctRole PhantomR = P
roleToIctRole NominalR = N
roleToIctRole RepresentationalR = R

(.*) :: ICTRole -> ICTRole -> ICTRole
(.*) = max

-- get the left most type in AppT
getTypeConstr :: Type -> Type
getTypeConstr t = head $ unappTy t

getRoleFromFields :: TypeName  -- ^ type variable name
                  -> [Type]
                  -> [ICTRole]
                  -> Q ICTRole
getRoleFromFields tn tys roles = let fieldToRoles = zip tys roles in
                      case lookup (VarT tn) fieldToRoles of
                          -- if tn is in the fields directly, then the role will be the ICT role
                          Just r -> return r
                          -- For each of the type in the data constr field, we 
                          -- infer role and getRole recursively
                          Nothing -> do
                                  roles <- forM fieldToRoles $ \(f::Type, r) -> do
                                    -- check whether tn is in f
                                    case r of 
                                      P -> return P
                                      -- tn = a, XLit (Maybe a)
                                      N -> if tn `elem` getAllVarNames f 
                                              then return N
                                              else return P
                                      -- Maybe (Either [a] Int) -- need recursive call
                                      -- Maybe (Either (Proxy a) Int) -- need recursive call
                                      -- Maybe (Either (Xlit) a) Int) -- need recursive call
                                      R -> do
                                        role <- getRole tn f
                                        return $ min R role
                                  return $ maximum roles

getRole :: TypeName  -- ^ type variable name
        -> Type      -- ^ field type
        -> Q ICTRole

-- The type variable is directly used in constr such as
-- 'data I a = I a'
-- 'data Proxy a = Proxy'
getRole tn (VarT nm) = if tn == nm
                        then return R
                        else return P
                          
-- data I a = I Int. 'a' is not used since the field is a named constructor
getRole tn (ConT nm) = return P
-- The type variable is used as arguments of type constr such as 
-- 1. 'data I a = I (Either (Maybe a) Int)' -- hard
-- 2. 'data I a = I (Either (XLit (Maybe a)) Int)' -- hard
-- 3. 'data I a = I (Either a Int) (Proxy a) (XLit a)'
-- 4. 'data I a = I (forall a. a)'
-- This function will get ICTRole of a in 'Either a Int', 'Proxy a', 'XLit a' respectively. 
-- XLit is a type family
-- need to infer cn's role recursively
getRole tn t2@(getTypeConstr -> ConT cn) = do
                      ictRoles <- inferICTRoles cn
                      let fields = getConstrFields t2
                      getRoleFromFields tn fields ictRoles
                                            

-- 'data I a = I (k (Maybe a)) Int'
getRole tn t2@(getTypeConstr -> VarT nm) = return N
-- data I a = I [a]
getRole tn t2@(getTypeConstr -> ListT) = let vars = getAllVarNames t2 in 
                                    if tn `notElem` vars 
                                        then return P
                                        else do 
                                          let fields = getConstrFields t2
                                          let ictRoles = [R]
                                          getRoleFromFields tn fields ictRoles
-- data A a = A (a :: *)
getRole tn (getTypeConstr -> AppKindT ty kind)   = getRole tn ty
getRole tn (getTypeConstr -> SigT ty kind)       = getRole tn ty
getRole tn (ArrowT) = undefined
-- data I a = I (a :+: b)
-- Generally same as ConT
getRole tn (InfixT t1 nm t2) =  do
                      ictRoles <- inferICTRoles nm
                      let fields = [t1, t2]
                      getRoleFromFields tn fields ictRoles

-- no need to handle this case, 
getRole tn t@(UInfixT t1 nm t2) = error $ show t ++ " is impossible case for deriving topdown" 

getRole tn (ParensT ty) = getRole tn ty
-- data I a b = I (a, b)
-- data I a b = I (Proxy a, b)
getRole tn t2@(getTypeConstr -> (TupleT n)) = 
                                  let vars = getAllVarNames t2 in 
                                    if tn `notElem` vars 
                                        then return P -- quick exit
                                         else do 
                                          let fields = getConstrFields t2
                                          let ictRoles = replicate n R
                                          getRoleFromFields tn fields ictRoles

-- data I a = I (forall b . t)
-- no need to handle the forall since the name a 
-- will be unique in t
getRole tn t2@(ForallT bnd cxt ty) = getRole tn ty
getRole tn t2@(ForallVisT bnd ty) = getRole tn ty
getRole tn f = return P -- not correct. There are other cases which needs to be handled

type Prefix = String

{-|
Note: 

If one writes the data constr with different names in type header like the following
data T4 k a b where
  T41 :: a -> k b -> T4 k a b -- in template haskell, the type vars will 
                              -- be quantified twice as 
                              -- forall a1 k1 b1. a1 -> k1 b1 -> T4 k1 a1 b1
The process will construct [(k1, k), (b1, b), (a1, a)]

If one writes the data constr like the following:

data T4 k a b where
  T41 :: c -> f d -> T4 f c d -- or, equivalently,
                              -- forall c f d. c -> f d -> T4 f c d
  T42 :: y -> g x -> T4 g y x -- or, equivalently,
                              -- forall y g x. y -> g x -> T4 g y x


This map will be constructed in a wrong way so that generation process may fail.
See #13885
-}
getGadtTyVarNameMap :: [Name] -> [Name] -> [(Name, Name)]
getGadtTyVarNameMap nm1 nm2 = let pstr1 = nm1 & nub & sort & map (\n -> (show n & reverse & dropWhile (/= '_') & reverse, n)) 
                                  pstr2 = nm2 & nub & sort & map (\n -> (show n & reverse & dropWhile (/= '_') & reverse, n)) 
                                in pair pstr1 pstr2
                              where 
                                pair :: [(Prefix, Name)] -> [(Prefix, Name)] -> [(Name, Name)]
                                pair [] strs2 = []
                                pair ((p, s):xs) str2 = case lookup p str2 of
                                                          Nothing -> pair xs str2
                                                          Just a -> (s, a) : pair xs str2

{- | This function returns each ICT role of a type constructor -}
inferICTRoles :: TypeName -> Q [ICTRole]
inferICTRoles tn = do 
         isTF <- isTypeFamily tn
         roles <- getTypeNameRoles tn
         if | isTF -> do 
                    return $ map roleToIctRole roles
           -- Rep and Phantom inference are the same for ICT role inference
            | all (`elem` [PhantomR]) roles -> 
                  do
                    return $ map roleToIctRole roles
           -- else nominal. we should handle this case, since for type role
           -- a type var in both nominal and representational position will
           -- be inferred as nominal. However, we want it to be representational for 
           -- class instance deriving.
            | otherwise ->  do
                  (tvbs, cons) <- getTyVarCons tn
                  let vars = map (getTVBName . voidTyVarBndrFlag) tvbs
                  gadt <- isGadt tn
                  let constrFields = if gadt 
                                        then
                                          -- GADT will quantify type variable twice. see #13885
                                          -- one in type constr, one in data constr with different var name
                                          -- this process will make the variable in data constr
                                          -- quantified by the vars declared in type constr
                                          let gadtTyVarNames = map getTVBName tvbs
                                              gadtConsTyVarNames = getAllVarNames cons
                                              m = getGadtTyVarNameMap gadtConsTyVarNames gadtTyVarNames
                                              replaceCons = replaceAllNames m cons
                                              conArgs = concatMap (getConArgs True) replaceCons
                                          in
                                              conArgs
                                        else
                                          concatMap (getConArgs False) cons :: [Type]
                  roles <- sequence [(fmap maximum $ sequence [ do {r <- getRole tn con; 
                                                                    return r}  | con <- constrFields]) | tn <- vars]
                  return roles

-- need to expand type synonym
-- need to consider rank N forall quantifiers
getContextType :: [Name] -> Type -> Q [Type]
{-
data T a = T a

   a \in [a]   field = a
--------------------------
    a in context
-}
getContextType ns t@(VarT a) = if a `elem` ns
                                then return $ [t]
                                else return []
{-
data T a = T Int

   a \not \in []  field Constr
-------------------------------
       [] 
-}
getContextType ns t@(ConT _) = return []


{-
data T k a = T (k a)

   k \in [k, a],  field = k a
---------------------------------
        (k a) \in context

 what if t contains other local type variable, need to consider forall?

data T k a  = forall b . T a (k b)

k b should in the context
-}

getContextType ns t@(getTypeConstr -> VarT cn) = do
                                            -- nms <- ask
                                            let varsInTy = getAllVarNames t
                                            if all (\x -> x `elem` ns) varsInTy
                                              then return [t]
                                              else return []
{-
data T a = T (Maybe a)

   role Maybe = [R]
   field = Maybe a
   a \in [a]
-------------------------------------
        a \in context

data T k a = T (Maybe (k a))

    role ConT = [R]
    field = ConT (k a)
    k is Nominal
-------------------------------------
          (k a) \in context
-}
getContextType ns t@(getTypeConstr -> ConT cn) = do 
                                            -- if it is type family return Just t
                                            -- else infer the roles and 
                                            isTF <- isTypeFamily cn
                                            if | isTF -> do 
                                                  let varsInTy = getAllVarNames t
                                                  if all (\x -> x `elem` ns) varsInTy
                                                    then return [t]
                                                    else return []
                                               | otherwise -> do
                                                   roles <- inferICTRoles cn
                                                   let fields = getConstrFields t
                                                   let field2RoleMap = zip fields roles
                                                   -- only generate fields that are representative
                                                   let repField2Role = filter (\(x,r) -> r == R) field2RoleMap
                                                   tyss <- forM repField2Role $ \(x,R) -> getContextType ns x
                                                   return $ concat tyss

getContextType ns t@(ParensT ty) = getContextType ns ty

getContextType ns t@(InfixT t1 n t2) = do 
                                t1s <- getContextType ns t1
                                t2s <- getContextType ns t2
                                return $ t1s ++ t2s

getContextType ns t@(getTypeConstr -> SigT ty kind) = getContextType ns ty

getContextType ns t@(getTypeConstr -> TupleT i) = do
                                            let fields = getConstrFields t
                                            tyss <- forM fields $ \t -> getContextType ns t
                                            return $ concat tyss

getContextType ns t@(getTypeConstr -> ListT) = do
                                            let fields = getConstrFields t
                                            tyss <- forM fields $ \t -> getContextType ns t
                                            return $ concat tyss

-- ForallVisT -- GHC 8.10
-- PromotedT Name
-- UInfixT Type Name Type
-- PromotedInfixT Type Name Type
-- PromotedUInfixT Type Name Type
-- UnboxedTupleT Int
-- UnboxedSumT SumArity
-- MulArrowT -- GHC 9.0
-- PromotedTupleT Int
-- PromotedNilT	 '[]
-- PromotedConsT	 (':)
-- ConstraintT Constraint
-- LitT TyLit	 0,1,2, etc. -- return []
-- WildCardT	should return []
-- ImplicitParamT String Type	 ?x :: t
getContextType ns t = return []

getConTypes :: Con -> Q [Type]
getConTypes (NormalC n bts)       = return $ map snd bts
getConTypes (RecC  n bts)         = return $ map third bts
getConTypes (InfixC t1 n t2)      = return $ map snd [t1, t2]
getConTypes (ForallC bnd ctx con) = do 
                                let varNames = map (getTVBName . voidTyVarBndrFlag) bnd
                                getConTypes con
getConTypes (GadtC n bts t)       = return $ map snd bts
getConTypes (RecGadtC n bts t)    = return $ map third bts

-- need expand type synonym
-- ^ This function will get type class context from a data type
getContextTypes :: TypeName -> Q [Type]
getContextTypes tn = do 
            (tvbs, cons) <- getTyVarCons tn
            -- GHC 8.10
            let vars :: [Name] = map (getTVBName . voidTyVarBndrFlag) tvbs
            gadt <- isGadt tn
            if gadt 
              then do 
                let gadtTyVarNames = map getTVBName tvbs
                let gadtConsTyVarNames = getAllVarNames cons
                let m = getGadtTyVarNameMap gadtConsTyVarNames gadtTyVarNames
                let replaceCons = replaceAllNames m cons
                conTys <- fmap concat $ forM replaceCons $ \con -> getConTypes con
                ts     <- fmap concat $ forM conTys $ \ty -> getContextType vars ty
                return ts
              else do         
                conTys <- fmap concat $ forM cons $ \con -> getConTypes con
                ts     <- fmap concat $ forM conTys $ \ty -> getContextType vars ty
                return ts

foo tn = (getContextTypes tn) 

generateClassContext1 :: ClassName -> TypeName -> Q Cxt
generateClassContext1 cn tn = do 
                        tys <- getContextTypes tn
                        let cxt :: [Pred] = map (\ty -> AppT (ConT cn) ty) tys
                        return cxt

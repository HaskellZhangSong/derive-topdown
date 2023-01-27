{-# LANGUAGE TemplateHaskell  #-}
module Data.Derive.TopDown.Standalone (
    deriving_
  , derivings
  , derivingss
  , deriving_with_breaks
  , _deriving_
  , _derivings
  , _derivingss
  , _deriving_with_breaks
#if __GLASGOW_HASKELL__ >= 802
  , strategy_deriving
  , strategy_deriving_with_breaks
  , strategy_derivings
  , strategy_derivingss
  , _strategy_deriving
  , _strategy_deriving_with_breaks
  , _strategy_derivings
  , _strategy_derivingss
#endif
  ) where

import Data.Derive.TopDown.Lib
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import qualified GHC.Generics as G
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.List (foldl')
import Data.Primitive.Types
import Data.Typeable

data TypeContext = Generated | Wildcard

#if __GLASGOW_HASKELL__ >= 802
genStandaloneDerivingDecl :: ClassName -> TypeName -> Maybe DerivStrategy -> TypeContext -> [TypeName] -> StateT [Type] Q [Dec]
genStandaloneDerivingDecl cn tn st tcxt breaks = do
#else
genStandaloneDerivingDecl :: ClassName -> TypeName -> TypeContext -> [TypeName] -> StateT [Type] Q [Dec]
genStandaloneDerivingDecl cn tn tcxt breaks = do
#endif
                   (tvbs,cons) <- lift $ getTyVarCons tn
                   classContext <- case tcxt of 
                                     Generated -> lift $ generateClassContext cn tn
                                     Wildcard -> return [WildCardT]
                   let typeNames = map getTVBName tvbs
                   instanceType <- lift $ foldl' appT (conT tn) $ map varT typeNames
                    -- Stop generating further instances
                   -- 1. it is already a member of that type class
                   -- 2. we have already generated it, which is kind of same with case 1
                   -- 3. for GHC.Generic, if it is a primitive type like Int, Double
                   -- 4. It will stop on the types in breaks
                   -- 5. It will stop on primitive types and Integer when deriving Typeable
                   isMember <- lift $ isInstance' cn [instanceType]              
                   isPrimitive <-lift $ isInstance' ''Prim [instanceType]
                   let isGeneric = ''G.Generic == cn
                   table <- get
                   if isMember || elem instanceType table || elem tn breaks ||
                      (isPrimitive && isGeneric) || (isGeneric && tn == ''Integer) ||
                      (cn == ''Typeable && isPrimitive) || (cn == ''Typeable && tn == ''Integer)
                     then return []
                     else do
#if __GLASGOW_HASKELL__ >= 802
                       declareType <- lift (decType tn)
                       let standaloneD = \strategy -> [StandaloneDerivD strategy classContext (AppT (ConT cn) instanceType)]
                       let c = if st == Nothing
                                 then standaloneD Nothing
                                 else case declareType of
                                         Data    -> case st of
                                               Just NewtypeStrategy -> standaloneD Nothing
                                               _                    -> standaloneD st
                                         _       -> standaloneD st
#else
                       let c = [StandaloneDerivD classContext (AppT (ConT cn) instanceType)]
#endif
                       modify (instanceType:)
                       names <- lift $ fmap concat $ mapM getCompositeTypeNames cons

                       names' <- lift $ filterM (\x -> fmap not (isTypeFamily x)) names
#if __GLASGOW_HASKELL__ >= 802
                       xs <- mapM (\n -> genStandaloneDerivingDecl cn n st tcxt breaks) names'
#else
                       xs <- mapM (\n -> genStandaloneDerivingDecl cn n tcxt breaks) names'
#endif
                       return $ concat xs ++ c


deriving_ :: Name -- ^ class name
          -> Name -- ^ type name
          -> Q [Dec]

#if __GLASGOW_HASKELL__ >= 802
deriving_ cn tn = evalStateT (genStandaloneDerivingDecl cn tn Nothing Generated []) []
#else
deriving_ cn tn = evalStateT (genStandaloneDerivingDecl cn tn Generated []) []
#endif

{- | This is particularly useful with 'Generic' class.

For the types like 'Int', 'Char','Ratio' or other types which are not 'Generic', there must be a way to stop the generation process on those types.

However, the deriving topdown function will only stop generating 'Generic' instances on primitive types and 'Integer' by default, so you do not need to break on them manually.

Another circumtances might be deriving for 'Typeable' class. Since there is a bug in GHC, isInstance function in TH library is not working on 'Typeable', you can manually give the types which are already instances of 'Typeable' to stop the generation process.

For others cases, there no need to use this function, bacause for a data type @A@ which is composited by another type, when you manually write an instance declaration for @A@, the process will stop on @A@ automatically since it is already an instance of the type class.
-}
deriving_with_breaks :: Name -- ^ class name
          -> Name -- ^ type name
          -> [Name] -- ^ type names that stop the deriving process
          -> Q [Dec]

#if __GLASGOW_HASKELL__ >= 802
deriving_with_breaks cn tn bs = evalStateT (genStandaloneDerivingDecl cn tn Nothing Generated bs) []
#else
deriving_with_breaks cn tn bs = evalStateT (genStandaloneDerivingDecl cn tn Generated bs) []
#endif

derivings :: [Name] -- ^ class names
          -> Name   -- ^ type name
          -> Q [Dec]
derivings cns tn = fmap concat (mapM (\x -> deriving_ x tn) cns)

derivingss :: [Name] -- ^ class names
           -> [Name] -- ^ type names
           -> Q [Dec]
derivingss cns tns = fmap concat (mapM (\x -> derivings cns x) tns)


#if __GLASGOW_HASKELL__ >= 802
strategy_deriving :: DerivStrategy
                  -> Name
                  -> Name
                  -> Q [Dec]

strategy_deriving st cn tn = evalStateT (genStandaloneDerivingDecl cn tn (Just st) Generated []) []

strategy_deriving_with_breaks :: DerivStrategy
                  -> Name
                  -> Name
                  -> [Name] -- ^ type names that stop the deriving process
                  -> Q [Dec]

strategy_deriving_with_breaks st cn tn bs = evalStateT (genStandaloneDerivingDecl cn tn (Just st) Generated bs) []

strategy_derivings :: DerivStrategy
                   -> [Name]
                   -> Name
                   -> Q [Dec]

strategy_derivings st cns tn = fmap concat $ (mapM (\x -> strategy_deriving st x tn) cns)

strategy_derivingss :: DerivStrategy
                    -> [Name]
                    -> [Name]
                    -> Q [Dec]
strategy_derivingss st cns tns = fmap concat $ (mapM (\x -> strategy_derivings st cns x) tns)
#endif


{- | A standalone deriving clause with class context of the instance a wildcard. Need to be used with @PartialTypeSignatures@ language extension -}
_deriving_ :: Name -- ^ class name
          -> Name -- ^ type name
          -> Q [Dec]

#if __GLASGOW_HASKELL__ >= 802
_deriving_ cn tn = evalStateT (genStandaloneDerivingDecl cn tn Nothing Wildcard []) []
#else
_deriving_ cn tn = evalStateT (genStandaloneDerivingDecl cn tn Wildcard []) []
#endif



_deriving_with_breaks :: Name -- ^ class name
           -> Name -- ^ type name
           -> [Name] -- ^ type names that stop the deriving process
           -> Q [Dec]

#if __GLASGOW_HASKELL__ >= 802
_deriving_with_breaks cn tn bs = evalStateT (genStandaloneDerivingDecl cn tn Nothing Wildcard bs) []
#else
_deriving_with_breaks cn tn bs = evalStateT (genStandaloneDerivingDecl cn tn Wildcard bs) []
#endif

_derivings :: [Name] -- ^ class names
          -> Name   -- ^ type name
          -> Q [Dec]
_derivings cns tn = fmap concat (mapM (\x -> _deriving_ x tn) cns)

_derivingss :: [Name] -- ^ class names
           -> [Name] -- ^ type names
           -> Q [Dec]
_derivingss cns tns = fmap concat (mapM (\x -> _derivings cns x) tns)


#if __GLASGOW_HASKELL__ >= 802
_strategy_deriving :: DerivStrategy
                  -> Name
                  -> Name
                  -> Q [Dec]

_strategy_deriving st cn tn = evalStateT (genStandaloneDerivingDecl cn tn (Just st) Wildcard []) []

_strategy_deriving_with_breaks :: DerivStrategy
                  -> Name
                  -> Name
                  -> [Name] -- ^ type names that stop the deriving process
                  -> Q [Dec]

_strategy_deriving_with_breaks st cn tn bs = evalStateT (genStandaloneDerivingDecl cn tn (Just st) Wildcard bs) []

_strategy_derivings :: DerivStrategy
                   -> [Name]
                   -> Name
                   -> Q [Dec]

_strategy_derivings st cns tn = fmap concat $ (mapM (\x -> strategy_deriving st x tn) cns)

_strategy_derivingss :: DerivStrategy
                    -> [Name]
                    -> [Name]
                    -> Q [Dec]
_strategy_derivingss st cns tns = fmap concat $ (mapM (\x -> strategy_derivings st cns x) tns)
#endif

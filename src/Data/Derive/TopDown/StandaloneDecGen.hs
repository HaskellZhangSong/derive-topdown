-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown.DecGen
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown.StandaloneDecGen (
    gen_standalone_deriving_decl
)
where

import Data.Derive.TopDown.Lib
import Language.Haskell.TH
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.Derive.TopDown.IsInstance
import Data.Derive.TopDown.Types
import Data.List (foldl1')
import Data.Primitive.Types
import GHC.Generics
import Debug.Trace

reset_strategy :: TypeName -> Maybe DerivStrategy -> Q (Maybe DerivStrategy)
reset_strategy tn st = do
        declareType <- decType tn
        case (declareType, st) of
          (_, Nothing) -> return Nothing
          (Data, Just NewtypeStrategy) -> return Nothing
          _ -> return st

type Debug = Bool

gen_standalone_deriving_decl :: ClassName
                             -> TypeName
                             -> Maybe DerivStrategy  
                             -> [TypeName]  -- ^ a list of types that breaks the generation process
                             -> ContextGenerator -- ^ a context generator
                             -> Debug
                             -> StateT [Type] Q [Dec]
gen_standalone_deriving_decl cn tn st breaks cg debug = do
                       when debug $ do
                           traceM $ "standalone_deriving_decl: `" 
                              ++ show tn ++ "\' for class `"  ++ show cn ++ "\'"
                       (tvbs, cons) <- lift $ getTyVarCons tn
                       let typeNames = map getTVBName tvbs
                       isCnHighOrderClass <- lift $ isHigherOrderClass cn
                       -- prevent calling isInstance class with * -> * and type with *
                       if isCnHighOrderClass && null typeNames
                        then return []
                        else do
                           saturatedType <- lift $ foldl1' appT (conT tn : map varT typeNames)
                           instanceType <- if isCnHighOrderClass && (not . null) typeNames
                                                then let pns = init typeNames
                                                      in  if null pns
                                                            then lift $ conT tn
                                                            else lift $ foldl1' appT (conT tn : (map varT pns))
                                                else return saturatedType
                           -- Stop generating further instances
                           -- 1. it is already a member of that type class
                           -- 2. we have already generated it, which is kind of same with case 1
                           -- 3. for GHC.Generic, if it is a primitive type like Int, Double
                           -- 4. It will stop on the types in breaks
                           -- 5. It will stop on primitive types and Integer when deriving Typeable
                           isMember <- lift $ isInstance' cn [instanceType]
                           when debug $ do
                              traceM $ "standalone_deriving_decl: `" ++ show tn 
                                       ++ "\' is instance of `" ++ show cn ++ "\':"
                                       ++ show isMember
                           isPrimitive <-lift $ isInstance' ''Prim [saturatedType]
                           let isGeneric = ''Generic == cn
                           let isGeneric1 = ''Generic1 == cn
                           table <- get
                           if isMember || elem instanceType table || elem tn breaks ||
                              isPrimitive || (tn == ''Integer && (isGeneric || isGeneric1))
                              then return []
                              else do
                                 classContext <- if isCnHighOrderClass 
                                                   then return []
                                                   else lift $ cg  cn tn
                                 when debug $ do
                                    traceM $ "standalone_deriving_decl: `"
                                             ++ show tn ++ "\' context is " 
                                             ++ pprint classContext
                                 s <- lift $ reset_strategy tn st
                                 let decl = [StandaloneDerivD s classContext (AppT (ConT cn) instanceType)]
                                 modify (instanceType:)
                                 names <- lift $ fmap concat $ mapM getCompositeTypeNames cons
                                 names' <- lift $ filterM (\x -> isTypeFamily x >>= \b -> return $ not b) names
                                 xs <- mapM (\n -> gen_standalone_deriving_decl cn n st breaks cg debug) names'
                                 return $ concat xs ++ decl
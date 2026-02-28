-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown.InstanceDecGen
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown.InstanceDecGen (
    gen_instance_decl
) where

import           Control.Monad
import           Control.Monad.State
import           Data.Derive.TopDown.IsInstance
import           Data.Derive.TopDown.Lib
import           Data.Derive.TopDown.Types
import           Data.List                      ( foldl1' )
import           Data.Primitive.Types
import           Language.Haskell.TH
import           Debug.Trace

gen_instance_decl
  :: ClassName
  -> TypeName
  -> [TypeName]  -- ^ a list of types that breaks the generation process
  -> Maybe Overlap
  -> ContextGenerator -- ^ a context generator
  -> Bool             -- ^ is debug
  -> StateT [Type] Q [Dec]
gen_instance_decl cn tn breaks mo cg debug = do
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
      instanceType  <- if isCnHighOrderClass && (not . null) typeNames
        then
          let pns = init typeNames
          in  if null pns
                then lift $ conT tn
                else lift $ foldl1' appT (conT tn : (map varT pns))
        else return saturatedType
      isMember <- lift $ isInstance' cn [instanceType]
      when debug $ do
        traceM $ "standalone_deriving_decl: `" ++ show tn 
                ++ "\' is instance of `" ++ show cn ++ "\':"
                ++ show isMember
      table    <- get
      isPrimitive <-lift $ isInstance' ''Prim [saturatedType]
      if isMember || elem instanceType table || elem tn breaks || isPrimitive
         -- normally empty instance will not be used to derive Generic
         -- so I do not check Generic and Generic1
        then return []
        else do
          classContext <- if isCnHighOrderClass
            then return []
            else lift $ cg cn tn
          when debug $ do
            traceM $ "standalone_deriving_decl: `"
                        ++ show tn ++ "\' context is " 
                        ++ pprint classContext
          let decl =
                [InstanceD mo classContext (AppT (ConT cn) instanceType) []]
          modify (instanceType :)
          names  <- lift $ fmap concat $ mapM getCompositeTypeNames cons
          names' <- lift
            $ filterM (\x -> isTypeFamily x >>= \b -> return $ not b) names
          xs <- mapM (\n -> gen_instance_decl cn n breaks mo cg debug) names'
          return $ concat xs ++ decl
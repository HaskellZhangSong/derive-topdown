-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown.Instance
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown.Instance
  ( instance_
  , instance_with_breaks
  , instances
  , instancess
  , instance_with
  , instance_debug
  , instance_with_breaks_debug
  , instances_debug
  , instancess_debug
  , instance_with_debug
  ) where

import           Control.Monad.State
import           Data.Derive.TopDown.CxtGen     ( genInferredContext )
import           Data.Derive.TopDown.Types
import           Data.Derive.TopDown.InstanceDecGen
import           Language.Haskell.TH

instance_
  :: Name -- ^ class name
  -> Name -- ^ type name
  -> Q [Dec]
instance_ cn tn =
  evalStateT (gen_instance_decl cn tn [] Nothing genInferredContext False) []

instance_with_breaks
  :: Name -- ^ class name
  -> Name -- ^ type name
  -> [Name] -- ^ type names that stop the deriving process
  -> Q [Dec]
instance_with_breaks cn tn bs =
  evalStateT (gen_instance_decl cn tn bs Nothing genInferredContext False) []

instances
  :: [Name] -- ^ class names
  -> Name   -- ^ type name
  -> Q [Dec]
instances cns tn = fmap concat (mapM (\x -> instance_ x tn) cns)

instancess
  :: [Name] -- ^ class names
  -> [Name] -- ^ type names
  -> Q [Dec]
instancess cns tns = fmap concat (mapM (\x -> instances cns x) tns)

instance_with
  :: ClassName
  -> TypeName
  -> [TypeName]        -- ^ a list of types that breaks the generation process
  -> Maybe Overlap
  -> ContextGenerator -- ^ a context generator
  -> Q [Dec]
instance_with cn tn bs mo cg = evalStateT (gen_instance_decl cn tn bs mo cg False) []


instance_debug
  :: Name -- ^ class name
  -> Name -- ^ type name
  -> Q [Dec]
instance_debug cn tn =
  evalStateT (gen_instance_decl cn tn [] Nothing genInferredContext True) []

instance_with_breaks_debug
  :: Name -- ^ class name
  -> Name -- ^ type name
  -> [Name] -- ^ type names that stop the deriving process
  -> Q [Dec]
instance_with_breaks_debug cn tn bs =
  evalStateT (gen_instance_decl cn tn bs Nothing genInferredContext True) []

instances_debug
  :: [Name] -- ^ class names
  -> Name   -- ^ type name
  -> Q [Dec]
instances_debug cns tn = fmap concat (mapM (\x -> instance_ x tn) cns)

instancess_debug
  :: [Name] -- ^ class names
  -> [Name] -- ^ type names
  -> Q [Dec]
instancess_debug cns tns = fmap concat (mapM (\x -> instances cns x) tns)

instance_with_debug
  :: ClassName
  -> TypeName
  -> [TypeName]        -- ^ a list of types that breaks the generation process
  -> Maybe Overlap
  -> ContextGenerator -- ^ a context generator
  -> Q [Dec]
instance_with_debug cn tn bs mo cg = evalStateT (gen_instance_decl cn tn bs mo cg True) []
{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown.Standalone
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown.Standalone (
     deriving_
   , deriving_with_breaks
   , derivings
   , derivingss
   , deriving_with
   , deriving_debug
   , deriving_with_breaks_debug
   , derivings_debug
   , derivingss_debug
   , deriving_with_debug
#if __GLASGOW_HASKELL__ >= 802
   , strategy_deriving
   , strategy_derivings
   , strategy_derivingss
   , strategy_deriving_debug
   , strategy_derivings_debug
   , strategy_derivingss_debug
#endif
) 
where

import Data.Derive.TopDown.CxtGen (genInferredContext)
import Language.Haskell.TH
import Control.Monad.State
import Data.Derive.TopDown.Types
import Data.Derive.TopDown.StandaloneDecGen

deriving_ :: Name -- ^ class name
          -> Name -- ^ type name
          -> Q [Dec]
deriving_ cn tn = evalStateT (gen_standalone_deriving_decl cn tn Nothing [] genInferredContext False) []

{- | This is particularly useful with 'Generic' class.

For the types like 'Int', 'Char','Ratio' or other types which are not 'Generic', there must be a way to stop the generation process on those types.

However, the deriving topdown function will only stop generating 'Generic' instances on primitive types and 'Integer' by default, so you do not need to break on them manually.

Another circumtances might be deriving for 'Typeable' class. Since there is a bug in GHC, isInstance function in TH library is not working on 'Typeable', you can manually give the types which are already instances of 'Typeable' to stop the generation process.

For others cases, there is no need to use this function, bacause for a data type @A@ which is composited by another type, when you manually write an instance declaration for @A@, the process will stop on @A@ automatically since it is already an instance of the type class.
-}
deriving_with_breaks :: Name -- ^ class name
          -> Name -- ^ type name
          -> [Name] -- ^ type names that stop the deriving process
          -> Q [Dec]
deriving_with_breaks cn tn bs = evalStateT (gen_standalone_deriving_decl cn tn Nothing bs genInferredContext False) []

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

strategy_deriving st cn tn = evalStateT (gen_standalone_deriving_decl cn tn (Just st) [] genInferredContext False) []

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

{-| Context generator be the following 3 functions

 1. @genHoleContext@: It requires PartialTypeSignatures to make the context of deriving
    context a `hole' e.g. @_ => Cls (D a b)@. This case cannot handle type family
    since GHC cannot handle it

 2. @genInferredContext@: It will try to infer the context including cases with type families.

 3. @genAllFieldsContext@: It will put all fields into the context. It may generate like the followings

 @
 data List a = Nil | Cons a (List a)
 deriving instance (Show a, Show (List a)) => Show (List a)
 @
-}
deriving_with :: ClassName
              -> TypeName
              -> Maybe DerivStrategy -- ^ deriving strategy
              -> [TypeName]        -- ^ a list of types that breaks the generation process
              -> ContextGenerator -- ^ a context generator,  @genInferredContext@, @genHoleContext@ or @genAllFieldsContext@
              -> Q [Dec]
deriving_with cn tn st bs cg = evalStateT (gen_standalone_deriving_decl cn tn st bs cg False) []



deriving_debug :: Name -- ^ class name
          -> Name -- ^ type name
          -> Q [Dec]
deriving_debug cn tn = evalStateT (gen_standalone_deriving_decl cn tn Nothing [] genInferredContext True) []

{- | This is particularly useful with 'Generic' class.

For the types like 'Int', 'Char','Ratio' or other types which are not 'Generic', there must be a way to stop the generation process on those types.

However, the deriving topdown function will only stop generating 'Generic' instances on primitive types and 'Integer' by default, so you do not need to break on them manually.

Another circumtances might be deriving for 'Typeable' class. Since there is a bug in GHC, isInstance function in TH library is not working on 'Typeable', you can manually give the types which are already instances of 'Typeable' to stop the generation process.

For others cases, there is no need to use this function, bacause for a data type @A@ which is composited by another type, when you manually write an instance declaration for @A@, the process will stop on @A@ automatically since it is already an instance of the type class.
-}
deriving_with_breaks_debug :: Name -- ^ class name
          -> Name -- ^ type name
          -> [Name] -- ^ type names that stop the deriving process
          -> Q [Dec]
deriving_with_breaks_debug cn tn bs = evalStateT (gen_standalone_deriving_decl cn tn Nothing bs genInferredContext True) []

derivings_debug :: [Name] -- ^ class names
          -> Name   -- ^ type name
          -> Q [Dec]
derivings_debug cns tn = fmap concat (mapM (\x -> deriving_ x tn) cns)

derivingss_debug :: [Name] -- ^ class names
           -> [Name] -- ^ type names
           -> Q [Dec]
derivingss_debug cns tns = fmap concat (mapM (\x -> derivings cns x) tns)


#if __GLASGOW_HASKELL__ >= 802
strategy_deriving_debug :: DerivStrategy
                  -> Name
                  -> Name
                  -> Q [Dec]

strategy_deriving_debug st cn tn = evalStateT (gen_standalone_deriving_decl cn tn (Just st) [] genInferredContext True) []

strategy_derivings_debug :: DerivStrategy
                   -> [Name]
                   -> Name
                   -> Q [Dec]

strategy_derivings_debug st cns tn = fmap concat $ (mapM (\x -> strategy_deriving st x tn) cns)

strategy_derivingss_debug :: DerivStrategy
                    -> [Name]
                    -> [Name]
                    -> Q [Dec]
strategy_derivingss_debug st cns tns = fmap concat $ (mapM (\x -> strategy_derivings st cns x) tns)
#endif

{-| Context generator be the following 3 functions

 1. @genHoleContext@: It requires PartialTypeSignatures to make the context of deriving
    context a `hole' e.g. @_ => Cls (D a b)@. This case cannot handle type family
    since GHC cannot handle it

 2. @genInferredContext@: It will try to infer the context including cases with type families.

 3. @genAllFieldsContext@: It will put all fields into the context. It may generate like the followings

 @
 data List a = Nil | Cons a (List a)
 deriving instance (Show a, Show (List a)) => Show (List a)
 @
-}
deriving_with_debug :: ClassName
              -> TypeName
              -> Maybe DerivStrategy -- ^ deriving strategy
              -> [TypeName]        -- ^ a list of types that breaks the generation process
              -> ContextGenerator -- ^ a context generator,  @genInferredContext@, @genHoleContext@ or @genAllFieldsContext@
              -> Q [Dec]
deriving_with_debug cn tn st bs cg = evalStateT (gen_standalone_deriving_decl cn tn st bs cg True) []

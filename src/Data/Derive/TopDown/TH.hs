{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Derive.TopDown.TH (
   deriving_th
 , deriving_ths
 , deriving_thss
 , deriving_th_with_breaks) where
import Data.Derive.TopDown.Lib

import Language.Haskell.TH.Lib
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)

import Control.Monad.State
import Control.Monad.Trans
import Data.List (foldl')

genTH :: (ClassName, Name -> Q [Dec]) -> TypeName -> [TypeName] -> StateT [Type] Q [Dec]
genTH (className,deriveFunction) typeName breaks = do
                       (tvbs, cons) <- lift $ getTyVarCons typeName
                       compositeNames <- lift $ fmap concat $ mapM getCompositeTypeNames cons
                       let typeNames = map getTVBName tvbs
                       instanceType <- lift $ foldl' appT (conT typeName) $ map varT typeNames
                       isInstance <- lift $ isInstance' className [instanceType]
                       table <- get
                       if isInstance || elem instanceType table || elem typeName breaks
                          then return []
                          else do
                             decl <- lift $ deriveFunction typeName
                             (modify (instanceType :))
                             subTypeNames <- lift $ fmap concat $ mapM getCompositeTypeNames cons
                             decls <- mapM (\n -> genTH (className,deriveFunction) n breaks) subTypeNames
                             return $ concat decls ++ decl


deriving_th :: (Name, Name -> Q [Dec]) -- ^ class name and corresponding instance generation function
           -> Name -- ^ type name
           -> Q [Dec]
deriving_th cd tname = evalStateT (genTH cd tname []) []


deriving_th_with_breaks :: (Name, Name -> Q [Dec]) -- ^ class name and corresponding instance generation function
           -> Name -- ^ type name
           -> [Name] -- ^ type names that break generation
           -> Q [Dec]
deriving_th_with_breaks cd tname bs = evalStateT (genTH cd tname bs) []


deriving_ths :: [(Name, Name -> Q [Dec])] -- ^ class names and corresponding instance generation functions
            -> Name -- ^ type name
            -> Q [Dec]
deriving_ths cds typeName = fmap concat (mapM (\c -> deriving_th c typeName) cds)

deriving_thss :: [(Name, Name -> Q [Dec])] -- ^ class names and corresponding instance generation functions
             -> [Name] -- ^ type names
             -> Q [Dec]
deriving_thss cds typeNames = fmap concat (mapM (\t -> deriving_ths cds t) typeNames)

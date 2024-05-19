-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown (
   module Data.Derive.TopDown.Standalone
 , module Data.Derive.TopDown.Instance
 , module Data.Derive.TopDown.TH
 , module Data.Derive.TopDown.CxtGen
 , char, int, int8, int16, int32, int64, word, word8, word16, word32, word64
 , primitives, state, realWord
#if __GLASGOW_HASKELL__ >= 802
 , stock, anyclass, newtype_
 , DerivStrategy(StockStrategy, AnyclassStrategy, NewtypeStrategy)
#endif
)
where

#if __GLASGOW_HASKELL__ >= 802
import Language.Haskell.TH.Syntax
#endif
import Data.Derive.TopDown.Standalone
import Data.Derive.TopDown.Instance
import Data.Derive.TopDown.TH
import Data.Derive.TopDown.CxtGen

#if __GLASGOW_HASKELL__ >= 802
{-|
The name @sock@ and @anyclass@ are still allowed to be used as functions or arguments. 
See <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DerivingStrategies>
-}
stock, anyclass, newtype_ :: DerivStrategy
stock     = StockStrategy
anyclass  = AnyclassStrategy
newtype_  = NewtypeStrategy
#endif

-- | Primitive types with hashes that may cause to stop the generation process
char, int, int8, int16, int32, int64, word, word8, word16, word32, word64 :: Name
char   = Name (mkOccName "Char#")   (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
int    = Name (mkOccName "Int#")    (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
int8   = Name (mkOccName "Int8#")   (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
int16  = Name (mkOccName "Int16#")  (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
int32  = Name (mkOccName "Int32#")  (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
int64  = Name (mkOccName "Int64#")  (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
word   = Name (mkOccName "Word#")   (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
word8  = Name (mkOccName "Word8#")  (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
word16 = Name (mkOccName "Word16#") (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
word32 = Name (mkOccName "Word32#") (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
word64 = Name (mkOccName "Word64#") (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))

-- | list of @Name@: @[char, int, int8, int16, int32, int64, word, word8, word16, word32, word64]@
primitives :: [Name]
primitives = [char, int, int8, int16, int32, int64, word, word8, word16, word32, word64]

state :: Name
state  = Name (mkOccName "State#") (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))

realWord :: Name
realWord  = Name (mkOccName "RealWord#") (NameG TcClsName (mkPkgName "ghc-prim") (mkModName "GHC.Prim"))
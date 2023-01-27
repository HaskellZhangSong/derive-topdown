{-|

Module      : Data.Derive.TopDown
Description : Help Haskellers derive class instances for composited data types.
Copyright   : (c) songzh
License     : BSD3
Maintainer  : Haskell.Zhang.Song@hotmail.com
Stability   : experimental

Class dependencies can be complex sometimes, such as numeric and monadic classes. Making instances of them can be very tedious. Functoins in this module will help you derive the specified class instance with all the superclass instances of it.  For using this module, you may need to enable the following langauge extensions: @TemplateHaskell@, @StandaloneDeriving@, @DeriveGeneric@, @DeriveDataTypeable@, @GeneralizedNewtypeDeriving@, @DeriveAnyClass@

You may also need to enable GHC options @-ddump-splices@. 

For example:

> data A = A
> deriving_superclasses ''Ord ''A

You wil get:

>    deriving_superclasses ''Ord ''A
>  ======>
>    deriving instance Ord A
>    deriving instance Eq A

'Eq' is automatically derived when 'Ord' is derived, since 'Eq' is a superclass of 'Ord'

> newtype IO_ a = IO_ (IO a)
> strategy_deriving_superclasses newtype_ ''MonadIO ''IO_ 

You will get:

>    strategy_deriving_superclasses newtype_ ''MonadIO ''IO_
>  ======>
>    deriving newtype instance MonadIO IO_
>    deriving newtype instance Monad IO_
>    deriving newtype instance Applicative IO_
>    deriving newtype instance Functor IO_

Appearently, @Functor f => Applicative f => Monad f => MonadIO f@

> newtype F32 = F32 Float
> newtype_deriving_superclasses ''RealFloat ''F32

You will get:

>    newtype_deriving_superclasses ''RealFloat ''F32
>  ======>
>    deriving newtype instance RealFloat F32
>    deriving newtype instance RealFrac F32
>    deriving newtype instance Real F32
>    deriving newtype instance Num F32
>    deriving newtype instance Ord F32
>    deriving newtype instance Eq F32
>    deriving newtype instance Fractional F32
>    deriving newtype instance Floating F32

Some of these examples are from [#13368](https://ghc.haskell.org/trac/ghc/ticket/13368).
-}

module Data.Derive.Superclass 
       (deriving_superclasses,
#if __GLASGOW_HASKELL__ >= 802        
        strategy_deriving_superclasses,
        newtype_deriving_superclasses,
        gnds
#endif
        )where

import Data.Derive.TopDown.Lib
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Debug.Trace
import Control.Monad
import Data.List
import Control.Monad.Trans.State
import Control.Monad.Trans
import Data.Maybe
import Language.Haskell.TH.Ppr

isHigherOrderClass :: Name -> Q Bool
isHigherOrderClass ty = do
                    cla <- reify ty
                    case cla of
                        ClassI (ClassD _ _ vars _ _) _ -> do
#if __GLASGOW_HASKELL__ > 810
                                                    let (KindedTV _ _ k) = head vars
#else 
                                                    let (KindedTV _ k) = head vars
#endif
                                                    if k == StarT
                                                        then return True
                                                        else return False
                        _ -> error $ show ty ++ " is not a class"
                    


deriving_superclasses :: Name -> Name -> Q [Dec]
deriving_superclasses cn tn = do
                            a <- evalStateT (deriving_superclasses' 
#if __GLASGOW_HASKELL__ >= 802
                                             Nothing 
#endif                            
                                             cn tn) []
                            return a

#if __GLASGOW_HASKELL__ >= 802
strategy_deriving_superclasses :: DerivStrategy -> Name -> Name -> Q [Dec]
strategy_deriving_superclasses st cn tn = do
                            a <- evalStateT (deriving_superclasses' (Just st) cn tn) []
                            return a

-- |Use newtype strategy to derive all the superclass instances.
newtype_deriving_superclasses = strategy_deriving_superclasses NewtypeStrategy

-- |Abbreviation for @newtype_deriving_superclasses@.
gnds = newtype_deriving_superclasses
#endif

#if __GLASGOW_HASKELL__ >= 802
deriving_superclasses' :: Maybe DerivStrategy -> Name -> Name -> StateT [Type] Q [Dec]
deriving_superclasses' st cn tn = do
#else
deriving_superclasses' :: Name -> Name -> StateT [Type] Q [Dec]
deriving_superclasses' cn tn = do
#endif
                    (tvbs,cons) <- lift $ getTyVarCons tn
                    let tp = AppT (ConT cn) (ConT tn) 
                    types <- get
                    isCnHighOrderClass <- lift $ isHigherOrderClass cn
                    classContext <- if isCnHighOrderClass
                                        then lift $ generateClassContext cn tn
                                        else return []
                    let typeNames = map getTVBName tvbs
                    isIns <- lift $ isInstance' cn [ConT tn]
                    if (isIns || elem tp types)
                        then return []
                        else
                            do
                            topClassInstance <- return [StandaloneDerivD 
#if __GLASGOW_HASKELL__ >= 802
                                                            st
#endif
                                                            classContext tp]

                            modify (tp:)
                            ci <- lift $ reify cn
                            case ci of
                                ClassI (ClassD ctx _ _ _ _) _ -> do
                                                    let classConTs = map getTypeConstructor ctx
                                                    ss <- fmap (nub.concat) $ forM classConTs $ \(ConT className) -> do
                                                                                    superclass_decls <- deriving_superclasses' 
#if __GLASGOW_HASKELL__ >= 802
                                                                                                            st
#endif
                                                                                                            className tn
                                                                                    return superclass_decls
                                                    return $ topClassInstance ++ ss
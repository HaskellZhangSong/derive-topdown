{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Derive.TopDown.SYB where

import Data.Data
import Data.Typeable
import Data.Generics.Aliases
import Data.List

everythingMBut :: forall r m. Monad m => (m r -> m r -> m r)
                          -> GenericQ (m (r, Bool))
                          -> GenericQ (m r)
everythingMBut k f = go
      where
            go :: forall a. Data a => a -> m r
            go x = do 
                (res, stop) <- f x
                if stop 
                    then return res
                    else do 
                      let ls = gmapQ go x :: [m r]
                      foldl' k (return res) ls

{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TypeFamilies,RankNTypes, GADTs #-}
module Main where

import System.Exit (exitFailure, exitSuccess)
import Data.Derive.TopDown.CxtGen
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Test.HUnit
import Control.Monad
import Data.Proxy

type family XLit a
-- Normal types
data T1 a = T1 [a]
data T2 a b = T2 (b,b)
data T3 a = T3 (Either (Maybe a) Int) | T31 (Proxy a) | T32 (XLit a)
data T4 a = T4 (Either (XLit (Maybe a)) Int)
data T41 a = T41 (Either (XLit (Maybe a)) Int) | T42 Int | T43 (XLit a)
data T5 a = T5 (Either a Int) (Proxy a) (XLit a)
data T6 a = T6 (forall a. a)
data T7 a b = T7 (a, b)
data T8 a b = T8 (Proxy a, b)

$( return [] ) 
normalTests = [assertEqual "T1" [R] $(roleListToExp ''T1)
              ,assertEqual "T2" [P, R] $(roleListToExp ''T2)
              ,assertEqual "T3" [R] $(roleListToExp ''T3)
              ,assertEqual "T4" [N] $(roleListToExp ''T4)
              ,assertEqual "T5" [R] $(roleListToExp ''T5)
              ,assertEqual "T6" [P] $(roleListToExp ''T6)
              ,assertEqual "T7" [R,R] $(roleListToExp ''T7)
              ,assertEqual "T8" [P,R] $(roleListToExp ''T8)
              ]

-- Gadt types

data G1 k a b where
  G1 :: a -> k b -> G1 k a b

data G2 k a b where
  G2 :: a -> (Proxy b) -> G2 k a b

data G3 k a b where
  G3 :: XLit a -> (Proxy a) -> b -> (Proxy b) -> G3 k a b


$( return [] ) 
gadtTests = [
             assertEqual "G1" [N,R,N] $(roleListToExp ''G1)
            ,assertEqual "G2" [P,R,P] $(roleListToExp ''G2)
            ,assertEqual "G3" [P,N,R] $(roleListToExp ''G3)]

-- RankN types


main :: IO () 
main = do
    runTestTT $ TestList $ map TestCase normalTests
    runTestTT $ TestList $ map TestCase gadtTests
    return () -- exitSuccess
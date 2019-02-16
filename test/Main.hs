{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Main
  ( main
  )
where

import Vending
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Control.State.Transition
  ( TRC (TRC)
  , IRC (IRC)
  --, TransitionRule (..)
  --, judgmentContext
  --, trans
  , applySTS
  )

main = defaultMain $ vmtests

vmtests :: TestTree
vmtests = testGroup "Vending Machine STS Tests" [unitTests]

properties :: TestTree
properties = undefined


unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
       testCase "Test Initial State" $
            (case applySTS @VMACHINE $ IRC (VEnv True 1) of
                Left error -> VState 0 0
                Right state -> state)
            @?= 
            VState 0 initNumOfSodas

       ,testCase "Test Deposit" $
            (case applySTS @VMACHINE $ TRC (VEnv True 1, VState 0 1, Deposit 2) of
                Left error -> VState 0 0
                Right state -> state)
            @?= 
            VState 2 1

       ,testCase "Test Get Soda" $
            (case applySTS @VMACHINE $ TRC (VEnv True 1, VState 1 1, Push) of
                Left error -> VState 0 initNumOfSodas
                Right state -> state)
            @?= 
            VState 0 0

       ,testCase "Test Small Deposit" $
            (case applySTS @VMACHINE $ TRC (VEnv True 1, VState 0 1, Push) of
                Left error -> error
                Right state -> [])
            @?= 
            [SmallDeposit]

       ,testCase "Test Out Of Soda" $
            (case applySTS @VMACHINE $ TRC (VEnv True 1, VState 1 0, Push) of
                Left error -> error
                Right state -> [])
            @?= 
            [OutOfSoda]

       ,testCase "Test Small Deposit and No Soda" $
            (case applySTS @VMACHINE $ TRC (VEnv True 1, VState 0 0, Push) of
                Left error -> error
                Right state -> [])
            @?= 
            [OutOfSoda, SmallDeposit]

    ]

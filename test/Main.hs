{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Main
  ( main
  )
where

import Vending
import Test.Tasty
import Test.Tasty.HUnit

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
            (applySTS @VMACHINE $ IRC (VEnv True 1))
            @?= 
            (Right (DState 0 initNumOfSodas))

       ,testCase "Test Deposit" $
            (applySTS @VMACHINE $ TRC (VEnv True 1, DState 0 1, VSignal False 2))
            @?= 
            (Right (DState 2 1))

       ,testCase "Test Get Soda" $
            (applySTS @VMACHINE $ TRC (VEnv True 1, DState 1 1, VSignal True 0))
            @?= 
            (Right (DState 0 0))

       ,testCase "Test Small Deposit" $
            (applySTS @VMACHINE $ TRC (VEnv True 1, DState 0 1, VSignal True 0))
            @?= 
            (Left [[NotDeposit], [DeductFailure SmallDeposit]])

       ,testCase "Test Out Of Soda" $
            (applySTS @VMACHINE $ TRC (VEnv True 1, DState 1 0, VSignal True 0))
            @?= 
            (Left [[NotDeposit], [DeductFailure OutOfSoda]])

       ,testCase "Test Small Deposit and No Soda" $
            (applySTS @VMACHINE $ TRC (VEnv True 1, DState 0 0, VSignal True 0))
            @?= 
            (Left [[NotDeposit], [DeductFailure SmallDeposit, DeductFailure OutOfSoda]])

       ,testCase "Test Small Deposit and Out of Order" $
            (applySTS @VMACHINE $ TRC (VEnv False 1, DState 0 1, VSignal True 0))
            @?= 
            (Left [[OutOfOrder, NotDeposit], [DeductFailure SmallDeposit, OutOfOrder]])
    ]

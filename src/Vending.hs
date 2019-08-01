  {-#  LANGUAGE TypeSynonymInstances  #-}
  {-#  LANGUAGE FlexibleInstances  #-}
  {-#  LANGUAGE TypeFamilies  #-}
 
  module Vending where
 
 
  import Control.State.Transition
    ( Environment
    , PredicateFailure
    , STS
    , Signal
    , State
    , TransitionRule
    , initialRules
    , transitionRules
    , TRC (TRC)
    , judgmentContext
    , (?!)
    )
 
  initNumOfSodas :: Int
  initNumOfSodas = 100
  --  VEND Transition Types
 
  data VEnv=VEnv
    {  power ::  Bool
    ,  cost ::  Int
    } deriving (Eq, Show)
 
  data VState=VState
    {  tokens ::  Int
    ,  sodas ::  Int
    } deriving (Eq, Show)
 
  data VSignal=VSignal
    {  vend ::  Bool
    ,  amnt ::  Int
    } deriving (Eq, Show)
 
      
  data VMACHINE
  instance STS VMACHINE where
      type Environment VMACHINE = VEnv
      type State VMACHINE = VState
      type Signal VMACHINE = VSignal
 
      data PredicateFailure VMACHINE
        = SmallDeposit
        | OutOfSoda
        | OutOfOrder
        | NotPush
        | NotDeposit
        | NonZeroAmnt
        deriving (Eq, Show)
 
      initialRules = [return $ VState 0 initNumOfSodas ]
 
      transitionRules = [vendDeposit, vendPush]
  vendDeposit :: TransitionRule VMACHINE
  vendDeposit = do
    TRC (VEnv p _, VState t sd, sig) <- judgmentContext
    let VSignal vend amnt= sig
    vend  == False?! NotDeposit
    p  == True?! OutOfOrder
    return $        VState (t+amnt) sd
  vendPush :: TransitionRule VMACHINE
  vendPush = do
    TRC (VEnv p c, VState t sd, sig) <- judgmentContext
    let VSignal vend amnt= sig
    amnt  == 0?! NonZeroAmnt
    p  == True?! OutOfOrder
    c  <= t?! SmallDeposit
    sd  > 0?! OutOfSoda
    return $        VState (t-c) (sd-1)

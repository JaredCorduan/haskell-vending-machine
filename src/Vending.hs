  {-#  LANGUAGE EmptyDataDecls  #-}
  {-#  LANGUAGE MultiParamTypeClasses  #-}
  {-#  LANGUAGE ScopedTypeVariables  #-}
  {-#  LANGUAGE TypeApplications  #-}
  {-#  LANGUAGE TypeFamilies  #-}
 
  module Vending where
 
 
  import Control.State.Transition
  --  DEDUCT Transition Types
 
  data DEnv=DEnv
    {  _dcost ::  Int
    } deriving (Eq, Show)
 
  data DState=DState
    {  _tokens ::  Int
    ,  _sodas ::  Int
    } deriving (Eq, Show)
  data DEDUCT
  instance STS DEDUCT where
      type Environment DEDUCT = DEnv
      type State DEDUCT = DState
      type Signal DEDUCT = ()
 
      data PredicateFailure DEDUCT
        = SmallDeposit
        | OutOfSoda
        deriving (Eq, Show)
 
      initialRules = []
      transitionRules = [deduct]
 
  deduct :: TransitionRule DEDUCT
  deduct = do
    TRC (DEnv c, DState t sd, _) <- judgmentContext
    c   <= t?! SmallDeposit
    sd   > 0?! OutOfSoda
    return $       DState  (t-c)  (sd-1)
  initNumOfSodas :: Int
  initNumOfSodas = 100
  --  VEND Transition Types
 
  data VEnv=VEnv
    {  _power ::  Bool
    ,  _cost ::  Int
    } deriving (Eq, Show)
 
  data VSignal=VSignal
    {  _vend ::  Bool
    ,  _amnt ::  Int
    } deriving (Eq, Show)
  data VMACHINE
  instance STS VMACHINE where
      type Environment VMACHINE = VEnv
      type State VMACHINE = DState
      type Signal VMACHINE = VSignal
 
      data PredicateFailure VMACHINE
        = OutOfOrder
        | NotPush
        | NotDeposit
        | NonZeroAmnt
        | DeductFailure (PredicateFailure DEDUCT)
        deriving (Eq, Show)
 
      initialRules = [return $ DState 0 initNumOfSodas ]
 
      transitionRules = [vendDeposit, vendPush]
 
  instance Embed DEDUCT VMACHINE where
    wrapFailed = DeductFailure
  vendDeposit :: TransitionRule VMACHINE
  vendDeposit = do
    TRC (VEnv p _, DState t sd, sig) <- judgmentContext
    let VSignal vend amnt= sig
    vend  == False?! NotDeposit
    p  == True?! OutOfOrder
    return $        DState  (t+amnt)  sd
  vendPush :: TransitionRule VMACHINE
  vendPush = do
    TRC (VEnv p c, DState t sd, sig) <- judgmentContext
    let VSignal vend amnt= sig
    amnt  == 0?! NonZeroAmnt
    p  == True?! OutOfOrder
    DState          t'  sd' <- trans @DEDUCT $ TRC (DEnv c,DState t sd,())
    return $        DState  t'  sd'

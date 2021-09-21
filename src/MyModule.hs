{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -Wall #-}

module MyModule where

import           Ledger
import           Ledger.Constraints.TxConstraints
import           Plutus.Contract.StateMachine
import           Plutus.V1.Ledger.Value
import           PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                        (Show (show) )
import Plutus.Contract
import qualified Ledger.Typed.Scripts as Scripts

import Types
import Data.Text (Text)
import Data.String
import Ledger.Typed.Tx
import Plutus.Contract.Types (Promise(Promise))
import Data.Monoid (Last(Last))

-- NOTES
--
-- Questions
--
-- * [ ] When staring the script, what stops us from sending arbitrary
--       currencies to the script? The script validator is not run on the
--       initial transaction, so it's up to the transaction creator to ensure
--       there is a valid initial state, else funds might be locked forever.
--       Does the StateMachine stuff ensure Value is initially 0?
--

--------------------------------------------------------------------------------
-- On Chain
--------------------------------------------------------------------------------

{-# INLINABLE loanValidator #-}
loanValidator
  :: (Maybe ThreadToken, LoanParams)
  -> Loan
  -> LoanAction
  -> ScriptContext
  -> Bool
loanValidator =
  mkValidator . uncurry mkLoanStateMachine

-- | A loan that will must be repaid and has a fixed interest amount.
{-# INLINABLE mkLoanStateMachine #-}
mkLoanStateMachine
  :: Maybe ThreadToken -> LoanParams -> StateMachine Loan LoanAction
mkLoanStateMachine threadTokenMay LoanParams {..} = mkStateMachine
  threadTokenMay
  (\(State st scriptValue) action -> case st of
    LoanOffer -> case action of
      Rescind -> Just
        ( mustBeSignedBy lender <> mustPayToPubKey lender scriptValue
        , State LoanComplete mempty
        )
      Accept -> Just
        ( mustBeSignedBy borrower <> mustPayToPubKey borrower scriptValue
        , State (LoanActive (scriptValue <> interest)) mempty
        )
      Repay{}   -> Nothing
      Forgive{} -> Nothing
    LoanActive balance -> case action of
      Rescind -> Nothing
      Accept  -> Nothing
      Repay repayValue
        | Just newBalance <- repayBalance balance repayValue -> Just
          ( -- mustBeSignedBy borrower <> -- Let's let anyone pay!
            mustPayToPubKey lender repayValue
          , State
            (if isZero newBalance then LoanComplete else LoanActive newBalance)
            mempty
          )
        | otherwise -> Nothing
      Forgive forgiveValue
        | Just newBalance <- repayBalance balance forgiveValue -> Just
          (mempty, State (LoanActive newBalance) mempty)
        | otherwise -> Nothing
    LoanComplete  -> Nothing
    LoanRescinded -> Nothing
  )
  (\case
    LoanComplete  -> True
    LoanRescinded -> True
    _             -> False
  )

repayBalance :: Value -> Value -> Maybe Value
repayBalance loanBalance repayment
  | allPositive repayment && not (isZero repayment) && allPositive newBalance
  = Just newBalance
  | otherwise
  = Nothing
  where newBalance = loanBalance <> inv repayment

-- | Check that all currencies' values are >= 0
allPositive :: Value -> Bool
allPositive (Value v) = all (all (>= 0)) v

loanTypedValidator :: Maybe ThreadToken -> LoanParams -> Scripts.TypedValidator (StateMachine Loan LoanAction)
loanTypedValidator threadTokenMay loanParams = Scripts.mkTypedValidator @(StateMachine Loan LoanAction)
    ($$(PlutusTx.compile [|| loanValidator ||])
      `applyCode` liftCode (threadTokenMay, loanParams)
    )
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @Loan @LoanAction

--------------------------------------------------------------------------------
-- Off Chain
--------------------------------------------------------------------------------

type LoanSchema =
      Endpoint "Offer"   Value
  .\/ Endpoint "Rescind" ()
  .\/ Endpoint "Accept"  ()
  .\/ Endpoint "Repay"   Value
  .\/ Endpoint "Forgive" Value

loanStateMachineInstance :: LoanParams -> Maybe ThreadToken -> StateMachineInstance Loan LoanAction
loanStateMachineInstance loanParams threadTokenMay = StateMachineInstance
          (mkLoanStateMachine threadTokenMay loanParams)
          (loanTypedValidator threadTokenMay loanParams)

loanClient :: LoanParams -> Maybe ThreadToken -> StateMachineClient Loan LoanAction
loanClient loanParams threadTokenMay = mkStateMachineClient (loanStateMachineInstance loanParams threadTokenMay)

lenderContract :: LoanParams -> Contract (Last ThreadToken) LoanSchema Text ()
lenderContract loanParams = do
  threadToken <- mapErr' getThreadToken
  tell (Last (Just threadToken))
  let client = loanClient loanParams (Just threadToken)
  awaitPromise $ endpoint @"Offer" $ \loanValue -> do
    logInfo $ "Offering loan of value: " ++ Prelude.show loanValue
    go client =<< mapErr' (runInitialise client LoanOffer loanValue)
    return ()
  return ()
  where
    go :: StateMachineClient Loan LoanAction -> Loan -> Contract a LoanSchema Text ()
    go client st = case st of
      LoanOffer -> selectList
        [ -- Wait for offer acceptance
          waitForUpdate'
        , -- Allow rescind the offer
          endpoint @"Rescind" $ \_ -> runStep' Rescind
        ]
      LoanActive _ -> selectList
        [ -- Wait for repayment
          waitForUpdate'
        , -- Allow forgiving the loan
          endpoint @"Forgive" $ \forgiveValue -> runStep' (Forgive forgiveValue)
        ]
      LoanComplete -> logInfo "Loan completed"
      LoanRescinded -> logInfo "Loan offer rescinded"
      where
        runStep' :: LoanAction -> Contract a LoanSchema Text ()
        runStep' i = do
          r <- mapErr' $ runStep client i
          case r of
            TransitionFailure _err -> do
              logError "TransitionFailure"
              error ()
            TransitionSuccess st' -> go client st'

        waitForUpdate' = Promise $ do
          st'May <- mapErr' $ waitForUpdate client
          case st'May of
            Nothing -> logError "Contract instance was unexpectedly terminated"
            Just (OnChainState _ (TypedScriptTxOutRef _ (TypedScriptTxOut _ st')) _)
              -> go client st'

borrowerContract :: LoanParams -> Maybe ThreadToken -> Contract () LoanSchema Text ()
borrowerContract loanParams threadTokenMay = do
  let client = loanClient loanParams threadTokenMay
  onChainStateMay <- mapErr' $ getOnChainState client
  case onChainStateMay of
    Nothing -> do
      logError "No loan offer found"
      error ()
    Just (OnChainState _ (TypedScriptTxOutRef _ (TypedScriptTxOut _ st)) _, _) -> do
      go client st
  where
    go :: StateMachineClient Loan LoanAction -> Loan -> Contract () LoanSchema Text ()
    go client st = case st of
      LoanOffer -> selectList
        [ -- Accept offer
          endpoint @"Accept" $ \_ -> runStep' Accept
        , -- Wait for rescind offer
          waitForUpdate'
        ]
      LoanActive _ -> selectList
        [ -- Repayment
          endpoint @"Repay" $ \repayValue -> runStep' (Repay repayValue)
        , -- Wait for forgiving the loan
          waitForUpdate'
        ]
      LoanComplete -> logInfo "Loan completed"
      LoanRescinded -> logInfo "Loan offer rescinded"
      where
        runStep' :: LoanAction -> Contract () LoanSchema Text ()
        runStep' i = do
          r <- mapErr' $ runStep client i
          case r of
            TransitionFailure _err -> do
              logError "TransitionFailure"
              error ()
            TransitionSuccess st' -> go client st'

        waitForUpdate' = Promise $ do
          st'May <- mapErr' $ waitForUpdate client
          case st'May of
            Nothing -> logError "Contract instance was unexpectedly terminated"
            Just (OnChainState _ (TypedScriptTxOutRef _ (TypedScriptTxOut _ st')) _)
              -> go client st'

mapErr' :: IsString str => Contract w s SMContractError a -> Contract w s str a
mapErr' = mapError (fromString . Prelude.show)

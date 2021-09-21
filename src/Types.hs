{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Types where

import           GHC.Generics
import           Ledger
import           PlutusTx
import           Prelude                        ( )

data LoanParams = LoanParams
  { interest :: Value  -- ^ Total interest value on the loan
  , lender   :: PubKeyHash
  , borrower :: PubKeyHash
  }

makeLift ''LoanParams

data Loan
  = LoanOffer
  | LoanActive Value -- ^ Amount left to repay (including interest)
  | LoanComplete
  | LoanRescinded
  deriving (Generic)

unstableMakeIsData ''Loan
makeLift ''Loan

data LoanAction
  = Rescind
  -- | Repay some of the loan
  | Accept
  | Repay Value
  -- | Forgive some or all of the loan balance
  | Forgive Value

unstableMakeIsData ''LoanAction

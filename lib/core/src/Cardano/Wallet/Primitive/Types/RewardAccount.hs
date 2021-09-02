{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- This module provides types related to delegation of thee wallet's stake to
-- pools.
--
module Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)

    -- * Certificates for reward accounts
    , DelegationCertificate (..)
    , dlgCertAccount
    , dlgCertPoolId

    -- * Stake key management
    , StakeKeyCertificate (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.StakePools
    ( PoolId )
import Control.DeepSeq
    ( NFData (..) )
import Data.ByteString
    ( ByteString )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )
import Quiet
    ( Quiet (..) )

-- | A reward account is used in group-type addresses for delegation.
--
-- It is the hash of the public key corresponding to the account address.
--
newtype RewardAccount = RewardAccount { unRewardAccount :: ByteString }
    deriving (Generic, Eq, Ord)
    deriving Show via (Quiet RewardAccount)

instance NFData RewardAccount

instance Buildable RewardAccount where
    build = build . Hash @"RewardAccount" . unRewardAccount

instance ToText RewardAccount where
    toText = toText . Hash @"RewardAccount" . unRewardAccount

instance FromText RewardAccount where
    fromText = fmap (RewardAccount . getHash @"RewardAccount") . fromText

{-------------------------------------------------------------------------------
                             Stake Pool Delegation
-------------------------------------------------------------------------------}

-- | Represents a delegation certificate.
data DelegationCertificate
    = CertDelegateNone RewardAccount
    -- ^ Deregister stake key
    | CertDelegateFull RewardAccount PoolId
    -- ^ Delegate to stake pool
    | CertRegisterKey RewardAccount
    -- ^ Register stake key
    deriving (Generic, Show, Eq, Ord)

instance NFData DelegationCertificate

dlgCertAccount :: DelegationCertificate -> RewardAccount
dlgCertAccount = \case
    CertDelegateNone acc -> acc
    CertDelegateFull acc _ -> acc
    CertRegisterKey acc -> acc

dlgCertPoolId :: DelegationCertificate -> Maybe PoolId
dlgCertPoolId = \case
    CertDelegateNone{} -> Nothing
    CertDelegateFull _ poolId -> Just poolId
    CertRegisterKey _ -> Nothing

{-------------------------------------------------------------------------------
                              Stake Key Management
-------------------------------------------------------------------------------}

-- | A property of a stake key belonging to the wallet, indicating whether it is
-- registered or not.
data StakeKeyCertificate
    = StakeKeyRegistration
    | StakeKeyDeregistration
    deriving (Generic, Show, Read, Eq)

instance NFData StakeKeyCertificate

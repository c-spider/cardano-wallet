{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module provides a high-level interface for coin selection.
--
-- It handles the following responsibilities:
--
--  - selecting inputs from the UTxO set to pay for user-specified outputs;
--  - selecting inputs from the UTxO set to pay for collateral;
--  - producing change outputs to return excess value to the wallet;
--  - balancing a selection to pay for the transaction fee.
--
-- Use the 'performSelection' function to perform a coin selection.
--
module Cardano.Wallet.Primitive.CoinSelection
    ( performSelection
    , PerformSelection
    , SelectionConstraints (..)
    , SelectionParams (..)
    , SelectionError (..)

    , accountForExistingInputs

    , prepareOutputs
    , ErrPrepareOutputs (..)
    , ErrOutputTokenBundleSizeExceedsLimit (..)
    , ErrOutputTokenQuantityExceedsLimit (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionCriteria (..)
    , SelectionLimit
    , SelectionResult
    , SelectionSkeleton
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    , TxIn
    , TxOut
    , txOutMaxTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Control.Monad.Random.Class
    ( MonadRandom )
import Data.Bifunctor
    ( first )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Word
    ( Word16 )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Wallet.Primitive.CoinSelection.Balance as Balance
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty.Extra as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Performs a coin selection.
--
-- This function has the following responsibilities:
--
--  - selecting inputs from the UTxO set to pay for user-specified outputs;
--  - selecting inputs from the UTxO set to pay for collateral;
--  - producing change outputs to return excess value to the wallet;
--  - balancing a selection to pay for the transaction fee.
--
performSelection
    :: (HasCallStack, MonadRandom m) => PerformSelection m TokenBundle
performSelection =
    accountForExistingInputs
        performSelectionInner

performSelectionInner
    :: (HasCallStack, MonadRandom m) => PerformSelection m TokenBundle
performSelectionInner selectionConstraints selectionParams =
    -- TODO:
    --
    -- https://jira.iohk.io/browse/ADP-1037
    -- Adjust coin selection and fee estimation to handle collateral inputs
    --
    -- https://jira.iohk.io/browse/ADP-1070
    -- Adjust coin selection and fee estimation to handle pre-existing inputs
    --
    case prepareOutputs selectionConstraints outputsToCover of
        Left e ->
            pure $ Left $ SelectionOutputsError e
        Right preparedOutputsToCover ->
            first SelectionBalanceError <$> Balance.performSelection
                computeMinimumAdaQuantity
                computeMinimumCost
                assessTokenBundleSize
                SelectionCriteria
                    { assetsToBurn
                    , assetsToMint
                    , extraCoinSource = rewardWithdrawal
                    , outputsToCover = preparedOutputsToCover
                    , selectionLimit =
                        computeSelectionLimit $ F.toList preparedOutputsToCover
                    , utxoAvailable
                    }
  where
    SelectionConstraints
        { assessTokenBundleSize
        , computeMinimumAdaQuantity
        , computeMinimumCost
        , computeSelectionLimit
        } = selectionConstraints
    SelectionParams
        { assetsToBurn
        , assetsToMint
        , outputsToCover
        , rewardWithdrawal
        , utxoAvailable
        } = selectionParams

type PerformSelection m change =
    SelectionConstraints ->
    SelectionParams ->
    m (Either SelectionError (SelectionResult change))

-- | Specifies all constraints required for coin selection.
--
-- Selection constraints:
--
--    - place limits on the coin selection algorithm, enabling it to produce
--      selections that are acceptable to the ledger.
--
--    - are dependent on the current set of protocol parameters.
--
--    - are not specific to a given selection.
--
data SelectionConstraints = SelectionConstraints
    { assessTokenBundleSize
        :: TokenBundleSizeAssessor
        -- ^ Assesses the size of a token bundle relative to the upper limit of
        -- what can be included in a transaction output. See documentation for
        -- the 'TokenBundleSizeAssessor' type to learn about the expected
        -- properties of this field.
    , computeMinimumAdaQuantity
        :: TokenMap -> Coin
        -- ^ Computes the minimum ada quantity required for a given output.
    , computeMinimumCost
        :: SelectionSkeleton -> Coin
        -- ^ Computes the minimum cost of a given selection skeleton.
    , computeSelectionLimit
        :: [TxOut] -> SelectionLimit
        -- ^ Computes an upper bound for the number of ordinary inputs to
        -- select, given a current set of outputs.
    , maximumCollateralInputCount
        :: Word16
        -- ^ Specifies an inclusive upper bound on the number of unique inputs
        -- that can be selected as collateral.
    }
    deriving Generic

-- | Specifies all parameters that are specific to a given selection.
--
data SelectionParams = SelectionParams
    { assetsToBurn
        :: TokenMap
        -- ^ Specifies a set of assets to burn.
    , assetsToMint
        :: TokenMap
        -- ^ Specifies a set of assets to mint.
    , existingInputs
        :: UTxO
        -- ^ Specifies a set of existing inputs to include.
    , outputsToCover
        :: NonEmpty TxOut
        -- ^ Specifies a set of outputs that must be paid for.
    , rewardWithdrawal
        :: Maybe Coin
        -- ^ Specifies the value of a withdrawal from a reward account.
    , utxoAvailable
        :: UTxOIndex
        -- ^ Specifies the set of all available UTxO entries. The algorithm
        -- will choose entries from this set when selecting ordinary inputs
        -- and collateral inputs.
    }
    deriving (Eq, Generic, Show)

-- | Indicates that an error occurred while performing a coin selection.
--
data SelectionError
    = SelectionBalanceError Balance.SelectionError
    | SelectionOutputsError ErrPrepareOutputs
    deriving (Eq, Show)

accountForExistingInputs
    :: Functor m
    => PerformSelection m change
    -> PerformSelection m change
accountForExistingInputs performSelectionFn constraints params =
    fmap modifyResult <$> performSelectionFn
        (modifyConstraints constraints)
        (modifyParams params)
  where
    modifyConstraints :: SelectionConstraints -> SelectionConstraints
    modifyConstraints
        = over #computeMinimumCost
            modifyComputeMinimumCost
        . over #computeSelectionLimit
            modifyComputeSelectionLimit
      where
        modifyComputeMinimumCost
            :: (SelectionSkeleton -> Coin)
            -> (SelectionSkeleton -> Coin)
        modifyComputeMinimumCost = (. modifySkeleton)
          where
            modifySkeleton :: SelectionSkeleton -> SelectionSkeleton
            modifySkeleton =
                over #skeletonInputCount (+ UTxO.size existingInputs)

        modifyComputeSelectionLimit
            :: ([TxOut] -> SelectionLimit)
            -> ([TxOut] -> SelectionLimit)
        modifyComputeSelectionLimit = (modifySelectionLimit .)
          where
            modifySelectionLimit :: SelectionLimit -> SelectionLimit
            modifySelectionLimit = fmap $ subtract $ UTxO.size existingInputs

    modifyParams :: SelectionParams -> SelectionParams
    modifyParams
        = over #assetsToMint
            modifyAssetsToMint
        . over #existingInputs
            modifyExistingInputs
        . over #rewardWithdrawal
            modifyRewardWithdrawal
        . over #utxoAvailable
            modifyUTxOAvailable
      where
        modifyAssetsToMint :: TokenMap -> TokenMap
        modifyAssetsToMint = TokenMap.add (view #tokens existingInputValue)

        modifyExistingInputs :: UTxO -> UTxO
        modifyExistingInputs = const UTxO.empty

        modifyRewardWithdrawal :: Maybe Coin -> Maybe Coin
        modifyRewardWithdrawal =
            Just . (view #coin existingInputValue <>) . fromMaybe mempty

        modifyUTxOAvailable :: UTxOIndex -> UTxOIndex
        modifyUTxOAvailable = UTxOIndex.deleteMany $ UTxO.dom existingInputs

    modifyResult :: SelectionResult change -> SelectionResult change
    modifyResult
        = over #inputsSelected
            modifyInputsSelected
        . over #extraCoinSource
            modifyExtraCoinSource
      where
        modifyInputsSelected
            :: (NonEmpty (TxIn, TxOut)) -> (NonEmpty (TxIn, TxOut))
        modifyInputsSelected =
            (NE.appendr $ Map.toList $ unUTxO existingInputs)

        modifyExtraCoinSource :: Maybe Coin -> Maybe Coin
        modifyExtraCoinSource = const (view #rewardWithdrawal params)

    SelectionParams {existingInputs} = params

    existingInputValue :: TokenBundle
    existingInputValue = existingInputs
        & unUTxO
        & fmap (view #tokens)
        & F.fold

-- | Prepares the given user-specified outputs, ensuring that they are valid.
--
prepareOutputs
    :: SelectionConstraints
    -> NonEmpty TxOut
    -> Either ErrPrepareOutputs (NonEmpty TxOut)
prepareOutputs constraints outputsUnprepared
    | (address, assetCount) : _ <- excessivelyLargeBundles =
        Left $
            -- We encountered one or more excessively large token bundles.
            -- Just report the first such bundle:
            ErrPrepareOutputsTokenBundleSizeExceedsLimit $
            ErrOutputTokenBundleSizeExceedsLimit {address, assetCount}
    | (address, asset, quantity) : _ <- excessiveTokenQuantities =
        Left $
            -- We encountered one or more excessive token quantities.
            -- Just report the first such quantity:
            ErrPrepareOutputsTokenQuantityExceedsLimit $
            ErrOutputTokenQuantityExceedsLimit
                { address
                , asset
                , quantity
                , quantityMaxBound = txOutMaxTokenQuantity
                }
    | otherwise =
        pure outputsToCover
  where
    SelectionConstraints
        { assessTokenBundleSize
        , computeMinimumAdaQuantity
        } = constraints

    -- The complete list of token bundles whose serialized lengths are greater
    -- than the limit of what is allowed in a transaction output:
    excessivelyLargeBundles :: [(Address, Int)]
    excessivelyLargeBundles =
        [ (address, assetCount)
        | output <- F.toList outputsToCover
        , let bundle = view #tokens output
        , bundleIsExcessivelyLarge bundle
        , let address = view #address output
        , let assetCount = Set.size $ TokenBundle.getAssets bundle
        ]

      where
        bundleIsExcessivelyLarge b = case assessSize b of
            TokenBundleSizeWithinLimit -> False
            OutputTokenBundleSizeExceedsLimit -> True
          where
            assessSize = view #assessTokenBundleSize assessTokenBundleSize

    -- The complete list of token quantities that exceed the maximum quantity
    -- allowed in a transaction output:
    excessiveTokenQuantities :: [(Address, AssetId, TokenQuantity)]
    excessiveTokenQuantities =
        [ (address, asset, quantity)
        | output <- F.toList outputsToCover
        , let address = view #address output
        , (asset, quantity) <-
            TokenMap.toFlatList $ view #tokens $ view #tokens output
        , quantity > txOutMaxTokenQuantity
        ]

    outputsToCover =
        Balance.prepareOutputsWith computeMinimumAdaQuantity outputsUnprepared

-- | Indicates a problem when preparing outputs for a coin selection.
--
data ErrPrepareOutputs
    = ErrPrepareOutputsTokenBundleSizeExceedsLimit
        ErrOutputTokenBundleSizeExceedsLimit
    | ErrPrepareOutputsTokenQuantityExceedsLimit
        ErrOutputTokenQuantityExceedsLimit
    deriving (Eq, Generic, Show)

data ErrOutputTokenBundleSizeExceedsLimit = ErrOutputTokenBundleSizeExceedsLimit
    { address :: !Address
      -- ^ The address to which this token bundle was to be sent.
    , assetCount :: !Int
      -- ^ The number of assets within the token bundle.
    }
    deriving (Eq, Generic, Show)

-- | Indicates that a token quantity exceeds the maximum quantity that can
--   appear in a transaction output's token bundle.
--
data ErrOutputTokenQuantityExceedsLimit = ErrOutputTokenQuantityExceedsLimit
    { address :: !Address
      -- ^ The address to which this token quantity was to be sent.
    , asset :: !AssetId
      -- ^ The asset identifier to which this token quantity corresponds.
    , quantity :: !TokenQuantity
      -- ^ The token quantity that exceeded the bound.
    , quantityMaxBound :: !TokenQuantity
      -- ^ The maximum allowable token quantity.
    }
    deriving (Eq, Generic, Show)

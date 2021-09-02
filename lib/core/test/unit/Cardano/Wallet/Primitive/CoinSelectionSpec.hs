{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.CoinSelectionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( PerformSelection
    , SelectionConstraints (..)
    , SelectionParams (..)
    , accountForExistingInputs
    )
import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionLimit
    , SelectionLimitOf (..)
    , SelectionResult (..)
    , SelectionSkeleton (..)
    )
import Cardano.Wallet.Primitive.CoinSelection.Gen
    ( genSelectionLimit
    , genSelectionSkeleton
    , shrinkSelectionLimit
    , shrinkSelectionSkeleton
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId, shrinkAssetId )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxIn, genTxOut, shrinkTxIn, shrinkTxOut )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..), dom )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO, shrinkUTxO )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex, shrinkUTxOIndex )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , checkCoverage
    , conjoin
    , cover
    , genericShrink
    , property
    )
import Test.QuickCheck.Extra
    ( NotNull (..) )

import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.CoinSelectionSpec" $

    describe "accountForExistingInputs" $ do
        it "prop_accountForExistingInputs_computeMinimumCost" $
            property prop_accountForExistingInputs_computeMinimumCost
        it "prop_accountForExistingInputs_computeSelectionLimit" $
            property prop_accountForExistingInputs_computeSelectionLimit
        it "prop_accountForExistingInputs_inputsSelected" $
            property prop_accountForExistingInputs_inputsSelected
        it "prop_accountForExistingInputs_utxoAvailable" $
            property prop_accountForExistingInputs_utxoAvailable

type ComputeMinimumCost = SelectionSkeleton -> Coin

prop_accountForExistingInputs_computeMinimumCost
    :: UTxO -> SelectionSkeleton -> Property
prop_accountForExistingInputs_computeMinimumCost existingInputs skeleton =
    checkCoverage $
    cover 10
        (computeMinimumCost' skeleton > computeMinimumCost skeleton)
        "computeMinimumCost' skeleton > computeMinimumCost skeleton" $
    conjoin
        [ computeMinimumCost' skeleton >= computeMinimumCost skeleton
        , computeMinimumCost' skeleton == computeMinimumCost
            (skeleton & over #skeletonInputCount (+ UTxO.size existingInputs))
        ]
  where
    computeMinimumCost :: ComputeMinimumCost
    computeMinimumCost = Coin . fromIntegral . length . show

    computeMinimumCost' :: ComputeMinimumCost
    computeMinimumCost' = getReport $ accountForExistingInputs
        (const . Report . view #computeMinimumCost)
        emptySelectionConstraints {computeMinimumCost}
        emptySelectionParams {existingInputs}

prop_accountForExistingInputs_computeSelectionLimit
    :: UTxO -> [TxOut] -> Property
prop_accountForExistingInputs_computeSelectionLimit existingInputs txOuts =
    checkCoverage $
    cover 10
        (computeSelectionLimit txOuts == NoLimit)
        "computeSelectionLimit txOuts == NoLimit" $
    cover 10
        (computeSelectionLimit txOuts /= NoLimit)
        "computeSelectionLimit txOuts /= NoLimit" $
    conjoin
        [ computeSelectionLimit' txOuts <= computeSelectionLimit txOuts
        , computeSelectionLimit' txOuts == fmap
            (subtract (UTxO.size existingInputs)) (computeSelectionLimit txOuts)
        ]
  where
    computeSelectionLimit :: [TxOut] -> SelectionLimit
    computeSelectionLimit txOuts
        | even (length txOuts) =
            NoLimit
        | otherwise =
            MaximumInputLimit $ max 0 (100 - length txOuts)

    computeSelectionLimit' :: [TxOut] -> SelectionLimit
    computeSelectionLimit' = getReport $ accountForExistingInputs
        (const . Report . view #computeSelectionLimit)
        emptySelectionConstraints {computeSelectionLimit}
        emptySelectionParams {existingInputs}

prop_accountForExistingInputs_inputsSelected
    :: UTxO -> NotNull UTxO -> Property
prop_accountForExistingInputs_inputsSelected
    existingInputs' (NotNull inputsSelected) =
    checkCoverage $
    cover 10
        (existingInputs /= UTxO.empty && inputsSelected /= UTxO.empty)
        "existingInputs /= UTxO.empty && inputsSelected /= UTxO.empty" $
    conjoin
        [ dom existingInputs `Set.disjoint`   dom inputsSelected
        , dom existingInputs `Set.isSubsetOf` dom inputsSelected'
        ]
  where
    existingInputs :: UTxO
    existingInputs = existingInputs' `UTxO.excluding` dom inputsSelected

    inputsSelected' :: UTxO
    inputsSelected'
        = expectRight (UTxO . Map.fromList . F.toList . view #inputsSelected)
        $ runIdentity
        $ accountForExistingInputs
            performSelectionFn
            emptySelectionConstraints
            emptySelectionParams {existingInputs}
      where
        performSelectionFn :: PerformSelection Identity ()
        performSelectionFn _ _ = Identity $ Right emptySelectionResult
            {inputsSelected = NE.fromList $ Map.toList $ unUTxO inputsSelected}

prop_accountForExistingInputs_utxoAvailable :: UTxO -> UTxO -> Property
prop_accountForExistingInputs_utxoAvailable utxoAvailable existingInputs =
    checkCoverage $
    cover 10
        (not (dom utxoAvailable `Set.disjoint` dom existingInputs))
        "not (dom utxoAvailable `Set.disjoint` dom existingInputs)" $
    conjoin
        [ dom utxoAvailable' ==
          dom utxoAvailable `Set.difference` dom existingInputs
        , all (`Set.notMember` dom utxoAvailable') (dom existingInputs)
        ]
  where
    utxoAvailable' :: UTxO
    utxoAvailable' = UTxOIndex.toUTxO $ getReport $ accountForExistingInputs
        (const (Report . view #utxoAvailable))
        emptySelectionConstraints
        emptySelectionParams
            { utxoAvailable = UTxOIndex.fromUTxO utxoAvailable
            , existingInputs
            }

--------------------------------------------------------------------------------
-- Empty values
--------------------------------------------------------------------------------

emptySelectionConstraints :: SelectionConstraints
emptySelectionConstraints = SelectionConstraints
    { assessTokenBundleSize =
        shouldNotEvaluate "assessTokenBundleSize"
    , computeMinimumAdaQuantity =
        shouldNotEvaluate "computeMinimumAdaQuantity"
    , computeMinimumCost =
        shouldNotEvaluate "computeMinimumCost"
    , computeSelectionLimit =
        shouldNotEvaluate "computeSelectionLimit"
    , maximumCollateralInputCount =
        shouldNotEvaluate "maximumCollateralInputCount"
    }
  where
    shouldNotEvaluate = shouldNotEvaluateFor "emptySelectionConstraints"

emptySelectionParams :: SelectionParams
emptySelectionParams = SelectionParams
    { assetsToBurn =
        shouldNotEvaluate "assetsToBurn"
    , assetsToMint =
        shouldNotEvaluate "assetsToMint"
    , existingInputs =
        shouldNotEvaluate "existingInputs"
    , outputsToCover =
        shouldNotEvaluate "outputsToCover"
    , rewardWithdrawal =
        shouldNotEvaluate "rewardWithdrawal"
    , utxoAvailable =
        shouldNotEvaluate "utxoAvailable"
    }
  where
    shouldNotEvaluate = shouldNotEvaluateFor "emptySelectionParams"

emptySelectionResult :: SelectionResult change
emptySelectionResult = SelectionResult
    { inputsSelected =
        shouldNotEvaluate "inputsSelected"
    , extraCoinSource =
        shouldNotEvaluate "extraCoinSource"
    , outputsCovered =
        shouldNotEvaluate "outputsCovered"
    , changeGenerated =
        shouldNotEvaluate "changeGenerated"
    , utxoRemaining =
        shouldNotEvaluate "utxoRemaining"
    }
  where
    shouldNotEvaluate = shouldNotEvaluateFor "emptySelectionResult"

shouldNotEvaluateFor :: String -> String -> a
shouldNotEvaluateFor contextName fieldName = error $ unwords
    [fieldName, "was unexpectedly evaluated in", contextName]

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary AssetId where
    arbitrary = genAssetId
    shrink = shrinkAssetId

instance Arbitrary SelectionLimit where
    arbitrary = genSelectionLimit
    shrink = shrinkSelectionLimit

instance Arbitrary SelectionSkeleton where
    arbitrary = genSelectionSkeleton
    shrink = shrinkSelectionSkeleton

instance Arbitrary TxIn where
    arbitrary = genTxIn
    shrink = shrinkTxIn

instance Arbitrary TxOut where
    arbitrary = genTxOut
    shrink = shrinkTxOut

instance Arbitrary UTxO where
    arbitrary = genUTxO
    shrink = shrinkUTxO

instance Arbitrary UTxOIndex where
    arbitrary = genUTxOIndex
    shrink = shrinkUTxOIndex

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

newtype Report a b = Report { getReport :: a }

instance Functor (Report a) where
    fmap _ (Report a) = Report a

-- TODO: Get rid of this
expectRight :: (b -> c) -> Either a b -> c
expectRight f (Right b) = f b
expectRight _ (Left  _) = error "unexpected Left"

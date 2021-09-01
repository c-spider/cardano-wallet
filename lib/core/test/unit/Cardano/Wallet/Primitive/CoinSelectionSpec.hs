{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
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
    ( SelectionResult (..)
    )
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
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Property, checkCoverage, conjoin, cover, genericShrink, property )

import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.CoinSelectionSpec" $

    describe "accountForExistingInputs" $ do
        it "prop_accountForExistingInputs_inputsSelected" $
            property prop_accountForExistingInputs_inputsSelected
        it "prop_accountForExistingInputs_utxoAvailable" $
            property prop_accountForExistingInputs_utxoAvailable

prop_accountForExistingInputs_inputsSelected
    :: UTxO -> NonEmpty (TxIn, TxOut) -> Property
prop_accountForExistingInputs_inputsSelected existingInputs inputsSelected =
    -- add coverage check
    conjoin
        [ unUTxO existingInputs `Map.isSubmapOf`
            Map.fromList (F.toList inputsSelected')
        ]
  where
    inputsSelected' :: NonEmpty (TxIn, TxOut)
    inputsSelected'
        = either (error "unexpected Left") (view #inputsSelected)
        $ runIdentity
        $ accountForExistingInputs
            performSelectionFn
            emptySelectionConstraints
            emptySelectionParams {existingInputs}
      where
        performSelectionFn :: PerformSelection Identity change
        performSelectionFn _constraints _params =
            Identity $ Right emptySelectionResult {inputsSelected}

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
    utxoAvailable'
        = UTxOIndex.toUTxO
        $ fst
        $ accountForExistingInputs
            performSelectionFn
            emptySelectionConstraints
            emptySelectionParams
                { utxoAvailable = UTxOIndex.fromUTxO utxoAvailable
                , existingInputs
                }
      where
        performSelectionFn :: PerformSelection ((,) UTxOIndex) change
        performSelectionFn _constraints params =
            (view #utxoAvailable params, Right emptySelectionResult)

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

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink

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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
    ( SelectionLimit (..), SelectionResult (..), SelectionSkeleton (..) )
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
import Data.Set
    ( Set )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonNegative (..)
    , Property
    , checkCoverage
    , conjoin
    , cover
    , genericShrink
    , oneof
    , property
    , shrinkMapBy
    , (===)
    )
import Test.QuickCheck.Extra
    ( NotNull (..), liftShrink3 )

import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.CoinSelectionSpec" $

    describe "accountForExistingInputs" $ do
        it "prop_accountForExistingInputs_inputsSelected" $
            property prop_accountForExistingInputs_inputsSelected
        it "prop_accountForExistingInputs_minimumCost" $
            property prop_accountForExistingInputs_minimumCost
        it "prop_accountForExistingInputs_selectionLimit" $
            property prop_accountForExistingInputs_selectionLimit
        it "prop_accountForExistingInputs_utxoAvailable" $
            property prop_accountForExistingInputs_utxoAvailable

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
        performSelectionFn _constraints _params =
            Identity $ Right emptySelectionResult
                { inputsSelected =
                    NE.fromList $ Map.toList $ unUTxO inputsSelected
                }

prop_accountForExistingInputs_minimumCost
    :: UTxO -> SelectionSkeleton -> Property
prop_accountForExistingInputs_minimumCost existingInputs skeleton =
    checkCoverage $
    cover 10
        (computeMinimumCost skeleton < computeMinimumCost' skeleton)
        "computeMinimumCost skeleton < computeMinimumCost' skeleton" $
    conjoin
        [ computeMinimumCost
            (skeleton & over #skeletonInputCount (+ UTxO.size existingInputs))
            ==
            computeMinimumCost' skeleton
        , computeMinimumCost skeleton <= computeMinimumCost' skeleton
        ]
  where
    computeMinimumCost :: SelectionSkeleton -> Coin
    computeMinimumCost = Coin . fromIntegral . length . show

    computeMinimumCost' :: SelectionSkeleton -> Coin
    computeMinimumCost' = runReport $ accountForExistingInputs
        performSelectionFn
        emptySelectionConstraints {computeMinimumCost}
        emptySelectionParams {existingInputs}
      where
        performSelectionFn
            :: PerformSelection (Report (SelectionSkeleton -> Coin)) ()
        performSelectionFn constraints _params =
            Report $ view #computeMinimumCost constraints

-- TODO: do something similar to minimumCost (with the function)
prop_accountForExistingInputs_selectionLimit
    :: UTxO -> SelectionLimit -> Property
prop_accountForExistingInputs_selectionLimit existingInputs selectionLimit =
    checkCoverage $
    cover 10 (selectionLimit == NoLimit) "selectionLimit == NoLimit" $
    cover 10 (selectionLimit /= NoLimit) "selectionLimit /= NoLimit" $
    selectionLimit' === case selectionLimit of
        NoLimit ->
            NoLimit
        MaximumInputLimit n ->
            MaximumInputLimit (n - UTxO.size existingInputs)
  where
    selectionLimit' :: SelectionLimit
    selectionLimit' = runReport $ accountForExistingInputs
        performSelectionFn
        emptySelectionConstraints {computeSelectionLimit = const selectionLimit}
        emptySelectionParams {existingInputs}
      where
        performSelectionFn :: PerformSelection (Report SelectionLimit) ()
        performSelectionFn constraints _params =
            Report $ view #computeSelectionLimit constraints []

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
    utxoAvailable' = UTxOIndex.toUTxO $ runReport $ accountForExistingInputs
        performSelectionFn
        emptySelectionConstraints
        emptySelectionParams
            { utxoAvailable = UTxOIndex.fromUTxO utxoAvailable
            , existingInputs
            }
      where
        performSelectionFn :: PerformSelection (Report UTxOIndex) ()
        performSelectionFn _constraints params =
            Report $ view #utxoAvailable params

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
-- Generators and shrinkers
--------------------------------------------------------------------------------

genSelectionLimit :: Gen SelectionLimit
genSelectionLimit = oneof
    [ MaximumInputLimit . getNonNegative <$> arbitrary
    , pure NoLimit
    ]

shrinkSelectionLimit :: SelectionLimit -> [SelectionLimit]
shrinkSelectionLimit = \case
    MaximumInputLimit n ->
        MaximumInputLimit . getNonNegative <$> shrink (NonNegative n)
    NoLimit ->
        []

genSelectionSkeleton :: Gen SelectionSkeleton
genSelectionSkeleton = SelectionSkeleton
    <$> genSkeletonInputCount
    <*> genSkeletonOutputs
    <*> genSkeletonChange
  where
    genSkeletonInputCount =
        getNonNegative <$> arbitrary @(NonNegative Int)
    genSkeletonOutputs =
        arbitrary @[TxOut]
    genSkeletonChange =
        arbitrary @[(Set AssetId)]

shrinkSelectionSkeleton :: SelectionSkeleton -> [SelectionSkeleton]
shrinkSelectionSkeleton =
    shrinkMapBy tupleToSkeleton skeletonToTuple $ liftShrink3
        shrink
        shrink
        shrink
  where
    skeletonToTuple (SelectionSkeleton a b c) = (a, b, c)
    tupleToSkeleton (a, b, c) = (SelectionSkeleton a b c)

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

newtype Report a b = Report { runReport :: a }

instance Functor (Report a) where
    fmap _ (Report a) = Report a

-- TODO: Get rid of this
expectRight :: (b -> c) -> Either a b -> c
expectRight f (Right b) = f b
expectRight _ (Left  _) = error "unexpected Left"

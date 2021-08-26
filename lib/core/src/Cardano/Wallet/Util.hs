-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- General utility functions.

module Cardano.Wallet.Util
    ( internalError
    , isInternalError
    , tina
    ) where

import Prelude

import Control.Exception
    ( ErrorCall, displayException )
import Data.Foldable
    ( asum )
import Data.List
    ( isPrefixOf )
import Data.Maybe
    ( fromMaybe )
import Fmt
    ( Builder, fmt, (+|) )
import GHC.Stack
    ( HasCallStack )

-- | Calls the 'error' function, which will usually crash the program.
internalError :: HasCallStack => Builder -> a
internalError msg = error $ fmt $ "INTERNAL ERROR: "+|msg

-- | Tests whether an 'Exception' was caused by 'internalError'.
isInternalError :: ErrorCall -> Bool
isInternalError e = "INTERNAL ERROR" `isPrefixOf` displayException e

-- | Take the first 'Just' from a list of 'Maybe', or die trying.
-- There is no alternative.
tina :: HasCallStack => Builder -> [Maybe a] -> a
tina msg = fromMaybe (internalError msg) . asum

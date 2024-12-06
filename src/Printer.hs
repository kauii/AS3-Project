module Printer (printDescription) where

import qualified Data.Map as Map
import Utils
import Control.Monad (when)

-- Print a description if its flag condition is met
printDescription :: Map.Map String Bool -> (String, String, Bool) -> IO ()
printDescription flags (desc, flagKey, requiredState) =
    when (checkFlag flagKey requiredState flags) $
        putStrLn desc
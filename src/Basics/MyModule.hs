module Basics.MyModule where

import Data.Default
import Ledger          (POSIXTime(..))
import Ledger.TimeSlot ( slotToEndPOSIXTime
                       , slotToBeginPOSIXTime
                       , slotToPOSIXTimeRange )

main :: IO ()
main = do
  let endPosixTime = slotToEndPOSIXTime   def 10
      begPosixTime = slotToBeginPOSIXTime def 10
      timeRange    = slotToPOSIXTimeRange def 10
  print $ getPOSIXTime endPosixTime
  print $ getPOSIXTime begPosixTime
  print timeRange


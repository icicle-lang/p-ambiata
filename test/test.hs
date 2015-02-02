import           Control.Monad

import qualified PTest

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      PTest.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure

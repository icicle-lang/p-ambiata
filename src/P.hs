module P (
    module X
  , Prelude.toInteger
  , Prelude.truncate
  , Prelude.seq

  ) where


import qualified Prelude

import           P.Applicative as X
import           P.Bool as X
import           P.Monoid as X
import           Control.Applicative as X
import           Control.Monad as X hiding (
                     mapM
                   , sequence
                   , forM
                   , mapM_
                   , sequence_
                   , forM_
                   , msum
                   )
import           Data.Eq as X
import           Data.Bool as X
import           Data.Char as X (Char)
import           Data.Function as X
import           Data.List as X (
                     intercalate
                   , isPrefixOf
                   , drop
                   , length
                   , null
                   , splitAt
                   , tail
                   , break
                   , filter
                   )
import           Data.Maybe as X hiding (fromJust)
import           Data.Monoid as X
import           Data.Either as X
import           Data.Int as X
import           Data.Ord as X
import           Data.Tuple as X
import           Data.Traversable as X
import           Data.Foldable as X
import           GHC.Num as X
import           Text.Show as X
import           Text.Read as X (Read, reads)

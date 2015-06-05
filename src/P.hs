module P (
    module X
  , Prelude.seq
  ) where


import qualified Prelude

import           P.Applicative as X
import           P.Bool as X
import           P.Either as X
import           P.EitherT as X
import           P.Foldable as X
import           P.Maybe as X
import           P.Monoid as X
import           P.Ord as X
import           P.List as X
import           P.Function as X
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
                   , break
                   , filter
                   , reverse
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
import           GHC.Real as X
import           GHC.Float as X
import           Text.Show as X
import           Text.Read as X (Read, reads, readMaybe, readEither)

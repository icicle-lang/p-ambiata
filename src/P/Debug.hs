{-# LANGUAGE NoImplicitPrelude #-}
module P.Debug (
  -- * Functions for development/debuggung only
  -- | Cannot be used in production code, but might be useful
  -- during development or debugging
    undefined
  , error
  , trace
  , traceM
  , traceIO
  ) where

import qualified Prelude as P
import qualified Debug.Trace as T

{-# WARNING undefined "Do not use 'undefined' in production code" #-}
undefined :: a
undefined = P.undefined

{-# WARNING error "Do not use 'error' in production code" #-}
error :: P.String -> a
error = P.error

{-# WARNING trace "Do not use 'trace' in production code" #-}
trace :: P.String -> a -> a
trace = T.trace

{-# WARNING traceM "Do not use 'traceM' in production code" #-}
traceM :: P.Monad m => P.String -> m ()
traceM = T.traceM

{-# WARNING traceIO "Do not use 'traceIO' in production code" #-}
traceIO :: P.String -> P.IO ()
traceIO = T.traceIO

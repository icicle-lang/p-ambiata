{-# LANGUAGE CPP #-}
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

#if MIN_VERSION_base(4,9,0)
import           GHC.Stack (HasCallStack)
#endif

{-# WARNING undefined "Do not use 'undefined' in production code" #-}
#if MIN_VERSION_base(4,9,0)
undefined :: HasCallStack => a
#else
undefined :: a
#endif
undefined = P.undefined

{-# WARNING error "Do not use 'error' in production code" #-}
#if MIN_VERSION_base(4,9,0)
error :: HasCallStack => P.String -> a
#else
error :: P.String -> a
#endif
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

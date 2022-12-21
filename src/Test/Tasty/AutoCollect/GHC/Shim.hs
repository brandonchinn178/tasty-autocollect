{-# LANGUAGE CPP #-}

module Test.Tasty.AutoCollect.GHC.Shim (module X) where

-- GHC-specific shims
import Test.Tasty.AutoCollect.GHC.Shim_Common as X
#if __GLASGOW_HASKELL__ == 900
import Test.Tasty.AutoCollect.GHC.Shim_9_0 as X
#elif __GLASGOW_HASKELL__ == 902
import Test.Tasty.AutoCollect.GHC.Shim_9_2 as X
#elif __GLASGOW_HASKELL__ == 904
import Test.Tasty.AutoCollect.GHC.Shim_9_4 as X
#endif

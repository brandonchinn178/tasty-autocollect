{-# LANGUAGE CPP #-}

module Test.Tasty.AutoCollect.GHC.Shim (module X) where

-- GHC-specific shims
#if __GLASGOW_HASKELL__ == 900
import Test.Tasty.AutoCollect.GHC.Shim_9_0 as X
#endif

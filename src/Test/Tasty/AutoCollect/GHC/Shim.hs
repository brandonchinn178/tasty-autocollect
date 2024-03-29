{-# LANGUAGE CPP #-}

module Test.Tasty.AutoCollect.GHC.Shim (module X) where

-- GHC-specific shims
import Test.Tasty.AutoCollect.GHC.Shim_Common as X
#if __GLASGOW_HASKELL__ == 904
import Test.Tasty.AutoCollect.GHC.Shim_9_4 as X
#elif __GLASGOW_HASKELL__ == 906
import Test.Tasty.AutoCollect.GHC.Shim_9_6 as X
#elif __GLASGOW_HASKELL__ == 908
import Test.Tasty.AutoCollect.GHC.Shim_9_8 as X
#endif

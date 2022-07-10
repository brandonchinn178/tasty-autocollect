module Test.Tasty.AutoCollect.GHC.Shim_9_0 (
  -- * Re-exports
  module X,

  -- * Compat
  srcSpanStart,
  mkOccNameVar,
  mkOccNameTC,
) where

-- Re-exports
import GHC.Driver.Main as X (getHscEnv)
import GHC.Hs as X
import GHC.Parser.Annotation as X (AnnotationComment (..), getAnnotationComments)
import GHC.Plugins as X hiding (getHscEnv, srcSpanStart, varName)
import GHC.Types.Name.Cache as X (NameCache)

import qualified GHC.Types.Name.Occurrence as NameSpace (tcName, varName)
import qualified GHC.Types.SrcLoc as GHC (srcSpanStart)

{----- Compat -----}

srcSpanStart :: SrcSpan -> Either String RealSrcLoc
srcSpanStart ss =
  case GHC.srcSpanStart ss of
    RealSrcLoc srcLoc _ -> Right srcLoc
    UnhelpfulLoc s -> Left $ unpackFS s

mkOccNameVar :: String -> OccName
mkOccNameVar = mkOccName NameSpace.varName

mkOccNameTC :: String -> OccName
mkOccNameTC = mkOccName NameSpace.tcName

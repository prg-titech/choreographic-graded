{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Choreographic.Graded
import Choreographic.Graded.Location (SingletonSymbol, SingletonSymbolSet)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Type.Set as TS

aSym :: SingletonSymbol "a"
aSym = singletonSymbol (Proxy @"a")

bSym :: SingletonSymbol "b"
bSym = singletonSymbol (Proxy @"b")

cSym :: SingletonSymbol "c"
cSym = singletonSymbol (Proxy @"c")

abSym :: SingletonSymbolSet (TS.AsSet '["a", "b"])
abSym = singletonSymbolSet (Proxy @'["a", "b"])

bcSym :: SingletonSymbolSet (TS.AsSet '["b", "c"])
bcSym = singletonSymbolSet (Proxy @'["b", "c"])

main :: IO ()
main = putStrLn "Test suite not yet implemented."

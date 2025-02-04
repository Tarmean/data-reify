{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.ReifySpec where

import qualified Data.List as L
import Data.Reify
import Prelude ()
import Prelude.Compat
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
  describe "reifyGraph" $
    it "should produce a Graph with unique key-value pairs" $ do -- #11
      g <- reifyGraph s1
      nubGraph g `shouldBe` g

data State = State Char [State]
  deriving (Eq, Show)

data StateDeRef = StateDeRef Char [Unique]
  deriving (Eq, Show)

s1, s2, s3 :: State
s1 = State 'a' [s2,s3]
s2 = State 'b' [s1,s2]
s3 = State 'c' [s2,s1]

instance MonadGlobals State m => MuRef State m where
  type DeRef State = StateDeRef
  mapDeRef f (State a tr) = StateDeRef a <$> traverse f tr

nubGraph :: Eq e => Graph e -> Graph e
nubGraph (Graph netlist start) = Graph (L.nub netlist) start

deriving instance Eq e => Eq (Graph e)

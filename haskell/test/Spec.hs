module Main where

import qualified Data.Map.Strict as M
import           FormalityNet
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "FormalityNet" $ do
    it "annihilates CON-CON with a == b" $ do
      reduce inZaZa `shouldBe` (outZaZa, 1)
    it "annihilates OP1-OP1" $ do
      reduce inUU `shouldBe` (outUU, 1)
    it "annihilates OP2-OP2" $ do
      reduce inBB `shouldBe` (outBB, 1)
    it "annihilates ITE-ITE" $ do
      reduce inII `shouldBe` (outII, 1)

zE :: Net
zE = makeNet
  [ Node Z 0 (Ptr 0 P) (Ptr 1 L) (Ptr 1 R) 0
  , Node Z 0 Free (Ptr 0 L) (Ptr 0 R) 0
  ]

uE:: Net
uE = makeNet
  [ Node U 0 (Ptr 0 P) (Ptr 1 L) Free 1
  , Node Z 0 Free (Ptr 0 L) Free 0
  ]

inZaZa :: Net
inZaZa= makeNet
  [ Node Z 0 (Ptr 1 P) (Ptr 2 L) (Ptr 2 R) 0
  , Node Z 0 (Ptr 0 P) (Ptr 3 L) (Ptr 3 R) 0
  , Node Z 0 Free (Ptr 0 L) (Ptr 0 R) 0
  , Node Z 0 Free (Ptr 1 L) (Ptr 1 R) 0
  ]

outZaZa :: Net
outZaZa = Net nodes [0,1] [] where
  nodes = M.fromList $
    [ (2, Node Z 0 Free (Ptr 3 L) (Ptr 3 R) 0)
    , (3, Node Z 0 Free (Ptr 2 L) (Ptr 2 R) 0)
    ]

zaZb :: Net
zaZb = makeNet
  [ Node Z 0 (Ptr 1 P) Free Free 0
  , Node Z 1 (Ptr 0 P) Free Free 0
  ]

inUU :: Net
inUU = makeNet
  [ Node U 0 (Ptr 1 P) (Ptr 2 L) Free 2
  , Node U 0 (Ptr 0 P) (Ptr 3 L) Free 2
  , Node Z 0 Free (Ptr 0 L) Free 0
  , Node Z 0 Free (Ptr 1 L) Free 0
  ]

outUU :: Net
outUU = Net nodes [0,1] [] where
  nodes = M.fromList $
    [ (2, Node Z 0 Free (Ptr 3 L) Free 0)
    , (3, Node Z 0 Free (Ptr 2 L) Free 0)
    ]

inBB :: Net
inBB = makeNet
  [ Node B 0 (Ptr 1 P) (Ptr 2 L) (Ptr 2 R) 0
  , Node B 0 (Ptr 0 P) (Ptr 3 L) (Ptr 3 R) 0
  , Node Z 0 Free (Ptr 0 L) (Ptr 0 R) 0
  , Node Z 0 Free (Ptr 1 L) (Ptr 1 R) 0
  ]

outBB :: Net
outBB = Net nodes [0,1] [] where
  nodes = M.fromList $
    [ (2, Node Z 0 Free (Ptr 3 L) (Ptr 3 R) 0)
    , (3, Node Z 0 Free (Ptr 2 L) (Ptr 2 R) 0)
    ]

inII :: Net
inII = makeNet
  [ Node I 0 (Ptr 1 P) (Ptr 2 L) (Ptr 2 R) 0
  , Node I 0 (Ptr 0 P) (Ptr 3 L) (Ptr 3 R) 0
  , Node Z 0 Free (Ptr 0 L) (Ptr 0 R) 0
  , Node Z 0 Free (Ptr 1 L) (Ptr 1 R) 0
  ]

outII :: Net
outII = Net nodes [0,1] [] where
  nodes = M.fromList $
    [ (2, Node Z 0 Free (Ptr 3 L) (Ptr 3 R) 0)
    , (3, Node Z 0 Free (Ptr 2 L) (Ptr 2 R) 0)
    ]

nN :: Net
nN = makeNet
  [ Node N 0 (Ptr 1 P) Free Free 2
  , Node N 0 (Ptr 0 P) Free Free 2
  ]




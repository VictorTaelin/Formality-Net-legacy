{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module FormalityNet where

import           Control.Monad                  (guard, unless, when)
import           Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict                as M
import qualified Data.Set                       as Set

data Net = Net
  { netNodes   :: M.Map Int Node
  , freedNodes :: [Int]
  , netRedex   :: [(Int, Int)]
  } deriving (Eq, Show)

data Port
  = Ptr { toNode :: Int, toSlot :: Slot }
  | Free
  deriving (Eq, Show)

data Slot = P | A1 | A2 deriving (Eq, Show)
type Kind = Int

data Node
  = CON { kind :: Kind, slotP :: Port, slotA1 :: Port, slotA2 :: Port }
  | OP1 { kind :: Kind, slotP :: Port, slotA1 :: Port, numArg :: Int }
  | OP2 { kind :: Kind, slotP :: Port, slotA1 :: Port, slotA2 :: Port }
  | ITE { kind :: Kind, slotP :: Port, slotA1 :: Port, slotA2 :: Port }
  | NUM { num :: Int, slotP :: Port }
  deriving (Eq, Show)

data NodeType = Con | Op1 | Op2 | Ite | Num

makeNet :: [Node] -> Net
makeNet nodes = let n = M.fromList $ zip [0..] nodes in Net n [] (findRedexes n)

findRedexes :: M.Map Int Node -> [(Int, Int)]
findRedexes nodes = deDup $ (toNode . slotP) <$> M.filterWithKey isRedex nodes
  where
    isRedex _ n = case slotP n of
      (Ptr _ P) -> True
      _         -> False
    deDup = Set.toList . (M.foldrWithKey f Set.empty)
    f k v s
      | Set.member (v, k) s = s
      | otherwise = (Set.insert (k, v) s)

defaultNode :: Int -> NodeType -> Kind -> Int -> Node
defaultNode n Con k m = CON k (Ptr n P) (Ptr n A1) (Ptr n A2)
defaultNode n Op1 k m = OP1 k (Ptr n P) (Ptr n A1) m
defaultNode n Op2 k m = OP2 k (Ptr n P) (Ptr n A1) (Ptr n A2)
defaultNode n Ite k m = ITE k (Ptr n P) (Ptr n A1) (Ptr n A2)
defaultNode n Num k m = NUM m (Ptr n P)

typeOf :: Node -> NodeType
typeOf = \case
  CON _ _ _ _ -> Con
  OP1 _ _ _ _ -> Op1
  OP2 _ _ _ _ -> Op2
  ITE _ _ _ _ -> Ite
  NUM _ _     -> Num

slot :: Slot -> Node -> Port
slot s n = case s of P -> slotP n; A1 -> slotA1 n; A2 -> slotA2 n

setSlot :: Slot -> Port -> Node -> Node
setSlot s q n = case s of
  P -> n { slotP = q }; A1 -> n {slotA1 = q }; A2 -> n { slotA2 = q }

allocNode :: NodeType -> Kind -> Int -> State Net Int
allocNode t k n = do
  freed <- gets freedNodes
  case freed of
    (f:_) -> return f
    [] -> do
      addr <- gets $ (maybe 0 ((+1) . fst)) . M.lookupMax . netNodes
      let node = defaultNode addr t k n
      modify (\n -> n { netNodes = M.insert addr node $ netNodes n })
      return addr

freeNode :: Int -> State Net ()
freeNode addr = do
  modify $ \n -> n
      { netNodes = M.delete addr $ netNodes n
      , freedNodes = addr : (freedNodes n)
      }

getPort :: Int -> Slot -> M.Map Int Node -> Port
getPort addr s net = (slot s) (net M.! addr)

setPort :: Int -> Slot -> Port -> State Net ()
setPort addr s port = modify $ \net ->
  net {netNodes = M.adjust (setSlot s port) addr $ netNodes net }

linkSlots :: (Int, Slot) -> (Int, Slot) -> State Net ()
linkSlots (ia, sa) (ib, sb) = do
  setPort ia sa $ Ptr ib sb
  setPort ib sb $ Ptr ia sa
  when (sa == P && sb == P) $
    modify (\n -> n {netRedex = (ia,ib) : netRedex n})

linkBack :: (Int, Slot) -> (Int, Slot) -> State Net ()
linkBack (ia, sa) (ib, sb) = do
  net <- gets netNodes
  case (getPort ia sa net, getPort ib sb net) of
    (Ptr ia' sa', Ptr ib' sb') -> linkSlots (ia', sa') (ib', sb')
    (Ptr ia' sa', x)           -> setPort ia' sa' x
    (x, Ptr ib' sb')           -> setPort ib' sb' x

unlinkPort :: (Int, Slot) -> State Net ()
unlinkPort (ia, sa) = do
  net <- gets netNodes
  case (getPort ia sa net) of
    (Ptr ib sb) -> do
      setPort ia sa $ Ptr ia sa
      setPort ib sb $ Ptr ib sb
    _ -> return ()

rewrite :: (Int, Int) -> State Net ()
rewrite (iA, iB) = do
  net <- gets netNodes
  let a = net M.! iA
  let b = net M.! iB

  match (iA,a) (iB,b)

  mapM_ (\x -> unlinkPort (iA, x)) [P,A1,A2] >> freeNode iA
  unless (iA == iB) (mapM_ (\x -> unlinkPort (iB, x)) [P,A1,A2] >> freeNode iB)

match :: (Int, Node) -> (Int, Node) -> State Net ()
match (iA,a) (iB,b) = case (typeOf a, typeOf b) of
  (Con, Con) -> if
    | kind a == kind b -> annihilate (iA, a) (iB, b)
    | otherwise        -> duplicate (iA, a) (iB, b)
  (Op1, Op1) -> linkBack (iA, A1) (iB, A1)
  (Op2, Op2) -> annihilate (iA, a) (iB, b)
  (Ite, Ite) -> annihilate (iA, a) (iB, b)
  (Num, Num) -> pure ()

  (Con, Op2) -> duplicate (iA, a) (iB, b)
  (Con, Ite) -> duplicate (iA, a) (iB, b)
  (Con, Num) -> do
    iP <- allocNode Num 0 (num b)
    iQ <- allocNode Num 0 (num b)
    linkBack (iA, A1) (iP, P)
    linkBack (iA, A2) (iQ, P)

  (Op1, Con) -> duplicate1 (iA, a) (iB, b)
  (Op1, Op2) -> duplicate1 (iA, a) (iB, b)
  (Op1, Ite) -> duplicate1 (iA, a) (iB, b)
  (Op1, Num) -> do
    let x = arithmetic (kind a) (num b) (numArg a)
    iP <- allocNode Num 0 (num b)
    linkBack (iA, A1) (iP, P)

  (Op2, Ite) -> duplicate (iA, a) (iB, b)
  (Op2, Num) -> do
    iP <- allocNode Op1 (kind a) (num b)
    linkBack (iA, A1) (iP, P)
    linkBack (iA, A2) (iP, A1)

  (Ite, Num) -> do
    iP <- allocNode Con (kind a) 0
    linkBack (iA, A1) (iP, P)
    if
      | (num b) == 0 -> linkBack (iA, A2) (iP,A1)
      | otherwise    -> linkBack (iA, A2) (iP,A2)

  _ -> match (iB,b) (iA,a)

annihilate :: (Int, Node) -> (Int, Node) -> State Net ()
annihilate (iA, a) (iB, b) = do
  linkBack (iA, A1) (iB, A1)
  linkBack (iA, A2) (iB, A2)

duplicate1 :: (Int, Node) -> (Int, Node) -> State Net ()
duplicate1 (iA, a) (iB, b) = do
  iP <- allocNode (typeOf b) (kind b) 0
  iQ <- allocNode (typeOf b) (kind b) 0
  iR <- allocNode (typeOf a) (kind a) 0
  linkSlots (iR, A2) (iQ, A1)
  linkSlots (iR, A1) (iP, A1)
  linkBack (iP,P) (iA,A1)
  linkBack (iQ,P) (iA,A2)
  linkBack (iR,P) (iB,A1)

duplicate :: (Int, Node) -> (Int, Node) -> State Net ()
duplicate (iA, a) (iB, b) = do
  iP <- allocNode (typeOf b) (kind b) 0
  iQ <- allocNode (typeOf b) (kind b) 0
  iR <- allocNode (typeOf a) (kind a) 0
  iS <- allocNode (typeOf a) (kind a) 0
  linkSlots (iS, A1) (iP, A2)
  linkSlots (iR, A2) (iQ, A1)
  linkSlots (iS, A2) (iQ, A2)
  linkSlots (iR, A1) (iP, A1)
  linkBack (iP,P) (iA,A1)
  linkBack (iQ,P) (iA,A2)
  linkBack (iR,P) (iB,A1)
  linkBack (iS,P) (iB,A2)

arithmetic :: Kind -> Int -> Int -> Int
arithmetic f n m =
  case f of
    0 -> n + m
    1 -> n - m
    2 -> n * m
    3 -> n `div` m
    4 -> n `mod` m
    5 -> n ^ m
    6 -> n ^ (m `div` (2 ^ 32))
--    7 -> n `and` m
--    8 -> n `or` m
--    9 -> n `xor` m
--    10 -> not m
--    11 -> lsh n m
--    12 -> rsh n m
--    13 -> if n > m then 1 else 0
--    14 -> if n < m then 1 else 0
--    15 -> if n == m then 1 else 0
    _ -> error "[ERROR]\nInvalid interaction"

reduce :: Net -> (Net, Int)
reduce net =
  let go net count =
        case netRedex net of
          [] -> (net, count)
          r:rs -> let newNet = execState (rewrite r) (net { netRedex = rs })
                   in go newNet (count + 1)
  in go net 0




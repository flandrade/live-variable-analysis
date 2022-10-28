{-# LANGUAGE ScopedTypeVariables #-}

module CFG where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe ()
import qualified Data.Set as S
import qualified Expression as E

-- | Blocks can be either skip, assignment, or conditional.
-- For instance, the assignment "x = 1" can be represented as:
-- Assignment (Add (E.Variable "x") (E.Literal 1)
data BlockType
  = Skip
  | Assignment E.AExpression E.AExpression
  | Conditional E.BExpression

-- | A block from the CFG is represented as a record than contains
-- a block (block type), an outLink (exit blocks) and the label.
-- For instance:
-- ---------
-- | x = 1 |---> Block 3
-- ---------
-- .   |
--  Block 2
--
-- The block "1" can be represented as the following record:
-- Block
--   { block: Assignment (Add (E.Variable "x") (E.Literal 1))
-- .   label: 1
--     outLink: [2, 3]
-- . }
data Block = Block
  { block :: BlockType,
    label :: Int,
    outLink :: [Int]
  }

-- | A control Control-Flow Graph (CFG) is a list of blocks
type CFG = [Block]

-- | A record that contains LVin and LVOut equations of a single block.
-- LVin: Live variables at the entry of the block
-- LVout: Live variables at the exit of the block
data LV = LV
  { lvIn :: S.Set String,
    lvOut :: S.Set String
  }
  deriving (Eq, Show)

-- | Gen: Variables that are read in the block
gen ::
  -- | Block type (assignment, skip or conditional)
  BlockType ->
  S.Set String
gen Skip = S.empty
gen (Assignment _ expression) = E.fvAExp expression
gen (Conditional expression) = E.fvBExp expression

-- | Kill: Variables that are written to in the block
-- If block n contains an assignment x := e, then Kill_n = {x}.
-- Otherwise Kil_ln = ∅.
kill ::
  -- | Block type (assignment, skip or conditional)
  BlockType ->
  S.Set String
kill Skip = S.empty
kill (Assignment (E.Variable var) _) = S.singleton var
kill (Conditional expression) = S.empty

-- | Transfer function to get LVIn of a single block
-- LVIn_n = (LVOut_n − Kill_n) ∪ Gen_n
transferFunction ::
  -- | Block type (assignment, skip or conditional)
  BlockType ->
  -- | Set of variables (LVOut) in block
  S.Set String ->
  S.Set String
transferFunction block lOut = S.union (S.difference lOut (kill block)) (gen block)

-- | Get LVOut of a single block.
-- A variable is live at the exit of a block if it is live at the entrance of any of the blocks
-- following it. For instance, the block 1:
-- ---------
-- | x = 1 |---> Block 3
-- ---------
-- .   |
--  Block 2
-- LVOut1 = LVIn2 U LVIn3
getLVOut ::
  -- | A CFG's block
  Block ->
  -- | The CFG's live variables
  M.Map Int LV ->
  S.Set String
getLVOut inBlock lVEquations = fromListToLVOut (getSetByLabelList (outLink inBlock) lVEquations)
  where
    getSetByLabelList :: [Int] -> M.Map Int LV -> [S.Set String]
    getSetByLabelList list equations = L.map (\elem -> toSet (M.lookup elem equations)) list
      where
        toSet = maybe S.empty lvIn

    fromListToLVOut :: [S.Set String] -> S.Set String
    fromListToLVOut = L.foldl (flip S.union) S.empty

-- | Get the live variable equations given a previous point
getLV ::
  -- | The live variable equations (LVIn and LVOut) from the previous point
  M.Map Int LV ->
  -- | The CFG
  CFG ->
  M.Map Int LV
getLV equations graph = M.fromList $ L.map (\cfg -> (label cfg, LV {lvIn = getNewLVIn (label cfg) (block cfg) equations, lvOut = getLVOut cfg equations})) graph
  where
    getNewLVIn :: Int -> BlockType -> M.Map Int LV -> S.Set String
    getNewLVIn lb block equations = transferFunction block (maybe S.empty lvOut (M.lookup lb equations))

-- | The live variable analysis:
-- 1. We obtain the chain, starting with a empty set for all LVIn and LVOut
-- LV' = Map (1, LV { LVIn=empty, LVOut=empty })
-- 2. We get the new variable using this point
-- LV'' = getLV LV' ...
-- We repeat (1) and (2) until the LV of the previous point is equal to the LV of the
-- current point.
--
-- For this, we create an infinite list that will return its values when the previous
-- condition is met. The result of the analysis is the last element of the list.
analysis ::
  -- | A control Control-Flow Graph (CFG)
  CFG ->
  M.Map Int LV
analysis graph = fst . last . takeWhile snd . iterate next $ (initial, True)
  where
    labels :: [Int]
    labels = L.map label graph

    -- Initial point in our chain: LV' = Map (1, LV { LVIn=empty, LVOut=empty })
    initial :: M.Map Int LV
    initial = L.foldr (\x -> M.insert x LV {lvIn = S.empty, lvOut = S.empty}) M.empty labels

    next :: (M.Map Int LV, Bool) -> (M.Map Int LV, Bool)
    next (prev, _) = L.foldl update (prev, False) labels

    -- We calculate the LV of the new LV and compare it to the previous point
    update :: (M.Map Int LV, Bool) -> Int -> (M.Map Int LV, Bool)
    update (prev, preCond) _ = (newLV, preCond || not (M.isSubmapOf prev newLV))
      where
        newLV = getLV prev graph

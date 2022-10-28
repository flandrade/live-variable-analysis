-- |
-- | Author      :  Fernanda Lucia Andrade Guanoquiza
-- | Date        :  2022-10-03
-- |
-- | Live Variable Analysis - Implementation
module Main where

import qualified CFG as G
import qualified Expression as E
import Print (showLV)

-- | Handout Example
example1 :: IO ()
example1 = do
  let block1 = G.Assignment (E.Variable "x") (E.Literal 10)
  let block2 = G.Conditional (E.Leq (E.Literal 1) (E.Variable "x"))
  let block3 = G.Assignment (E.Variable "x") (E.Sub (E.Variable "x") (E.Literal 1))
  let block4 = G.Assignment (E.Variable "y") (E.Add (E.Variable "y") (E.Variable "x"))
  let block5 = G.Assignment (E.Variable "z") (E.Literal 2)
  let block6 = G.Assignment (E.Variable "z") (E.Mul (E.Variable "y") (E.Variable "z"))
  let graph =
        [ G.Block {G.label = 1, G.outLink = [2], G.block = block1},
          G.Block {G.label = 2, G.outLink = [3, 5], G.block = block2},
          G.Block {G.label = 3, G.outLink = [4], G.block = block3},
          G.Block {G.label = 4, G.outLink = [2], G.block = block4},
          G.Block {G.label = 5, G.outLink = [6], G.block = block5},
          G.Block {G.label = 6, G.outLink = [], G.block = block6}
        ] ::
          G.CFG

  let result = G.analysis graph
  putStrLn "Handout Example"
  putStrLn $ showLV result

-- | Example from Slides (p. 22)
example2 :: IO ()
example2 = do
  let block1 = G.Assignment (E.Variable "x") (E.Literal 1)
  let block2 = G.Conditional (E.Leq (E.Literal 1) (E.Variable "y"))
  let block3 = G.Assignment (E.Variable "x") (E.Sub (E.Variable "x") (E.Literal 1))
  let block4 = G.Assignment (E.Variable "x") (E.Literal 2)
  let graph =
        [ G.Block {G.label = 1, G.outLink = [2], G.block = block1},
          G.Block {G.label = 2, G.outLink = [3, 4], G.block = block2},
          G.Block {G.label = 3, G.outLink = [2], G.block = block3},
          G.Block {G.label = 4, G.outLink = [], G.block = block4}
        ] ::
          G.CFG

  let result = G.analysis graph
  putStrLn "Exercise from Slides"
  putStrLn $ showLV result

main :: IO ()
main = example1 >> example2

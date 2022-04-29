{-# OPTIONS_GHC -Wall #-}
module Main where

import Game

--
-- Nonograms game program
--
-- Main terms:
-- Answer - image to find (puzzle solution)
-- Cell - cell of grid
-- Hint - number on the side of the grid
--
 

main :: IO ()
main = run

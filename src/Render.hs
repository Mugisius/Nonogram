module Render where

import Types
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Draw all game
drawApp :: GameState -> Picture
drawApp (Playing board rsX rsY) = Translate rsX rsY $ pictures [pictureGrid board, 
                                                      pictureCells board, 
                                                      pictureHints board]
drawApp Win = pictureWin

-- Draw parts of game
pictureGrid :: Board -> Picture
pictureGrid (Board x y _ _ _ _) = Pictures (genereteGrid x y)

genereteGrid :: Int -> Int -> [Picture]
genereteGrid x y = (genereteGridX (x+1) (y+1) 0) ++ (genereteGridY (y+1) (x+1) 0)

genereteGridX :: Int -> Int -> Int -> [Picture]
genereteGridX 0 _ _ = []
genereteGridX count size done | count == done = []
                              | otherwise = (Line [(shift, 0), (shift, len)]) 
                                               : genereteGridX count size (done + 1)
                                where shift = fromIntegral (done*gridSize)
                                      len = fromIntegral (size*gridSize)
                              
genereteGridY :: Int -> Int -> Int -> [Picture]
genereteGridY 0 _ _ = []
genereteGridY count size done | count == done = []
                              | otherwise = (Line [(0, shift), (len, shift)]) 
                                               : genereteGridY count size (done + 1)
                                where shift = fromIntegral (done*gridSize)
                                      len = fromIntegral (size*gridSize)
pictureCells :: Board -> Picture
pictureCells (Board _ _ _ _ cells _) = Pictures (genereteCells cells 0)

genereteCells :: [[Cell]] -> Int -> [Picture]
genereteCells [] _ = []
genereteCells (x:xs) shift = (genereteCellsLine x shift 0) ++ (genereteCells xs (shift+1))

genereteCellsLine :: [Cell] -> Int -> Int -> [Picture]
genereteCellsLine [] _ _ = []
genereteCellsLine (White:xs) vShift hShift = (genereteCellsLine xs vShift (hShift+1))
genereteCellsLine (Black:xs) vShift hShift = (Polygon [(bX, lY), (uX, lY), 
                                                       (uX, rY), (bX, rY)]) 
                                                       : (genereteCellsLine xs vShift (hShift+1))
                                            where bX = fI (spaceSize + hShift*gridSize)
                                                  lY = fI (spaceSize + vShift*gridSize)
                                                  uX = fI ((hShift+1)*gridSize - spaceSize)
                                                  rY = fI ((vShift+1)*gridSize - spaceSize)
                                                  fI = fromIntegral

pictureHints :: Board -> Picture
pictureHints (Board sX sY hX hY _ _) = Pictures (genereteHintsX hX sX 0  ++ 
                                                 genereteHintsY hY sY 0)

genereteHintsX :: Hints -> Int -> Int -> [Picture]
genereteHintsX [] _ _ = []
genereteHintsX (x:xs) shiftX shiftY = genereteHintsXLine x shiftX shiftY ++ 
                                      genereteHintsX xs shiftX (shiftY+1)

genereteHintsXLine :: [Int] -> Int -> Int -> [Picture]
genereteHintsXLine [] _ _ = []
genereteHintsXLine (x:xs) shiftX shiftY = (Translate sX sY $ Scale 0.2 0.2 $ Text $ show x)
                                                : genereteHintsXLine xs (shiftX+1) shiftY
                                          where sX = (fromIntegral (shiftX*(gridSize)))
                                                            + (fromIntegral gridSize)/4
                                                sY = (fromIntegral (shiftY*(gridSize))) 
                                                            + (fromIntegral gridSize)/4

genereteHintsY :: Hints -> Int -> Int -> [Picture]
genereteHintsY [] _ _ = []
genereteHintsY (x:xs) shiftY shiftX = genereteHintsYLine x shiftY shiftX ++
                                      genereteHintsY xs shiftY (shiftX+1)

genereteHintsYLine :: [Int] -> Int -> Int -> [Picture]
genereteHintsYLine [] _ _ = []
genereteHintsYLine (x:xs) shiftY shiftX = (Translate sX sY $ Scale 0.2 0.2 $ Text $ show x) 
                                                : genereteHintsYLine xs (shiftY+1) shiftX
                                          where sX = (fromIntegral (shiftX*(gridSize))) 
                                                            + (fromIntegral gridSize)/4
                                                sY = (fromIntegral (shiftY*(gridSize))) 
                                                            + (fromIntegral gridSize)/4

pictureWin :: Picture
pictureWin = Translate (-250.0) 0 (Text "You win!")


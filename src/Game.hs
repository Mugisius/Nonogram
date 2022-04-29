module Game
    ( run
    ) where

import Types
import Render
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


--------------------------------------------------------------------------------
--------------------------------LOAD PART---------------------------------------
--------------------------------------------------------------------------------

loadBoard :: [String] -> Either Error Board
loadBoard s = tryLoadBoard (chechFile s) (calculateSizes s) 
                           (calculateHints s) (calculateAnswer s)

chechFile :: [String] -> Maybe Error
chechFile [] = Just (Error "Empty file")
chechFile (x:xs) | all (==(length x)) (map (length) (x:xs)) = Nothing
                 | otherwise = Just (Error "Wrong file format")

-- Load board from file
tryLoadBoard :: Maybe Error -> (Int, Int) -> (Hints, Hints) -> [[Cell]] -> Either Error Board
tryLoadBoard (Just er) _ _ _ = Left er
tryLoadBoard _ (sX, sY) (hX, hY) a = Right (Board sX sY hX hY (blankBoard sX sY) a)

-- Сalculate hints from answer
calculateHints :: [String] -> (Hints, Hints)
calculateHints s1 = (reverse $ map (reverse . filter (\y -> y/=0) . 
                          foldl (\ (a:as) z  -> if z=='_' then (0:(a:as)) else (a+1):as) [0]) s1,
                    map (filter (\y -> y/=0) . 
                          foldl (\ (a:as) z  -> if z=='_' then (0:(a:as)) else (a+1):as) [0]) s2)
                      where s2 = transpose s1
                            transpose :: [[a]] -> [[a]]
                            transpose ([]:_) = []
                            transpose x = (map head x) : transpose (map tail x)

calculateAnswer :: [String] -> [[Cell]]
calculateAnswer s = reverse (map (map (\ x -> if x=='_' then White else Black )) s)

calculateSizes :: [String] -> (Int, Int)
calculateSizes [] = (0,0)
calculateSizes (x:xs) = (length x, length xs + 1)

blankBoard :: Int -> Int -> [[Cell]]
blankBoard n m | m < 1 = []
               | otherwise = blankLine n : blankBoard n (m-1)

blankLine :: Int -> [Cell]
blankLine n | n < 1 = []
            | otherwise = White : blankLine (n-1) 


--------------------------------------------------------------------------------
--------------------------------GAME PART---------------------------------------
--------------------------------------------------------------------------------

-- Handle clicks on cells
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y))  
            (Playing board@(Board _ _ _ _ cells _) rsX rsY) 
                = Playing board{ displayCells = (changeCell cX cY cells) } rsX rsY
                  where cX = round ((-rsX + x - gr/2)/gr)
                        cY = round ((-rsY + y - gr/2)/gr)
                        gr = fromIntegral gridSize
handleEvent _ s = s

changeCell :: Int -> Int -> [[Cell]] -> [[Cell]]
changeCell _ _ [] = []
changeCell n 0 (y:ys) = (changeCellInLine n y) : ys
changeCell n m (y:ys) | m < 0 = (y:ys)
                      | otherwise = y : (changeCell n (m-1) ys)

changeCellInLine :: Int -> [Cell] -> [Cell]
changeCellInLine _  [] = []
changeCellInLine 0 (White:xs) = Black : xs
changeCellInLine 0 (Black:xs) = White : xs
changeCellInLine n (x:xs) | n < 0 = (x:xs)
                          | otherwise = x : (changeCellInLine (n-1) xs)

-- Check displayCells==answerCells and finish the game if True
updateApp :: Float -> GameState -> GameState
updateApp _ (Playing board@(Board _ _ _ _ curCells targetCells) rsX rsY) 
                                              | curCells == targetCells = Win
                                              | otherwise = Playing board rsX rsY
updateApp _ s = s

run :: IO ()
run = do
  ans <- readFile answerPath
  case (loadBoard (lines ans)) of
    (Left (Error s)) -> putStrLn ("Error: "++s)
    (Right b) -> do
      play displayType bgColor fps initState drawApp handleEvent updateApp
        where displayType = InWindow "Nonogram" (windowSizeX, windowSizeY) (100, 100) 
              windowSizeX = (sizeX b + maxHintsX + 2)*gridSize
              windowSizeY = (sizeY b + maxHintsY + 2)*gridSize
              maxHintsX = maximum (map length (hintsX b))
              maxHintsY = maximum (map length (hintsY b))
              bgColor = white
              fps = 10
              initState = Playing b renderShiftX renderShiftY
              renderShiftX = -(fromIntegral ((sizeX b + maxHintsX)*gridSize))/2
              renderShiftY = -(fromIntegral ((sizeY b + maxHintsY)*gridSize))/2
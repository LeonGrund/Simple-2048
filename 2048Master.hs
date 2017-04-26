import Data.List
import System.IO

--instantiate integers
score = 0
undoScore = 0
maxInt = 2048
gridSize = 4
maxUndo = 1

--instantiate grids
grid = [[],[],[],[]]
undoGrid = [[1,2,3,4],
	       [5,6,7,8],
	       [9,10,11,12],
	       [13,14,15,16]]

--instantiate boolean
hasUndone = True

--we want to write a function that prints out introduction
intro :: IO ()
intro = do {putStrLn "#################################"; 
			putStrLn "           Console2048           ";
			putStrLn "#################################";
			putStrLn "Use the arrow keys to move around";
			putStrLn " Combine numbers to get to 2048! ";
			putStrLn " ";
			putStrLn "#################################";
			putStrLn " ";
			putStrLn "     Press any key to start...   ";
			putStrLn " ";
			putStrLn "#################################";}

--intro x = #################################\n\n     Press any key to start...   "

printGrid :: [[Integer]] -> IO ()
printGrid x = do {putStrLn $ show (x!!0);
	      	  putStrLn $ show (x!!1);
		  putStrLn $ show (x!!2);
		  putStrLn $ show (x!!3);}

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

getDirection :: String -> Integer
getDirection dir = case dir of
	     "w" -> 1
	     "d" -> 3
	     "s" -> 2
	     "a" -> 4
	     _ -> 5

upGrid :: [[Integer]] -> [[Integer]]
upGrid g = g

downGrid :: [[Integer]] -> [[Integer]]
downGrid g = g 

rightGrid :: [[Integer]] -> [[Integer]]
rightGrid g = g

leftGrid :: [[Integer]] -> [[Integer]]
leftGrid g = g


--up/down/right/leftGrid have to be coded
 
updateGrid :: Integer -> [[Integer]] -> [[Integer]]
updateGrid dir grid =
	   if dir == 1
	      then (replaceNth 1 [99] grid)
	      else if dir == 2
		then  (downGrid grid)
		else if dir == 3
			then (rightGrid grid)
			else if dir == 4
			     then (leftGrid grid)
			     else grid

{-
updateGrid 1 grid = upGrid grid 
updateGrid 2 grid = downGrid grid
updateGrid 4 grid = leftGrid grid
updateGrid 3 grid = rightGrid grid
updateGrid _ grid = grid
-}

main = do
	intro
	printGrid undoGrid


--THIS IN WHILE LOOP

	dir <- getLine
	--printGrid (replaceNth 1 [99] ([[1,1,1,1],[2,2,2,2]]))
	printGrid (updateGrid (getDirection dir) undoGrid)
	--printGrid undoGrid
	

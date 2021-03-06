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
aGrid = [[0,0,0,0],
		[0,0,0,0],
		[0,0,0,0],
		[0,0,0,0]]
testGrid = [0,0,0,0]

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

checkRow :: Integer -> [Integer] ->Bool
checkRow num row 
         |num`elem`row =True
         |otherwise=False

		 
indY :: [[Integer]] -> Integer -> Integer
indY grid num = 
     	  if (checkRow num (grid!!0))
	     then 0
	     else if (checkRow num (grid!!1))
	     	  then 1
		  else if (checkRow num (grid!!2))
		       then 2
		       else if (checkRow num (grid!!3))
		       	    then 3
			    else 9999


replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs
	 
--given index, new val, and list, replaces element
--replaceEl :: Integer -> Integer -> [[Integer]] -> [[Integer]]
--replaceEl ind newVal li = 


--replaceXY :: Integer -> Integer -> Integer -> [[Integer]] -> [[Integer]]
--replaceXY x1 y1 newVal grid = (replaceNth  ((grid!!y1)!!x1) newVal grid)

--replaceHelp :: [[Integer]] -> 
--Input: X Y newValue Grid
--Output: "updated" Grid
replaceNum :: Integer -> Integer -> Integer -> [[Integer]] -> [[Integer]]
replaceNum x y newVal (x1: x2 : x3 : x4) = 
			if (y==0)	
				then let l1 = x2
					in (x1 : l1 : x3 : x4)
			else if (y == 1)
				then x1 : x2 : x3 : x4
			else if (y == 2)
				then x1 : x2 : x3 : x4
			else if (y == 3)
				then x1 : x2 : x3 : x4
			else [[1]]

getDirection :: String -> String
getDirection dir = case dir of
	     "w" -> "up"
	     "d" -> "right"
	     "s" -> "down"
	     "a" -> "left"
	     _ -> "ErrorInvalidDir"

upGrid :: [[Integer]] -> [[Integer]]
upGrid g = g

downGrid :: [[Integer]] -> [[Integer]]
downGrid g = g 

rightGrid :: [[Integer]] -> [[Integer]]
rightGrid g = g

leftGrid :: [[Integer]] -> [[Integer]]
leftGrid g = g


--up/down/right/leftGrid have to be coded
 
updateGrid :: String -> [[Integer]] -> [[Integer]]
updateGrid dir grid =
	   if dir == "up"
	      then (replaceNth 1 [99] grid)
	      else if dir == "down"
		then  (downGrid grid)
		else if dir == "right"
			then (rightGrid grid)
			else if dir == "left"
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
	--printGrid(replaceNth 
	printGrid(replaceNum 0 0 1 aGrid)
	--intro
	--printGrid undoGrid
	

--THIS IN WHILE LOOP

	--dir <- getLine
	--printGrid (replaceNth (indY undoGrid 6) [9,9,9,9] undoGrid)

	--printGrid (updateGrid (getDirection dir) undoGrid)
	
	--printGrid undoGrid
	

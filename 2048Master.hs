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

--given grid and integer, insert integer into grid at random
--location that contains a 0
insertValue :: [[Integer]] -> Integer -> [[Integer]]
insertValue x y  =  x

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

--will take x and y coordinate and replace the value with newVal	 
--replaceNth2d x1 y1 newVal ([x]:xs)
--	replaceNth2d = [newVal :x] : xs


printGrid :: [[Integer]] -> IO ()
printGrid x = do {putStrLn $ show (x!!0);
	  	  putStrLn $ show (x!!1);
		  putStrLn $ show (x!!2);
		  putStrLn $ show (x!!3);}

getDirection :: String -> String
getDirection dir = case dir of
	     "w" -> "up"
	     "d" -> "right"
	     "s" -> "down"
	     "a" -> "left"
	     _ -> "ErrorInvalidDir"

--up/down/right/leftGrid have to be coded 
--updateGrid :: String ->  [[Integer],[Integer],[Integer],[Integer]] -> [[Integer],[Integer],[Integer],[Integer]]
--updateGrd dir grid = case dir of
--	  "up" g -> upGrid g
--	  "down" g ->downGrid g
--	  "right" g -> rightGrid g
--	  "left" g -> leftGrid g
--	  _ g -> g

main = do
	intro
	printGrid undoGrid
	--printGrid(insertValue aGrid 5)

--THIS IN WHILE LOOP

	--dir <- getLine
	--updateGrid (getDirection dir) (undoGrid)
	--printGrid undoGrid
	

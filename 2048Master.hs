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
printGrid x = do {putStrLn $ show (x!!0); putStrLn $ show (x!!1); putStrLn $ show (x!!2); putStrLn $ show (x!!3);}

main = do
	intro
	printGrid undoGrid

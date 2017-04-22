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
undoGrid = [[],[],[],[]]

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

main = do
	intro 
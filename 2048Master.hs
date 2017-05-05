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
undoGrid = [[0,0,0,0],
	       [0,0,2,0],
	       [0,0,0,2],
	       [0,0,0,0]]

--instantiate boolean
hasUndone = True

--we want to write a function that prints out introduction
intro :: IO ()
intro = do {putStrLn "####################################"; 
			putStrLn "           Console2048           ";
			putStrLn "####################################";
			putStrLn "Use the w,a,s,d keys to move around";
			putStrLn " Combine numbers to get to 2048! ";
			putStrLn "####################################";
			putStrLn " ";}

printGrid :: [[Integer]] -> IO ()
printGrid x = do {putStrLn $ show (x!!0);
	      	  putStrLn $ show (x!!1);
		  putStrLn $ show (x!!2);
		  putStrLn $ show (x!!3);}

printIndex :: [[Integer]] -> IO ()
printIndex x = do {putStrLn $ show (x!!0);
	      	  putStrLn $ show (x!!1);}

--check if num is in row 
checkRow :: Integer -> [Integer] ->Bool
checkRow num row 
         |num`elem`row =True
         |otherwise=False

--given a grid and a number, indY returns the row number (y index) containing that number 
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

--returns what col the number is in the particular row
indXHelper :: [Integer] -> Integer -> Integer
indXHelper row num =
	   if (row!!0 == num)
	      then 0
	   else if (row!!1 == num)
	      then 1
	   else if (row!!2 == num)
	      then 2
	   else if (row!!3 == num)
	      then 3
	   else 9999

--checks each row for the number and return the col number (x index)
indX :: [[Integer]] -> Integer -> Integer
indX grid num = 
     	  if (checkRow num (grid!!0))
	     then (indXHelper (grid!!0) num)
	  else if (checkRow num (grid!!1))
	     then (indXHelper (grid!!1) num)
	  else if (checkRow num (grid!!2))
	     then (indXHelper (grid!!2) num)
	  else if (checkRow num (grid!!3))
	     then (indXHelper (grid!!3) num)
	  else 9999

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

getDirection :: String -> String
getDirection dir = case dir of
	     "w" -> "up"
	     "d" -> "right"
	     "s" -> "down"
	     "a" -> "left"
	     _ -> "ErrorInvalidDir"

--replaces a number in grid at x,y with newVal and return that updated grid
replaceXY :: Integer -> Integer -> Integer -> [[Integer]] -> [[Integer]]
replaceXY x y newVal ([a,b,c,d] : [e,f,g,h] : [i,j,k,l] : [[m,n,o,p]]) = 
		if (y == 0 && x == 0)
			then [newVal,b,c,d] : [e,f,g,h] : [i,j,k,l]:[[m,n,o,p]]
		else if (y == 0 && x == 1)
			then [a,newVal,c,d] : [e,f,g,h] : [i,j,k,l]:[[m,n,o,p]]
		else if (y == 0 && x == 2)
			then [a,b,newVal,d] : [e,f,g,h] : [i,j,k,l]:[[m,n,o,p]]
		else if (y == 0 && x == 3)
			then [a,b,c,newVal] : [e,f,g,h] : [i,j,k,l]:[[m,n,o,p]]
		else if (y == 1 && x == 0)
			then [a,b,c,d] : [newVal,f,g,h] : [i,j,k,l]:[[m,n,o,p]]
		else if (y == 1 && x == 1)
			then [a,b,c,d] : [e,newVal,g,h] : [i,j,k,l]:[[m,n,o,p]]
		else if (y == 1 && x == 2)
			then [a,b,c,d] : [e,f,newVal,h] : [i,j,k,l]:[[m,n,o,p]]
		else if (y == 1 && x == 3)
			then [a,b,c,d] : [e,f,g,newVal] : [i,j,k,l]:[[m,n,o,p]]
		else if (y  == 2 && x == 0)
			then [a,b,c,d] : [e,f,g,h] : [newVal,j,k,l]:[[m,n,o,p]]
		else if (y == 2 && x == 1)
			then [a,b,c,d] : [e,f,g,h] : [i,newVal,k,l]:[[m,n,o,p]]
		else if (y == 2 && x == 2)
			then [a,b,c,d] : [e,f,g,h] : [i,j,newVal,l]:[[m,n,o,p]]
		else if (y == 2 && x == 3)
			then [a,b,c,d] : [e,f,g,h] : [i,j,k,newVal]:[[m,n,o,p]]
		else if (y == 3 && x == 0)
			then [a,b,c,d] : [e,f,g,h] : [i,j,k,l]:[[newVal,n,o,p]]
		else if (y == 3 && x == 1)
			then [a,b,c,d] : [e,f,g,h] : [i,j,k,l]:[[m,newVal,o,p]]
		else if (y == 3 && x == 2)
			then [a,b,c,d] : [e,f,g,h] : [i,j,k,l]:[[m,n,newVal,p]]
		else if (y == 3 && x == 3)
			then [a,b,c,d] : [e,f,g,h] : [i,j,k,l]:[[m,n,o,newVal]]
	
		else [[1]]

--calls the replaceXY function depending on the move 
upGrid :: Integer -> Integer -> Integer -> [[Integer]] -> [[Integer]]
upGrid x y newVal grid = (replaceXY x 0 newVal grid)   

downGrid :: Integer -> Integer -> Integer -> [[Integer]] -> [[Integer]]
downGrid x y newVal grid = (replaceXY x 3 newVal grid)   

leftGrid :: Integer -> Integer -> Integer -> [[Integer]] -> [[Integer]]
leftGrid x y newVal grid = (replaceXY 0 y newVal grid)   

rightGrid :: Integer -> Integer -> Integer -> [[Integer]] -> [[Integer]]
rightGrid x y newVal grid = (replaceXY 3 y newVal grid)   


--main function: calls helper function to update grid depending on user input 
updateGrid :: String -> Integer -> Integer -> Integer -> [[Integer]] -> [[Integer]]
updateGrid dir x y newVal grid =
	   if dir == "up"
	      then (upGrid x y newVal grid)
	   else if dir == "down"
	      then (downGrid x y newVal grid)
	   else if dir == "right"
	      then (rightGrid x y newVal grid)
	   else if dir == "left"
           then (leftGrid x y newVal grid)
       else grid

--take grid and number we looking for, output [[x,y],[x,y]] list pairs
indexPairs :: [[Integer]] -> Integer -> [[Integer]]
indexPairs grid num = 
		if grid /= [[1]]
			then ([[(indX grid num),(indY grid num)]] ++ (indexPairs (replaceXY (indX grid num) (indY grid num) 0 grid) num))
		else [[1]]

validNumCheck :: [[Integer]] -> Bool
validNumCheck list =
		if (list!!1)!!0 == 9999
			then False
		else True

playGame :: [[Integer]] -> Integer -> IO ()
playGame grid curNum = do {
			

			dir <- getLine;

			(if (getDirection dir) == "ErrorInvalidDir"
				then (playGame grid curNum)
			else do {

				printIndex (indexPairs grid curNum);

				(if (validNumCheck (indexPairs grid curNum)) == True
					then printGrid (updateGrid (getDirection dir) (((indexPairs grid curNum)!!1)!!0) (((indexPairs grid curNum)!!1)!!1) curNum (replaceXY (((indexPairs grid curNum)!!1)!!0) (((indexPairs grid curNum)!!1)!!1) 0 (updateGrid (getDirection dir) (((indexPairs grid curNum)!!0)!!0) (((indexPairs grid curNum)!!0)!!1) curNum (replaceXY (((indexPairs grid curNum)!!0)!!0) (((indexPairs grid curNum)!!0)!!1) 0 grid)))) 
			-- we know they should be added, call andrew_func 
				else printGrid (updateGrid (getDirection dir) (((indexPairs grid curNum)!!0)!!0) (((indexPairs grid curNum)!!0)!!1) (curNum*2) (replaceXY (((indexPairs grid curNum)!!0)!!0) (((indexPairs grid curNum)!!0)!!1) 0 grid)));

				(if (validNumCheck (indexPairs grid curNum)) == True
					then playGame (updateGrid (getDirection dir) (((indexPairs grid curNum)!!1)!!0) (((indexPairs grid curNum)!!1)!!1) curNum (replaceXY (((indexPairs grid curNum)!!1)!!0) (((indexPairs grid curNum)!!1)!!1) 0 (updateGrid (getDirection dir) (((indexPairs grid curNum)!!0)!!0) (((indexPairs grid curNum)!!0)!!1) curNum (replaceXY (((indexPairs grid curNum)!!0)!!0) (((indexPairs grid curNum)!!0)!!1) 0 grid)))) curNum

				else playGame (updateGrid (getDirection dir) (((indexPairs grid curNum)!!0)!!0) (((indexPairs grid curNum)!!0)!!1) (curNum*2) (replaceXY (((indexPairs grid curNum)!!0)!!0) (((indexPairs grid curNum)!!0)!!1) 0 grid)) (curNum*2));
			});
			}


main = do

intro
printGrid undoGrid
playGame undoGrid 2
		 

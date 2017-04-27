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

--check if num is in row 
checkRow :: Integer -> [Integer] ->Bool
checkRow num row 
         |num`elem`row =True
         |otherwise=False

--given a grid and a num, indY returns the row number (y index) containing that num in grid 
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

--checks what col the num is in the particular row
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

--checks each row for num and return the col number (x index) of the num in grid
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
	      then (upGrid	 grid)
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
		
main = do
	intro
	printGrid undoGrid


--THIS IN WHILE LOOP

	dir <- getLine	

	--replace the row containing 6 with [9,9,9,9]
	--printGrid (replaceNth (indY undoGrid 6) [9,9,9,9] undoGrid)

	--replace the row containing at index of col where 4 is  with [9,9,9,9]
	printGrid (replaceNth (indX undoGrid 4) [9,9,9,9] undoGrid)

	--printGrid (updateGrid (getDirection dir) undoGrid)
	
	--printGrid undoGrid
	

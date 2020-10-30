{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A

cellInSet :: Char -> [Char] -> Bool
cellInSet char set
	| set == [] = False
	| char == head set = True
	| otherwise = cellInSet char (tail set)

upCells :: [Char]
upCells = [winUp, startUp, verPipe, botLeft, botRight]

downCells :: [Char]
downCells = [winDown, startDown, verPipe, topLeft, topRight]

leftCells :: [Char]
leftCells = [horPipe, botRight, topRight, startLeft, winLeft]

rightCells :: [Char]
rightCells = [horPipe, botLeft, topLeft, startRight, winRight]

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell {cellType :: (Char)}
	deriving (Eq, Ord) --TODO

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Lvl {cellArray :: (A.Array Position Cell)} --TODO
    deriving (Eq, Ord)

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Level where
	show (Lvl arr) = foldl printElem [endl] list
		where
		maxIndex = snd $ snd $ A.bounds arr
		list = A.assocs arr
		printElem string asoc
			| snd (fst asoc) == maxIndex = string ++ [(cellType (snd asoc))] ++ [endl]
			| otherwise = string ++ [cellType (snd asoc)]

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel rightDown = Lvl $ A.array ((0,0), rightDown) [((i, j), (Cell emptySpace)) | i <- [0 .. (fst rightDown)], j <- [0 .. (snd rightDown)]]

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

-- helper pentru a obtine celula de la o pozitie din matrice
getCell :: Position -> Level -> Cell
getCell pos (Lvl arr) = arr A.! pos

addCell :: (Char, Position) -> Level -> Level
addCell (pipeType, (f, s)) (Lvl arr)
	| f < 0 || s < 0 || f > (fst $ snd $ A.bounds arr) || s > (snd $ snd $ A.bounds arr) = Lvl arr
	| (cellType $ getCell (f, s) (Lvl arr)) /= emptySpace = Lvl arr
	| otherwise = Lvl $ modify arr $ (Cell pipeType)
								where
								modify arr1 newPipe = arr1 A.// [((f, s), newPipe)]

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos [] = emptyLevel pos
createLevel pos list = foldl addCellReverse (emptyLevel pos) (reverse list)
		where
		-- Este folosita pentru a evita recursivitatea explicita
		addCellReverse level pipeTypeAndPos = addCell pipeTypeAndPos level

{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

newPosition :: Position -> Directions -> Position
newPosition (line, column) d = case d of
				North -> (line - 1, column)
				South -> (line + 1, column)
				East -> (line, column + 1)
				West -> (line, column - 1)

moveCell :: Position -> Directions -> Level -> Level
moveCell pos dir level
	| fst newPos < 0 || snd newPos < 0 || fst newPos > (fst $ snd $ A.bounds (cellArray level)) || snd newPos > (snd $ snd $ A.bounds (cellArray level)) = level -- out of bounds
	| cellInSet (cellType (getCell pos level)) startCells || cellInSet (cellType (getCell pos level)) winningCells = level
	| getCell (newPosition pos dir) level /= (Cell emptySpace) = level --celula vecina nu e goala
	| otherwise = addCell (cellType $ getCell pos level, (newPosition pos dir)) (Lvl (putSpace (cellArray level)))
	where
	newPos = newPosition pos dir
	putSpace cellArr = cellArr A.// [(pos, (Cell emptySpace))] -- pune celula libera la vechea coordonata

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection (Cell type1) (Cell type2) dir
	| cellInSet type1 upCells && cellInSet type2 downCells && dir == North = True
	| cellInSet type1 downCells && cellInSet type2 upCells && dir == South = True
	| cellInSet type1 leftCells && cellInSet type2 rightCells && dir == West = True
	| cellInSet type1 rightCells && cellInSet type2 leftCells && dir == East = True
	| otherwise = False


{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

getStartCell :: Level -> Position
getStartCell (Lvl arr) = findInList list
					where
					list = A.assocs arr
					findInList l
						| l == [] = (-1,-1)
						| cellInSet (cellType (snd (head l))) startCells = fst (head l)
						| otherwise = findInList (tail l)

initialDir :: Cell -> Directions
initialDir (Cell char)
	| char == startUp = South
	| char == startDown = North
	| char == startLeft = East
	| otherwise = West

initialStartPos :: Cell -> Position -> Position
initialStartPos (Cell char) (x, y)
	| char == startUp = (x - 1, y)
	| char == startDown = (x + 1, y)
	| char == startLeft = (x, y - 1)
	| otherwise = (x, y + 1)

nextDirPos :: Cell -> Position -> Directions -> (Position, Directions) -- precDir = directia din care vine
nextDirPos (Cell char) (x, y) prevDir
	| prevDir == South && (cellInSet char downCells) = if char == topLeft then ((x, y + 1), West) else if char == topRight then ((x, y - 1), East) else ((x - 1, y), South)
	| prevDir == North && (cellInSet char upCells) = if char == botLeft then ((x, y + 1), West) else if char == botRight then ((x, y - 1), East) else ((x + 1, y), North)
	| prevDir == East && (cellInSet char rightCells) = if char == topLeft then ((x + 1, y), North) else if char == botLeft then ((x - 1, y), South) else ((x, y - 1), East)
	| prevDir == West && (cellInSet char leftCells) = if char == topRight then ((x + 1, y), North) else if char == botRight then ((x - 1, y), South) else ((x, y + 1), West)
	| otherwise = ((-1,-1), South)

wonLevel :: Level -> Bool
wonLevel level = findRoute initialPos initialDirection
				where
				maxLine = fst $ snd $ A.bounds $ cellArray level
				maxColumn = snd $ snd $ A.bounds $ cellArray level
				initialPos = initialStartPos (getCell (getStartCell level) level) (getStartCell level)
				initialDirection = initialDir $ getCell (getStartCell level) level
				findRoute pos dir
					| fst pos < 0 || fst pos > maxLine || snd pos < 0 || snd pos > maxColumn = False -- out of bounds
					| cellInSet (cellType (getCell pos level)) winningCells = True -- arrived at win
					| nextPos == (-1, -1) = False -- next is out of bounds
					| connection (getCell nextPos level) (getCell pos level) nextDir == False = False -- not connected
					|otherwise = findRoute nextPos nextDir -- go to next cell
					where
					(nextPos, nextDir) = if fst pos >= 0 && snd pos >= 0 then nextDirPos (getCell pos level) pos dir else ((-1, -1), South)
				
isMoveValid :: Position -> Position -> Directions -> Level -> Bool
isMoveValid initialPos (maxLine, maxColumn) dir level
	| nextLine < 0 || nextColumn < 0 || nextLine > maxLine || nextColumn > maxColumn = False -- next pos out of bounds
	| (cellType (getCell initialPos level)) == emptySpace = False
	| cellInSet (cellType (getCell initialPos level)) startCells || cellInSet (cellType (getCell initialPos level)) winningCells = False --next pos imutable
	| getCell (nextLine, nextColumn) level /= (Cell emptySpace) = False -- next pos is not free
	| otherwise = True
		where
		(nextLine, nextColumn) = newPosition initialPos dir
	
zero :: Int
zero = 0

instance ProblemState Level (Position, Directions) where
    successors level = foldl createState [] listPosDirection
		where
		limits = snd $ A.bounds $ cellArray level
		listPosDirection = [((x, y), dir) | x <- [zero .. (fst limits)], y <- [zero .. (snd limits)], dir <- [North, South, East, West]]
		createState state (pos, dir)
			| (isMoveValid pos limits dir level) == False = state
			| otherwise = state ++ [((pos, dir), (moveCell pos dir level))]

    isGoal = wonLevel
    reverseAction ((pos, dir), level) = if valid then ((nextPos, nextDir), newLevel) else ((pos, dir), level)
						where
						limits = snd $ A.bounds $ cellArray level
						valid = isMoveValid nextPos limits nextDir level
						newLevel = moveCell nextPos nextDir level
						nextPos = newPosition pos dir
						nextDir = case dir of
							South -> North
							North -> South
							East -> West
							West -> East
	
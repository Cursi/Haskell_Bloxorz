{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Bloxorz where

import ProblemState

import qualified Data.Matrix as M

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = Cell (Char, [Position])
    deriving (Eq, Ord)

instance Show Cell where
    show (Cell cell) = [(fst cell)] 

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = Level (M.Matrix (Cell), [Position], Int)
    deriving (Eq)

{-
    *** Opțional ***
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

instance Ord Level where
    compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show.

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

incrementPosition :: Position -> Int -> Int -> Position
incrementPosition position xIncrement yIncrement = ((fst position) + xIncrement, (snd position) + yIncrement)

concatStrings :: [[Char]] -> [Char]
concatStrings strings = foldl1 (\ x y -> x ++ y) strings

printLevel :: M.Matrix (Cell) -> [Position] -> [Char]
printLevel matrix blockPositions = unlines [concatStrings [show ((M.setElem (Cell (block, [])) (incrementPosition (head blockPositions) 1 1) newMatrix) M.! (x, y)) | y <- [1..(M.ncols matrix)]] | x <- [1..(M.nrows matrix)]]
    where newMatrix = if (last blockPositions) == (-1, -1) then matrix else (M.setElem (Cell (block, [])) (incrementPosition (last blockPositions) 1 1) matrix)

instance Show Level where
    show (Level (matrix, blockPositions, gameState)) = 
        case gameState of
            1 -> "\n" ++ printLevel matrix blockPositions ++ "Game Over\n"
            2 -> "\n" ++ printLevel matrix blockPositions ++ "Congrats! You won!\n"
            _ -> "\n" ++ printLevel matrix blockPositions
{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel cornerPosition blockPosition = Level (newMatrix, newBlockPositions, 0) 
    where
        newMatrix = M.fromList ((fst cornerPosition) + 1) ((snd cornerPosition) + 1) (repeat (Cell (emptySpace, [])))
        newBlockPositions = [blockPosition, (-1,- 1)]
{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard
        'S' pentru tile soft
        'W' pentru winning tile
-}

addTile :: Char -> Position -> Level -> Level
addTile tileType tilePosition (Level (matrix, blockPositions, _)) = Level (newMatrix, blockPositions, 0)
    where 
        newMatrix = M.setElem (neededTile tileType) (incrementPosition tilePosition 1 1) matrix
        neededTile 'H' = (Cell (hardTile, []))
        neededTile 'S' = (Cell (softTile, []))
        neededTile 'W' = (Cell (winningTile, []))
        neededTile _ = undefined

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch switchPosition linkedPositions (Level (matrix, blockPositions, _)) = Level (newMatrix, blockPositions, 0)
    where newMatrix = M.setElem (Cell (switch, linkedPositions)) (incrementPosition switchPosition 1 1) matrix

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

-- gameState din Level poate lua valorile:
--     0 - neterminat
--     1 - pierdut
--     2 - castigat

replaceCells :: Char -> [Position] -> M.Matrix (Cell) -> M.Matrix (Cell)
replaceCells replaceChar linkedPositions matrix = M.fromList (M.nrows matrix) (M.ncols matrix) [if (incrementPosition (i,j) (-1) (-1)) `elem` linkedPositions then (Cell (replaceChar, [])) else M.getElem i j matrix | i <- [1..M.nrows matrix], j <- [1..M.ncols matrix]]

activate :: Cell -> Level -> Level
activate (Cell (tile, linkedPositions)) (Level (matrix, blockPositions, gameState)) = (Level (newMatrix, blockPositions, newGameState))
    where
        newMatrix 
            | (tile == switch) = if firstLinkedTile == emptySpace then replaceCells hardTile linkedPositions matrix else replaceCells emptySpace linkedPositions matrix
            | otherwise = matrix
            where 
                (Cell (firstLinkedTile, _)) = M.getElem (fst (incrementPosition (head linkedPositions) 1 1)) (snd (incrementPosition (head linkedPositions) 1 1)) matrix 
        newGameState 
            | (tile == softTile) = if gameState /= 0 then gameState else if (last blockPositions) == (-1, -1) then 1 else 0
            | (tile == winningTile) = if gameState /= 0 then gameState else if (last blockPositions) == (-1, -1) then 2 else 0
            | (tile == emptySpace) = 1
            | otherwise = if gameState /= 0 then gameState else 0
{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

getMinXPosition :: Position -> Position -> Position
getMinXPosition firstPosition secondPosition = if (fst firstPosition) < (fst secondPosition) then firstPosition else secondPosition

getMaxXPosition :: Position -> Position -> Position
getMaxXPosition firstPosition secondPosition = if (fst firstPosition) > (fst secondPosition) then firstPosition else secondPosition

getMinYPosition :: Position -> Position -> Position
getMinYPosition firstPosition secondPosition = if (snd firstPosition) < (snd secondPosition) then firstPosition else secondPosition

getMaxYPosition :: Position -> Position -> Position
getMaxYPosition firstPosition secondPosition = if (snd firstPosition) > (snd secondPosition) then firstPosition else secondPosition

move :: Directions -> Level -> Level
move direction (Level (matrix, blockPositions, gameState)) = activate firstCell (activate secondCell (Level (matrix, newBlockPositions, gameState)))
    where
        firstCell = M.getElem (fst (incrementPosition (head newBlockPositions) 1 1)) (snd (incrementPosition (head newBlockPositions) 1 1)) matrix 
        secondCell = if (last newBlockPositions) == (-1, -1) then (Cell (hardTile, [])) else (M.getElem (fst (incrementPosition (last newBlockPositions) 1 1)) (snd (incrementPosition (last newBlockPositions) 1 1)) matrix)
        newBlockPositions = 
            case direction of
                North -> if secondPosition == (-1, -1) then [incrementPosition firstPosition (-1) 0, incrementPosition firstPosition (-2) 0] else if (snd firstPosition) == (snd secondPosition) then [incrementPosition (getMinXPosition firstPosition secondPosition) (-1) 0, (-1, -1)] else [incrementPosition firstPosition (-1) 0, incrementPosition secondPosition (-1) 0]
                South -> if secondPosition == (-1, -1) then [incrementPosition firstPosition 1 0, incrementPosition firstPosition 2 0] else if (snd firstPosition) == (snd secondPosition) then [incrementPosition (getMaxXPosition firstPosition secondPosition) 1 0, (-1, -1)] else [incrementPosition firstPosition 1 0, incrementPosition secondPosition 1 0]
                West -> if secondPosition == (-1, -1) then [incrementPosition firstPosition 0 (-1), incrementPosition firstPosition 0 (-2)] else if (fst firstPosition) == (fst secondPosition) then [incrementPosition (getMinYPosition firstPosition secondPosition) 0 (-1), (-1, -1)] else [incrementPosition firstPosition 0 (-1), incrementPosition secondPosition 0 (-1)]
                East -> if secondPosition == (-1, -1) then [incrementPosition firstPosition 0 1, incrementPosition firstPosition 0 2] else if (fst firstPosition) == (fst secondPosition) then [incrementPosition (getMaxYPosition firstPosition secondPosition) 0 1, (-1, -1)] else [incrementPosition firstPosition 0 1, incrementPosition secondPosition 0 1]
                where 
                    firstPosition = (head blockPositions) 
                    secondPosition = (last blockPositions) 

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (Level (_, _, gameState)) = gameState == 0
{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

continueGameInDirection :: (Directions, Level) -> Bool
continueGameInDirection (_, level) = continueGame level 

instance ProblemState Level Directions where
    successors s = filter continueGameInDirection [(North, move North s), (South, move South s), (East, move East s), (West, move West s)] 
    isGoal (Level (_, _, gameState)) = gameState == 2 

    -- Doar petru BONUS
    -- heuristic = undefined
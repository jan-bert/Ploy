module Ploy where  -- do NOT CHANGE export of module

import Board

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
import Data.Char
import Data.Bits ( (.&.), (.|.), shift )



-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ (show startR) ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 

rotate :: Int -> Int -> Int
rotate o tr = (.&.) ((.|.) (shift o tr) (shift o (tr-8))) 255



-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

gameFinished :: Board -> Bool
gameFinished x = gameFinishedrec x False False False False

gameFinishedrec :: Board -> Bool -> Bool -> Bool -> Bool -> Bool
gameFinishedrec [] wo wc bo bc = if (wo && wc && bo && bc) then False
                                 else True 
gameFinishedrec (x:xs) wo wc bo bc = gameFinishedrec xs (rowhaswhiteOther x wo) (rowhaswhiteComander x wc) (rowhasblackOther x bo) (rowhasblackComander x bc)                        

rowhaswhiteComander :: [Cell] -> Bool -> Bool
rowhaswhiteComander _ True = True
rowhaswhiteComander [] _ = False
rowhaswhiteComander (x:xs) t = case x of 
    Empty -> rowhaswhiteComander xs t
    Piece White i -> if(i == 170 || i == 85) then True
                    else rowhaswhiteComander xs t
    Piece Black _ -> rowhaswhiteComander xs t

rowhaswhiteOther :: [Cell] -> Bool -> Bool
rowhaswhiteOther _ True = True
rowhaswhiteOther [] _ = False
rowhaswhiteOther (x:xs) t = case x of 
    Empty -> rowhaswhiteOther xs t 
    Piece White i -> if(i /= 170 && i /= 85) then True
                    else rowhaswhiteOther xs t 
    Piece Black _ -> rowhaswhiteOther xs t

rowhasblackComander :: [Cell] -> Bool -> Bool
rowhasblackComander _ True = True
rowhasblackComander [] _ = False
rowhasblackComander (x:xs) t = case x of 
    Empty -> rowhasblackComander xs t
    Piece Black i -> if(i == 170 || i == 85) then True
                     else rowhasblackComander xs t
    Piece White _ -> rowhasblackComander xs t

rowhasblackOther :: [Cell] -> Bool -> Bool
rowhasblackOther _ True = True
rowhasblackOther [] _ = False
rowhasblackOther (x:xs) t = case x of 
    Empty -> rowhasblackOther xs t
    Piece Black i -> if(i /= 170 && i /= 85) then True
                     else rowhasblackOther xs t
    Piece White _ -> rowhasblackOther xs t



-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

isValidMove :: Board -> Move -> Bool
isValidMove b m = if (turn m) > -1 && (turn m) < 8 && (start m) == (target m) then True
                  else if (turn m) > -1 && (turn m) < 8 && (start m) /= (target m)  then checkLine b (start m) (line (start m) (target m))
                  else False

checkLine :: Board -> Pos -> [Pos] -> Bool
checkLine b s (x:[]) = case (b!!(invertRow (row x))!!(collumchartoint (col x))) of 
                      Empty -> True
                      Piece p _ -> if p /= returnPlayer ((b!!(invertRow (row s)))!!(collumchartoint (col s))) then True
                                   else False
                       
checkLine b s (x:xs) = if(x == s) then checkLine b s xs 
                      else if(b!!(invertRow (row x)))!!(collumchartoint (col x)) /= Empty then False
                      else checkLine b s xs

invertRow:: Int -> Int 
invertRow 1 = 8
invertRow 2 = 7
invertRow 3 = 6
invertRow 4 = 5
invertRow 5 = 4
invertRow 6 = 3
invertRow 7 = 2
invertRow 8 = 1
invertRow 9 = 0

collumchartoint:: Char -> Int 
collumchartoint 'i' = 8
collumchartoint 'h' = 7
collumchartoint 'g' = 6
collumchartoint 'f' = 5
collumchartoint 'e' = 4
collumchartoint 'd' = 3
collumchartoint 'c' = 2
collumchartoint 'b' = 1
collumchartoint 'a' = 0


returnPlayer :: Cell -> Player
returnPlayer c = case c of
              Piece p _ -> p 



-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves p c = case c of
              Empty -> []
              Piece _ i -> addRotation (Move p p 0) 1 ++ moveFigur p i 0 (returnFigur i 0 0)

returnFigur :: Int -> Int -> Int -> Int
returnFigur i c f = if c >= 8 then f
                  else if c < 8 && i `mod` 2 == 1 then returnFigur (i`div`2) (c+1) (f+1)
                  else  returnFigur (i`div`2) (c+1) f

moveFigur :: Pos -> Int -> Int -> Int -> [Move]
moveFigur p i 0 f = if i `mod` 2 == 1 then up p f ++ moveFigur p (i`div`2) 1 f
                    else moveFigur p (i`div`2) 1 f
moveFigur p i 1 f = if i `mod` 2 == 1 then digul p f ++ moveFigur p (i`div`2) 2 f
                    else moveFigur p (i`div`2) 2 f
moveFigur p i 2 f = if i `mod` 2 == 1 then right_me p f ++ moveFigur p (i`div`2) 3 f
                    else moveFigur p (i`div`2) 3 f
moveFigur p i 3 f = if i `mod` 2 == 1 then digdr p f ++ moveFigur p (i`div`2) 4 f
                    else moveFigur p (i`div`2) 4 f
moveFigur p i 4 f = if i `mod` 2 == 1 then down p f ++ moveFigur p (i`div`2) 5 f
                    else moveFigur p (i`div`2) 5 f
moveFigur p i 5 f = if i `mod` 2 == 1 then digdl p f ++ moveFigur p (i`div`2) 6 f
                    else moveFigur p (i`div`2) 6 f
moveFigur p i 6 f = if i `mod` 2 == 1 then left_me p f ++ moveFigur p (i`div`2) 7 f
                    else moveFigur p (i`div`2) 7 f
moveFigur p i 7 f = if i `mod` 2 == 1 then digur p f ++ moveFigur p (i`div`2) 8 f
                    else moveFigur p (i`div`2) 8 f
moveFigur _ _ _ _ = []


digul :: Pos -> Int -> [Move]
digul p 1 = if ((row p) < 9) && (((ord(col p))-96) > 1) then [Move p (Pos (chr((ord(col p))-1)) ((row p)+1)) 0] ++ addRotation (Move p (Pos (chr((ord(col p))-1)) ((row p)+1)) 0) 1
         else []
digul p 2 = if ((row p) < 8) && (((ord(col p))-96) > 2) then [Move p (Pos (chr((ord(col p))-1)) ((row p)+1)) 0, Move p (Pos (chr((ord(col p))-2)) ((row p)+2)) 0]
         else if ((row p) == 8) && (((ord(col p))-96) >= 2) then [Move p (Pos (chr((ord(col p))-1)) ((row p)+1)) 0]
         else if ((row p) <= 8) && (((ord(col p))-96) == 2) then [Move p (Pos (chr((ord(col p))-1)) ((row p)+1)) 0]
         else []
digul p 3 = if ((row p) < 7) && (((ord(col p))-96) > 3) then [Move p (Pos (chr((ord(col p))-1)) ((row p)+1)) 0, Move p (Pos (chr((ord(col p))-2)) ((row p)+2)) 0, Move p (Pos (chr((ord(col p))-3)) ((row p)+3)) 0]
         else if ((row p) == 7) && (((ord(col p))-96) >= 3) then [Move p (Pos (chr((ord(col p))-1)) ((row p)+1)) 0, Move p (Pos (chr((ord(col p))-2)) ((row p)+2)) 0]
         else if ((row p) <= 7) && (((ord(col p))-96) == 3) then [Move p (Pos (chr((ord(col p))-1)) ((row p)+1)) 0, Move p (Pos (chr((ord(col p))-2)) ((row p)+2)) 0]
         else if ((row p) == 8) && (((ord(col p))-96) >= 2) then [Move p (Pos (chr((ord(col p))-1)) ((row p)+1)) 0]
         else if ((row p) <= 8) && (((ord(col p))-96) == 2) then [Move p (Pos (chr((ord(col p))-1)) ((row p)+1)) 0]
         else []
digul p 4 = if ((row p) < 9) && (((ord(col p))-96) > 1) then [Move p (Pos (chr((ord(col p))-1)) ((row p)+1)) 0]
         else []   

digur :: Pos -> Int -> [Move]
digur p 1 = if ((row p) < 9) && (((ord(col p))-96) < 9) then [Move p (Pos (chr((ord(col p))+1)) ((row p)+1)) 0] ++ addRotation (Move p (Pos (chr((ord(col p))+1)) ((row p)+1)) 0) 1
         else []
digur p 2 = if ((row p) < 8) && (((ord(col p))-96) < 8) then [Move p (Pos (chr((ord(col p))+1)) ((row p)+1)) 0, Move p (Pos (chr((ord(col p))+2)) ((row p)+2)) 0]
         else if ((row p) == 8) && (((ord(col p))-96) <= 8) then [Move p (Pos (chr((ord(col p))+1)) ((row p)+1)) 0]
         else if ((row p) <= 8) && (((ord(col p))-96) == 8) then [Move p (Pos (chr((ord(col p))+1)) ((row p)+1)) 0]
         else []
digur p 3 = if ((row p) < 7) && (((ord(col p))-96) < 7) then [Move p (Pos (chr((ord(col p))+1)) ((row p)+1)) 0, Move p (Pos (chr((ord(col p))+2)) ((row p)+2)) 0, Move p (Pos (chr((ord(col p))+3)) ((row p)+3)) 0]
         else if ((row p) == 7) && (((ord(col p))-96) <= 7) then [Move p (Pos (chr((ord(col p))+1)) ((row p)+1)) 0, Move p (Pos (chr((ord(col p))+2)) ((row p)+2)) 0]
         else if ((row p) <= 7) && (((ord(col p))-96) == 7) then [Move p (Pos (chr((ord(col p))+1)) ((row p)+1)) 0, Move p (Pos (chr((ord(col p))+2)) ((row p)+2)) 0]
         else if ((row p) == 8) && (((ord(col p))-96) <= 8) then [Move p (Pos (chr((ord(col p))+1)) ((row p)+1)) 0]
         else if ((row p) <= 8) && (((ord(col p))-96) == 8) then [Move p (Pos (chr((ord(col p))+1)) ((row p)+1)) 0]
         else []
digur p 4 = if ((row p) < 9) && (((ord(col p))-96) < 9) then [Move p (Pos (chr((ord(col p))+1)) ((row p)+1)) 0]
         else []   

digdl :: Pos -> Int -> [Move]
digdl p 1 = if ((row p) > 1) && (((ord(col p))-96) > 1) then [Move p (Pos (chr((ord(col p))-1)) ((row p)-1)) 0] ++ addRotation (Move p (Pos (chr((ord(col p))-1)) ((row p)-1)) 0) 1
         else []
digdl p 2 = if ((row p) > 2) && (((ord(col p))-96) > 2) then [Move p (Pos (chr((ord(col p))-1)) ((row p)-1)) 0, Move p (Pos (chr((ord(col p))-2)) ((row p)-2)) 0]
         else if ((row p) == 2) && (((ord(col p))-96) >= 2) then [Move p (Pos (chr((ord(col p))-1)) ((row p)-1)) 0]
         else if ((row p) >= 2) && (((ord(col p))-96) == 2) then [Move p (Pos (chr((ord(col p))-1)) ((row p)-1)) 0]
         else []
digdl p 3 = if ((row p) > 3) && (((ord(col p))-96) > 3) then [Move p (Pos (chr((ord(col p))-1)) ((row p)-1)) 0, Move p (Pos (chr((ord(col p))-2)) ((row p)-2)) 0, Move p (Pos (chr((ord(col p))-3)) ((row p)-3)) 0]
         else if ((row p) == 3) && (((ord(col p))-96) >= 3) then [Move p (Pos (chr((ord(col p))-1)) ((row p)-1)) 0, Move p (Pos (chr((ord(col p))-2)) ((row p)-2)) 0]
         else if ((row p) >= 3) && (((ord(col p))-96) == 3) then [Move p (Pos (chr((ord(col p))-1)) ((row p)-1)) 0, Move p (Pos (chr((ord(col p))-2)) ((row p)-2)) 0]
         else if ((row p) == 2) && (((ord(col p))-96) >= 2) then [Move p (Pos (chr((ord(col p))-1)) ((row p)-1)) 0]
         else if ((row p) >= 2) && (((ord(col p))-96) == 2) then [Move p (Pos (chr((ord(col p))-1)) ((row p)-1)) 0]
         else []
digdl p 4 = if ((row p) > 1) && (((ord(col p))-96) > 1) then [Move p (Pos (chr((ord(col p))-1)) ((row p)-1)) 0]
         else [] 

digdr :: Pos -> Int -> [Move]
digdr p 1 = if ((row p) > 1) && (((ord(col p))-96) < 9) then [Move p (Pos (chr((ord(col p))+1)) ((row p)-1)) 0] ++ addRotation (Move p (Pos (chr((ord(col p))+1)) ((row p)-1)) 0) 1
         else []
digdr p 2 = if ((row p) > 2) && (((ord(col p))-96) < 8) then [Move p (Pos (chr((ord(col p))+1)) ((row p)-1)) 0, Move p (Pos (chr((ord(col p))+2)) ((row p)-2)) 0]
         else if ((row p) == 2) && (((ord(col p))-96) <= 8) then [Move p (Pos (chr((ord(col p))+1)) ((row p)-1)) 0]
         else if ((row p) >= 2) && (((ord(col p))-96) == 8) then [Move p (Pos (chr((ord(col p))+1)) ((row p)-1)) 0]
         else []
digdr p 3 = if ((row p) > 3) && (((ord(col p))-96) < 7) then [Move p (Pos (chr((ord(col p))+1)) ((row p)-1)) 0, Move p (Pos (chr((ord(col p))+2)) ((row p)-2)) 0, Move p (Pos (chr((ord(col p))+3)) ((row p)-3)) 0]
         else if ((row p) == 3) && (((ord(col p))-96) <= 7) then [Move p (Pos (chr((ord(col p))+1)) ((row p)-1)) 0, Move p (Pos (chr((ord(col p))+2)) ((row p)-2)) 0]
         else if ((row p) >= 3) && (((ord(col p))-96) == 7) then [Move p (Pos (chr((ord(col p))+1)) ((row p)-1)) 0, Move p (Pos (chr((ord(col p))+2)) ((row p)-2)) 0]
         else if ((row p) == 2) && (((ord(col p))-96) <= 8) then [Move p (Pos (chr((ord(col p))+1)) ((row p)-1)) 0]
         else if ((row p) >= 2) && (((ord(col p))-96) == 8) then [Move p (Pos (chr((ord(col p))+1)) ((row p)-1)) 0]
         else []
digdr p 4 = if ((row p) > 1) && (((ord(col p))-96) < 9) then [Move p (Pos (chr((ord(col p))+1)) ((row p)-1)) 0]
         else [] 

up :: Pos -> Int -> [Move]
up p 1 = if ((row p) < 9) then [Move p (Pos (col p) ((row p)+1)) 0] ++ addRotation (Move p (Pos (col p) ((row p)+1)) 0) 1
         else []
up p 2 = if ((row p) < 8) then [Move p (Pos (col p) ((row p)+1)) 0, Move p (Pos (col p) ((row p)+2)) 0]
         else if ((row p) == 8) then [Move p (Pos (col p) ((row p)+1)) 0]
         else []
up p 3 = if ((row p) < 7) then [Move p (Pos (col p) ((row p)+1)) 0, Move p (Pos (col p) ((row p)+2)) 0, Move p (Pos (col p) ((row p)+3)) 0]
         else if ((row p) == 7) then [Move p (Pos (col p) ((row p)+1)) 0, Move p (Pos (col p) ((row p)+2)) 0]
         else if ((row p) == 8) then [Move p (Pos (col p) ((row p)+1)) 0]
         else []
up p 4 = if ((row p) < 9) then [Move p (Pos (col p) ((row p)+1)) 0]
         else []


down :: Pos -> Int -> [Move]
down p 1 = if ((row p) > 1) then [Move p (Pos (col p) ((row p)-1)) 0] ++ addRotation (Move p (Pos (col p) ((row p)-1)) 0) 1
         else []
down p 2 = if ((row p) > 2) then [Move p (Pos (col p) ((row p)-1)) 0, Move p (Pos (col p) ((row p)-2)) 0]
         else if ((row p) == 2) then [Move p (Pos (col p) ((row p)-1)) 0]
         else []
down p 3 = if ((row p) > 3) then [Move p (Pos (col p) ((row p)-1)) 0, Move p (Pos (col p) ((row p)-2)) 0, Move p (Pos (col p) ((row p)-3)) 0]
         else if ((row p) == 3) then [Move p (Pos (col p) ((row p)-1)) 0, Move p (Pos (col p) ((row p)-2)) 0]
         else if ((row p) == 2) then [Move p (Pos (col p) ((row p)-1)) 0]
         else []
down p 4 = if ((row p) > 1) then [Move p (Pos (col p) ((row p)-1)) 0]
         else []

left_me :: Pos -> Int -> [Move]
left_me p 1 = if (((ord(col p))-96) > 1) then [Move p (Pos (chr((ord(col p))-1)) (row p)) 0] ++ addRotation (Move p (Pos (chr((ord(col p))-1)) (row p)) 0) 1
         else []
left_me p 2 = if (((ord(col p))-96) > 2) then [Move p (Pos (chr((ord(col p))-1)) (row p)) 0, Move p (Pos (chr((ord(col p))-2)) (row p)) 0]
         else if (((ord(col p))-96) == 2) then [Move p (Pos (chr((ord(col p))-1)) (row p)) 0]
         else []
left_me p 3 = if (((ord(col p))-96) > 3) then [Move p (Pos (chr((ord(col p))-1)) (row p)) 0, Move p (Pos (chr((ord(col p))-2)) (row p)) 0, Move p (Pos (chr((ord(col p))-3)) (row p)) 0]
         else if (((ord(col p))-96) == 3) then [Move p (Pos (chr((ord(col p))-1)) (row p)) 0, Move p (Pos (chr((ord(col p))-2)) (row p)) 0]
         else if (((ord(col p))-96) == 2) then [Move p (Pos (chr((ord(col p))-1)) (row p)) 0]
         else []
left_me p 4 = if (((ord(col p))-96) > 1) then [Move p (Pos (chr((ord(col p))-1)) (row p)) 0]
              else []
         

right_me :: Pos -> Int -> [Move]
right_me p 1 = if (((ord(col p))-96) < 9) then [Move p (Pos (chr((ord(col p))+1)) (row p)) 0] ++ addRotation (Move p (Pos (chr((ord(col p))+1)) (row p)) 0) 1
         else []
right_me p 2 = if (((ord(col p))-96) < 8) then [Move p (Pos (chr((ord(col p))+1)) (row p)) 0, Move p (Pos (chr((ord(col p))+2)) (row p)) 0]
         else if (((ord(col p))-96) == 8) then [Move p (Pos (chr((ord(col p))+1)) (row p)) 0]
         else []
right_me p 3 = if (((ord(col p))-96) < 7) then [Move p (Pos (chr((ord(col p))+1)) (row p)) 0, Move p (Pos (chr((ord(col p))+2)) (row p)) 0, Move p (Pos (chr((ord(col p))+3)) (row p)) 0]
         else if (((ord(col p))-96) == 7) then [Move p (Pos (chr((ord(col p))+1)) (row p)) 0, Move p (Pos (chr((ord(col p))+2)) (row p)) 0]
         else if (((ord(col p))-96) == 8) then [Move p (Pos (chr((ord(col p))+1)) (row p)) 0]
         else []
right_me p 4 = if (((ord(col p))-96) < 9) then [Move p (Pos (chr((ord(col p))+1)) (row p)) 0]
               else []

addRotation :: Move -> Int -> [Move]
addRotation m c = if c < 8 then [Move (start m) (target m) c] ++ addRotation m (c+1)
                  else []



-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves b p = if gameFinished b then []
                else filter (isValidMove b) (listMovesrec b p 9)

listMovesrec :: Board -> Player -> Int -> [Move]
listMovesrec [] _ _ = []
listMovesrec (b:bs) p r = listMovesRow b p r 0 ++ listMovesrec bs p (r-1)


listMovesRow :: [Cell] -> Player -> Int -> Int -> [Move]
listMovesRow [] _ _ _ = []
listMovesRow (x:xs) p r c = case x of 
                            Empty -> [] ++ listMovesRow xs p r (c+1)
                            Piece p2 _ -> if p == p2 then (possibleMoves (Pos (chr(c + 97)) r) x) ++ (listMovesRow xs p r (c+1))
                                          else []  ++ (listMovesRow xs p r (c+1)) 
module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
import Data.Char



-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Black | White deriving Show
data Cell = Piece Player Int | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2 
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################


validateFEN :: String -> Bool
validateFEN ('/':_) = False
validateFEN x = validateFENrec x 0 0 0 0 

validateFENrec :: String -> Int -> Int -> Int -> Int -> Bool
validateFENrec [] c r s w  = if r == 8 && (c `mod` 8 == 0) && s < 16 && w < 16  then True
                         else False
validateFENrec(',':',':xs) c r s w = validateFENrec xs (c+2) r s w
validateFENrec(',':xs) c r s w = validateFENrec xs (c+1) r s w
validateFENrec('/':xs) c r s w = if (c `mod` 8 == 0) && xs /= [] then validateFENrec xs c (r+1) s w
                             else False
validateFENrec('b':xs) c r s w = if readInt xs 0 > -1 then validateFENrec xs c r (s+1) w
                             else False 
validateFENrec('w':xs) c r s w = if readInt xs 0 > -1 then validateFENrec xs c r s (w+1)
                             else False  
validateFENrec(x:xs) c r s w = if (ord x) <= 57 && (ord x >= 48) then validateFENrec xs c r s w
                               else False

readInt :: String -> Int -> Int
readInt [] c = if c < 256 && c > 0 then c 
                    else -1
readInt(',':_) c = if c < 256 && c > 0 then c 
                    else -1
readInt(x:xs) c = if c == 0 then readInt xs (c + (ord x) - 48) 
                  else if c > 0 && c < 10 then readInt xs (c * 10 + (ord x) -48)
                  else if c > 10 && c < 100 then readInt xs (c * 10 + (ord x) -48) 
                  else -1

-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################


buildBoard :: String -> Board
buildBoard x = buildBoardrec (['/'] ++ x)

buildBoardrec:: String -> Board
buildBoardrec [] = []
buildBoardrec('/':xs) = [evalrow xs False] ++ buildBoardrec xs
buildBoardrec(_:xs) = buildBoardrec xs

evalrow:: String -> Bool -> [Cell]
evalrow [] b = if b == False then [Empty]
               else[]
evalrow(',':',':xs) b = [evalPiece ([','])] ++ [evalPiece ([','])] ++ evalrow xs b
evalrow(',':[]) b = [evalPiece ([','])] ++ evalrow [] b
evalrow(',':'/':_) b = [evalPiece ([','])] ++ evalrow [] b
evalrow('w':xs) _ = [evalPiece (['w'] ++ xs)] ++ evalrow xs True
evalrow('b':xs) _ = [evalPiece (['b'] ++ xs)] ++ evalrow xs True
evalrow('/':_) b = if b == False then [Empty]
                    else[]
evalrow(_:xs) b = evalrow xs b

evalPiece :: String -> Cell
evalPiece ('w':xs) = Piece White (readInt xs 0)
evalPiece ('b':xs) = Piece Black (readInt xs 0)
evalPiece(',':_) = Empty

-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Functional Points        ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################

line :: Pos -> Pos -> [Pos]
line s z = if (ord (col s)) == (ord (col z)) then ver s z 
           else if ((row s) == (row z)) then hor s z
           else dia s z


hor :: Pos -> Pos -> [Pos]
hor s z= if (ord (col s)) > (ord (col z)) then hor_min s z 
      else hor_plus s z 

hor_min :: Pos -> Pos -> [Pos]
hor_min s z = if (ord (col s)) > (ord (col z)) then [s] ++ hor_min (Pos (chr((ord(col s))-1)) (row s)) z
          else [z]


hor_plus :: Pos -> Pos -> [Pos]
hor_plus s z = if (ord (col s)) < (ord (col z)) then [s] ++ hor_plus (Pos (chr((ord(col s))+1)) (row s)) z
          else [z]


ver :: Pos -> Pos -> [Pos]
ver s z = if (row s) > (row z) then ver_min s z 
          else ver_plus s z 

ver_min :: Pos -> Pos -> [Pos]
ver_min s z = if (row s) > (row z) then [s] ++ ver_min (Pos (col s) ((row s)-1)) z
              else [z]

          
ver_plus :: Pos -> Pos -> [Pos]
ver_plus s z = if (row s) < (row z) then [s] ++ ver_plus (Pos (col s) ((row s)+1)) z
              else [z]


dia :: Pos -> Pos -> [Pos]
dia s z = if (row s) > (row z) && (ord (col s)) > (ord (col z)) then dia_down_right s z 
          else if (row s) < (row z) && (ord (col s)) > (ord (col z)) then dia_up_right s z 
          else if (row s) > (row z) && (ord (col s)) < (ord (col z)) then dia_down_left s z
          else dia_up_left s z

dia_down_right :: Pos -> Pos -> [Pos]
dia_down_right s z = if (row s) > (row z) && (ord (col s)) > (ord (col z)) then [s] ++ dia_down_right (Pos (chr((ord(col s))-1)) ((row s)-1)) z
                     else [z]
                  
dia_up_right :: Pos -> Pos -> [Pos]
dia_up_right s z = if ((row s) < (row z)) && ((ord (col s)) > (ord (col z))) then [s] ++ dia_up_right (Pos (chr((ord(col s))-1)) ((row s)+1)) z
                   else [z] 

dia_down_left :: Pos -> Pos -> [Pos]
dia_down_left s z = if ((row s) > (row z)) && ((ord (col s)) < (ord (col z))) then [s] ++ dia_down_left (Pos (chr((ord(col s))+1)) ((row s)-1)) z
                    else [z]


dia_up_left :: Pos -> Pos -> [Pos]
dia_up_left s z = if ((row s) < (row z)) && ((ord (col s)) < (ord (col z))) then [s] ++ dia_up_left (Pos (chr((ord(col s))+1)) ((row s)+1)) z
                  else [z]
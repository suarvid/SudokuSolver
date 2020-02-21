module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe

------------------------------------------------------------------------------
-- | Miscallenous stuff used for developement | --

allFilledSudoku :: Sudoku
allFilledSudoku = Sudoku (replicate 9 $ replicate 9 (Just 1))
c :: Row
c = [ Just 1 ,Just 2 ,Just 3 ,Nothing,Just 5,Just 6,Just 7,Just 4,Just 8]
k :: Row
k= [Just 3,Just 6,Nothing,Nothing,Just 7,Just 1,Just 2,Nothing,Nothing]



sudMat :: [Row]
sudMat = [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
         , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
         , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
         , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
         , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
         , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
         , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
         , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
         , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
                                                ] 

                                              where j = Just
                                                    n = Nothing

------------------------------------------------------------------------------
-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells
--data Maybe a = Nothing | Just a  

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms
-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 $ replicate 9 Nothing)

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rs) =
 goodLength rs && all goodLength rs && all goodCell (concat rs)
 where
   goodLength :: [a] -> Bool
   goodLength xs = length xs == 9
   goodCell :: Cell -> Bool
   goodCell (Just d) = d > 0 && d < 10
   goodCell Nothing  = True

-- All values allowed in a Sudoku. There's one for only numerical values and one which
-- contains both the numerical values and 'Nothing' .

allowedNumValues :: [Cell]
allowedNumValues = [Just j | j <- [1..9]]
allowedValues :: [Cell]
allowedValues = allowedNumValues ++ [Nothing]

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku xs) = all isJust(concat xs)

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku xs) = putStrLn (convertSudokuToString (Sudoku xs))
-- Converts each row into a string and adds a newline character between rows.

convertSudokuToString :: Sudoku -> String
convertSudokuToString (Sudoku [])     = []
convertSudokuToString (Sudoku xs) = unlines $ map convertRowToString xs

--Maps readCell to convert all cells in a row to an array of characters.
convertRowToString :: Row -> String
convertRowToString  =  map readCell

readCell:: Cell -> Char
readCell c         | isNothing c =  '.'
                   | otherwise = intToDigit $ fromJust c




-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku [] = error" Not a valid filepath."
readSudoku fp = do 
                 text <- lines <$> readFile fp
                 let sud = Sudoku (stringsToRows text)
                 if isSudoku sud then 
                  return sud
                  else error"Given file is not a sudoku."
                
-- Converts a list of strings to a list of rows.
stringsToRows :: [String] -> [Row]
stringsToRows []  = []
stringsToRows xs = map stringToRow xs

-- Converts a string to a row.
stringToRow :: String -> Row
stringToRow []         = []
stringToRow (x:xs)         | x == '.' = Nothing : stringToRow xs
                           | isDigit x && n <= 9 && n > 0  = Just n : stringToRow xs
                           | otherwise  = error "Incorrect format for a Sudoku."
                             where n =  digitToInt x

                             
------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [(2,rJust),(5,rNothing)] 

rJust :: Gen Cell
rJust = elements [Just j | j<- [1..9]]

rNothing :: Gen Cell
rNothing = return Nothing


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = Sudoku <$> rRows

--Generates a vector with length 9 where each index contains a vector of length 9 of cells.
rRows :: Gen [Row]
rRows = vectorOf 9 $ vectorOf 9 cell

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell
--also Row and [[Maybe Int]]


-- * D1
-- Filters the given block for items which of satisfy the conditions of isJust (those who are not equals to Nothing)
-- and makes sure the length of the block before and after is the same using the nub function. (Removing duplicates and 
--checks length before and after.)
isOkayBlock :: Block -> Bool
isOkayBlock xs = length (nub b) == length b
            where b = filter isJust xs

-- The ordinary matrix in a sudoku is already a list of blocks containing all rows. To get all columns this method transposes the matrix.
--To get the 'chunks' of 3x3 matrices another function is used.
blocks :: Sudoku -> [Block]
blocks (Sudoku xs ) =  xs ++ transpose xs ++  b
                  where b = toChunks xs 

-- This function splits the list of blocks into three chunks which then are made into blocks and added together and thereafter
-- returned as a list of blocks.
toChunks :: [Block]-> [Block]
toChunks [] = []
toChunks xs = createBlocksFromChunk a ++ createBlocksFromChunk b ++ createBlocksFromChunk c  
                         where [a,b,c] = chunksOf 3 xs    
                               --This is a local function which is used as an auxiliary function by toChunks.
                               createBlocksFromChunk xs =   createBlocks (xs !! 0) (xs!! 1) (xs!! 2)


--This function recursively takes and drops 3 units from 3 blocks and adds them together into 
-- a list of blocks. It is done in a way that takes the three first items from each list and adds
--them together into a 3x3 block used in Sudoku. Thereafter it drops 3 items and does the same thing again until
-- all blocks are empty.
createBlocks:: Block->Block->Block->[Block]
createBlocks [] [] []             = []
createBlocks xs ys zs = (x ++ y ++ z) : createBlocks x' y' z'
               where x = take 3 xs
                     y = take 3 ys
                     z = take 3 zs
                     x'= drop 3 xs
                     y'= drop 3 ys
                     z'= drop 3 zs
--Checks that the total length of the list of blocks is 27, and that all blocks have a length of 9. 
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud = length b == 27   && all( ==9) (map length b) 
                       where b = blocks sud

-- * D3

isOkay :: Sudoku -> Bool
isOkay sud  = all isOkayBlock $ blocks sud


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1
-- Should return a list of the positions in the sudoku that are still blank
-- Returns a list of all blank positions in a given sudoku
blanks :: Sudoku -> [Pos]
blanks (Sudoku [])  = []
blanks (Sudoku (r:rows))  | Nothing  `elem` r = findNothing r col ++ blanks (Sudoku rows)
                          | otherwise        = blanks (Sudoku rows)
                             where col = 9 - length (r:rows)

-- Returns a list of all blank positions in a given Row
findNothing :: Row -> Int -> [Pos]
findNothing [] _        = []
findNothing (r:rs) c    | isNothing r = (c,index) : findNothing rs c
                        | otherwise = findNothing rs c
                          where index = 9 - length (r:rs)

-- Pos is (row, col)
-- Verifies that all positions given by the function blanks actually are blank in the sudoku
prop_blanks_allBlanks :: Sudoku -> Bool
prop_blanks_allBlanks sud = prop_blanks_allBlanksAux sud (blanks sud)

-- Recursively checks that every position in the list of positions is Nothing in the given sudoku
prop_blanks_allBlanksAux :: Sudoku -> [Pos] -> Bool
prop_blanks_allBlanksAux _ [] = True
prop_blanks_allBlanksAux (Sudoku xs) (b:bs) = isNothing (xs !! row !! col) && prop_blanks_allBlanksAux (Sudoku xs) bs
  where (row,col) = b 

-- * E2
-- Update the given list with the new value at the given index
(!!=) :: [a] -> (Int,a) -> [a]
[]      !!= _      = []
(x:xs) !!= (i,y)   | i==0      = y:xs
                   | otherwise = x: (xs !!= (i-1,y)) 
                   
-- Stuff that can go wrong: Wrong length of the list, Wrong element inserted, element inserted at wrong position etc
prop_bangBangEquals_correct :: [Integer] -> (Int,Integer) -> Bool
prop_bangBangEquals_correct arr (i,y)  = (null arr || arr' !! i' == y) && arr == ((arr !!= (i',y)) !!= (i',x)) && length arr == length (arr !!= (i',y))
                                           where x = arr !! i'
                                                 i' = i `mod` length arr
                                                 arr' = arr !!= (i',y)


-- * E3
-- Given a Sudoku, a position, and a new Cell value, updates the given position with the new Value
update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku xs) (row,col)  c  = Sudoku( ys ++ [swapRow !!= (col,c)] ++ tailRows)
                            where (ys,zs) = splitAt row xs
                                  swapRow     = head zs
                                  tailRows    = drop 1 zs

-- Checks that the position that is updated actually has the new value
prop_update_updated :: Sudoku -> (Int,Int) -> Cell -> Bool
prop_update_updated (Sudoku xs) (row,col) newCell = newCell == newSudMatrix !! row' !! col'
  where 
    row' = abs (row `mod` 9)
    col' = abs (col `mod` 9)
    (Sudoku newSudMatrix) = update (Sudoku xs) (row',col') newCell
------------------------------------------------------------------------------

-- * F1
-- Returns a solved (completely filled) sudoku if there exists such a solution, otherwise returns Nothing
solve :: Sudoku -> Maybe Sudoku
solve sud | null solutions = Nothing -- in case there are valid sudokus without solutions
          | isSudoku sud = Just (head solutions)
          | otherwise = Nothing
  where solutions = solve' [sud] (blanks sud)
          

-- Should return a list of all possible solutions
solve' :: [Sudoku]-> [Pos] -> [Sudoku]
solve' [] _       = []
solve' sud  []    = sud
solve' suds (b:bs) = solve' sudList bs 
  where sudList = filter isOkay (getAllUpdated suds b)
    
  
-- returns a list of all variations of the given sudoku where the given position has been updated with values 1-9
-- calls updatedRows to generate the possible variations of the current row to be updated
updatedSudokus :: Sudoku -> Pos -> [Sudoku]
updatedSudokus (Sudoku xs) pos = map
  (Sudoku . (\ x -> take row xs ++ [x] ++ take (9 - row) (drop (row + 1) xs))) (updatedRows (Sudoku xs) pos)
  where 
    (row,col) = pos

-- produces a list of rows where the given position has been updated, all possible variations for values 1-9
updatedRows :: Sudoku -> Pos -> [Row]
updatedRows (Sudoku xs) (row,col) = map (\x -> (xs !! row) !!= (col,x)) allowedNumValues

-- Uses updatedSudoku to produce a list of all possible variations of a list of given sudokus, produces non-valid sudokus as well, filtering happens in solve'
getAllUpdated :: [Sudoku] -> Pos -> [Sudoku]
getAllUpdated []   _   = []
getAllUpdated (s:ss) p = updatedSudokus s p ++ getAllUpdated ss p 


-- * F2
-- Reads a sudoku from a given filepath, tries to solve it and prints the solution or an error message stating that there are no solutions to the given sudoku
readAndSolve :: FilePath -> IO ()
readAndSolve fp = 
  do 
    sud <- readSudoku fp
    let solved = solve sud
    maybe (print "No solutions.") printSudoku solved

-- * F3
-- Checks if a given solution is a solution to the given unsolved sudoku
-- uses solve', but not solve B)
--isSolutionOf :: Sudoku -> Sudoku -> Bool
--isSolutionOf solution orgSud = solution `elem` solve' [orgSud] (blanks orgSud) 

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol sud | isOkay sol  && isFilled sol = sud == updatePositions sol pos Nothing
                     | otherwise = False
                     where pos = blanks sud

updatePositions  :: Sudoku-> [Pos] -> Cell-> Sudoku
updatePositions sud (p:ps) val = updatePositions (update sud p val) ps val
updatePositions sud []  _      = sud

-- Checks that all solutions produced by solve' actually are solutions to the sudoku used to produce the list of solutions
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = collect (isOkay sud) $ conjoin (map (\x -> x `elem` solutions ==> x `isSolutionOf` sud) solutions)
    where solutions = solve' [sud] (blanks sud)


fewerChecks prop =
  quickCheckWith stdArgs{maxSuccess=30 } prop
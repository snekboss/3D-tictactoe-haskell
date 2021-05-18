import Data.List
import Data.Function
import Data.Maybe
import Text.Read

emptyChar = '.'
playerChar = 'X'
computerChar = 'O'

humanPlayer = 1
computerPlayer = 2



-- ============================== Board to String related stuff ==============================
--Desc: This is a helper function, meant to be called by toStrOneRow.
--Arg1: A row string.
--Ret: An output string.
toStrOneRow_helper :: String -> String
toStrOneRow_helper [c] = [' ', c]
toStrOneRow_helper (c : remStr) = [' ', c] ++ (toStrOneRow_helper remStr)


--Desc: Takes a row like "-XO" and prints it like "- X O"
--      Uses toStrOneRow_helper function to achieve this.
--Arg1: A row like "-XO".
--Ret: An output string like "- X O ".
toStrOneRow :: String -> String
toStrOneRow (c : remStr) = c : toStrOneRow_helper remStr


--Arg1: One list of strings.
--Ret: String output
toStrOneList :: [String] -> String
toStrOneList [s] = toStrOneRow s
toStrOneList (s : remList) = (toStrOneRow s) ++ "   " ++ toStrOneList remList


--Arg1: A game board.
--Ret: Formatted output string of the board.
toStrBoard :: [[String]] -> String
toStrBoard [] = ""
toStrBoard (list : remListOfListOfStrings) = toStrOneList list ++ "\n" ++ toStrBoard remListOfListOfStrings


--Shows an up-down (default) board in REPL as a back-forward board.
showBoard board = do
    putStrLn (toStrBoard (getBackForwardBoards board))



-- ============================== Board related functions (hopefully) ==============================
--Arg1: Dimension N of the NxNxN tic-tac-toe board.
--Ret: An empty board. For example, dims=3, then [["---", "---", "---"], ["---", "---", "---"], ["---", "---", "---"]].
initBoard :: Int -> [[String]]
initBoard dims = replicate dims (replicate dims (replicate dims (emptyChar)))


--Arg1: The size of the board
--Arg2: A tictactoe move triplet (col, row, face), where each coordinate is in the inclusive range [1, Arg2].
--Ret: True if the move triplet is out of bounds.
isOutOfBounds :: Int -> (Int, Int, Int) -> Bool
isOutOfBounds size (col, row, face) = col < 1 || row < 1 || face < 1 || col > size || row > size || face > size


--Desc: Apply a function to the element at the given index (very cool).
--Arg1: Index to which the function is to be applied.
--Arg2: The function.
--Arg3: The input list.
--Ret: Same list, but with the function applied at the i'th element.
map_at :: Int -> (a -> a) -> [a] -> [a]
map_at i f list = let (prevElems, target : remElems) = splitAt i list
                  in prevElems ++ (f target : remElems)


--Desc: Update element (col, row, face) of a three-dimensional array.
--Arg1: (col, row, face) triplet whose value needs to be changed.
--Arg2: The new value.
--Arg3: Input 3D grid
--Ret: New grid, where the value at (col, row, face) is set to Arg2.
update3 :: (Int, Int, Int) -> a -> [[[a]]] -> [[[a]]]
update3 (col, row, face) val grid = map_at col (map_at row (map_at face (const val))) grid


--Arg1: A tictactoe triplet (col, row, face), where each coordinate is in the inclusive range [1, (length Arg2)].
--Arg2: A character value: '-' or 'X' or 'O'.
--Arg3: A 3D tictactoe board.
--Ret: The new tictactoe board, where the board at the triplet's coordinates now contains Arg2's value.
--NOTES: The Arg1 triplet is assumed to be inside the boundaries of the board.
setCell :: (Int, Int, Int) -> Char -> [[String]] -> [[String]]
setCell (col, row, face) = update3 ((face - 1), (row - 1), (col - 1))


--Arg1: A tictactoe triplet (col, row, face), where each coordinate is in the inclusive range [1, (length Arg2)].
--Arg2: A 3D tictactoe board.
--Ret: A char equal to '.' or 'X' or 'O'.
--NOTES: The Arg1 triplet is assumed to be inside the boundaries of the board.
getCell :: (Int, Int, Int) -> [[String]] -> Char
getCell (col, row, face) board = board !! (face - 1) !! (row - 1) !! (col - 1)


--Arg1: A tictactoe move triplet (col, row, face), where each coordinate is in the inclusive range [1, (length Arg2)].
--Arg2: A 3D tictactoe board.
--Ret: True if the move is available on the board.
--NOTES: The Arg1 triplet is assumed to be inside the boundaries of the board.
isEmpty :: (Int, Int, Int) -> [[String]] -> Bool
isEmpty triplet board = (getCell triplet board) == (emptyChar)




-- ============================== Extracting 2D boards out of the 3D board ==============================
-- ******************** Common Helper Functions ********************
--Arg1: The 3D board.
--Ret: The same 3D board, however each 2D board is transposed.
getEachTransposed :: [[String]] -> [[String]]
getEachTransposed [] = []
getEachTransposed (list : remLists) = transpose list : getEachTransposed remLists




-- ******************** The functions that get me certain types of 2D boards out of *the* 3D Board. ********************
--Arg1: The default 3D board of size NxNxN (same thing as getUpDownBoards).
--Ret: A list of N 2D-boards (up-down), all of size NxN.
getUpDownBoards :: [[String]] -> [[String]]
getUpDownBoards board3D = board3D


--Arg1: The default 3D board of size NxNxN (same thing as getUpDownBoards).
--Ret: A list of N 2D-boards (back-forward), all of size NxN.
getBackForwardBoards :: [[String]] -> [[String]]
getBackForwardBoards board3D = transpose board3D


--Arg1: The default 3D board of size NxNxN (same thing as getUpDownBoards).
--Ret: A list of N 2D-boards (left-right), all of size NxN.
getLeftRightBoards :: [[String]] -> [[String]]
getLeftRightBoards board3D = transpose (getEachTransposed board3D)


--TODO: Implement the function that gets the 3D diagonals.
{-
Using the alphabet board as an example: 
    - an# (from left-top-back to right-bottom-forward)
    - sni (from left-bottom-back to right-top-forward)
    - gnu (from left-top-forward to right-bottom-back)
    - ins (from right-top-forward to left-bottom-back)
-}
get3DDiagonals :: [[String]] -> [[String]]
get3DDiagonals board3D = []




-- ============================== Game loop, etc. ==============================

--Asks the size of the board.
--Returns an IO Int. Use the "<-" operator when working with this function.
askBoardSize = do
        putStr "Enter board size: "
        input <- getLine
        let dims = readMaybe input :: Maybe Int
         in
            if dims == Nothing then do
                putStrLn "Invalid input."
                askBoardSize
            else if dims < (Just 3) then do
                putStrLn "The size cannot be less than 3."
                askBoardSize
            else do
                return (fromJust dims)


--Asks the starting player (human or computer).
--Returns an IO Int. Use the "<-" operator when working with this function.
askStarterPlayer = do
        putStrLn "Would you like to start first? (y/n)"
        answer <- getLine
        if answer == "y" || answer == "Y" then do
            return humanPlayer
        else if answer == "n" || answer == "N" then do
            return computerPlayer
        else do
            putStrLn "Please enter 'y' or 'n'."
            askStarterPlayer


--Desc: This is how you start the game.
run = do
    size <- askBoardSize
    starter <- askStarterPlayer
    gameStart starter (initBoard size)


--Desc: Do game initialization stuff here.
gameStart player board = do
        gameLoop player board


--Asks the move triplet (face row col) of the player.
--Returns (col, row, face). Yes, it's reversed. Because otherwise, the rest of the code doesn't work.
getPlayerMove board = do
        putStr "Your move (face row col)?: "
        input <- getLine
        let maybeNums = map readMaybe (words input) :: [Maybe Int]
            in
                if length maybeNums /= 3 || elem Nothing maybeNums then do
                    putStrLn "Please enter 3 numbers."
                    getPlayerMove board
                else do
                    let [face, row, col] = maybeNums
                        triplet = (fromJust col, fromJust row, fromJust face)
                        boardSize = length board
                        in
                            if isOutOfBounds boardSize triplet then do
                                putStrLn ""
                                putStrLn "Your move is out of bounds."
                                putStrLn ("Please fit your coordinates into the inclusive [1," ++ (show boardSize) ++ "] range.")
                                putStrLn ""
                                getPlayerMove board
                            else if (isEmpty triplet board) == False then do
                                putStrLn ""
                                putStrLn "You cannot make your move there (cell is not empty)."
                                putStrLn ""
                                getPlayerMove board
                            else do
                                return triplet


--TODO: Implement computer AI.
getComputerMove board = do
    --TODO: Must treat the triplets as (col, row, face).
    --It's the same as (x,y,z), but I should think of them that way.
    putStrLn "TODO: Computer moves at (3,2,1) (face row col)."
    return (1,2,3) -- (col, row, face)


--TODO: Check the winning conditions, etc.
{-

backForward list has to be transposed.

-}

--Arg1: A single 2D tictactoe board.
--Ret: The two diagonals of the 2D board.
getDiagonals :: [String] -> [String]
getDiagonals board2D = let size = length board2D
                           diagonal1 = zipWith (!!) board2D [0..(size - 1)]
                           diagonal2 = zipWith (!!) (reverse (transpose board2D)) [0..(size - 1)]
                       in [diagonal1, diagonal2]


--Arg1: A single 2D tictactoe board.
--Ret: Returns (playerChar) or (computerChar) if one of them won; otherwise returns (emptyChar).
--WARNING: Only checks horizontally and the 2 diagonals. No vertical checks.
is2Dtictactoe :: Char -> [String] -> Char
is2Dtictactoe c board2D = let boardSize = length board2D
                              winRow = replicate boardSize c
                              horizontalWin = or (map (==winRow) board2D)
                              diagonalWin = or (map (==winRow) (getDiagonals board2D))
                          in
                              if horizontalWin || diagonalWin then c else (emptyChar)

--Arg1: A list of 2D tictactoe boards.
--Ret: True if player has a full row in at least one of the boards.
is2Dtictactoe_forPlayer :: [[String]] -> Bool
is2Dtictactoe_forPlayer boards2D = elem (playerChar) (map (is2Dtictactoe (playerChar)) boards2D)

--Arg1: A list of 2D tictactoe boards.
--Ret: True if computer has a full row in at least one of the boards.
is2Dtictactoe_forComputer :: [[String]] -> Bool
is2Dtictactoe_forComputer boards2D = elem (computerChar) (map (is2Dtictactoe (computerChar)) boards2D)


--Arg1: The 3D board.
{-Ret:
gameInProgress = 0,
player1Wins = 1,
player2Wins = 2,
draw = 3.
-}
getOutcome :: [[String]] -> Int
getOutcome board = let ud = getUpDownBoards board
                       lf = getLeftRightBoards board
                       bfTranspose = transpose (getBackForwardBoards board) --transpose bf, because we want vertical checks on that.
                       allBoards2D = [ud, lf, bfTranspose]
                       playerWins = or (map is2Dtictactoe_forPlayer allBoards2D)
                       computerWins = or (map is2Dtictactoe_forComputer allBoards2D)
                       concatted = concat (concat board)
                       thereAreRemainingEmptyCells = elem (emptyChar) concatted
                       --TODO: Check the 4 3D diagonals.
                    in
                        if playerWins then 1
                        else if computerWins then 2
                        else if thereAreRemainingEmptyCells then 0 -- gameInProgress
                        else 3 --draw


--Announces the winner (player if outcome == 1; computer if outcome == 2; draw if outcome == 3).
--WARNING: Do not call this function if outcome == 0 (game in progress).
--Returns an IO action which does things (probably).
announceWinner outcome =
    if outcome == 1 then do
        putStrLn "You win!"
    else if outcome == 2 then do
        putStrLn "You lose."
    else do {-outcome == 3-}
        putStrLn "It's a draw!"


--Desc: Game loop.
--Arg1: 1 for player1 (human); 2 for player2 (computer).
--Arg2: The current state of the game board.
--Ret: Some monadic IO magic.
gameLoop player board = do
         let outcome = getOutcome board
             in
                 if outcome == 0 then do
                     showBoard board
                     if player == (humanPlayer) then do
                         playerMove <- getPlayerMove board
                         let newBoard = setCell playerMove playerChar board
                             in gameLoop (3 - player) newBoard
                     else do
                         computerMove <- getComputerMove board
                         let newBoard = setCell computerMove computerChar board
                             in gameLoop (3 - player) newBoard
                 else do
                     showBoard board
                     announceWinner outcome





-- ================================================== TESTS ==================================================
debug_getAlphabetBoard = [["abc", "def", "ghi"], ["jkl", "mno", "pqr"], ["stu", "vwx", "yz#"]]

debug_getAlphabetBoard_LeftRight = [["adg", "jmp", "svy"], ["beh", "knq", "twz"], ["cfi", "lor", "ux#"]]
debug_getAlphabetBoard_BackForward = [["abc", "jkl", "stu"], ["def", "mno", "vwx"], ["ghi", "pqr", "yz#"]]


unitTest_LeftRight = let defaultBoard = debug_getAlphabetBoard
                         leftRight2DBoards = getLeftRightBoards defaultBoard
                         in leftRight2DBoards == debug_getAlphabetBoard_LeftRight


unitTest_BackForward = let defaultBoard = debug_getAlphabetBoard
                           backForward2DBoards = getBackForwardBoards defaultBoard
                           in backForward2DBoards == debug_getAlphabetBoard_BackForward
                           

debug_showBoard board = do
        putStrLn ""
        putStr (show board)
        putStr "\n\n"
        putStr (toStrBoard board)
        
debug_showBoardTranspose board = do
        putStrLn ""
        putStr (show (transpose board))
        putStr "\n\n"
        putStr (toStrBoard (transpose board))

-- some 2D boards
debug_2db1 = ["...", "...", "..."]
debug_2db2 = ["XXX", "..O", "O.."]
debug_2db3 = ["X..", ".XO", "O.X"]
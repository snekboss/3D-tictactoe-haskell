import Data.List

{-

[
    [
    "---",
    "---",
    "---"
    ],
    
    [
    "---",
    "---",
    "---"
    ],
    
    [
    "---",
    "---",
    "---"
    ],
]

-}


emptyChar = '.'
playerChar = 'X'
computerChar = 'O'



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




-- ============================== Board related functions (hopefully) ==============================
--Arg1: Dimension N of the NxNxN tic-tac-toe board.
--Ret: An empty board. For example, dims=3, then [["---", "---", "---"], ["---", "---", "---"], ["---", "---", "---"]].
initBoard :: Int -> [[String]]
initBoard dims = replicate dims (replicate dims (replicate dims (emptyChar)))


--Arg1: The size of the board
--Arg2: A tictactoe move triplet (x,y,z), where each coordinate is in the inclusive range [1, Arg2].
--Ret: True if the move triplet is out of bounds.
isOutOfBounds :: Int -> (Int, Int, Int) -> Bool
isOutOfBounds size (x, y, z) = x < 1 || y < 1 || z < 1 || x > size || y > size || z > size


--TODO: Yes, I need to implement this function...
--Arg1: A tictactoe triplet (x,y,z), where each coordinate is in the inclusive range [1, (length Arg2)].
--Arg2: A character value: '-' or 'X' or 'O'.
--Arg3: A 3D tictactoe board.
--Ret: The new tictactoe board, where the board at the triplet's coordinates now contains Arg2's value.
--NOTES: The Arg1 triplet is assumed to be inside the boundaries of the board.
setCell :: (Int, Int, Int) -> Char -> [[String]] -> [[String]]
setCell (x, y, z) moveChar (face : remFaces) = [] -- TODO: My brain is fried...


--Arg1: A tictactoe triplet (x,y,z), where each coordinate is in the inclusive range [1, (length Arg2)].
--Arg2: A 3D tictactoe board.
--Ret: A char equal to '.' or 'X' or 'O'.
--NOTES: The Arg1 triplet is assumed to be inside the boundaries of the board.
getCell :: (Int, Int, Int) -> [[String]] -> Char
getCell (x, y, z) board = (( board !! (z - 1)) !! (y - 1)) !! (x - 1)


--Arg1: A tictactoe move triplet (x,y,z), where each coordinate is in the inclusive range [1, (length Arg2)].
--Arg2: A 3D tictactoe board.
--Ret: True if the move is available on the board.
--NOTES: The Arg1 triplet is assumed to be inside the boundaries of the board.
isEmpty :: (Int, Int, Int) -> [[String]] -> Bool
isEmpty triplet board = (getCell triplet board) == (emptyChar)



-- ============================== Game loop, etc. ==============================
--Desc: This is how you start the game.
run = do
        putStr "Enter board size: "
        dims <- readLn :: IO Int -- TODO: Exception if bad input. Yay or nay?
        if dims < 3 then do
            putStrLn "The size cannot be less than 3."
            return ()
        else do
            gameStart (initBoard dims)


--Desc: The start of the game. Do your initialization stuff here.
gameStart board = do
        gameLoop board


--Desc: Game loop. Godspeed.
--Arg1: 1 for player1; 2 for player2.
--Arg2: The current state of the game board.
--Ret: Some monadic IO magic.
gameLoop board = do
         putStr (toStrBoard board)
         putStr "Your move (x y z)?: "
         (x, y, z) <- readLn :: IO (Int, Int, Int) -- TODO: I must type (x, y, z). Also, exception if bad input. What do?
         if isOutOfBounds (length board) (x, y, z) then do
             putStrLn ""
             putStrLn "Your move is out of bounds."
             putStrLn ("Please fit your coordinates into the inclusive [1," ++ (show (length board)) ++ "] range.")
             putStrLn ""
             gameLoop board
         else if (isEmpty (x, y, z) board) == False then do
             putStrLn ""
             putStrLn "You cannot make your move there."
             putStrLn ""
             gameLoop board
         else do
             {-
             -- TODO: Write the main game loop plz. Win conditions, minimax, etc.
             let boardPlayerMoved = setCell (x, y, z) (playerChar) board
                 
                 boardComputerMoved = 
             -}
             putStrLn "TODO: Implement the main game loop plz."
             gameLoop board
         
         
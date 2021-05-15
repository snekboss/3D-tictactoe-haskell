import Data.List
import Data.Maybe
import Text.Read

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


--Desc: Apply a function to the element at the given index (very cool).
--Arg1: Index to which the function is to be applied.
--Arg2: The function.
--Arg3: The input list.
--Ret: Same list, but with the function applied at the i'th element.
map_at :: Int -> (a -> a) -> [a] -> [a]
map_at i f list = let (prevElems, target : remElems) = splitAt i list
                  in prevElems ++ (f target : remElems)


--Desc: Update element (x, y, z) of a three-dimensional array.
--Arg1: (x,y,z) triplet whose value needs to be changed.
--Arg2: The new value.
--Arg3: Input 3D grid
--Ret: New grid, where the value at (x,y,z) is set to Arg2.
update3 :: (Int, Int, Int) -> a -> [[[a]]] -> [[[a]]]
update3 (x, y, z) val grid = map_at x (map_at y (map_at z (const val))) grid


--TODO: Might not working as intended (though, it COULD be working as intended...)
--      What this function does VS the image of a 3D grid in my head are two different things.
--      Must decide which lists represent which parts of the board, etc.
--      Use `debug_show3Dgrid` to get an idea.
--Arg1: A tictactoe triplet (x,y,z), where each coordinate is in the inclusive range [1, (length Arg2)].
--Arg2: A character value: '-' or 'X' or 'O'.
--Arg3: A 3D tictactoe board.
--Ret: The new tictactoe board, where the board at the triplet's coordinates now contains Arg2's value.
--NOTES: The Arg1 triplet is assumed to be inside the boundaries of the board.
setCell :: (Int, Int, Int) -> Char -> [[String]] -> [[String]]
setCell (x, y, z) = update3 ((z - 1), (y - 1), (x - 1))


--TODO: Might not working as intended (though, it COULD be working as intended...)
--      What this function does VS the image of a 3D grid in my head are two different things.
--      Must decide which lists represent which parts of the board, etc.
--      Use `debug_show3Dgrid` to get an idea.
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


--Asks the move triplet (x,y,z) of the player.
--Returns an IO (Int, Int, Int) (probably). Use the "<-" operator when working with this function.
getPlayerMove board = do
        putStr "Your move (x y z)?: "
        input <- getLine
        let maybeNums = map readMaybe (words input) :: [Maybe Int]
            in
                if length maybeNums /= 3 || elem Nothing maybeNums then do
                    putStrLn "Please enter 3 numbers."
                    getPlayerMove board
                else do
                    let [x, y, z] = maybeNums
                        triplet = (fromJust x, fromJust y, fromJust z)
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
    putStrLn "TODO: Computer moves at (1,2,3)."
    return (1,2,3)


--TODO: Check the winning conditions, etc.
getOutcome board = 0 



--Announces the winner (player if outcome == 1; computer if outcome == 2).
--Returns an IO action which does things (probably).
announceWinner outcome =
    if outcome == 1 then do
        putStrLn "You win!"
    else do
        putStrLn "You lose."


--Desc: Game loop.
--Arg1: 1 for player1 (human); 2 for player2 (computer).
--Arg2: The current state of the game board.
--Ret: Some monadic IO magic.
gameLoop player board = do
         let outcome = getOutcome board
             in
                 if outcome == 0 then do
                     putStr (toStrBoard board)
                     if player == (humanPlayer) then do
                         playerMove <- getPlayerMove board
                         let newBoard = setCell playerMove playerChar board
                             in gameLoop (3 - player) newBoard
                     else do
                         computerMove <- getComputerMove board
                         let newBoard = setCell computerMove computerChar board
                             in gameLoop (3 - player) newBoard
                 else do
                     announceWinner outcome


debug_show3Dgrid = do
        putStrLn ""
        putStr (show [["abc", "def", "ghi"], ["jkl", "mno", "pqr"], ["stu", "vwx", "yz#"]])
        putStr "\n\n"
        putStr (toStrBoard [["abc", "def", "ghi"], ["jkl", "mno", "pqr"], ["stu", "vwx", "yz#"]])
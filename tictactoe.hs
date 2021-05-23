import Data.List
import Data.Function
import Data.Maybe
import Text.Read

emptyChar = '.'
player1Char = 'X'
player2Char = 'O'

player1 = 1
player2 = 2



-- ============================== Board to String related stuff ==============================
--Intersperses a string with ' '.
--Arg1: One row, like ".XO".
--Ret: An output string, like ". X O".
toStrRow :: String -> String
toStrRow = intersperse ' '


--Arg1: One 2D board.
--Ret: String output of the 2D board.
toStrBoard2D :: [String] -> String
toStrBoard2D board2D = intercalate "   " (map toStrRow board2D)


--Arg1: 3D board.
--Ret: Formatted output string of the board.
toStrBoard :: [[String]] -> String
toStrBoard board3D = unlines (map toStrBoard2D board3D)


--Shows an up-down (default) board, but transposes it first, because it looks right...
--When printed like this, we can easily choose a move based on (face, row, col) in that order.
--What we see matches with what we get. Or something... Anyway.
showBoard :: [[String]] -> IO ()
showBoard board = do
    putStrLn (toStrBoard (transpose board))



-- ============================== Board related functions (hopefully) ==============================
--Arg1: Dimension N of the NxNxN tic-tac-toe board.
--Ret: An empty board. For example, dims=3, then [["---", "---", "---"], ["---", "---", "---"], ["---", "---", "---"]].
initBoard :: Int -> [[String]]
initBoard dims = replicate dims (replicate dims (replicate dims emptyChar))


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
map_at i f list =
    let (prevElems, target : remElems) = splitAt i list
    in prevElems ++ (f target : remElems)


--Desc: Update element (col, row, face) of a three-dimensional array.
--Arg1: (col, row, face) triplet whose value needs to be changed.
--Arg2: The new value.
--Arg3: Input 3D grid
--Ret: New grid, where the value at (col, row, face) is set to Arg2.
update3 :: (Int, Int, Int) -> a -> [[[a]]] -> [[[a]]]
update3 (col, row, face) val grid = map_at col (map_at row (map_at face (const val))) grid


--Arg1: A tictactoe triplet (col, row, face), where each coordinate is in the inclusive range [1, (length Arg2)].
--Arg2: emptyChar or player1Char or player2Char.
--Arg3: A 3D tictactoe board.
--Ret: The new tictactoe board, where the board at the triplet's coordinates now contains Arg2's value.
--NOTES: The Arg1 triplet is assumed to be inside the boundaries of the board.
setCell :: (Int, Int, Int) -> Char -> [[String]] -> [[String]]
setCell (col, row, face) = update3 ((face - 1), (row - 1), (col - 1))


--Arg1: A tictactoe triplet (col, row, face), where each coordinate is in the inclusive range [1, (length Arg2)].
--Arg2: A 3D tictactoe board.
--Ret:emptyChar or player1Char or player2Char.
--NOTES: The Arg1 triplet is assumed to be inside the boundaries of the board.
getCell :: (Int, Int, Int) -> [[String]] -> Char
getCell (col, row, face) board = board !! (face - 1) !! (row - 1) !! (col - 1)


--Arg1: A tictactoe move triplet (col, row, face), where each coordinate is in the inclusive range [1, (length Arg2)].
--Arg2: A 3D tictactoe board.
--Ret: True if the move is available on the board.
--NOTES: The Arg1 triplet is assumed to be inside the boundaries of the board.
isEmpty :: (Int, Int, Int) -> [[String]] -> Bool
isEmpty triplet board = (getCell triplet board) == emptyChar



-- ============================== Extracting 2D boards and 3D diagonals out of the 3D board ==============================
-- ==================== Extracting 2D boards out of the 3D board ====================
-- ********** Common Helper Functions **********
--Arg1: The 3D board.
--Ret: The same 3D board, however each 2D board is transposed.
getEachTransposed :: [[String]] -> [[String]]
getEachTransposed = map transpose



-- ********** Extracting 2D boards. **********
--Arg1: The default 3D board of size NxNxN (same thing as getUpDownBoards).
--Ret: A list of N 2D-boards (up-down), all of size NxN.
getUpDownBoards :: [[String]] -> [[String]]
getUpDownBoards board3D = board3D


--Arg1: The default 3D board of size NxNxN (same thing as getUpDownBoards).
--Ret: A list of N 2D-boards (back-forward), all of size NxN.
getBackForwardBoards :: [[String]] -> [[String]]
getBackForwardBoards board3D = getEachTransposed (transpose board3D)
-- getBackForwardBoards board3D = transpose board3D


--Arg1: The default 3D board of size NxNxN (same thing as getUpDownBoards).
--Ret: A list of N 2D-boards (left-right), all of size NxN.
getLeftRightBoards :: [[String]] -> [[String]]
getLeftRightBoards board3D = transpose (getEachTransposed board3D)




-- ==================== Extracting the 3D diagonals ====================
--WARNING: Very clever functions ahead.

--Arg1: A board 2D.
--Ret: The first diagonal of a square matrix..
diag :: [[a]] -> [a]
diag [] = []
diag ((x : xs) : rows) = x : diag (map tail rows)


--Arg1: A board 2D.
--Ret: The other diagonal of a square matrix.
otherDiag :: [[a]] -> [a]
otherDiag = diag . (map reverse)


--Ret: The four diagonals of the 3D board (very very clever function).
{-
diag (map diag cube)
otherDiag (map diag cube)
diag (map otherDiag cube)
otherDiag (map otherDiag cube)
-}
cornerDiags :: [[[a]]] -> [[a]]
cornerDiags cube = [f (map g cube) | f <- [diag, otherDiag], g <- [diag, otherDiag]]




-- ============================== getOutcome related (winning condition) ==============================
--Arg1: A single 2D tictactoe board.
--Ret: The two diagonals of the 2D board.
getDiagonals2D :: [String] -> [String]
getDiagonals2D board2D = [diag board2D, otherDiag board2D]


--Arg1: Requested char.
--Arg2: A single tictactoe row.
--Ret: True if requested player has a win condition in this row.
isTictactoeRow :: Char -> String -> Bool
isTictactoeRow c row =
    let size = length row
    in if c == player1Char then
        row == (replicate size player1Char)
    else
        row == (replicate size player2Char)


--Arg1: Requested char.
--Arg2: A single board 2D.
--Ret: Returns the requestedChar if they won; otherwise returns emptyChar.
--WARNING: Only checks horizontally and the 2 diagonals. No vertical checks.
isTictactoeBoard2D :: Char -> [String] -> Char
isTictactoeBoard2D c board2D =
    let horizontalWin = or (map (isTictactoeRow c) board2D)
        diagonalWin = or (map (isTictactoeRow c) (getDiagonals2D board2D))
    in
        if horizontalWin || diagonalWin then c else emptyChar


--Arg1: A list of 2D tictactoe boards.
--Ret: True if player1 has a full row in at least one of the boards.
isTictactoeBoard2Dlist :: Char -> [[String]] -> Bool
isTictactoeBoard2Dlist c boards2D = elem c (map (isTictactoeBoard2D c) boards2D)


-- Outcomes:
outcomeGameInProgress = 0
outcomePlayer1Wins = 1
outcomePlayer2Wins = 2
outcomeDraw = 3


--Arg1: The 3D board.
{-Ret:
outcomeGameInProgress,
outcomePlayer1Wins,
outcomePlayer2Wins,
outcomeDraw.
-}
getOutcome :: [[String]] -> Int
getOutcome board =
    let ud = getUpDownBoards board
        lr = getLeftRightBoards board --this is actually doing what I thought back-forward was meant to do (vertical checks)
        bf = getBackForwardBoards board
        diagonals3D = cornerDiags board
        allBoards2D = [ud, lr, bf]
        player1Wins = or (map (isTictactoeBoard2Dlist player1Char) allBoards2D) || or (map (isTictactoeRow player1Char) diagonals3D)
        player2Wins = or (map (isTictactoeBoard2Dlist player2Char) allBoards2D) || or (map (isTictactoeRow player2Char) diagonals3D)
        concatted = concat (concat board)
        thereAreRemainingEmptyCells = elem emptyChar concatted
    in
        if player1Wins then outcomePlayer1Wins
        else if player2Wins then outcomePlayer2Wins
        else if thereAreRemainingEmptyCells then outcomeGameInProgress
        else outcomeDraw





-- ============================== heuristicAnalysis related ==============================

--Arg1: Board size.
--Ret: The score of the winning condition (a big number).
getWinningScore :: Int -> Int
getWinningScore boardSize = (boardSize ^ boardSize) ^ boardSize


--Arg1: Number of chips of a certain player.
--Arg2: The size of the board.
--Ret: A score value (preferably non-linear) WRT the count and boardSize.
countToScore :: Int -> Int -> Int
countToScore count boardSize =
    if count == boardSize then
        fromIntegral (getWinningScore boardSize) -- Win condition satisfied.
    else fromIntegral (boardSize ^ count)




--Arg1: player1Char or player2Char
--Arg2: A single row.
--Ret: Score of the requested player.
getScoresInRow :: Char -> String -> Int
getScoresInRow requestedChar row =
    let boardSize = length row
        player1Count = length [p | p <- row, p == player1Char]
        player1Score = countToScore player1Count boardSize
        player2Count = length [c | c <- row, c == player2Char]
        player2Score = countToScore player2Count boardSize
    in
        if player1Count > 0 && player2Count > 0 then
            0 -- Nobody can win in this row.
        else if requestedChar == player1Char then
            player1Score
        else
            player2Score
                         

--Arg1: player1Char or player2Char
--Arg2: A single board 2D.
--Ret: Score of the requested player.
getScoresInBoard2D :: Char -> [String] -> Int
getScoresInBoard2D c board2D =
    let scoreSum = sum (map (getScoresInRow c) board2D)
        [d1, d2] = map (getScoresInRow c) (getDiagonals2D board2D)
        totalScore = scoreSum + d1 + d2
    in totalScore


--Arg1: player1Char or player2Char
--Arg2: A list of 2D boards.
--Ret: Score of the requested player.
getScoresInListOf2Dboards :: Char -> [[String]] -> Int
getScoresInListOf2Dboards c listBoard2D = sum (map (getScoresInBoard2D c) listBoard2D)


--Arg1: player1Char or player2Char
--Arg2: A list of 3D diagonals.
--Ret: Score of the requested player.
getScoresInDiagonals3D :: Char -> [String] -> Int
getScoresInDiagonals3D c diagonals3D = sum (map (getScoresInRow c) diagonals3D)


--Arg1: player1Char or player2Char
--Arg2: Board 3D.
--Ret: Score of the requested player.
getHeuristicScores :: Char -> [[String]] -> Int
getHeuristicScores c board =
    let ud = getUpDownBoards board
        lr = getLeftRightBoards board
        bf = getBackForwardBoards board
        diagonals3D = cornerDiags board
        allBoards2D = [ud, lr, bf]
        scoreSum = sum (map (getScoresInListOf2Dboards c) allBoards2D)
        diag3Dsum = getScoresInDiagonals3D c diagonals3D
        totalScore = scoreSum + diag3Dsum
    in totalScore



-- ============================== minimax related ==============================
--Arg1: A 3D board.
--Ret: A list of all possible move triplets (col, row, face) whose values are in [1..boardSize].
getAllPossibleMoves :: [[String]] -> [(Int, Int, Int)]
getAllPossibleMoves board =
    let range = [1..(length board)]
    in [(c, r, f) | c <- range, r <- range, f <- range, isEmpty (c, r, f) board]



--Arg1: The 3D board.
--Arg2: All possible moves
--Arg3: Depth.
--Arg4: isMaxPlayer.
--Arg5: Alpha.
--Arg6: Beta.
--Arg7: bestMove so far (or something).
--Arg8: maxScore so far (or something). Because this is foreach for MAX.
--Ret: A tuple (Maybe (row, col, face), bestScore). I think it's (Maybe bestMoveTriplet, bestScore).
foreachMoves_max :: [[String]] -> [(Int, Int, Int)] -> Int -> Bool -> Int -> Int -> (Int, Int, Int) -> Int -> (Maybe (Int, Int, Int), Int)

foreachMoves_max _ [] _ _ _ _ bestMove maxScore =
    (Just bestMove, maxScore) -- No more moves.

foreachMoves_max board _ 0 isMaxPlayer alpha beta bestMove maxScore =
    minimax board 0 isMaxPlayer alpha beta
    -- TODO: Depth==0, so use minimax's existing pattern? I don't know.

foreachMoves_max board (move : remMoves) depth isMaxPlayer alpha beta bestMove maxScore =
    if beta <= alpha then
        (Just bestMove, maxScore)
    else
        let (_, curScore) = minimax board (depth - 1) (not isMaxPlayer) alpha beta
        in
            if curScore > maxScore then
                -- curScore is better, so move is the new bestMove.
                -- Check the remaining moves.
                let newAlpha = max alpha curScore
                in foreachMoves_max board remMoves depth isMaxPlayer newAlpha beta move curScore
            else
                -- Existing bestMove and maxScore are better. Keep them.
                -- Check the remaining moves.
                foreachMoves_max board remMoves depth isMaxPlayer alpha beta bestMove maxScore


--Arg1: The 3D board.
--Arg2: All possible moves
--Arg3: Depth.
--Arg4: isMaxPlayer.
--Arg5: Alpha.
--Arg6: Beta.
--Arg7: bestMove so far (or something).
--Arg8: minScore so far (or something). Because this is foreach for MIN.
--Ret: A tuple (Maybe (row, col, face), bestScore). I think it's (Maybe bestMoveTriplet, bestScore).
foreachMoves_min :: [[String]] -> [(Int, Int, Int)] -> Int -> Bool -> Int -> Int -> (Int, Int, Int) -> Int -> (Maybe (Int, Int, Int), Int)
foreachMoves_min _ [] _ _ _ _ bestMove minScore =
    (Just bestMove, minScore) -- No more moves.

foreachMoves_min board _ 0 isMaxPlayer alpha beta bestMove minScore =
    minimax board 0 isMaxPlayer alpha beta
    -- TODO: Depth==0, so use minimax's existing pattern? I don't know.

foreachMoves_min board (move : remMoves) depth isMaxPlayer alpha beta bestMove minScore =
    if beta <= alpha then
        (Just bestMove, minScore)
    else
        let (_, curScore) = minimax board (depth - 1) (not isMaxPlayer) alpha beta
        in
            if curScore < minScore then
                -- curScore is better, so move is the new bestMove.
                -- Check the remaining moves.
                let newBeta = min beta curScore
                in foreachMoves_min board remMoves depth isMaxPlayer alpha newBeta move curScore
            else
                -- Existing bestMove and minScore are better. Keep them.
                -- Check the remaining moves.
                foreachMoves_min board remMoves depth isMaxPlayer alpha beta bestMove minScore


--Arg1: A 3D board.
--Arg2: Depth.
--Arg3: IsMaximizingPlayer or not.
--Arg4: Alpha.
--Arg5: Beta.
--Ret: A tuple (Maybe (row, col, face), bestScore). I think it's (Maybe bestMoveTriplet, bestScore).
minimax :: [[String]] -> Int -> Bool -> Int -> Int -> (Maybe (Int, Int, Int), Int)
minimax board 0 isMaxPlayer _ _ = 
    -- Depth == 0, therefore return the heuristic estimate.
    let player1Score = getHeuristicScores player1Char board
        player2Score = getHeuristicScores player2Char board
    in (Nothing, (player2Score - player1Score))
    -- Player1 (human) is always minimizing, so return the score for the AI (player2).

minimax board depth isMaxPlayer alpha beta =
    let outcome = getOutcome board
        boardSize = length board
        winningScore = getWinningScore boardSize
    in 
        -- TODO: Replace the if checks with "case",
        -- but without Haskell complaining about redundant stuff.
        if outcome == outcomePlayer1Wins then
            (Nothing, (-1) * winningScore)
        else if outcome == outcomePlayer2Wins then
            (Nothing, winningScore)
        else if outcome == outcomeDraw then
            (Nothing, 0)
        else -- outcome == outcomeGameInProgress
            let chip = if isMaxPlayer then player2Char else player1Char
                allMoves = getAllPossibleMoves board
                -- allBoards = map (\m -> setCell m chip board) allMoves
                bigNum = winningScore -- TODO: Just a very big number.
                initialBestMove = head allMoves -- TODO: Don't care? Just the first move?
            in
                if isMaxPlayer then
                    foreachMoves_max board allMoves depth True alpha beta initialBestMove (-bigNum)
                else
                    foreachMoves_min board allMoves depth False alpha beta initialBestMove bigNum


{-
        case outcome of
            outcomePlayer1Wins -> (Nothing, (-1) * winningScore)
            outcomePlayer2Wins -> (Nothing, winningScore)
            outcomeDraw -> (Nothing, 0)
            outcomeGameInProgress ->
                let chip = if isMaxPlayer then player2Char else player1Char
                    allMoves = getAllPossibleMoves board
                    -- allBoards = map (\m -> setCell m chip board) allMoves
                    bigNum = winningScore -- TODO: Just a very big number.
                    initialBestMove = head allMoves -- TODO: Don't care? Just the first move?
                in
                    if isMaxPlayer then
                        foreachMoves_max board allMoves depth True alpha beta initialBestMove (-bigNum)
                    else
                        foreachMoves_min board allMoves depth False alpha beta initialBestMove bigNum
-}



-- ============================== Game loop, etc. ==============================

--Asks the size of the board.
--Ret: IO Int. Use the "<-" operator when working with this function.
askBoardSize :: IO Int
askBoardSize =
    do
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



--Asks who should start first. Human (player1) or computer (player2).
--Ret: IO Int. Use the "<-" operator when working with this function.
askStarterPlayer :: IO Int
askStarterPlayer =
    do
        putStrLn "Would you like to start first? (y/n)"
        answer <- getLine
        if answer == "y" || answer == "Y" then do
            return player1
        else if answer == "n" || answer == "N" then do
            return player2
        else do
            putStrLn "Please enter 'y' or 'n'."
            askStarterPlayer


--Desc: This is how you start the game.
--Ret: IO ().
run :: IO ()
run =
    do
        size <- askBoardSize
        starter <- askStarterPlayer
        gameStart starter (initBoard size)


--Desc: Do game initialization stuff here.
--Arg1: Player ID (Int). Basically, (player1) or (player2).
--Arg2: The 3D board.
--Ret: IO ().
gameStart :: Int -> [[String]] -> IO ()
gameStart player board =
    do
        gameLoop player board



--Desc: Asks the move triplet (face row col) of the player.
--      What the player gets asked VS what is returned is in the opposite order.
--      Reason: The game is easier to play with this input order (face, row, col),
--      while the rest of the code needs the other order (col, row, face).
--Arg1: The 3D board.
--Ret: IO (Int, Int, Int). These are (col, row, face). Yes, it's reversed.
getPlayerMove :: [[String]] -> IO (Int, Int, Int)
getPlayerMove board =
    do
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





--Arg1: A 3D board.
--Ret: IO (Int, Int, Int). These are (col, row, face) of the computer.
getComputerMove :: [[String]] -> IO (Int, Int, Int)
getComputerMove board =
    let boardSize = length board
        bigNum = 2 * (getWinningScore boardSize) -- TODO: Is there an INT_MAX or something?
        alpha = (-bigNum)
        beta = bigNum
        depth = 3 -- TODO: Fix hardcoded depth value. Ask for difficulty?
        (bestMove, _) = minimax board depth True alpha beta 
    in
        if bestMove == Nothing then do
            error "The game is in progress, but minimax returned Nothing as its best move. Perhaps the game should have ended?"
        else
            let move@(col, row, face) = (fromJust bestMove)
                moveStr = "(" ++ (show face) ++ ", " ++ (show row) ++ ", " ++ (show col) ++ ")"
            in do
                putStrLn ("Computer moves at: " ++ moveStr ++ " (face row col).")
                return move






--WARNING: Do not call this function if outcomeGameInProgress
--Desc: Announces the winner.
--Arg1: outcome.
--Ret: IO ()
announceWinner :: Int -> IO ()
announceWinner 0 = error "ERROR: Trying to announceWinner while the game is still in progress."
announceWinner outcome =
    if outcome == outcomePlayer1Wins then do
        putStrLn "Player wins!"
    else if outcome == outcomePlayer2Wins then do
        putStrLn "Computer wins!"
    else do {- outcomeDraw -}
        putStrLn "It's a draw!"


--Desc: Game loop.
--Arg1: (player1) or (player2)
--Arg2: The current state of the game board.
--Ret: IO ()
gameLoop :: Int -> [[String]] -> IO ()
gameLoop player board =
    do
        let outcome = getOutcome board
             in
                 if outcome == 0 then do
                     showBoard board
                     if player == player1 then do
                         playerMove <- getPlayerMove board
                         let newBoard = setCell playerMove player1Char board
                             in gameLoop (3 - player) newBoard
                     else do
                         computerMove <- getComputerMove board
                         let newBoard = setCell computerMove player2Char board
                             in gameLoop (3 - player) newBoard
                 else do
                     showBoard board
                     announceWinner outcome





-- ================================================== TESTS (delete later plz) ==================================================
{-
debug_getAlphabetBoard = [["abc", "def", "ghi"], ["jkl", "mno", "pqr"], ["stu", "vwx", "yz#"]]

debug_getAlphabetBoard_LeftRight = [["adg", "jmp", "svy"], ["beh", "knq", "twz"], ["cfi", "lor", "ux#"]]
debug_getAlphabetBoard_BackForward = [["abc", "jkl", "stu"], ["def", "mno", "vwx"], ["ghi", "pqr", "yz#"]]


unitTest_LeftRight =
    let defaultBoard = debug_getAlphabetBoard
        leftRight2DBoards = getLeftRightBoards defaultBoard
    in leftRight2DBoards == debug_getAlphabetBoard_LeftRight


unitTest_BackForward =
    let defaultBoard = debug_getAlphabetBoard
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
debug_test2D_1 = (is2Dtictactoe 'X' ["XXX",
                                     "...",
                                     "..."]) == 'X'
                                     
debug_test2D_2 = (is2Dtictactoe 'X' ["...",
                                     "XXX",
                                     "..."]) == 'X'

debug_test2D_3 = (is2Dtictactoe 'X' ["...",
                                     "...",
                                     "XXX"]) == 'X'

debug_test2D_4 = (is2Dtictactoe 'X' ["X..",
                                     "X..",
                                     "X.."]) == 'X'


debug_test2D_5 = (is2Dtictactoe 'X' [".X.",
                                     ".X.",
                                     ".X."]) == 'X'


debug_test2D_6 = (is2Dtictactoe 'X' ["..X",
                                     "..X",
                                     "..X"]) == 'X'


debug_test2D_7 = (is2Dtictactoe 'X' ["X..",
                                     ".X.",
                                     "..X"]) == 'X'

debug_test2D_8 = (is2Dtictactoe 'X' ["..X",
                                     ".X.",
                                     "X.."]) == 'X'

debug_test2D_all = debug_test2D_1 && debug_test2D_2 && debug_test2D_3 && debug_test2D_4 && debug_test2D_5 && debug_test2D_6 && debug_test2D_7 && debug_test2D_8


debug_2db2 = ["XXX", "..O", "O.."]
debug_2db3 = ["X..", ".XO", "O.X"]

debug_whynot1 = let board = initBoard 3
                    b1 = setCell (1, 1, 1) 'X' board
                    b2 = setCell (1, 1, 2) 'X' b1
                    b3 = setCell (1, 1, 3) 'X' b2
                in is2Dtictactoe_forPlayer1 (getLeftRightBoards b3)

debug_updown1 = let board = initBoard 3
                    b1 = setCell (1, 1, 2) 'X' board
                    b2 = setCell (2, 1, 2) 'X' b1
                    b3 = setCell (3, 1, 2) 'X' b2
                -- in putStrLn (toStrBoard (getUpDownBoards b3))
                in is2Dtictactoe_forPlayer1 (getBackForwardBoards b3)


debug_leftright1 = let board = initBoard 3
                       b1 = setCell (1, 1, 1) 'X' board
                       b2 = setCell (1, 1, 2) 'X' b1
                       b3 = setCell (1, 1, 3) 'X' b2
                   -- in putStrLn (toStrBoard (getUpDownBoards b3))
                   in is2Dtictactoe_forPlayer1 (getBackForwardBoards b3)


--(49, 49)
testHeurEmpty = getHeuristicScores (initBoard 3)

--(57, 49)
testHeurMeh = getHeuristicScores (setCell (1,1,2) 'X' (initBoard 3))

--(75, 49)
testHeurMid = getHeuristicScores (setCell (2,2,2) 'X' (initBoard 3))

--(63, 49)
testHeurCorner = getHeuristicScores (setCell (1,1,1) 'X' (initBoard 3))


testHeur2 = getHeuristicScores (setCell (2,1,1) 'X' (setCell (1,1,1) 'X' (initBoard 3)))
testHeur3 = getHeuristicScores (setCell (3,1,1) 'X' (setCell (2,1,1) 'X' (setCell (1,1,1) 'X' (initBoard 3))))
-}

module Board where

    ----------- Data Structures
    
        -- Move -> ((a, 6), (a, 7)) -> (origin, destination)
        type Move = ((Char, Int), (Char, Int))

        -- Piece color 
        data Piece = Empty |
            King Bool |
            Queen Bool | 
            Rook Bool |
            Knight Bool |
            Bishop Bool |
            Pawn Bool 
            deriving(Eq, Show)

        -- Get
        getPieceColor :: Piece -> Bool
        getPieceColor (King b) = b
        getPieceColor (Queen b) = b
        getPieceColor (Rook b) = b
        getPieceColor (Knight b) = b
        getPieceColor (Bishop b) = b
        getPieceColor (Pawn b) = b

        -- Board
        type Line = [Piece]
        type Board = [Line]
        

    ----------- Game creation

        -- Creates starting board
        makeBoard :: Board
        makeBoard = 
            [
                [ (Rook False), (Knight False), (Bishop False), (Queen False),  (King False), (Bishop False), (Knight False), (Rook False) ],
                [ (Pawn False), (Pawn False),   (Pawn False),   (Pawn False),   (Pawn False), (Pawn False), (Pawn False),   (Pawn False) ],
                [ Empty,        Empty,        Empty,         Empty,         Empty,          Empty,         Empty,            Empty      ],
                [ Empty,        Empty,        Empty,         Empty,         Empty,          Empty,         Empty,            Empty      ],
                [ Empty,        Empty,        Empty,         Empty,         Empty,          Empty,         Empty,            Empty      ],
                [ Empty,        Empty,        Empty,         Empty,         Empty,          Empty,         Empty,            Empty      ],
                [ (Pawn True), (Pawn True),   (Pawn True),   (Pawn True),   (Pawn True),    (Pawn True), (Pawn True),   (Pawn True)     ],
                [ (Rook True), (Knight True), (Bishop True), (Queen True), (King True),     (Bishop True), (Knight True), (Rook True)   ]
            ]

        checkBoard :: Board
        checkBoard = 
            [
                [ (Rook False), (Knight False), (Bishop False), (Queen False),  (King False), (Bishop False), (Knight False), (Rook False) ],
                [ (Pawn False), (Pawn False),   (Pawn False),   (Pawn False),   (Pawn False), (Pawn False), (Pawn False),   (Pawn False) ],
                [ Empty,        Empty,        Empty,         Empty,         Empty,          Empty,         Empty,            Empty      ],
                [ Empty,        Empty,        Empty,         Empty,         Empty,          Empty,         Empty,            Empty      ],
                [ Empty,        Empty,        Empty,         Empty,         (Queen False),          Empty,         Empty,            Empty      ],
                [ Empty,        Empty,        Empty,         Empty,         Empty,          Empty,         Empty,            Empty      ],
                [ (Pawn True), (Pawn True),   (Pawn True),   (Pawn True),   (King True),    (Pawn True), (Pawn True),   (Pawn True)     ],
                [ (Rook True), (Knight True), (Bishop True), (Queen True), Empty,     (Bishop True), (Knight True), (Rook True)   ]
            ]
    
    ----------- Draw

        -- Prints given board
        printBoard :: Int -> Board -> String 
        printBoard n [] = ""
        printBoard 1 (x:xs) = (show 1) ++ printBoardLine(x) ++ (show 1) ++ "\n    a   b   c   d   e   f   g   h\n"
        printBoard 8 (x:xs) = "    a   b   c   d   e   f   g   h\n" ++ (show 8) ++ printBoardLine(x) ++ (show 8) ++ "\n"  ++ (printBoard (8-1) xs)
        printBoard n (x:xs) = (show n) ++ printBoardLine(x) ++ (show n) ++ "\n"  ++ (printBoard (n-1) xs)

        -- Prints a board line
        printBoardLine :: [Piece] -> String
        printBoardLine [] = " | "
        printBoardLine (x:xs) = " | " ++ [getPieceChar x] ++ (printBoardLine xs) 

        -- Gets unicode char for a given piece
        getPieceChar :: Piece -> Char
        getPieceChar Empty = ' '
        getPieceChar (King True) = '♚'
        getPieceChar (Queen True) = '♛'
        getPieceChar (Rook True) = '♜'
        getPieceChar (Knight True) = '♞'
        getPieceChar (Bishop True) = '♝'
        getPieceChar (Pawn True) = '♟'
        getPieceChar (King False) = '♔'
        getPieceChar (Queen False) = '♕'
        getPieceChar (Rook False) = '♖'
        getPieceChar (Knight False) = '♘'
        getPieceChar (Bishop False) = '♗'
        getPieceChar (Pawn False) = '♙'


    ----------- Chess mechanics

        -- Check piece type
        isEmpty, isKing, isQueen, isRook, isKnight, isBishop, isPawn :: Piece -> Bool
        isEmpty Empty = True
        isEmpty _ = False
        isKing (King _) = True
        isKing _ = False
        isQueen (Queen _) = True
        isQueen _ = False
        isRook (Rook _) = True
        isRook _ = False
        isKnight (Knight _) = True
        isKnight _ = False
        isBishop (Bishop _) = True
        isBishop _ = False
        isPawn (Pawn _) = True
        isPawn _ = False

        -- Validate movement
        --validateMove :: Piece -> Move -> Bool
        --validateMove p m = (isValidMove p m)

        ----------- Basic Validation

        -- Wraps basic validations
        basicValidation :: Board -> Move -> Bool
        basicValidation b m = (validateCoordinates m) && (checkOrigin b m) && (checkTarget b m)

        -- Check if coordinates are valid
        validateCoordinates :: Move -> Bool
        validateCoordinates (o, d) = (validCoord o) && (validCoord d)
            where
                validCoord c = ((translateLine(fst(c)) >= 1 && translateLine(fst(c)) <= 8) && (snd(c) >= 1 && snd(c) <= 8))
            
        -- Check if there is a piece at origin position
        checkOrigin :: Board -> Move -> Bool
        checkOrigin b m = (findPiece b (fst m) /= Empty )
        
        -- Check if target position is empty or occupied by oposing player piece
        checkTarget :: Board -> Move -> Bool
        checkTarget b m = (findPiece b (snd m) == Empty) || ((getPieceColor (findPiece b (fst m)) /= (getPieceColor (findPiece b (snd m)))))

        -----------------------------
        
        ----------- Piece Type Validation
        
        -- Check if the move is possible based on the piece movement type
        isValidMove :: Board -> Piece -> (Int, Int) -> (Int, Int) -> Bool
        isValidMove b (King _) o d  
            | (fst(d) > fst(o) + 1) = False
            | (fst(d) < fst(o) - 1) = False
            | (snd(d) > snd(o) + 1) = False
            | (snd(d) < snd(o) - 1) = False
            | otherwise             = True
        isValidMove b (Rook _) o d
            | (fst(d) == fst(o)) = True
            | (snd(d) == snd(o)) = True
            | otherwise          = False
        isValidMove b (Bishop _) o d
            | (fst(d) < fst(o)) && ((snd(d) > snd(o) ) || (snd(d) < snd(o))) = True
            | (fst(d) > fst(o)) && ((snd(d) > snd(o) ) || (snd(d) < snd(o))) = True
            | otherwise          = False
        isValidMove b (Knight _) o d
            | (fst(d) == (fst(o) - 1)) && ((snd(d) == (snd(o) - 2)) || (snd(d) == (snd(o) + 2))) = True
            | (fst(d) == (fst(o) - 2)) && ((snd(d) == (snd(o) - 1)) || (snd(d) == (snd(o) + 1))) = True
            | (fst(d) == (fst(o) + 1)) && ((snd(d) == (snd(o) - 2)) || (snd(d) == (snd(o) + 2))) = True
            | (fst(d) == (fst(o) + 2)) && ((snd(d) == (snd(o) - 1)) || (snd(d) == (snd(o) + 1))) = True
            | otherwise                                                                          = False 
        isValidMove b (Queen _) o d
            | (fst(d) == fst(o)) = True
            | (snd(d) == snd(o)) = True
            | (fst(d) < fst(o)) && ((snd(d) > snd(o) ) || (snd(d) < snd(o))) = True
            | (fst(d) > fst(o)) && ((snd(d) > snd(o) ) || (snd(d) < snd(o))) = True
            | otherwise = False
        isValidMove b (Pawn True) o d
            | (snd(d) == (snd(o) + 1)) && (fst(d) == fst(o)) = True
            | ((fst(d) == (fst(o) - 1)) && ((snd(d) == (snd(o) + 1)) || (snd(d) == (snd(o) - 1))) && (findPiece b (translateLineBack(fst(d)),snd(d)) /= Empty)) = True
            | ((fst(d) == (fst(o) + 2)) && ((snd(d) == (snd(o) + 1)) || (snd(d) == (snd(o) - 1))) && (findPiece b (translateLineBack(fst(d)),snd(d)) /= Empty)) = True
            | ((snd(d) == (snd(o) + 2)) && (fst(d) == fst(o))) && ((snd(o) == 2) || (snd(o) == 7)) = True
            | otherwise = False
        isValidMove b (Pawn False) o d
            | (snd(d) == (snd(o) - 1)) && (fst(d) == fst(o)) = True
            | ((fst(d) == (fst(o) - 1)) && ((snd(d) == (snd(o) - 1)) || (snd(d) == (snd(o) + 1))) && (findPiece b (translateLineBack(fst(d)),snd(d)) /= Empty)) = True
            | ((fst(d) == (fst(o) + 2)) && ((snd(d) == (snd(o) - 1)) || (snd(d) == (snd(o) + 1))) && (findPiece b (translateLineBack(fst(d)),snd(d)) /= Empty)) = True
            | ((snd(d) == (snd(o) - 2)) && (fst(d) == fst(o))) && ((snd(o) == 2) || (snd(o) == 7)) = True
            | otherwise = False

        -----------------------------

        isDiagonal :: (Int, Int) -> (Int, Int) -> Bool
        isDiagonal desired p = (upLeft desired p) || (upRight desired p) || (downRight desired p) || (downLeft desired p)


        upLeft :: (Int, Int) -> (Int, Int) -> Bool
        upLeft o p
            | (fst(p) < 1 || snd(p) > 8) = False
            | (o == p)                  = True
            | otherwise                 = upLeft o (((fst p) - 1), ((snd p) + 1))
        upRight :: (Int, Int) -> (Int, Int) -> Bool
        upRight o p
            | (fst(p) > 8 || snd(p) > 8) = False
            | (o == p)                  = True
            | otherwise                 = upRight o (((fst p) + 1), ((snd p) + 1))
        downRight :: (Int, Int) -> (Int, Int) -> Bool
        downRight o p
            | (fst(p) > 8 || snd(p) < 1) = False
            | (o == p)                  = True
            | otherwise                 = downRight o (((fst p) + 1), ((snd p) - 1))
        downLeft :: (Int, Int) -> (Int, Int) -> Bool  
        downLeft o p      
            | (fst(p) < 1 || snd(p) < 1) = False
            | (o == p)                  = True
            | otherwise                 = downLeft o (((fst p) - 1), ((snd p) - 1))



        -- Check if the move is possible based on check conditions
        isCheckMove :: Board -> Piece -> (Int, Int) -> (Int, Int) -> Bool
        isCheckMove b (King True) o d  
            | ((fst(d) == fst(o)) && ((snd(d) == (snd(o) + 1)) || (snd(d) == (snd(o) - 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | ((snd(d) == snd(o)) && ((fst(d) == (fst(o) + 1)) || (fst(d) == (fst(o) - 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | ((fst(d) == (fst(o) + 1)) && ((snd(d) == (snd(o) + 1)) || (snd(d) == (snd(o) - 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | ((fst(d) == (fst(o) - 1)) && ((snd(d) == (snd(o) + 1)) || (snd(d) == (snd(o) - 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | otherwise             = False
        isCheckMove b (King False) o d  
            | ((fst(d) == fst(o)) && ((snd(d) == (snd(o) + 1)) || (snd(d) == (snd(o) - 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | ((snd(d) == snd(o)) && ((fst(d) == (fst(o) + 1)) || (fst(d) == (fst(o) - 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | ((fst(d) == (fst(o) + 1)) && ((snd(d) == (snd(o) + 1)) || (snd(d) == (snd(o) - 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | ((fst(d) == (fst(o) - 1)) && ((snd(d) == (snd(o) + 1)) || (snd(d) == (snd(o) - 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | otherwise             = False
        isCheckMove b (Rook True) o d
            | (fst(d) == fst(o)) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | (snd(d) == snd(o)) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | otherwise          = False
        isCheckMove b (Rook False) o d
            | (fst(d) == fst(o)) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | (snd(d) == snd(o)) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | otherwise                                                                             = False
        isCheckMove b (Bishop True) o d
            | (fst(d) < fst(o)) && ((snd(d) > snd(o) ) || (snd(d) < snd(o))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | (fst(d) < fst(o)) && ((snd(d) > snd(o) ) || (snd(d) < snd(o))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | otherwise          = False
        isCheckMove b (Bishop False) o d
            | (isDiagonal d o) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True))   = True
            | otherwise                                                                             = False
        isCheckMove b (Knight True) o d
            | ((fst(d) == (fst(o) - 1)) && ((snd(d) == (snd(o) - 2)) || (snd(d) == (snd(o) + 2)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | ((fst(d) == (fst(o) - 2)) && ((snd(d) == (snd(o) - 1)) || (snd(d) == (snd(o) + 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | ((fst(d) == (fst(o) + 1)) && ((snd(d) == (snd(o) - 2)) || (snd(d) == (snd(o) + 2)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | ((fst(d) == (fst(o) + 2)) && ((snd(d) == (snd(o) - 1)) || (snd(d) == (snd(o) + 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | otherwise          = False 
        isCheckMove b (Knight False) o d
            | ((fst(d) == (fst(o) - 1)) && ((snd(d) == (snd(o) - 2)) || (snd(d) == (snd(o) + 2)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | ((fst(d) == (fst(o) - 2)) && ((snd(d) == (snd(o) - 1)) || (snd(d) == (snd(o) + 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | ((fst(d) == (fst(o) + 1)) && ((snd(d) == (snd(o) - 2)) || (snd(d) == (snd(o) + 2)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | ((fst(d) == (fst(o) + 2)) && ((snd(d) == (snd(o) - 1)) || (snd(d) == (snd(o) + 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | otherwise          = False 
        isCheckMove b (Queen True) o d
            | (fst(d) == fst(o)) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | (snd(d) == snd(o)) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | (fst(d) < fst(o)) && ((snd(d) > snd(o) ) || (snd(d) < snd(o))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | (fst(d) < fst(o)) && ((snd(d) > snd(o) ) || (snd(d) < snd(o))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | otherwise = False
        isCheckMove b (Queen False) o d
            | (fst(d) == fst(o)) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | (snd(d) == snd(o)) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | ((fst(d) == (fst(o) - 1)) && ((snd(d) == (snd(o) + 1)) || (snd(d) == (snd(o) - 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | ((fst(d) == (fst(o) + 1)) && ((snd(d) == (snd(o) + 1)) || (snd(d) == (snd(o) - 1)))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | otherwise = False
        isCheckMove b (Pawn True) o d
            | ((snd(d) == (snd(o) + 1)) && (fst(d) == fst(o))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | ((fst(d) == (fst(o) - 1)) && ((snd(d) == (snd(o) + 1)) || (snd(d) == (snd(o) - 1))) && (findPiece b (translateLineBack(fst(d)),snd(d)) /= Empty)) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | ((fst(d) == (fst(o) + 2)) && ((snd(d) == (snd(o) + 1)) || (snd(d) == (snd(o) - 1))) && (findPiece b (translateLineBack(fst(d)),snd(d)) /= Empty)) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | (((snd(d) == (snd(o) + 2)) && (fst(d) == fst(o))) && ((snd(o) == 2) || (snd(o) == 7))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King False)) = True
            | otherwise = False
        isCheckMove b (Pawn False) o d
            | ((snd(d) == (snd(o) - 1)) && (fst(d) == fst(o))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | ((fst(d) == (fst(o) - 1)) && ((snd(d) == (snd(o) - 1)) || (snd(d) == (snd(o) + 1))) && (findPiece b (translateLineBack(fst(d)),snd(d)) /= Empty)) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | ((fst(d) == (fst(o) + 2)) && ((snd(d) == (snd(o) - 1)) || (snd(d) == (snd(o) + 1))) && (findPiece b (translateLineBack(fst(d)),snd(d)) /= Empty)) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | (((snd(d) == (snd(o) - 2)) && (fst(d) == fst(o))) && ((snd(o) == 2) || (snd(o) == 7))) && (findPiece b (translateLineBack(fst(d)),snd(d)) == (King True)) = True
            | otherwise = False
        isCheckMove b (Empty) o d = False

        isKingInCheck :: Board -> Bool -> Int -> Int -> Bool
        isKingInCheck b c 9 9 = False
        isKingInCheck b c x 9 = isKingInCheck b c (x+1) 1
        isKingInCheck b c x y
            | isCheckMove b (findPiece b ((translateLineBack x),y)) (x,y) (findKing b c 8) = True
            | otherwise = isKingInCheck b c x (y+1)


        isKingInCheckMate :: Board -> Bool -> Bool
        isKingInCheckMate b c = 
            let 
                (x, y) = (findKing b c 1)
            in (isKingInCheck b c 1 1) && (isKingInCheck (movePiece b ((translateLineBack(x),y), (translateLineBack(x+1), y))) c 1 1)  && (isKingInCheck (movePiece b ((translateLineBack(x),y), (translateLineBack(x+1), y+1))) c 1 1) && (isKingInCheck (movePiece b ((translateLineBack(x),y), (translateLineBack(x), y+1))) c 1 1) && (isKingInCheck (movePiece b ((translateLineBack(x),y), (translateLineBack(x-1), y+1))) c 1 1) && (isKingInCheck (movePiece b ((translateLineBack(x),y), (translateLineBack(x-1), y))) c 1 1) && (isKingInCheck (movePiece b ((translateLineBack(x),y), (translateLineBack(x-1), y-1))) c 1 1) && (isKingInCheck (movePiece b ((translateLineBack(x),y), (translateLineBack(x), y-1))) c 1 1) && (isKingInCheck (movePiece b ((translateLineBack(x),y), (translateLineBack(x+1), y-1))) c 1 1)
    
    ----------- Search piece

        -- Auxiliary to seraching line
        searchLine :: Board -> Int -> Line
        searchLine (x:xs) 8 = x
        searchLine (x:xs) n = searchLine xs (n+1)

        -- Finds the piece in a position of a given line
        findPosition :: Line -> Int -> Piece
        findPosition [] n = Empty
        findPosition (x:xs) 1 = x
        findPosition (x:xs) n = findPosition xs (n-1)
        
        -- Given a board position, finds the piece in it
        findPiece :: Board -> (Char, Int) -> Piece
        findPiece b p = findPosition (searchLine b (snd p)) (translateLine (fst p))

        -- Given a board, find the King position   

        findKing :: Board -> Bool -> Int -> (Int, Int)
        findKing (x:xs) c l
            | (findKingInLine x c 1) /= 0 = ((findKingInLine x c 1), l)
            | otherwise                   = findKing xs c (l-1)


        findKingInLine :: Line -> Bool -> Int -> Int
        findKingInLine l b 9 = 0
        findKingInLine [] b n  = 0
        findKingInLine (x:xs) b n 
            | ((King b) == x) = n
            | otherwise = findKingInLine xs b (n+1)
        findKingInLine (x:xs) b n = findKingInLine xs b (n+1)



    ----------- Swap Piece

        -- Swap Piece - puts given piece in a new destination
        swapPiece :: Board -> Piece -> (Char, Int) -> Board
        swapPiece b piece pos = swapPiece2 b piece (translateLine (fst pos)) (snd pos)

        -- Swap Piece - lines
        swapPiece2 :: Board -> Piece -> Int -> Int -> Board
        swapPiece2 (x:xs) p l 8 = (swapPiece3 x p l) : xs
        swapPiece2 (x:xs) p l c = x : (swapPiece2 xs p l (c+1))

        -- Swap Piece - columns
        swapPiece3 :: Line -> Piece -> Int -> Line
        swapPiece3 (x:xs) p 1 = p : xs
        swapPiece3 (x:xs) p c = x : (swapPiece3 xs p (c-1))



    ----------- Move Piece

        movePiece :: Board -> ((Char, Int), (Char, Int)) -> Board
        movePiece b m =  swapPiece (swapPiece b (findPiece b (fst m)) (snd m)) (Empty) (fst m)


    ----------- General

        -- Translates lines notation
        translateLine :: Char -> Int
        translateLine c = 
            case c of
                'a' -> 1 
                'b' -> 2 
                'c' -> 3 
                'd' -> 4 
                'e' -> 5 
                'f' -> 6  
                'g' -> 7
                'h' -> 8
                _   -> 0 

        -- Translate back lines notation
        translateLineBack :: Int -> Char
        translateLineBack c =
            case c of
                1 -> 'a'
                2 -> 'b'
                3 -> 'c'
                4 -> 'd'
                5 -> 'e'
                6 -> 'f'
                7 -> 'g'
                8 -> 'h'
                _ -> 'z'



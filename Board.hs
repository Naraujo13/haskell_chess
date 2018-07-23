
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
    
    ----------- Draw

        -- Prints given board
        printBoard :: Board -> String
        printBoard [] = ""
        printBoard (x:xs) = printBoardLine(x) ++ "\n" ++ (printBoard xs)

        -- Prints a board line
        printBoardLine :: [Piece] -> String
        printBoardLine [] = ""
        printBoardLine (x:xs) = " | " ++ [getPieceChar x] ++ " | " ++ (printBoardLine xs) 

        -- Gets unicode char for a given piece
        getPieceChar :: Piece -> Char
        getPieceChar Empty = '\2063'
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
    




    ----------- Search piece

        -- Auxiliary to seraching line
        searchLine :: Board -> Int -> Line
        searchLine (x:xs) 8 = x
        searchLine (x:xs) n = searchLine xs (n+1)

        -- Finds the piece in a position of a given line
        findPosition :: Line -> Int -> Piece
        findPosition (x:xs) 1 = x
        findPosition (x:xs) n = findPosition xs (n-1)
        
        -- Given a board position, finds the piece in it
        findPiece :: Board -> (Char, Int) -> Piece
        findPiece b p = findPosition (searchLine b (snd p)) (translateLine (fst p))

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



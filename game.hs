import Board
import Data.Char

board = makeBoard

main = do
    putStrLn "Bem vindo ao jogo de xadrez!\n"
    putStrLn (printBoard 8 board)
    game board

game :: Board -> IO ()
game b = if (isKingInCheckMate b True)
            then blackWin
            else if (isKingInCheckMate b False)
                    then whiteWin
                    else do 
                        nextWhiteMove b
                        nextBlackMove b
                        game b

blackWin :: IO ()
blackWin = putStrLn "Xeque Mate! O jogador preto ganhou!"

whiteWin :: IO ()
whiteWin = putStrLn "Xeque Mate! O jogador branco ganhou!"

nextWhiteMove :: Board -> IO ()
nextWhiteMove b = do 
    putStrLn "Jogador branco, é sua vez"
    move <- getMove b True
    return ()

nextBlackMove :: Board -> IO ()
nextBlackMove b = do
    putStrLn "Jogador preto, é a sua vez"
    move <- getMove b False
    return ()

getMove :: Board -> Bool -> IO String
getMove b c = do
    putStrLn "\tInsira sua jogada e tecle enter"
    move <- getLine
    makeMove b c (convertMove board move)
    return ""

convertMove :: Board -> String -> ((Char, Int),(Char,Int))
convertMove board (a:b:c:d:e:f) = ((a,(digitToInt b)),(d,(digitToInt e)))

makeMove :: Board -> Bool -> ((Char, Int),(Char,Int)) -> IO ()
makeMove b c ((oc,ol),(dc,dl)) = do 
    let piece = (findPiece b (oc,ol))
    if ((piece == (King c)) || (piece == (Queen c)) || (piece == (Bishop c)) || (piece == (Knight c)) || (piece == (Rook c)) || (piece == (Pawn c))) 
        && (isValidMove b piece (translateLine(oc),ol) (translateLine(dc),dl))
        && ((findPiece b (dc,dl) /= (King c)) && (findPiece b (dc,dl) /= (Queen c)) && (findPiece b (dc,dl) /= (Bishop c)) && (findPiece b (dc,dl) /= (Knight c)) && (findPiece b (dc,dl) /= (Rook c)) && (findPiece b (dc,dl) /= (Pawn c)))
        then do
            putStrLn (printBoard 8 (movePiece b ((oc,ol),(dc,dl))))
            if (isKingInCheck b False 1 1)
                then putStrLn "Xeque!"
                else putStrLn ""
        else do
            putStrLn "\nMovimento Inválido, tente novamente\n"
            if c
                then nextWhiteMove b
                else nextBlackMove b 
    return ()
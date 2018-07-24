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
                    else nextWhiteMove b

blackWin :: IO ()
blackWin = putStrLn "Xeque Mate! O jogador preto ganhou!"

whiteWin :: IO ()
whiteWin = putStrLn "Xeque Mate! O jogador branco ganhou!"

nextWhiteMove :: Board -> IO ()
nextWhiteMove b = do 
    putStrLn "Jogador branco, é sua vez"
    move <- getMove b
    return ()

getMove :: Board -> IO String
getMove b = do
    putStrLn "\tInsira sua jogada e tecle enter"
    move <- getLine
    makeMove b (convertMove board move)
    return ""

convertMove :: Board -> String -> ((Char, Int),(Char,Int))
convertMove board (a:b:c:d:e:f) = ((a,(digitToInt b)),(d,(digitToInt e)))

makeMove :: Board -> ((Char, Int),(Char,Int)) -> IO ()
makeMove b ((oc,ol),(dc,dl)) = do 
    let piece = (findPiece b (oc,ol))
    if ((piece == (King True)) || (piece == (Queen True)) || (piece == (Bishop True)) || (piece == (Knight True)) || (piece == (Rook True)) || (piece == (Pawn True))) 
        && (isValidMove b piece (translateLine(oc),ol) (translateLine(dc),dl))
        then do
            putStr (printBoard 8 (movePiece b ((oc,ol),(dc,dl))))
        else print (oc,ol)
    return ()
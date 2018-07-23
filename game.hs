import Board

board = makeBoard

main = do
    putStrLn "Bem vindo ao jogo de xadrez!"
    putStrLn (printBoard board)
    game board

game :: Board -> IO ()
game b = if (isKingInCheckMate b True)
            then blackWin
            else if (isKingInCheckMate b False)
                    then whiteWin
                    else nextWhiteMove b

blackWin :: IO ()
blackWin = putStrLn "O jogador preto ganhou!"

whiteWin :: IO ()
whiteWin = putStrLn "O jogador branco ganhou!"

nextWhiteMove :: Board -> IO ()
nextWhiteMove b = putStrLn "Jogador branco, é sua vez"


    
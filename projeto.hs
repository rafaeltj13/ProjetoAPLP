import System.Random

main :: IO ()
main = do
 mostrarMenu

 opcao <- getLine
 
 print "FIM"

mostrarMenu :: IO ()
mostrarMenu = do
 putStrLn ("------------------------")
 putStrLn ("Projeto APLP - Haskell")
 putStrLn ("------------------------")
 putStrLn ("Escolha uma modo de jogo:")
 putStrLn ("1 - Um jogador")
 putStrLn ("2 - Dois jogadores")
 putStrLn ("3 - Sair")
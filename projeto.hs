import System.Random

main :: IO ()
main = do
 mostrarMenu

 opcaoInput <- getLine

 if(opcaoInput == "1") then umJogador
  else if (opcaoInput == "2") then doisJogadores
   else putStrLn ("Até mais!")

mostrarMenu :: IO ()
mostrarMenu = do
 putStrLn ("------------------------")
 putStrLn ("Projeto APLP - Haskell")
 putStrLn ("------------------------")
 putStrLn ("Escolha um modo de jogo:")
 putStrLn ("1 - Um jogador")
 putStrLn ("2 - Dois jogadores")
 putStrLn ("3 - Sair")

umJogador :: IO ()
umJogador = do
 putStrLn ("TODO - Um Jogador")
 
doisJogadores :: IO ()
doisJogadores = do
 putStrLn ("TODO")

opcoesDeChute :: IO ()
opcoesDeChute = do
 putStrLn ("Selecione um local de chute:")
 putStrLn ("1 - Esquerda Baixo")
 putStrLn ("2 - Esquerda Cima")
 putStrLn ("3 - Meio Baixo")
 putStrLn ("4 - Meio Cima")
 putStrLn ("5 - Direita Baixo")
 putStrLn ("6 - Direita Cima")

resultado :: Int -> Int -> String
resultado placar1 placar2 =
 if(placar1 > placar2) then "Jogador 1 venceu!!!"
  else
    if(placar1 < placar2) then "Jogador2 venceu!!!"
     else "Empate!!!"

chute :: String -> Bool
chute localDoChute =
 if(localDoChute == "teste") then False
  else True
 --where(defesa = random)
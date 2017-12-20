import System.Random as Random
import Output as Imprimir

main :: IO ()
main = do
 mostrarMenu

 opcaoInput <- getLine
 let opcao = read opcaoInput :: Int

 if(opcao == 1) then umJogador
  else if (opcao == 2) then doisJogadores
   else putStrLn ("Até mais!")


--Mostra o menu do programa
mostrarMenu :: IO ()
mostrarMenu = do
 putStrLn ("------------------------")
 putStrLn ("Projeto APLP - Haskell")
 putStrLn ("------------------------")
 putStrLn ("Escolha um modo de jogo:")
 putStrLn ("1 - Um jogador")
 putStrLn ("2 - Dois jogadores")
 putStrLn ("3 - Sair")

 --Modo de jogo para um jogador
umJogador :: IO ()
umJogador = do
 putStrLn ("TODO - Um Jogador")
 
--Modo de jogo para dois jogadores
doisJogadores :: IO ()
doisJogadores = do

 let placar1 = 0
 let placar2 = 0

 --Primeira Rodada
 putStrLn(" ")
 putStrLn("Primeira Rodada!")
 mostrarPlacar2 placar1 placar2

 putStrLn("JOGADOR 1:")
 opcoesDeChute
 localChute11 <- getLine
 let chute11 = read localChute11 :: Int
 let foiGol = chutar chute11
 checarGol(foiGol)

 let aux = placar1
 let placar1 = aux + foiGol

 putStrLn("JOGADOR 2:")
 opcoesDeChute
 localChute21 <- getLine
 let chute21 = read localChute21 :: Int
 let foiGol = chutar chute21
 checarGol(foiGol)
 
 let aux = placar2
 let placar2 = aux + foiGol

 --Segunda Rodada
 putStrLn(" ")
 putStrLn("Segunda Rodada!")
 mostrarPlacar2 placar1 placar2

 putStrLn("JOGADOR 1:")
 opcoesDeChute
 localChute12 <- getLine
 let chute12 = read localChute12 :: Int
 let foiGol = chutar chute12
 checarGol(foiGol)

 let aux = placar1
 let placar1 = aux + foiGol

 putStrLn("JOGADOR 2:")
 opcoesDeChute
 localChute22 <- getLine
 let chute22 = read localChute22 :: Int
 let foiGol = chutar chute22
 checarGol(foiGol)
 
 let aux = placar2
 let placar2 = aux + foiGol

 --Terceira Rodada
 putStrLn(" ")
 putStrLn("Terceira Rodada!")
 mostrarPlacar2 placar1 placar2

 putStrLn("JOGADOR 1:")
 opcoesDeChute
 localChute13 <- getLine
 let chute13 = read localChute13 :: Int
 let foiGol = chutar chute13
 checarGol(foiGol)

 let aux = placar1
 let placar1 = aux + foiGol

 putStrLn("JOGADOR 2:")
 opcoesDeChute
 localChute23 <- getLine
 let chute23 = read localChute23 :: Int
 let foiGol = chutar chute23
 checarGol(foiGol)
 
 let aux = placar2
 let placar2 = aux + foiGol

 mostrarPlacar2 placar1 placar2

 --Quarta Rodada
 putStrLn(" ")
 putStrLn("Quarta Rodada!")
 mostrarPlacar2 placar1 placar2

 putStrLn("JOGADOR 1:")
 opcoesDeChute
 localChute14 <- getLine
 let chute14 = read localChute14 :: Int
 let foiGol = chutar chute14
 checarGol(foiGol)

 let aux = placar1
 let placar1 = aux + foiGol

 putStrLn("JOGADOR 2:")
 opcoesDeChute
 localChute24 <- getLine
 let chute24 = read localChute24 :: Int
 let foiGol = chutar chute24
 checarGol(foiGol)
 
 let aux = placar2
 let placar2 = aux + foiGol

 mostrarPlacar2 placar1 placar2

 --Quinta Rodada
 putStrLn(" ")
 putStrLn("Quinta Rodada!")
 mostrarPlacar2 placar1 placar2

 putStrLn("JOGADOR 1:")
 opcoesDeChute
 localChute15 <- getLine
 let chute15 = read localChute15 :: Int
 let foiGol = chutar chute15
 checarGol(foiGol)

 let aux = placar1
 let placar1 = aux + foiGol

 putStrLn("JOGADOR 2:")
 opcoesDeChute
 localChute25 <- getLine
 let chute25 = read localChute25 :: Int
 let foiGol = chutar chute25
 checarGol(foiGol)
 
 let aux = placar2
 let placar2 = aux + foiGol

 mostrarPlacar2 placar1 placar2

 let resultadoFinal = resultado placar1 placar2
 putStrLn(resultadoFinal)

--Mostra as opções de chute
opcoesDeChute :: IO ()
opcoesDeChute = do
 putStrLn ("Selecione um local de chute:")
 putStrLn ("1 - Esquerda Baixo")
 putStrLn ("2 - Esquerda Cima")
 putStrLn ("3 - Meio Baixo")
 putStrLn ("4 - Meio Cima")
 putStrLn ("5 - Direita Baixo")
 putStrLn ("6 - Direita Cima")

--Mostra o placar da partida de um jogador
mostrarPlacar :: Int -> Int -> IO ()
mostrarPlacar placar1 placar2 = do
  putStrLn(" ")
  putStrLn("Jogador: "++ (show placar1) ++ " x " ++ (show placar2) ++ " :PC")
  putStrLn(" ")

--Mostra o placar da partida entre dois jogadores
mostrarPlacar2 :: Int -> Int -> IO ()
mostrarPlacar2 placar1 placar2 = do
  putStrLn(" ")
  putStrLn("Jogador 1: "++ (show placar1) ++ " x " ++ (show placar2) ++ " :Jogador 2")
  putStrLn(" ")

--Checa se o chute foi gol
checarGol :: Int -> IO ()
checarGol foiGol = do
 if(foiGol == 1) then putStrLn("GOOOOOOOL!!!")
  else putStrLn("ERROOOOU!!!")

--Faz a simulação do chute
chutar :: Int -> Int
chutar localDoChute =
 if(localDoChute == $ puloGoleiro (localDoChute)) then 0
  else 1

--Calcula o vencedor da partida
resultado :: Int -> Int -> String
resultado placar1 placar2 =
 if(placar1 > placar2) then "Jogador 1 venceu!!!"
  else
    if(placar1 < placar2) then "Jogador 2 venceu!!!"
     else "Empate!!!"

puloGoleiro :: Int -> IO()
puloGoleiro localDoChute = do

 if(localDoChute <= 2) then Random.randomRIO(1, 2)
  else
    if(localDoChute <= 4) then Random.randomRIO(3, 4)
     else
      Random.randomRIO(5, 6)
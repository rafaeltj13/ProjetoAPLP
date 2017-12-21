import System.Random as Random

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
  let rodadaInicial = 1
  let placar1 = 0
  let placar2 = 0

  rodadas1jogador rodadaInicial placar1 placar2
 
--Modo de jogo para dois jogadores
doisJogadores :: IO ()
doisJogadores = do

 let rodadaInicial = 1
 let placar1 = 0
 let placar2 = 0

 rodadas2jogadores rodadaInicial placar1 placar2

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

chutar :: Int -> Int -> Int
chutar localDoChute puloGoleiro=
  if(localDoChute == puloGoleiro) then 0
   else 1

--Calcula o vencedor da partida
resultado :: Int -> Int -> String
resultado placar1 placar2 =
 if(placar1 > placar2 ) then "Jogador 1 venceu!!!"
  else
   if(placar1 < placar2) then"Jogador 2 venceu!!!"
    else "Empate!!!"

resultadoUmJogador :: Int -> Int -> String
resultadoUmJogador placar1 placar2 =
  if(placar1 > placar2 ) then "Jogador venceu!!!"
   else
    if(placar1 < placar2) then"PC venceu!!!"
     else "Empate!!!"

rodadas2jogadores :: Int -> Int -> Int -> IO()
rodadas2jogadores 6 placar1 placar2 = do
  mostrarPlacar2 placar1 placar2
  let resultadoFinal = resultado placar1 placar2
  putStrLn(resultadoFinal)

rodadas2jogadores rodadaAtual placar1 placar2 = do
    putStrLn(" ")
    imprimirRodada rodadaAtual
    mostrarPlacar2 placar1 placar2
    putStrLn("JOGADOR 1:")
    opcoesDeChute
    localChute1 <- getLine
    let chute1 = read localChute1 :: Int
    puloGoleiro <- randomRIO(chute1, chute1+1)
    let foiGol = chutar chute1 puloGoleiro
    checarGol(foiGol)

    let novoPlacar1 = placar1 + foiGol

    putStrLn("JOGADOR 2:")
    opcoesDeChute
    localChute2 <- getLine
    let chute2 = read localChute2 :: Int
    puloGoleiro <- randomRIO(chute2, chute2+1)
    let foiGol = chutar chute2 puloGoleiro
    checarGol(foiGol)

    let novoPlacar2 = placar2 + foiGol

    rodadas2jogadores (rodadaAtual+1) novoPlacar1 novoPlacar2

rodadas1jogador :: Int -> Int -> Int -> IO()
rodadas1jogador 6 placar1 placar2 = do
  mostrarPlacar placar1 placar2
  let resultadoFinal = resultadoUmJogador placar1 placar2
  putStrLn(resultadoFinal)

rodadas1jogador rodadaAtual placar1 placar2 = do
  putStrLn(" ")
  imprimirRodada rodadaAtual
  mostrarPlacar placar1 placar2

  putStrLn("JOGADOR:")
  opcoesDeChute
  localChute1 <- getLine
  let chute1 = read localChute1 :: Int
  puloGoleiro <- randomRIO(chute1, chute1+1)
  let foiGol = chutar chute1 puloGoleiro
  checarGol(foiGol)
  let novoPlacar1 = placar1 + foiGol

  putStrLn("PC:")
  chute2 <- randomRIO(1 :: Int, 6 :: Int)
  puloGoleiro <- randomRIO(chute2, chute2+1)
  let foiGol = chutar chute2 puloGoleiro
  checarGol(foiGol)
  let novoPlacar2 = placar2 + foiGol

  rodadas1jogador (rodadaAtual+1) novoPlacar1 novoPlacar2

imprimirRodada :: Int -> IO()
imprimirRodada rodadaAtual
 | rodadaAtual == 1 = putStrLn("Primeira Rodada!")
 | rodadaAtual == 2 = putStrLn("Segunda Rodada!")
 | rodadaAtual == 3 = putStrLn("Terceira Rodada!")
 | rodadaAtual == 4 = putStrLn("Quarta Rodada!")
 | rodadaAtual == 5 = putStrLn("Quinta Rodada!")
 | otherwise = putStrLn("Fim de jogo")
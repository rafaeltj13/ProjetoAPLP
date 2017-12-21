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
  let placar1 = 0
  let placar2 = 0

  --Primeira Rodada
  putStrLn(" ")
  putStrLn("Primeira Rodada!")
  mostrarPlacar placar1 placar2

  putStrLn("JOGADOR:")
  opcoesDeChute
  localChute11 <- getLine
  let chute11 = read localChute11 :: Int

  puloGoleiro <- randomRIO(chute11, chute11+1)
  let foiGol = chutar chute11 puloGoleiro
  checarGol(foiGol)

  let aux = placar1
  let placar1 = aux + foiGol

  putStrLn("PC:")
  opcoesDeChute
  localChute21 <- getLine
  let chute21 = read localChute21 :: Int

  puloGoleiro <- randomRIO(chute21, chute21+1)
  let foiGol = chutar chute21 puloGoleiro
  checarGol(foiGol)
  
  let aux = placar2
  let placar2 = aux + foiGol

  --Segunda Rodada
  putStrLn(" ")
  putStrLn("Segunda Rodada!")
  mostrarPlacar placar1 placar2

  putStrLn("JOGADOR:")
  opcoesDeChute
  localChute12 <- getLine
  let chute12 = read localChute12 :: Int

  puloGoleiro <- randomRIO(chute12, chute11+1)
  let foiGol = chutar chute12 puloGoleiro
  checarGol(foiGol)

  let aux = placar1
  let placar1 = aux + foiGol

  putStrLn("PC:")
  opcoesDeChute
  localChute22 <- getLine
  let chute22 = read localChute22 :: Int

  puloGoleiro <- randomRIO(chute22, chute22+1)
  let foiGol = chutar chute22 puloGoleiro
  checarGol(foiGol)
  
  let aux = placar2
  let placar2 = aux + foiGol

  --Terceira Rodada
  putStrLn(" ")
  putStrLn("Terceira Rodada!")
  mostrarPlacar placar1 placar2

  putStrLn("JOGADOR:")
  opcoesDeChute
  localChute13 <- getLine
  let chute13 = read localChute13 :: Int

  puloGoleiro <- randomRIO(chute13, chute13+1)
  let foiGol = chutar chute13 puloGoleiro
  checarGol(foiGol)

  let aux = placar1
  let placar1 = aux + foiGol

  putStrLn("PC:")
  opcoesDeChute
  localChute23 <- getLine
  let chute23 = read localChute23 :: Int

  puloGoleiro <- randomRIO(chute23, chute23+1)
  let foiGol = chutar chute23 puloGoleiro
  checarGol(foiGol)
  
  let aux = placar2
  let placar2 = aux + foiGol

  --Quarta Rodada
  putStrLn(" ")
  putStrLn("Quarta Rodada!")
  mostrarPlacar placar1 placar2

  putStrLn("JOGADOR:")
  opcoesDeChute
  localChute14 <- getLine
  let chute14 = read localChute14 :: Int

  puloGoleiro <- randomRIO(chute14, chute14+1)
  let foiGol = chutar chute14 puloGoleiro
  checarGol(foiGol)

  let aux = placar1
  let placar1 = aux + foiGol

  putStrLn("PC:")
  opcoesDeChute
  localChute24 <- getLine
  let chute24 = read localChute24 :: Int

  puloGoleiro <- randomRIO(chute24, chute24+1)
  let foiGol = chutar chute24 puloGoleiro
  checarGol(foiGol)
  
  let aux = placar2
  let placar2 = aux + foiGol

  --Quinta Rodada
  putStrLn(" ")
  putStrLn("Quinta Rodada!")
  mostrarPlacar placar1 placar2

  putStrLn("JOGADOR:")
  opcoesDeChute
  localChute15 <- getLine
  let chute15 = read localChute15 :: Int

  puloGoleiro <- randomRIO(chute15, chute15+1)
  let foiGol = chutar chute15 puloGoleiro
  checarGol(foiGol)

  let aux = placar1
  let placar1 = aux + foiGol

  putStrLn("PC:")
  opcoesDeChute
  localChute25 <- getLine
  let chute25 = read localChute25 :: Int

  puloGoleiro <- randomRIO(chute25, chute25+1)
  let foiGol = chutar chute25 puloGoleiro
  checarGol(foiGol)
  
  let aux = placar2
  let placar2 = aux + foiGol

  mostrarPlacar placar1 placar2

  let resultadoFinal = resultadoUmJogador placar1 placar2
  putStrLn(resultadoFinal)
 
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

imprimirRodada :: Int -> IO()
imprimirRodada rodadaAtual
 | rodadaAtual == 1 = putStrLn("Primeira Rodada!")
 | rodadaAtual == 2 = putStrLn("Segunda Rodada!")
 | rodadaAtual == 3 = putStrLn("Terceira Rodada!")
 | rodadaAtual == 4 = putStrLn("Quarta Rodada!")
 | rodadaAtual == 5 = putStrLn("Quinta Rodada!")
 | otherwise = putStrLn("Fim de jogo")
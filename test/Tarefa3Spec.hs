module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import Tarefa3
import LI12425
import Tarefa1Spec (mapa01)


testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [  "Teste atualizaJogo 1" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Oeste 10 1 1 1 [] 1] 2 2 0]] [] mapa01 [Inimigo (1,0) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 jogoInicial,
         "Teste atualizaJogo 2" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Oeste 10 1 1 1 [] 1] 2 1 0]] [] mapa01 [Inimigo (1,1 ) Sul 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Oeste 10 1 1 1 [] 1] 2 2 0]] [] mapa01 [Inimigo (1,0) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 3" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Oeste 10 1 1 1 [] 1] 2 0 0]] [torreFogo{tempoTorre = 1.5}] mapa01 [Inimigo (1,2) Sul 8.25 1 1 1 [projetil01{duracaoProjetil = Finita 9}] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Oeste 10 1 1 1 [] 1] 2 1 0]] [torreFogo] mapa01 [Inimigo (1,1) Sul 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 4" ~: Jogo (Base 19 (5,2) 300) [Portal (0,0) []] [torreFogo{tempoTorre = 0.5}] mapa01 [Inimigo (1,0) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Oeste 10 1 1 1 [] 1] 2 0 0]] [torreFogo{tempoTorre = 1.5}] mapa01 [Inimigo (5,3) Norte 7.5 1 1 1 [projetil01{duracaoProjetil = Finita 9}] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 5" ~: Jogo (Base 20 (5,2) 301) [Portal (0,0) []] [torreFogo{tempoTorre = 1.5}] mapa01 [Inimigo (1,0) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Oeste 10 1 1 1 [] 1] 2 0 0]] [torreFogo] mapa01 [Inimigo (1,1) Sul 1 1 1 1 [projetil01{duracaoProjetil = Finita 9}] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 6" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Este 10 1 1 1 [] 1] 2 2 0]] [] mapa01 [Inimigo (1,0) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 jogoInicial02,
         "Teste atualizaJogo 7" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Este 10 1 1 1 [] 1] 2 1 0]] [] mapa01 [Inimigo (1,1) Sul 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Este 10 1 1 1 [] 1] 2 2 0]] [] mapa01 [Inimigo (1,0) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 8" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Este 10 1 1 1 [] 1] 2 0 0]] [] mapa01 [Inimigo (1,2) Sul 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Este 10 1 1 1 [] 1] 2 1 0]] [] mapa01 [Inimigo (1,1) Sul 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 9" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (1,0) Este 10 1 1 1 [] 1,Inimigo (1,3) Sul 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Este 10 1 1 1 [] 1] 2 0 0]] [] mapa01 [Inimigo (1,2) Sul 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 10" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (1,1) Sul 10 1 1 1 [] 1,Inimigo (1,4) Sul 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (1,0) Este 10 1 1 1 [] 1,Inimigo (1,3) Sul 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 11" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (1,2) Sul 10 1 1 1 [] 1,Inimigo (2,4) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (1,1) Sul 10 1 1 1 [] 1,Inimigo (1,4) Sul 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 12" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (1,3) Sul 10 1 1 1 [] 1,Inimigo (3,4) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (1,2) Sul 10 1 1 1 [] 1,Inimigo (2,4) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 13" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (1,4) Sul 10 1 1 1 [] 1,Inimigo (4,4) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (1,3) Sul 10 1 1 1 [] 1,Inimigo (3,4) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 14" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (2,4) Este 10 1 1 1 [] 1,Inimigo (5,4) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (1,4) Sul 10 1 1 1 [] 1,Inimigo (4,4) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 15" ~: Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (3,4) Este 10 1 1 1 [] 1,Inimigo (5,3) Norte 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (2,4) Este 10 1 1 1 [] 1,Inimigo (5,4) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
         "Teste atualizaJogo 16" ~: Jogo (Base 19 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (4,4) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)] ~=? atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) []] [] mapa01 [Inimigo (3,4) Este 10 1 1 1 [] 1,Inimigo (5,3) Norte 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]),
 
         "Teste atualizaTorre 1" ~: (Torre (1,1) 1 1 1 1 1 projetil01 ,[Inimigo (1,1) Sul 0 1 1 1 [Projetil Fogo (Finita 20)] 1]) ~=? atualizaTorre 1 torre01 inimigos01,  
         "Teste atualizaTorre 2" ~: (torre02{tempoTorre = 0},[inimigo01]) ~=? atualizaTorre 1 torre02 inimigos01,
         "Teste atualizaTorre 3" ~: (Torre (1,1) 1 1 1 1 1 projetil01 ,[Inimigo (1,1) Este 4 1 1 1 [projetil01] 1]) ~=? atualizaTorre 1 torre01 [inimigo02{posicaoInimigo = (1,1)}],

         "Teste atualizaTorres 1" ~: ([Torre (1,1) 1 1 1 1 1 projetil01],[Inimigo (1,1) Sul 0 1 1 1 [Projetil Fogo (Finita 20)] 1,inimigo02]) ~=? atualizaTorres 1 [torre01] [inimigo01,inimigo02],
         "Teste atualizaTorres 2" ~: ([Torre (1,1) 1 1 1 1 1 projetil01,torre02{tempoTorre = 0}],[Inimigo (1,1) Sul 0 1 1 1 [Projetil Fogo (Finita 20)] 1]) ~=? atualizaTorres 1 [torre01,torre02] inimigos01,

         "Teste inimigosVivoseCreditos 1" ~: (inimigos01,0) ~=? inimigosVivoseCreditos inimigos01 0,
         "Teste inimigosVivoseCreditos 2" ~: (inimigos02,0) ~=? inimigosVivoseCreditos inimigos02 0,
         "Teste inimigosVivoseCreditos 3" ~: (inimigos01,1) ~=? inimigosVivoseCreditos (Inimigo (1,1) Norte 0 1 1 1 [] 1:inimigos01) 0,

         "Teste atualizaPosicaoInimigo 1" ~: Inimigo (1,2) Sul 1 1 1 1 [projetil01] 1 ~=? atualizaPosicaoInimigo 1 inimigo01 mapa01,
         "Teste atualizaPosicaoInimigo 2" ~: inimigo02 {posicaoInimigo =(3,4)} ~=? atualizaPosicaoInimigo 1 inimigo02 mapa01,
         "Teste atualizaPosicaoInimigo 3" ~: Inimigo (2,4) Este 0 1 1 1 [] 1 ~=? atualizaPosicaoInimigo 1 (Inimigo (1,4) Sul 0 1 1 1 [] 1) mapa01,
         "Teste atualizaPosicaoInimigo 4" ~: Inimigo (1,0) Este 0 1 1 1 [] 1 ~=? atualizaPosicaoInimigo 1 (Inimigo (0,0) Oeste 0 1 1 1 [] 1) mapa01, 
         "Teste atualizaPosicaoInimigo 4" ~: Inimigo (1,1) Sul 0 1 1 1 [] 1~=? atualizaPosicaoInimigo 1 (Inimigo (1,0) Oeste 0 1 1 1 [] 1) mapa01, 

         "Teste atualizaInimigos 1" ~: [Inimigo (1,2) Sul 0.75 1 1 1 [Projetil Fogo (Finita 9)] 1] ~=? atualizaInimigos 1 inimigos01 mapa01,
         "Teste atualizaInimigos 2" ~: [Inimigo (3,4) Este 5 1 1 1 [] 1] ~=? atualizaInimigos 1 inimigos02 mapa01,
         "Teste atualizaInimigos 3" ~: [Inimigo (1,2) Sul 0.75 1 1 1 [Projetil Fogo (Finita 9)] 1,Inimigo (3,4) Este 5 1 1 1 [] 1,Inimigo (2,4) Este 0 1 1 1 [] 1] ~=? atualizaInimigos 1 (inimigos01++inimigos02++[Inimigo (1,4) Sul 0 1 1 1 [] 1]) mapa01,
         "Teste atualizaInimigos 4" ~: [Inimigo (5,2) Norte 7.25 1 1 1 [projetil01{duracaoProjetil = Finita 8}] 1] ~=? atualizaInimigos 1 [Inimigo (5,3) Norte 7.5 1 1 1 [projetil01{duracaoProjetil = Finita 9}] 1] mapa01,

         "Teste projetilInimigo 1" ~: Inimigo (1,1) Sul 0.75 1 1 1 [Projetil  Fogo (Finita 9)] 1 ~=? projetilInimigo 1 inimigo01,
         "Teste projetilInimigo 2" ~: inimigo02  ~=? projetilInimigo 1 inimigo02,

         "Teste projeteisAtivos 1" ~: [projetil02 {duracaoProjetil = Finita 9}] ~=? projeteisAtivos 1 [projetil02],
         "Teste projeteisAtivos 2" ~: [Projetil Fogo (Finita 9)] ~=? projeteisAtivos 1 [projetil01],

         "Teste menosDuracao 1" ~: Finita 9 ~=? menosDuracao (Finita 10) 1,
         "Teste menosDuracao 2" ~: Infinita ~=? menosDuracao Infinita 1,

         "Teste inimigoBase 1" ~: (Base 10 (0,0) 0, inimigos01) ~=? inimigoBase inimigos01 (Base 10 (0,0) 0),
         "Teste inimigoBase 2" ~: (Base 10 (0,0) 0, inimigos02) ~=? inimigoBase inimigos02 (Base 10 (0,0) 0),
         "Teste inimigoBase 3" ~: (Base 9 (0,0) 0, []) ~=? inimigoBase [inimigo01{posicaoInimigo= (0,0)}] (Base 10 (0,0) 0),
         "Teste inimigoBase 4" ~: (Base 9 (0,0) 0, [inimigo01]) ~=? inimigoBase [inimigo01{posicaoInimigo= (0,0)},inimigo01] (Base 10 (0,0) 0),

         "Teste OndasFinal 1" ~: [] ~=? ondasFinal [Onda [] 0 0 0],
         "Teste OndasFinal 2" ~: [Onda [inimigo01] 1 0 0] ~=? ondasFinal [Onda [inimigo01] 1 0 0],

         "Teste atualizaOnda 1" ~: (Onda [] 1 1 0,[inimigo01,inimigo01]) ~=? atualizaOnda 1 (Onda [inimigo01] 1 0 0) inimigos01,
         "Teste atualizaOnda 2" ~: (Onda [inimigo01] 1 0 0,[]) ~=? atualizaOnda 1 (Onda [inimigo01] 1 1 0) [],

         "Teste atualizaOndas 1" ~: ([],[inimigo01,inimigo01]) ~=? atualizaOndas 1 [Onda [inimigo01] 0 0 0] inimigos01,
         "Teste atualizaOndas 2" ~: ([],inimigos01) ~=? atualizaOndas 1 [Onda [] 1 0 0] inimigos01,
         "Teste atualizaOndas 3" ~: ([Onda [inimigo02] 1 1 0],[inimigo01,inimigo01]) ~=? atualizaOndas 1 [Onda [inimigo01,inimigo02] 1 0 0,Onda [] 1 1 0] inimigos01,

         "Teste atualizaPortal 1" ~: (Portal (1,2) [], [inimigo01,inimigo01]) ~=? atualizaPortal 1 (Portal (1,2) [Onda [inimigo01] 0 0 0]) inimigos01,
         "Teste atualizaPortal 2" ~: (Portal (1,2) [Onda [inimigo02] 1 1 0],[inimigo01]) ~=? atualizaPortal 1 (Portal (1,2) [Onda [inimigo01,inimigo02] 1 0 0]) [],

         "Teste atualizaPortais 1" ~: ([Portal (1,2) [],Portal (2,1) []],[inimigo02,inimigo01,inimigo01]) ~=? atualizaPortais 1 [Portal (1,2) [Onda [inimigo01] 0 0 0],Portal (2,1) [Onda [inimigo02] 1 0 0]] inimigos01,
         "Teste atualizaPortais 2" ~: ([Portal (1,2) [Onda [inimigo02] 1 1 0]],[inimigo01]) ~=? atualizaPortais 1 [Portal (1,2) [Onda [inimigo01,inimigo02] 1 0 0]] [],

         "Teste elementoDaMatriz 1" ~: Just Terra ~=? elementoDaMatriz (1,1) mapa01,
         "Teste elementoDaMatriz 2" ~: Nothing ~=? elementoDaMatriz ((-1),2) mapa01,
         "Teste elementoDaMatriz 3" ~: Just Relva ~=? elementoDaMatriz (5,5) mapa01
      ]


-- | Dados para os Testes

jogoInicial :: Jogo
jogoInicial = Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Oeste 10 1 1 1 [] 1,Inimigo (0,0) Oeste 10 1 1 1 [] 1] 2 0 0]] [] mapa01 [] [(100,torreResina),(150,torreFogo),(200,torreGelo)]

jogoInicial02 :: Jogo
jogoInicial02 =  Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Este 10 1 1 1 [] 1,Inimigo (0,0) Este 10 1 1 1 [] 1] 2 0 0]] [] mapa01 [] [(100,torreResina),(150,torreFogo),(200,torreGelo)]

jogo01 :: Jogo
jogo01 = Jogo (Base 10 (0,0) 0) [Portal (1,2) [Onda [] 0 0 0]] [] mapa01 [] []

torre01 :: Torre
torre01 = Torre (1,1) 1 1 1 1 0 projetil01

torre02 :: Torre
torre02 = Torre (1,1) 1 1 0 1 1 projetil01

inimigo01 :: Inimigo
inimigo01 = Inimigo (1,1) Sul 1 1 1 1 [projetil01] 1

inimigos01 :: [Inimigo]
inimigos01 = [inimigo01]

inimigo02 :: Inimigo
inimigo02 = Inimigo (2,4) Este 5 1 1 1 [] 1

inimigos02 :: [Inimigo]
inimigos02 = [inimigo02]

projetil01 :: Projetil
projetil01 = Projetil Fogo (Finita 10)

projetil02 :: Projetil
projetil02 = Projetil Resina (Finita 10)

projetil03 :: Projetil
projetil03 = Projetil Gelo (Finita 10)

torreFogo :: Torre
torreFogo = Torre (2,1) 1.5 2 2 1.5 0 projetil01

torreResina :: Torre
torreResina = Torre (2,1) 1.5 2 2 1.5 0 projetil02

torreGelo :: Torre
torreGelo = Torre (2,1) 1.5 2 2 1.5 0 projetil03
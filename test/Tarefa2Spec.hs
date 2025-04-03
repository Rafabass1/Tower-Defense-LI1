module Tarefa2Spec (testesTarefa2) where

import Test.HUnit
import Tarefa2
import LI12425
import Tarefa1Spec (mapa01)

-- | Testes

testesTarefa2 :: Test
testesTarefa2 =
    TestLabel "Testes Tarefa 2" $
      test
        [ "Teste inimigosNoAlcance 1" ~: [] ~=? inimigosNoAlcance torre01 [],
          "Teste inimigosNoAlcance 2" ~: [inimigo01] ~=? inimigosNoAlcance torre01 inimigos1e02,
          "Teste inimigosNoAlcance 3" ~: [] ~=? inimigosNoAlcance torre01 [inimigo02],

          "Teste atingeInimigo 1" ~: inimigo01Atingido ~=? atingeInimigo torre01 inimigo01,
          "Teste atingeInimigo 2" ~: inimigo02Atingido ~=? atingeInimigo torre01 inimigo02,

          "Teste terminouJogo 1" ~: True ~=? terminouJogo jogoGanho,
          "Teste terminouJogo 2" ~: True ~=? terminouJogo jogoPerdido,
          "Teste terminouJogo 3" ~: False ~=? terminouJogo jogoEmProgresso ,

          "Teste ganhouJogo 1" ~: True ~=? ganhouJogo jogoGanho,
          "Teste ganhouJogo 2" ~: False ~=? ganhouJogo jogoPerdido,
          "Teste ganhouJogo 3" ~: False ~=? ganhouJogo jogoEmProgresso,

          "Teste perdeuJogo 1" ~: False ~=? perdeuJogo jogoGanho,
          "Teste perdeuJogo 2" ~: True ~=? perdeuJogo jogoPerdido,
          "Teste perdeuJogo 3" ~: False ~=? perdeuJogo jogoEmProgresso,

          "Teste multiplicarDuracao 1" ~: Finita 10 ~=? multiplicarDuracao (Finita 5) 2,
          "Teste multiplicarDuracao 2" ~: Infinita ~=? multiplicarDuracao Infinita 3,

          "Teste somarDuracoes 1" ~: Finita 8 ~=? somarDuracoes (Finita 3) (Finita 5),
          "Teste somarDuracoes 2" ~: Infinita ~=? somarDuracoes Infinita (Finita 5),

          "Teste existeGelo 1" ~: True ~=? existeGelo [Projetil Gelo (Finita 1)],
          "Teste existeGelo 2" ~: False ~=? existeGelo [Projetil Fogo (Finita 1)],

          "Teste existeResina 1" ~: True ~=? existeResina [Projetil Resina (Finita 1)],
          "Teste existeResina 2" ~: False ~=? existeResina [Projetil Gelo (Finita 1)],

          "Teste existeFogo 1" ~: True ~=? existeFogo [Projetil Fogo (Finita 1)],
          "Teste existeFogo 2" ~: False ~=? existeFogo [Projetil Gelo (Finita 1)] ,

          "Teste ativaInimigo 1" ~: (Portal (0,0) [Onda [] 1 1 0], [inimigo01]) ~=? ativaInimigo portal01 [],
          "Teste ativaInimigo 2" ~: (Portal (0,0) [Onda [] 1 1 0], [inimigo02, inimigo01]) ~=? ativaInimigo portal02 [inimigo01] ,

          "Teste removerGelo 1" ~: [Projetil Fogo (Finita 1)] ~=? removerGelo [Projetil Gelo (Finita 1), Projetil Fogo (Finita 1)],
          "Teste removerGelo 2" ~: [] ~=? removerGelo [Projetil Gelo (Finita 1)],

          "Teste removerFogo 1" ~: [Projetil Gelo (Finita 1)] ~=? removerFogo [Projetil Fogo (Finita 1), Projetil Gelo (Finita 1)],
          "Teste removerFogo 2" ~: [] ~=? removerFogo [Projetil Fogo (Finita 1)],

          "Teste removerResina 1" ~: [Projetil Gelo (Finita 1)] ~=? removerResina [Projetil Resina (Finita 1), Projetil Gelo (Finita 1)],
          "Teste removerResina 2" ~: [] ~=? removerResina [Projetil Resina (Finita 1)],

          "Teste fogoProjetil 1" ~: Projetil Fogo (Finita 1) ~=? fogoProjetil [Projetil Fogo (Finita 1), Projetil Gelo (Finita 1)],
          "Teste geloProjetil 1" ~: Projetil Gelo (Finita 1) ~=? geloProjetil [Projetil Fogo (Finita 1), Projetil Gelo (Finita 1)],
          "Teste resinaProjetil 1" ~: Projetil Resina (Finita 1) ~=? resinaProjetil [Projetil Resina (Finita 1), Projetil Gelo (Finita 1)] ,

          "Teste portaisSemInimigos 1" ~: True ~=? portaisSemInimigos [Portal (0,0) []],
          "Teste portaisSemInimigos 2" ~: False ~=? portaisSemInimigos [Portal (0,0) [Onda [inimigo01] 1 0 0]]
        ]

-- | Dados para os Testes

torre01 :: Torre
torre01 = Torre (1,1) 1 1 1 1 1 projetil01

inimigo01 :: Inimigo
inimigo01 = Inimigo (1,1) Norte 1 1 1 1 [] 1

inimigo01Atingido :: Inimigo
inimigo01Atingido = Inimigo (1,1) Norte 0 1 1 1 [projetil01] 1

inimigos1e02 :: [Inimigo]
inimigos1e02 = [inimigo01, inimigo02]

inimigo02 :: Inimigo
inimigo02 = Inimigo (3,3) Norte 1 1 1 1 [] 1

portal01 :: Portal
portal01 = Portal (0,0) [Onda [inimigo01] 1 0 0]

portal02 :: Portal
portal02 = Portal (0,0) [Onda [inimigo02] 1 0 0]

inimigo02Atingido :: Inimigo
inimigo02Atingido = Inimigo (3,3) Norte 0 1 1 1 [projetil01] 1

projetil01 :: Projetil
projetil01 = Projetil Fogo (Finita 1)

jogoGanho :: Jogo
jogoGanho = Jogo (Base 10 (0,0) 0) [Portal (1,2) [Onda [] 0 0 0]] [] mapa01 [] []

jogoPerdido :: Jogo
jogoPerdido = Jogo (Base 0 (1,0) 2) [Portal (0,0) [Onda [inimigo01] 1 0 0]] [] mapa01 [inimigo01] []

jogoEmProgresso :: Jogo
jogoEmProgresso = Jogo (Base 15 (1,0) 2) [Portal (0,0) [Onda [inimigo01] 1 0 0]] [] mapa01 [inimigo01] []
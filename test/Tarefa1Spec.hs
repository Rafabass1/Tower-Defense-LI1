module Tarefa1Spec where

import Test.HUnit
import Tarefa1
import LI12425

-- | Dados para os testes
jogoValido :: Jogo
jogoValido = Jogo base1 [portal1] [torre1] mapa01 [] []

jogoSemPortal :: Jogo
jogoSemPortal = Jogo base1 [] [torre1] mapa01 [] []

portal1 :: Portal
portal1 = Portal (1, 1) [onda1]

torre1 :: Torre
torre1 = Torre (2, 2) 1 1 1 1 1 (Projetil Fogo (Finita 1))

base1 :: Base
base1 = Base 100 (0, 0) 0

onda1 :: Onda
onda1 = Onda [] 0 0 0

mapa01 :: Mapa
mapa01 =  [ 
          [t, t, r, a, a, a],
          [r, t, r, a, r, r],
          [r, t, r, a, r, t],
          [r, t, r, a, r, t],
          [r, t, t, t, t, t],
          [a, a, a, a, r, r]
          ]
  where
    t = Terra
    r = Relva
    a = Agua

-- | Testes

testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ "existePortal - jogo com portal" ~: existePortal jogoValido ~?= True ,
        "existePortal - jogo sem portal" ~: existePortal jogoSemPortal ~?= False ,

        "portaisSobreTerra - portal sobre terra" ~: portaisSobreTerra jogoValido ~?= True ,
        "portaisSobreTerra - portal não sobre terra" ~: portaisSobreTerra (Jogo base1 [Portal (2, 0) [onda1]] [torre1] mapa01 [] []) ~?= False ,

        "portaisNaoSobrepostos - portal não sobreposto" ~: portaisNaoSobrepostos jogoValido ~?= True ,
        "portaisNaoSobrepostos - portal sobreposto a torre" ~: portaisNaoSobrepostos (Jogo base1 [Portal (2, 2) [onda1]] [torre1] mapa01 [] []) ~?= False ,
        "portaisNaoSobrepostos - portal sobreposto a base" ~: portaisNaoSobrepostos (Jogo base1 [Portal (0, 0) [onda1]] [torre1] mapa01 [] []) ~?= False ,

        "ondasAtivasValidas - uma onda ativa" ~: ondasAtivasValidas (Jogo base1 [Portal (1, 1) [Onda [] 0 0 0]] [torre1] mapa01 [] []) ~?= True ,
       
        "inimigosPorLancarValidos - inimigos válidos" ~: inimigosPorLancarValidos jogoValido ~?= True ,
        "inimigosPorLancarValidos - inimigos inválidos" ~: inimigosPorLancarValidos (Jogo base1 [portal1 {ondasPortal = [onda1 {inimigosOnda = [Inimigo (0, 0) Norte  0 0 0 0 [] 1]}]}] [torre1] mapa01 [] []) ~?= False ,

        "inimigosEmJogoValidos - inimigos válidos" ~: inimigosEmJogoValidos jogoValido ~?= True ,
        "inimigosEmJogoValidos - inimigos inválidos" ~: inimigosEmJogoValidos (Jogo base1 [portal1] [torre1] mapa01 [Inimigo (2, 2) Sul 1 0 0 (-1) [] 1] []) ~?= False ,

        "projeteisNormalizados - projéteis normalizados" ~: projeteisNormalizados [Projetil Fogo (Finita 1), Projetil Gelo (Finita 1)] ~?= True ,
        "projeteisNormalizados - projéteis não normalizados" ~: projeteisNormalizados [Projetil Fogo (Finita 1), Projetil Fogo (Finita 1)] ~?= False,

        "torresSobreRelva - torre sobre relva" ~: torresSobreRelva jogoValido ~?= True ,
        "torresSobreRelva - torre não sobre relva" ~: torresSobreRelva (Jogo base1 [portal1] [Torre (0, 0) 1 1 1 1 1 (Projetil Fogo (Finita 1))] mapa01 [] []) ~?= False ,

        "alcancePositivo - alcance positivo" ~: alcancePositivo jogoValido ~?= True ,
        "alcancePositivo - alcance não positivo" ~: alcancePositivo (Jogo base1 [portal1] [Torre (2, 2) 0 0 1 1 1 (Projetil Fogo (Finita 1))] mapa01 [Inimigo (20,20) Norte 15 15 5 5 [] 1] []) ~?= False ,

        "rajadaPositiva - rajada positiva" ~: rajadaPositiva jogoValido ~?= True ,
        "rajadaPositiva - rajada não positiva" ~: rajadaPositiva (Jogo base1 [portal1] [Torre (2, 2) 1 0 (-1) 1 1 (Projetil Fogo (Finita 1))] mapa01 [] []) ~?= False ,

        "cicloNaoNegativo - ciclo não negativo" ~: cicloNaoNegativo jogoValido ~?= True ,
        "cicloNaoNegativo - ciclo negativo" ~: cicloNaoNegativo (Jogo base1 [portal1] [Torre (2, 2) 1 1 1 (-1) 1 (Projetil Fogo (Finita 1))] mapa01 [] []) ~?= False ,

        "torresNaoSobrepostas - torres não sobrepostas" ~: torresNaoSobrepostas jogoValido ~?= True ,
        "torresNaoSobrepostas - torres sobrepostas" ~: torresNaoSobrepostas (Jogo base1 [portal1] [torre1, Torre (2, 2) 1 1 1 1 1 (Projetil Fogo (Finita 1))] mapa01 [] []) ~?= False ,

        "baseSobreTerra - base sobre terra" ~: baseSobreTerra jogoValido ~?= True ,
        "baseSobreTerra - base não sobre terra" ~: baseSobreTerra (Jogo (Base 100 (2, 0) 0) [portal1] [torre1] mapa01 [] []) ~?= False ,

        "baseCreditoPositivo - crédito positivo" ~: baseCreditoPositivo jogoValido ~?= True ,
        "baseCreditoPositivo - crédito negativo" ~: baseCreditoPositivo (Jogo (Base 1 (0, 0) (-5)) [portal1] [torre1] mapa01 [] []) ~?= False ,

        "baseNaoSobreposta - base não sobreposta" ~: baseNaoSobreposta jogoValido ~?= True ,
        "baseNaoSobreposta - base sobreposta a torre" ~: baseNaoSobreposta (Jogo (Base 100 (2, 2) 0) [portal1] [torre1] mapa01 [] []) ~?= False ,
        "baseNaoSobreposta - base sobreposta a portal" ~: baseNaoSobreposta (Jogo (Base 100 (1, 1) 0) [portal1] [torre1] mapa01 [] []) ~?= False ,

        "validaJogo - jogo válido" ~: validaJogo jogoValido ~?= True ,
        "validaJogo - jogo inválido" ~: validaJogo jogoSemPortal ~?= False ,

        "posicaoSobreTerra - posição sobre terra" ~: posicaoSobreTerra (1, 1) mapa01 ~?= True ,
        "posicaoSobreTerra - posição não sobre terra" ~: posicaoSobreTerra (0, 1) mapa01 ~?= False ,

        "posicaoSobreRelva - posição sobre relva" ~: posicaoSobreRelva (2, 0) mapa01 ~?= True ,
        "posicaoSobreRelva - posição não sobre relva" ~: posicaoSobreRelva (0, 0) mapa01 ~?= False ,

        "existeCaminho - caminho existe" ~: existeCaminho (0, 0) (1, 4) mapa01 ~?= True ,
        "existeCaminho - caminho não existe" ~: existeCaminho (0, 0) (5, 0) mapa01 ~?= False 
      ]
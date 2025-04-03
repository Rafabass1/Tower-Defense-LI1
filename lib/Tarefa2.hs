{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Nuno Henrique Dias Pereira <a110067@alunos.uminho.pt>
              Rafael Figueiras Esteves <a112032@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425

{-| Calcula os  inimigos ao  alcance de uma dada torre. 

==__Exemplos:__
>>> inimigosNoAlcance (Torre (2,1) 6.5 3 1 2 0 (Projetil Fogo (Finita 5)) [Inimigo (1,1) Este 10 1 1 15 [] 1]
[Inimigo (1,1) Este 10 1 1 15 [] 1]
>>> inimigosNoAlcance (Torre (2,1) 6.5 3 1 2 0 (Projetil Fogo (Finita 5)) [Inimigo (1,1) Este 10 1 1 15 [] 1,Inimigo (6,6) Este 10 1 1 15 [] 1]
[Inimigo (1,1) Este 10 1 1 15 [] 1]
 -}
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance _ [] = []
inimigosNoAlcance  t@(Torre pt _ at _ _ _ _) (i:is)
    | distancia pt (posicaoInimigo i) <= at  = i : inimigosNoAlcance t is
    | otherwise = inimigosNoAlcance t is

{-| Função que atualiza o estado de um inimigo assumindo que este acaba de ser atingido por um projétil de uma torre.

==__Exemplos:__
>>> atingeInimigo (Inimigo (1,1) Este 10 1 1 15 [] 1) (Torre (2,1) 6.5 3 1 2 0 (Projetil Fogo (Finita 5))
Inimigo (1,1) Este 3.5 1 1 15 [Projetil Fogo (Finita 5)] 1
>>> atingeInimigo (Inimigo (1,1) Este 10 1 1 15 [Projetil Fogo (Finita 5)] 1) (Torre (2,1) 6.5 3 1 2 0 (Projetil Gelo (Finita 5))
Inimigo (1,1) Este 3.5 1 1 15 [] 1

==__Propriedades:__
prop> Fogo e Gelo cancelam-se mutuamente.
prop> Fogo e Resina dobram a duração do projétil de Fogo.
prop> Projéteis iguais somam as suas durações.
-}
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo (Torre _ dt _ _ _ _ (Projetil tipo dur)) (Inimigo posI dI viI velI aI buI  projetI vO)
  |tipo ==  Fogo  =                                                                           -- ao ser atingido por fogo
    if existeGelo projetI then Inimigo posI dI (viI-dt) velI aI buI  (removerGelo projetI) vO      -- caso esteja sobre o efeito de gelo entao irao se cancelar
    else if existeResina projetI then Inimigo posI dI (viI-dt) velI aI buI  (Projetil Fogo (multiplicarDuracao dur 2):removerResina projetI)vO -- caso esteja sobre o efeito de resina entao ira duplicar a duracao do projetil de fogo (caso seja resina atingida por fogo ira ficar so com fogo)
    else if existeFogo projetI then let dpi = duracaoProjetil (fogoProjetil projetI)  -- caso esteja sobre o efeito de fogo entao a duracao do projetil anterior ira se sonar com a duracao do projetil disparado e ira dar a duracao do novo projetil e remover o projetil anterior de fogo para n esrar sobre o efeito de dois projeteis ao mesmo trempo
                                     in Inimigo posI dI (viI-dt) velI aI buI  (Projetil Fogo (somarDuracoes dpi dur):removerFogo projetI) vO
    else Inimigo posI dI (viI-dt) velI aI buI  [Projetil tipo dur] vO  -- caso n esteja sobre o efeito de nada (lista vazia) ira passar a estar sobre o efeito do projetil de fogo
  |tipo == Gelo =                                                         -- ao ser atingido por gelo
    if existeFogo projetI then Inimigo posI dI (viI-dt) velI aI buI  (removerFogo projetI) vO-- caso esteja sobre o efeito de fogo entao os efeitos irao se cancelar removendo o efeito de fogo
    else if existeGelo projetI then  let dpi = duracaoProjetil (geloProjetil projetI)  -- caso esteja sobre o efeito de gelo entao o a duracao do projetil novo sera a soma do projetil disparado com o projetil antigo e ira remover o projetil antigo para n estar sobre o efeito de dois ao mesmo trempo
                                     in Inimigo posI dI (viI-dt) velI aI buI  (Projetil Gelo (somarDuracoes dpi dur):(removerGelo projetI)) vO
    else if existeResina projetI then Inimigo posI dI (viI-dt) velI aI buI  ((Projetil tipo dur):projetI) vO -- caso esteja sobre o efeito de resina entao para chegar aqui significa q nem tem gelo nem fogo logo so esta sobre o efeito de resina logo ira ficar sobre o efeito de ambos gelo e resina
       else Inimigo posI dI (viI-dt) velI aI buI  [Projetil tipo dur] vO  -- caso n esteja sobre o efeito de nada fica sobre o efeito de gelo
  |tipo == Resina =                                                                  -- ao ser atingido por resina
    if existeResina projetI then let dpi = duracaoProjetil (resinaProjetil projetI)   -- caso esteja sobre o efeito de resina entao a soma da duracao do projetil antigo e do projetil disparado ira resultar na duracao do novo projetil e ira remover o projetil antigo para n estar sobre o efeito de dois ao mesmo tempo
                                 in Inimigo posI dI (viI-dt) velI aI buI  (Projetil Resina (somarDuracoes dpi dur):removerResina projetI) vO
    else if existeGelo projetI then Inimigo posI dI (viI-dt) velI aI buI  ((Projetil tipo dur):projetI) vO-- caso esteja sobre o efeito de gelo entao ira ficar ao mesmo tempo sobre efeito de gelo e resina
    else if existeFogo projetI then let dpi = duracaoProjetil (fogoProjetil projetI)  -- caso esteja sobre o efeito de fogo entao para chegar a esta fase significa q nem tem resina nem gelo logo ira resultar em ficar so sobre efeito de fogo em que a duracao do projetil novo é o dobro da duracao do projetil de fogo antigo  como pedido no enunciado
                                    in Inimigo posI dI (viI-dt) velI aI buI  (Projetil Fogo (multiplicarDuracao dpi 2):removerFogo projetI) vO
     else Inimigo posI dI (viI-dt) velI aI buI  [Projetil tipo dur] vO -- caso n esteja sobre o efeito de nada passa a estar sobre o efeito de resina

{- | Dado um Portal e a lista de inimigos atualmente em jogo, move o próximo
     inimigo a ser lançado pelo portal para a lista de inimigos ativos.
  
==__Exemplos:__
>>> ativaInimigo (Portal (0,0) [Onda [Inimigo (0,0) Este 10 1 1 15 [] 1] 2 0 0]) []
(Portal (0,0) [Onda [] 2 2 0],[Inimigo (0,0) Este 10 1 1 15 [] 1])
>>>ativaInimigo (Portal (0,0) [Onda [Inimigo (0,0) Este 10 1 1 15 [] 1] 2 2 0]) [] 
(Portal (0,0) [Onda [Inimigo (0,0) Este 10 1 1 15 [] 1] 2 2 0],[])-}
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo (Portal posP ((Onda (io:ios) co ton eo):os) ) i = if ton /= 0 then (Portal posP ((Onda (io:ios) co ton eo):os) , i) else (Portal posP ((Onda ios co co eo):os) , io:i)
     
{-| Verifica se o jogo terminou (se o jogador ganhou ou perdeu). 

==__Exemplos:__
>>> terminouJogo (Jogo (Base 10 (0,0) 0) [Portal (1,2) [Onda [] 0 0 0]] [] mapa01 [] [])
True
>>> terminouJogo Jogo (Base 10 (0,0) 0) [Portal (1,2) [Onda [] 0 0 0]] [] mapa01 [Inimigo (1,1) Este 10 1 1 15 [] 1] []
False 
>>> terminouJogo Jogo (Base 0 (0,0) 0) [Portal (1,2) [Onda [] 0 0 0]] [] mapa01 [Inimigo (1,1) Este 10 1 1 15 [] 1] []
True -}
terminouJogo :: Jogo -> Bool
terminouJogo j = ganhouJogo j || perdeuJogo j

{-| Verifica se o jogador ganhou o jogo. 

==__Exemplos:__
>>> ganhouJogo (Jogo (Base 10 (0,0) 0) [Portal (1,2) [Onda [] 0 0 0]] [] mapa01 [] [])
True 
>>> ganhouJogo Jogo (Base 10 (0,0) 0) [Portal (1,2) [Onda [] 0 0 0]] [] mapa01 [Inimigo (1,1) Este 10 1 1 15 [] 1] []
False -}
ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = null (inimigosJogo jogo) && portaisSemInimigos (portaisJogo jogo) && vidaBase (baseJogo jogo) > 0

{-| Verifica se o jogador perdeu o jogo. 

==__Exemplos:__
>>> perdeuuJogo (Jogo (Base 10 (0,0) 0) [Portal (1,2) [Onda [] 0 0 0]] [] mapa01 [] [])
False
>>> perdeuJogo Jogo (Base 0 (0,0) 0) [Portal (1,2) [Onda [] 0 0 0]] [] mapa01 [Inimigo (1,1) Este 10 1 1 15 [] 1] []
True -}
perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vidaBase (baseJogo jogo) <= 0

-- Funções auxiliares: 

-- | Multiplica a duração por um numero inteiro.
multiplicarDuracao :: Duracao -> Int -> Duracao
multiplicarDuracao (Finita d) i = Finita (d * fromIntegral i)
multiplicarDuracao Infinita _ =   Infinita

-- | Soma duas durações.
somarDuracoes :: Duracao -> Duracao -> Duracao
somarDuracoes Infinita _ = Infinita
somarDuracoes _ Infinita = Infinita
somarDuracoes (Finita d1) (Finita d2) = Finita (d1+d2)

-- | Verifica se existe um projetil de gelo numa lista de projeteis.
existeGelo :: [Projetil] -> Bool
existeGelo [] = False
existeGelo lista = any (\(Projetil tipo _) -> tipo == Gelo) lista

-- | Verifica se existe um projetil de resina numa lista de projeteis.
existeResina :: [Projetil] -> Bool
existeResina lista = any (\(Projetil tipo _) -> tipo == Resina) lista

-- | Verifica se existe um projetil de fogo numa lista de projeteis.
existeFogo :: [Projetil] -> Bool
existeFogo lista =  any (\(Projetil tipo _) -> tipo == Fogo) lista

-- | Remove um projetil de gelo da lista de projeteis.
removerGelo :: [Projetil] -> [Projetil]
removerGelo [] = []
removerGelo ((Projetil t d):xs) = if t == Gelo then xs else Projetil t d : removerGelo xs

-- | Remove um projetil de fogo da lista de projeteis.
removerFogo :: [Projetil] -> [Projetil]
removerFogo [] = []
removerFogo ((Projetil t d):xs) = if t == Fogo then xs else (Projetil t d) : removerFogo xs

-- | Remove um projetil de resina da lista de projeteis.
removerResina :: [Projetil] -> [Projetil]
removerResina [] = []
removerResina ((Projetil t d):xs) = if t == Resina then xs else (Projetil t d)  : removerResina xs

-- | Procura um projetil de fogo numa lista em que exista um projetil de fogo.
fogoProjetil :: [Projetil] -> Projetil
fogoProjetil ((Projetil t d):xs) | existeFogo ((Projetil t d):xs) =  if t == Fogo then (Projetil t d) else fogoProjetil xs

-- | Procura um projetil de gelo numa lista em que exista um projetil de fogo.
geloProjetil :: [Projetil] -> Projetil
geloProjetil ((Projetil t d):xs) | existeGelo ((Projetil t d):xs) =  if t == Gelo then (Projetil t d) else geloProjetil xs

-- | Procura um projetil de resina numa lista em que exista um projetil de fogo.
resinaProjetil :: [Projetil] -> Projetil
resinaProjetil ((Projetil t d):xs) | existeResina ((Projetil t d):xs) =  if t == Resina then (Projetil t d) else resinaProjetil xs

-- | Verifica se os portais estao sem inimigos.
portaisSemInimigos :: [Portal] -> Bool
portaisSemInimigos [] = True
portaisSemInimigos ((Portal _ o):ps) =  ondasSemInimigos o && portaisSemInimigos ps

-- | Verifica se as ondas estao sem inimigos.
ondasSemInimigos :: [Onda] -> Bool
ondasSemInimigos [] = True
ondasSemInimigos ((Onda i _ _ _):os) = null i && ondasSemInimigos os

-- | Calcula a distancia entre duas posições.
distancia :: Posicao -> Posicao -> Float
distancia (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)
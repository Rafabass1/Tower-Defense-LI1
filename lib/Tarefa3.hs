{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Nuno Henrique Dias Pereira <a110067@alunos.uminho.pt>
              Rafael Figueiras Esteves <a112032@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where
import LI12425
import Tarefa2

{-| Atualiza o estado do jogo conforme o tempo passado. 

==__Exemplos:__
>>> atualizaJogo 1 (Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Oeste 10 1 1 1 [] 1] 2 2 0]] [] [[t,t],[r,t]] [Inimigo (1,0) Este 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)])
Jogo (Base 20 (5,2) 300) [Portal (0,0) [Onda [Inimigo (0,0) Oeste 10 1 1 1 [] 1] 2 1 0]] [] [[Terra,Terra],[Relva,Terra]] [Inimigo (1,1 ) Sul 10 1 1 1 [] 1] [(100,torreResina),(150,torreFogo),(200,torreGelo)]-}
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo delta jogo = jogo {
    portaisJogo = novosPortais,
    inimigosJogo = novosInimigos,
    torresJogo = novasTorres,
    baseJogo = novaBase
  }
  where novasTorres = fst $ atualizaTorres delta (torresJogo jogo) novosInimigos
        novaBase = base2 {vidaBase = vidaBase base2, posicaoBase = posicaoBase base2, creditosBase = snd $ inimigosVivoseCreditos inimigosLista3 (creditosBase base2)}
        base2 = fst (inimigoBase inimigosLista3 (baseJogo jogo))
        novosPortais = fst (atualizaPortais delta (portaisJogo jogo) novosInimigos)
        inimigosLista1 = snd $ atualizaPortais delta (portaisJogo jogo) (inimigosJogo jogo)
        inimigosLista2 = snd $  atualizaTorres delta (torresJogo jogo) inimigosLista1
        inimigosLista3 = atualizaInimigos delta inimigosLista2 (mapaJogo jogo)
        novosInimigos = fst $ inimigosVivoseCreditos ( snd (inimigoBase inimigosLista3 (baseJogo jogo))) (creditosBase (baseJogo jogo))

-- TORRES
-- | Atualiza uma unica torre e os inimigos ativos no jogo.
atualizaTorre :: Tempo -> Torre -> [Inimigo] -> (Torre, [Inimigo])
atualizaTorre delta torre inimigos
  | tempoTorre torre <= 0 =
      let
        inimigosAlcance = inimigosNoAlcance torre inimigos
        atingidos = take (rajadaTorre torre) inimigosAlcance
        inimigosAtualizados = map (\inimigo ->
          if inimigo `elem` atingidos
          then atingeInimigo torre inimigo
          else inimigo
         ) inimigos
        torreAtualizada = torre { tempoTorre = cicloTorre torre }
      in
        (torreAtualizada,  inimigosAtualizados)
  | otherwise =
      (torre { tempoTorre = tempoTorre torre - delta }, inimigos)

{-| Atualiza todas as torres do jogo e os inimigos ativos no jogo. 

==__Exemplos:__
>>> atualizaTorres 1 [Torre (1,1) 1 1 1 1 0 (Projetil Fogo (Finita 10))] [Inimigo (1,1) Sul 1 1 1 1 [Projetil Fogo (Finita 10)] 1]
([Torre (1,1) 1 1 1 1 1 (Projetil Fogo (Finita 10))],[Inimigo (1,1) Sul 0 1 1 1 [Projetil Fogo (Finita 20)] 1]) -}
atualizaTorres :: Tempo -> [Torre] -> [Inimigo] -> ([Torre],[Inimigo])
atualizaTorres _ [] inimigos = ([], inimigos)
atualizaTorres delta (torre:torres) inimigos =
  let
    (torreAtualizada, inimigosAtualizados) = atualizaTorre delta torre inimigos
    (outrasTorresAtualizadas, inimigosFinal) = atualizaTorres delta torres inimigosAtualizados
  in
    (torreAtualizada : outrasTorresAtualizadas,   inimigosFinal)

-- INIMIGOS
-- | Retira os inimigos mortos da lista de inimigos.
inimigosVivoseCreditos :: [Inimigo] -> Creditos -> ([Inimigo],Creditos)
inimigosVivoseCreditos [] c = ([],c)
inimigosVivoseCreditos (i:is) c
 | vidaInimigo i <= 0 = inimigosVivoseCreditos is (c + butimInimigo i)
 | otherwise = let
                  (inimigos',creatualizados) = inimigosVivoseCreditos is c
                  in (i:inimigos',creatualizados)

-- | Atualiza a posição de um inimigo com base na sua direção e velocidade.
atualizaPosicaoInimigo :: Tempo -> Inimigo -> Mapa -> Inimigo
atualizaPosicaoInimigo delta inimigo m = inimigo { posicaoInimigo = novaPosicao } {direcaoInimigo = novaDirecao}
  where
    (x, y) = posicaoInimigo inimigo
    (novaPosicao,novaDirecao) = case direcaoInimigo inimigo of
      Norte ->  if elementoDaMatriz (x, y - 1) m ==  Just Terra then ((x, y - velocidadeInimigo inimigo * delta),Norte)
                else if elementoDaMatriz (x + 1, y) m ==  Just Terra then ((x + velocidadeInimigo inimigo * delta,y),Este)
                 else if  elementoDaMatriz (x - 1,y) m ==  Just Terra then ((x - velocidadeInimigo inimigo * delta,y),Oeste)
                   else ((x,y + velocidadeInimigo inimigo * delta),Sul)
      Sul   ->  if elementoDaMatriz (x, y + 1) m ==  Just Terra then ((x, y + velocidadeInimigo inimigo * delta),Sul)
                else if elementoDaMatriz (x + 1, y) m ==  Just Terra then ((x + velocidadeInimigo inimigo * delta,y),Este)
                 else if  elementoDaMatriz (x - 1,y) m ==  Just Terra then ((x - velocidadeInimigo inimigo * delta,y),Oeste)
                   else ((x,y - velocidadeInimigo inimigo * delta),Norte)
      Este  -> if elementoDaMatriz (x, y + 1 ) m ==  Just Terra then ((x, y + velocidadeInimigo inimigo * delta),Sul)
                else if elementoDaMatriz (x + 1, y) m ==  Just Terra then ((x + velocidadeInimigo inimigo * delta,y),Este)
                 else if  elementoDaMatriz (x,y - 1) m ==  Just Terra then ((x,y - velocidadeInimigo inimigo * delta),Norte)
                  else ((x-  velocidadeInimigo inimigo * delta,y),Oeste)
      Oeste -> if elementoDaMatriz (x, y + 1) m ==  Just Terra then ((x, y + velocidadeInimigo inimigo * delta),Sul)
                else if  elementoDaMatriz (x - 1,y) m ==  Just Terra then ((x - velocidadeInimigo inimigo * delta,y),Oeste)
                 else if elementoDaMatriz (x,y -1) m ==  Just Terra then ((x,y - velocidadeInimigo inimigo * delta),Norte)
                  else ((x+ velocidadeInimigo inimigo * delta,y),Este)

{-| Atualiza a lista de inimigos no jogo. 

==__Exemplos:__
>>> atualizaInimigos 1 [Inimigo (1,0) Este 1 1 1 1 [Projetil Fogo (Finita 10)] 1] [[Terra,Terra],[Relva,Terra]]
[Inimigo (1,1) Sul 0.75 1 1 1 [Projetil Fogo (Finita 9)] 1] -}
atualizaInimigos :: Tempo -> [Inimigo] -> Mapa -> [Inimigo]
atualizaInimigos delta inimigos mapa =  map (\inimigo -> atualizaPosicaoInimigo delta (projetilInimigo delta inimigo) mapa) inimigos

-- | Aplica ao inimigo os efeitos dos projeteis em que nele estao ativos.
projetilInimigo :: Tempo -> Inimigo -> Inimigo
projetilInimigo delta i
 | existeGelo projet = i {velocidadeInimigo = 0, projeteisInimigo = projet }
 | existeResina projet = i {velocidadeInimigo = velocidadeOriginal i * 0.6, projeteisInimigo = projet}
 | existeFogo projet = i {vidaInimigo = vidaInimigo i - 0.25*delta, projeteisInimigo = projet}
 | otherwise = i {velocidadeInimigo = velocidadeOriginal i, projeteisInimigo = projet}
  where projet = projeteisAtivos delta (projeteisInimigo i)

-- | Exclui da lista de uma lista de projeteis os projeteis que ja acabaram a sua duracao.
projeteisAtivos :: Tempo -> [Projetil] -> [Projetil]
projeteisAtivos _ [] = []
projeteisAtivos delta (p:ps)
 | duracaoProjetil p == Infinita = p : projeteisAtivos delta ps
 | duracaoProjetil p <= Finita 0 =  projeteisAtivos delta ps
 | otherwise = p {duracaoProjetil = menosDuracao (duracaoProjetil p) delta}  : projeteisAtivos delta ps

-- | Recebe uma duração e um tempo e devolve a duração menos o tempo.
menosDuracao :: Duracao -> Tempo -> Duracao
menosDuracao Infinita _ = Infinita
menosDuracao (Finita x) delta = Finita (x - delta)

-- | Recebe a lista de inimigos ativos e a base e devolve a um tuplo da base retirando a vida consuante se tem inimigos na mesma posicao e a lista de inimigos que estao numa posicao diferente da base.
inimigoBase :: [Inimigo] -> Base -> (Base,[Inimigo])
inimigoBase i b = (baseNova i b, inimigosNovos)
  where inimigosNovos = inimigosFiltrados i (posicaoBase b)

-- | Recebe a lista de inimigos ativos e a base e devolve a base retirando a vida consuante se tem inimigos na mesma posicao.
baseNova :: [Inimigo] -> Base -> Base
baseNova [] ba = ba
baseNova (i:is) ba 
    | (x,y) == posicaoBase ba = baseNova is (ba {vidaBase = vidaBase ba - ataqueInimigo i})
    | otherwise = baseNova is ba
      where (x,y) = (fromIntegral $ truncate a ,fromIntegral $ truncate b)
            (a,b) = posicaoInimigo i

-- | Recebe a lista de inimigos ativos e uma posicao e devolve a lista de inimigos que nao estao nessa posicao.       
inimigosFiltrados :: [Inimigo] -> Posicao -> [Inimigo]         
inimigosFiltrados [] _ = []
inimigosFiltrados (i:is) p
 | (a,b) /=  p = i : inimigosFiltrados is p
 | otherwise = inimigosFiltrados is p
  where (a,b) = (fromIntegral $ truncate x ,fromIntegral $ truncate y)
        (x,y) = posicaoInimigo i
        
-- PORTAIS:
-- | Mantem so as ondas que ainda tem inimigos.
ondasFinal :: [Onda] -> [Onda]
ondasFinal [] = []
ondasFinal (o:os) = if null (inimigosOnda o) then ondasFinal os else o : ondasFinal os

-- | Atualiza uma onde conforme o seu tempo de entrada e de lançamento.
atualizaOnda :: Tempo -> Onda -> [Inimigo] -> (Onda,[Inimigo])
atualizaOnda delta onda inimigos
  | null (inimigosOnda onda) = (onda, inimigos)
  | entradaOnda onda > 0 = (onda { entradaOnda = entradaOnda onda - delta }, inimigos)
  | tempoOnda onda > 0 = (onda { tempoOnda = tempoOnda onda - delta }, inimigos)
  | otherwise =
     let
       inimigos' = head (inimigosOnda onda) :  inimigos
       onda' = onda {inimigosOnda = tail (inimigosOnda onda), tempoOnda = cicloOnda onda}
       in (onda', inimigos')

{-| Atualiza todas as ondas do jogo e os inimigos ativos no jogo. 

==__Exemplos:__
>>> atualizaOndas 1 [Onda [Inimigo (1,1) Sul 1 1 1 1 [] 1] 0 0 0] []
([],[Inimigo (1,1) Sul 1 1 1 1 [] 1])-}
atualizaOndas :: Tempo -> [Onda] -> [Inimigo] -> ([Onda],[Inimigo])
atualizaOndas _ [] inimigos = ([], inimigos)
atualizaOndas delta (o:os) inimigos =
  let
    (ondaAtualizada, inimigosAtualizados) = atualizaOnda delta o inimigos
  in
    (ondasFinal $ ondaAtualizada : os, inimigosAtualizados)

-- | Atualiza o estado de um portal e os inimigos no jogo.
atualizaPortal :: Tempo -> Portal -> [Inimigo] -> (Portal,[Inimigo])
atualizaPortal delta portal inimigos =
   let
     ondasAtualizada = fst $ atualizaOndas delta (ondasPortal portal) inimigos
     inimigosAtualizados = snd $ atualizaOndas delta (ondasPortal portal) inimigos
     in (portal {ondasPortal = ondasAtualizada} , inimigosAtualizados)

{-| Atualiza todos os portais do jogo e os inimigos ativos no jogo.  

==__Exemplos:__
>>> tualizaPortais 1 [Portal (1,2) [Onda [Inimigo (1,1) Sul 1 1 1 1 [] 1] 1 0 0]] []
([Portal (1,2) []],[Inimigo (1,1) Sul 1 1 1 1 [] 1]) -}
atualizaPortais :: Tempo -> [Portal] -> [Inimigo] -> ([Portal],[Inimigo])
atualizaPortais _ [] inimigos = ([], inimigos)
atualizaPortais delta (p:ps) inimigos =
  let
    (portalAtualizado, inimigosAtualizados) = atualizaPortal delta p inimigos
    (outrasPortaisAtualizados, inimigosFinal) = atualizaPortais delta ps inimigosAtualizados
  in
    ( portalAtualizado : outrasPortaisAtualizados, inimigosFinal)

-- | Função que devolve o terreno da matriz na posição (linha,coluna).
elementoDaMatriz :: (Float,Float) -> Mapa -> Maybe Terreno
elementoDaMatriz (linha,coluna) m
 | linha < 0 || coluna < 0 = Nothing -- Caso a posição seja inválida
 | coluna' >= length m  = Nothing -- Caso a linha esteja fora dos limites
 | linha' >= length (m !! coluna') = Nothing -- Caso a coluna esteja fora dos limites
 | otherwise = Just ((m !! coluna') !! linha')
  where (linha',coluna') = (floor linha, floor coluna)
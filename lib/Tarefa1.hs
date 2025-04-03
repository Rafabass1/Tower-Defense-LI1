{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Nuno Henrique Dias Pereira <a110067@alunos.uminho.pt>
              Rafael Figueiras Esteves <a112032@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where
import Data.List
import LI12425

-- | Verifica se um jogo é válido.
--
-- === Exemplo
--
-- >>> validaJogo Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
validaJogo :: Jogo -> Bool
validaJogo jogo = portaisValidos jogo && inimigosValidos jogo && torresValidas jogo && baseValida jogo
  
-- Relativamente a Portais:

-- | Verefica se existe pelo menos um portal.
--
-- === Exemplo
--
-- >>> existePortal Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
existePortal :: Jogo -> Bool
existePortal jogo = not (null (portaisJogo jogo))

-- | Verifica se todos os portais estão sobre terra.
--
-- === Exemplo
--
-- >>> portaisSobreTerra Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
portaisSobreTerra :: Jogo -> Bool
portaisSobreTerra jogo = all (\portal -> posicaoSobreTerra (posicaoPortal portal) (mapaJogo jogo)) (portaisJogo jogo)

-- | Verifica se uma posição está sobre Terra.
--
-- === Exemplo
--
-- >>> posicaoSobreTerra (1, 1) [[Terra, Terra, Terra], [Relva, Relva, Relva],[Terra, Terra, Terra]]
-- True
posicaoSobreTerra :: Posicao -> Mapa -> Bool
posicaoSobreTerra (x, y) mapa = (mapa !! floor y) !! floor x == Terra

-- | Verifica se existe um caminho de terra entre duas posições.
-- | Verifica se existe um caminho de terra entre duas posições.
--
-- === Exemplo
--
-- >>> existeCaminho (0, 0) (2, 2) [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]]
-- True
existeCaminho :: Posicao -> Posicao -> Mapa -> Bool
existeCaminho origem destino mapa = aux [origem] []
  where
    aux [] _ = False
    aux (p:ps) visitados
      | p == destino = True
      | p `elem` visitados = aux ps visitados
      | otherwise = aux (ps ++ vizinhosTerra p) (p:visitados)
    
    vizinhosTerra (x, y) = filter (\(nx, ny) -> nx >= 0 && ny >= 0 && nx < fromIntegral (length (head mapa)) && ny < fromIntegral (length mapa) && (mapa !! floor ny) !! floor nx == Terra) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- | Verifica que o portal nao esta sobreposto a torres ou base.
--
-- === Exemplo
--
-- >>> portaisNaoSobrepostos Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
portaisNaoSobrepostos :: Jogo -> Bool
portaisNaoSobrepostos jogo = all (\portal -> not (any (sobreposicao (posicaoPortal portal) . posicaoTorre) (torresJogo jogo)) && not (sobreposicao (posicaoPortal portal) (posicaoBase (baseJogo jogo))) ) (portaisJogo jogo)

-- | Verifica se duas posições estão sobrepostas.
--
-- === Exemplo
--
-- >>> sobreposicao (1, 1) (1, 1)
-- True
sobreposicao :: Posicao -> Posicao -> Bool
sobreposicao (x1, y1) (x2, y2) = floor x1 == floor x2 && floor y1 == floor y2

-- | Verifica se há no máximo uma onda ativa por portal.
--
-- === Exemplo
--
-- >>> ondasAtivasValidas Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
ondasAtivasValidas :: Jogo -> Bool
ondasAtivasValidas jogo = all (\portal -> length (filter (\onda -> tempoOnda onda > 0) (ondasPortal portal)) <= 1) (portaisJogo jogo)

-- | Verifica se todos os portais são válidos.
--
-- === Exemplo
--
-- >>> portaisValidos Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
portaisValidos :: Jogo -> Bool
portaisValidos jogo = existePortal jogo && portaisSobreTerra jogo && portaisNaoSobrepostos jogo && ondasAtivasValidas jogo

-- Relativamente a inimigos:
-- |Verifica se todos os inimigos por lançar têm a posição do respetivo portal, nível de vida positivo e lista de projéteis ativos vazia.
--
-- === Exemplo
--
-- >>> inimigosPorLancarValidos Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
inimigosPorLancarValidos :: Jogo -> Bool
inimigosPorLancarValidos jogo = all (\portal -> all (\inimigo -> posicaoInimigo inimigo == posicaoPortal portal && vidaInimigo inimigo > 0 && null (projeteisInimigo inimigo)) (concatMap inimigosOnda (ondasPortal portal))) (portaisJogo jogo)

-- | Verifica se todos os inimigos em jogo estão sobre terra, não estão sobrepostos a torres e têm velocidade não negativa.
--
-- === Exemplo
--
-- >>> inimigosEmJogoValidos Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
inimigosEmJogoValidos :: Jogo -> Bool
inimigosEmJogoValidos jogo = all (\inimigo -> posicaoSobreTerra (posicaoInimigo inimigo) (mapaJogo jogo) && not (any (sobreposicao (posicaoInimigo inimigo) . posicaoTorre) (torresJogo jogo)) && velocidadeInimigo inimigo >= 0) (inimigosJogo jogo)

-- | Verifica se a lista de projéteis ativos está normalizada.
--
-- === Exemplo
--
-- >>> projeteisNormalizados [Projetil {tipoProjetil = Fogo}, Projetil {tipoProjetil = Gelo}]
-- True
projeteisNormalizados :: [Projetil] -> Bool
projeteisNormalizados projeteis = length projeteis == length (nubBy (\p1 p2 -> tipoProjetil p1 == tipoProjetil p2) projeteis)

-- | Verifica se todos os inimigos são válidos.
--
-- === Exemplo
--
-- >>> inimigosValidos Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
inimigosValidos :: Jogo -> Bool
inimigosValidos jogo = inimigosPorLancarValidos jogo && inimigosEmJogoValidos jogo && all (\inimigo -> projeteisNormalizados (projeteisInimigo inimigo)) (inimigosJogo jogo)

-- Relativamente a Torres:

-- | Verifica se todas as torres estão sobre relva.
--
-- === Exemplo
--
-- >>> torresSobreRelva Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
torresSobreRelva :: Jogo -> Bool
torresSobreRelva jogo = all (\torre -> posicaoSobreRelva (posicaoTorre torre) (mapaJogo jogo)) (torresJogo jogo)

-- | Verifica se uma posição está sobre Relva.
--
-- === Exemplo
--
-- >>> posicaoSobreRelva (1, 1) [[Terra, Terra, Terra], [Relva, Relva, Relva],[Terra, Terra, Terra]]
-- True
posicaoSobreRelva :: Posicao -> Mapa -> Bool
posicaoSobreRelva (x, y) mapa = (mapa !! floor y) !! floor x == Relva

-- | Verifica se todas as torres têm alcance positivo.
--
-- === Exemplo
--
-- >>> alcancePositivo Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
alcancePositivo :: Jogo -> Bool
alcancePositivo jogo = all (\torre -> alcanceTorre torre > 0) (torresJogo jogo)

-- | Verifica se todas as torres têm rajada positiva.
--
-- === Exemplo
--
-- >>> rajadaPositiva Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
rajadaPositiva :: Jogo -> Bool
rajadaPositiva jogo = all (\torre -> rajadaTorre torre > 0) (torresJogo jogo)

-- | Verifica se todas as torres têm ciclo não negativo.
--
-- === Exemplo
--
-- >>> cicloNaoNegativo Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
cicloNaoNegativo :: Jogo -> Bool
cicloNaoNegativo jogo = all (\torre -> cicloTorre torre >= 0) (torresJogo jogo)

-- | Verifica se as torres não estão sobrepostas.
--
-- === Exemplo
--
-- >>> torresNaoSobrepostas Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}, Torre {posicaoTorre = (1, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
torresNaoSobrepostas :: Jogo -> Bool
torresNaoSobrepostas jogo = 
    all (\(t1, t2) -> not (sobreposicao (posicaoTorre t1) (posicaoTorre t2))) 
        [(t1, t2) | (t1:resto) <- tails (torresJogo jogo), t2 <- resto]

-- | Verifica se todas as torres são válidas.
--
-- === Exemplo
--
-- >>> torresValidas Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}, Torre {posicaoTorre = (1, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
torresValidas :: Jogo -> Bool
torresValidas jogo = torresSobreRelva jogo && alcancePositivo jogo && rajadaPositiva jogo && cicloNaoNegativo jogo && torresNaoSobrepostas jogo

-- Relativamente a Base:
-- | Verifica se a base está colocada sobre terra.
--
-- === Exemplo
--
-- >>> baseSobreTerra Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
baseSobreTerra :: Jogo -> Bool
baseSobreTerra jogo = posicaoSobreTerra (posicaoBase (baseJogo jogo)) (mapaJogo jogo)

-- | Verifica se a base não tem crédito negativo.
--
-- === Exemplo
--
-- >>> baseCreditoPositivo Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
baseCreditoPositivo :: Jogo -> Bool
baseCreditoPositivo jogo = creditosBase (baseJogo jogo) >= 0

-- | Verifica se a base não está sobreposta a uma torre ou portal.
--
-- === Exemplo
--
-- >>> baseNaoSobreposta Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
baseNaoSobreposta :: Jogo -> Bool
baseNaoSobreposta jogo = 
    not (any (sobreposicao (posicaoBase (baseJogo jogo)) . posicaoTorre) (torresJogo jogo)) &&
    not (any (sobreposicao (posicaoBase (baseJogo jogo)) . posicaoPortal) (portaisJogo jogo))

-- | Verifica se a base é válida.
--
-- === Exemplos
--
-- >>> baseValida Jogo {mapaJogo = [[Terra, Terra, Terra], [Relva, Relva, Relva], [Terra, Terra, Terra]], portaisJogo = [Portal {posicaoPortal = (1, 1), ondasPortal = [Onda {tempoOnda = 10, inimigosOnda = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}]}]}], torresJogo = [Torre {posicaoTorre = (2, 2), alcanceTorre = 5, rajadaTorre = 3, cicloTorre = 2}], inimigosJogo = [Inimigo {posicaoInimigo = (1, 1), vidaInimigo = 100, projeteisInimigo = [], velocidadeInimigo = 1}], baseJogo = Base {posicaoBase = (0, 0), creditosBase = 100}}
-- True
baseValida :: Jogo -> Bool
baseValida jogo = baseSobreTerra jogo && baseCreditoPositivo jogo && baseNaoSobreposta jogo
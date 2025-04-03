module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3 
import Desenhar (framesDoPortal)

reageTempo :: Tempo -> ImmutableTowers -> IO ImmutableTowers
reageTempo delta it@(ImmutableTowers {estado= Pausado}) =  return $ it {jogo = atualizaJogo 0 (jogo it)}
reageTempo delta it@(ImmutableTowers {estado= Jogando}) = return $ it {jogo = atualizaJogo delta (jogo it),indiceFrame = novoIndice, tempoAcc = tempoRestante } 
 where  frames = framesDoPortal (imagens it) -- Obtemos os frames passando as imagens
        novoTempo = tempoAcc it + delta
        tempoPorFrame = 0.099 -- Tempo por quadro (ajustável)
        numFrames = length frames 
        (novoIndice, tempoRestante)
            | novoTempo >= tempoPorFrame =
                ((indiceFrame it + 1) `mod` numFrames, novoTempo - tempoPorFrame)
            | otherwise = (indiceFrame it, novoTempo)
reageTempo delta it@(ImmutableTowers {estado = ColocarTorre}) = return $ it {jogo = atualizaJogo delta (jogo it),indiceFrame = novoIndice, tempoAcc = tempoRestante } 
 where  frames = framesDoPortal (imagens it) -- Obtemos os frames passando as imagens
        novoTempo = tempoAcc it + delta
        tempoPorFrame = 0.099 -- Tempo por quadro (ajustável)
        numFrames = length frames 
        (novoIndice, tempoRestante)
            | novoTempo >= tempoPorFrame =
                ((indiceFrame it + 1) `mod` numFrames, novoTempo - tempoPorFrame)
            | otherwise = (indiceFrame it, novoTempo)
reageTempo _ it = return  it


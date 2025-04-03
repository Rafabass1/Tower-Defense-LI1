module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import Tarefa2 (ganhouJogo, perdeuJogo)
import Data.List (sortBy)
import LI12425

-- Função principal de desenho
desenha :: ImmutableTowers -> IO Picture
desenha im@(ImmutableTowers { estado = MenuInicial}) = desenhaMenuInicial im
desenha im@(ImmutableTowers { estado = ColocarTorre, jogo = jog}) 
  | ganhouJogo (jogo im) = desenhaVitoria (imagens im)
  | perdeuJogo (jogo im) = desenhaDerrota (imagens im)
  | otherwise = return $ Pictures $ [desenhaLoja1 (lojaJogo jog) (imagens im)] ++ desenhaMapa (mapaJogo jog) (imagens im) ++ [desenhaBase (baseJogo jog) (imagens im), desenhaPortal im (portaisJogo jog), desenhaInimigo im (inimigosJogo jog), desenhaTorres (ordenaTorres (torresJogo jog)) (imagens im), desenhaCreditos (creditosBase (baseJogo jog)) (imagens im), desenhaVida (vidaBase (baseJogo jog)) (imagens im), desenhaCursor (torreComprada im) (posicaoCursor im) (imagens im)]
desenha im@(ImmutableTowers { estado = Pausado, jogo = jog}) = return $ Pictures $ desenhaMapa (mapaJogo jog) (imagens im) ++ [desenhaBase (baseJogo jog) (imagens im), desenhaPortal im (portaisJogo jog), desenhaInimigo im (inimigosJogo jog), desenhaTorres (ordenaTorres (torresJogo jog)) (imagens im), desenhaCreditos (creditosBase (baseJogo jog)) (imagens im), desenhaVida (vidaBase (baseJogo jog)) (imagens im), desenhaPausa (imagens im)]
desenha im@(ImmutableTowers { estado = Jogando, jogo = jog, desenhaLoja = True }) 
  | ganhouJogo (jogo im) = desenhaVitoria (imagens im)
  | perdeuJogo (jogo im) = desenhaDerrota (imagens im)
  | otherwise = return $ Pictures $ [desenhaLoja1 (lojaJogo jog) (imagens im)] ++ desenhaMapa (mapaJogo jog) (imagens im) ++ [desenhaBase (baseJogo jog) (imagens im), desenhaPortal im (portaisJogo jog), desenhaInimigo im (inimigosJogo jog), desenhaTorres (ordenaTorres (torresJogo jog)) (imagens im), desenhaCreditos (creditosBase (baseJogo jog)) (imagens im), desenhaVida (vidaBase (baseJogo jog)) (imagens im)]
desenha im@(ImmutableTowers { estado = Jogando, jogo = jog}) 
 | ganhouJogo jog =  desenhaVitoria (imagens im)
 | perdeuJogo jog =  desenhaDerrota (imagens im)
 | otherwise = return $ Pictures $ desenhaMapa (mapaJogo jog) (imagens im) ++ [desenhaBase (baseJogo jog) (imagens im), desenhaPortal im (portaisJogo jog), desenhaInimigo im (inimigosJogo jog), desenhaTorres (ordenaTorres (torresJogo jog)) (imagens im), desenhaCreditos (creditosBase (baseJogo jog)) (imagens im), desenhaVida (vidaBase (baseJogo jog)) (imagens im)]
desenha im@(ImmutableTowers { estado = EscolherNivel }) = return $ escolherNivel (imagens im) (noVitorias im)
desenha _ = return Blank

desenhaVidaIni :: Float -> Posicao -> Picture
desenhaVidaIni vida (x,y) = Translate x y $ Pictures [ Color green $ rectangleSolid (vida*2) 10, Color (greyN 0.5) $ rectangleWire 20 10]

-- Desenha a torre comprada em cima do cursor
desenhaCursor :: Maybe Torre -> Posicao -> Imagens -> Picture
desenhaCursor Nothing _ _ = Blank
desenhaCursor (Just t) (x,y) im = Translate x y $ (torres t im)

desenhaPausa :: Imagens -> Picture
desenhaPausa im = Pictures [fundo,pausado,retomar,sair] 
   where fundo = procurarImagem "quadro_madeira" im
         pausado = Translate 0 330 $ Pictures [Scale 1.3 1 $ procurarImagem "botao_metal" im, procurarImagem "pausado" im]
         retomar = Scale 0.75 0.75 $ Pictures [Scale 0.85 0.85 $ procurarImagem "botao_achatado" im ,Scale 0.6 0.6 $ Translate 10 20 $ procurarImagem "retomar_jogo" im]
         sair = Scale 0.5 0.5 $ Translate 0 (-480) $ procurarImagem "simbolo_home" im

desenhaVitoria :: Imagens ->  IO Picture
desenhaVitoria im = return $ Pictures [fundo,inicio]
 where fundo = Pictures [Scale 0.299 0.299 $ procurarImagem "fundoParede2" im, Scale 1.3 1.3 $ Translate 10 250 $ procurarImagem "vitoriafundo" im , Translate 0 300 $ procurarImagem "vitoria" im]
       inicio = Scale 0.65 0.65 $ Translate 0 (-100) $ Pictures [Translate (-10) 0 $ procurarImagem "botao_achatado" im , Translate 0 20 $ procurarImagem "inicio" im]

desenhaDerrota :: Imagens -> IO Picture
desenhaDerrota im = return $ Pictures [fundo,inicio]
 where fundo = Pictures [Scale 0.299 0.299 $ procurarImagem "fundoParede2" im,  Scale 1.3 1.3 $ Translate (-10) 245 $ procurarImagem "derrotafundo" im , Translate 10 300 $ procurarImagem "derrota" im]
       inicio =  Scale 0.65 0.65 $ Translate 0 (-100) $ Pictures [Translate (-10) 0 $ procurarImagem "botao_achatado" im ,Translate 0 20 $ procurarImagem "inicio" im]
 
escolherNivel :: Imagens  -> (Int,Int) -> Picture
escolherNivel im (n1,n2)
 | n1 == 0 = Pictures [escNivel,nivel1,nivel2,nivel3,cadeado2,cadeado3]
 | n1 >= 1 && n2 == 0 = Pictures [escNivel,nivel1,nivel2,nivel3,cadeado3]
 | n2 >= 1 = Pictures [escNivel,nivel1,nivel2,nivel3]
 | otherwise = Pictures [escNivel,nivel1, nivel2, nivel3]
  where nivel1 = Translate (-400) (-20) $ Scale 0.7 0.7 $  Pictures [procurarImagem "escudo_2" im , Scale 1.9 1.9 $ Translate 0 15 $ procurarImagem "no1" im]
        nivel2 = Translate 0 (-20) $ Scale 0.7 0.7 $  Pictures [procurarImagem "escudo_2" im ,Scale 1.9 1.9 $ Translate 0 15 $ procurarImagem "no2" im]
        nivel3 = Translate 400 (-20) $  Scale 0.7 0.7 $ Pictures [procurarImagem "escudo_2" im ,Scale 1.9 1.9 $ Translate 0 15 $ procurarImagem "no3" im]
        escNivel = Pictures [Scale 0.299 0.299 $ procurarImagem "fundoParede2" im, Scale 2 1 $ Translate 0 400 $ procurarImagem "botao_metal" im,Translate 0 407 $ procurarImagem "selecionar_nivel" im]
        cadeado2 = Translate 0 0 $ Scale 0.5 0.5 $ procurarImagem "cadeado" im
        cadeado3 = Translate 400 0 $ Scale 0.5 0.5 $ procurarImagem "cadeado" im
 
framesDoPortal :: Imagens -> [Picture]
framesDoPortal imagens = [frame1,frame2,frame3,frame4,frame5,frame6,frame1,frame2,frame3,frame4,frame5,frame6,frame1,frame2,frame3,frame4,frame5,frame6,frame1,frame2,frame3,frame4,frame5,frame6,frame1,frame2,frame3,frame4,frame5,frame6,frame1,frame2,frame3,frame4,frame5,frame6,frame1,frame2,frame3,frame4,frame5,frame6,frame1,frame2,frame3,frame4,frame5,frame6]
 where frame1 = procurarImagem "portalFrame1" imagens
       frame2 = procurarImagem "portalFrame2" imagens
       frame3 = procurarImagem "portalFrame3" imagens
       frame4 = procurarImagem "portalFrame4" imagens
       frame5 = procurarImagem "portalFrame5" imagens
       frame6 = procurarImagem "portalFrame6" imagens

desenhaCreditos :: Creditos -> Imagens -> Picture
desenhaCreditos creditos imagens = Translate 800 465 $  Pictures [fundo , Scale 3 3 $ procurarImagem "moeda_creditos" imagens, Scale 0.3 0.3 $  Translate 100 (-45) $ Text (show creditos)]
 where fundo = Translate 55 0 $ Color (greyN 0.8) $ rectangleSolid 145 50

desenhaVida :: Float -> Imagens -> Picture
desenhaVida vidas imagens = Translate 630 465 $ Pictures [fundo, Scale 0.095 0.095 $ Translate (-5) 25 $ procurarImagem "coracao_vidas" imagens, Scale 0.3 0.3 $ Translate 120 (-45) $ Text (show (round vidas))]
 where fundo = Translate 30 0 $ Color (greyN 0.8) $ rectangleSolid 120 50

-- Função para desenhar a loja
desenhaLoja1 :: Loja -> Imagens -> Picture
desenhaLoja1 loja imagens = Pictures (fundoLoja : tituloLoja : torresLoja)
  where
    fundoLoja = Translate (-620) 325 $ Color (greyN 0.8) $ rectangleSolid 650 325
    torresLoja = zipWith (desenhaTorreLoja imagens) loja [0..]
    tituloLoja = Translate (-620) 465 $ Scale 0.3 0.3 $ procurarImagem "titulo_loja" imagens

-- Função auxiliar para desenhar uma torre na loja
desenhaTorreLoja :: Imagens -> (Creditos, Torre) -> Int -> Picture
desenhaTorreLoja imagens (preco, torre) idx = Translate x y $ Pictures [nomeTorrePic, torrePic, precoPic, moedaPic, botaoPic, comprarPic]
  where
    x = fromIntegral (idx `mod` 3) * 200 - 815
    y = fromIntegral (idx `div` 3) * (-150) + 300
    torrePic = torres torre imagens
    precoPic = Translate 15 (-65) $ Scale 0.2 0.2 $ imagemPreco preco imagens
    moedaPic = Translate (-20) (-65) $ Scale 1 1 $ procurarImagem "moeda_creditos" imagens
    nomeTorrePic = Translate 0 105 $ Scale 0.2 0.2 $ imagemNomeTorre (tipoProjetil (projetilTorre torre)) imagens
    botaoPic = Translate 0 (-100) $ Scale 0.2 0.2 $ procurarImagem "botao_quadrado" imagens
    comprarPic = Translate 0 (-100) $ Scale 0.15 0.15 $ procurarImagem "comprar" imagens

-- Função para obter a imagem do nome da torre
imagemNomeTorre :: TipoProjetil -> Imagens -> Picture
imagemNomeTorre Fogo imagens = procurarImagem "torre_fogo" imagens
imagemNomeTorre Gelo imagens = procurarImagem "torre_gelo" imagens
imagemNomeTorre Resina imagens = procurarImagem "torre_resina" imagens
imagemNomeTorre _ _ = Blank

-- Função para obter a imagem do preço da torre
imagemPreco :: Creditos -> Imagens -> Picture
imagemPreco 150 imagens = procurarImagem "_150" imagens
imagemPreco 200 imagens = procurarImagem "_200" imagens
imagemPreco 250 imagens = procurarImagem "_250" imagens
imagemPreco _ _ = Blank

desenhaTorres :: [Torre] -> Imagens -> Picture
desenhaTorres [] _ = Blank
desenhaTorres (t:ts) im = Pictures [(Translate (x*60-60*y-2) (y*(-30)-30*x+42) (torres t im)) , desenhaTorres ts im]
  where (x,y) = posicaoTorre t

-- Função única para ordenar as torres pela posição
ordenaTorres :: [Torre] -> [Torre]
ordenaTorres = sortBy (\(Torre (x1, y1) _ _ _ _ _ _) (Torre (x2, y2) _ _ _ _ _ _) -> compare (x1, y1) (x2, y2))

torres :: Torre -> Imagens -> Picture
torres t ima 
 |tipoProjetil (projetilTorre t) == Fogo = Translate 0 (-15) $ Pictures  [procurarImagem "base_fogo_1" ima,Translate 0 31 $ procurarImagem "meio_fogo_1" ima, Translate 0 67 $ procurarImagem "topo_fogo_1_verde" ima]
 |tipoProjetil (projetilTorre t) == Gelo = Translate 0 (-15) $ Pictures  [procurarImagem "base_gelo_1" ima,Translate 0 31 $ procurarImagem "meio_gelo_1" ima,Translate 0 67 $ procurarImagem "topo_gelo_1_verde" ima]
 |tipoProjetil (projetilTorre t) == Resina = Translate 0 (-15) $ Pictures  [procurarImagem "base_resina_1" ima,Translate 0 31 $ procurarImagem "meio_resina_1" ima,Translate 0 67 $ procurarImagem "topo_resina_1_verde" ima]
 |otherwise = Blank

animacaoNorte ::  Imagens -> [Picture]
animacaoNorte imagens = [frame0,frame1,frame2,frame3,frame4,frame5,frame6,frame7,frame8,frame9,frame10,frame11,frame12,frame13,frame14,frame15,frame0,frame1,frame2,frame3,frame4,frame5,frame6,frame7,frame8,frame9,frame10,frame11,frame12,frame13,frame14,frame15,frame0,frame1,frame2,frame3,frame4,frame5,frame6,frame7,frame8,frame9,frame10,frame11,frame12,frame13,frame14,frame15]
  where
   frame0 = procurarImagem "raposa_045_01" imagens
   frame1 = procurarImagem "raposa_045_02" imagens
   frame2 = procurarImagem "raposa_045_03" imagens
   frame3 = procurarImagem "raposa_045_04" imagens
   frame4 = procurarImagem "raposa_045_05" imagens
   frame5 = procurarImagem "raposa_045_06" imagens
   frame6 = procurarImagem "raposa_045_07" imagens
   frame7 = procurarImagem "raposa_045_08" imagens
   frame8 = procurarImagem "raposa_045_09" imagens
   frame9 = procurarImagem "raposa_045_10" imagens
   frame10 = procurarImagem "raposa_045_11" imagens
   frame11 = procurarImagem "raposa_045_12" imagens
   frame12 = procurarImagem "raposa_045_13" imagens
   frame13 = procurarImagem "raposa_045_14" imagens
   frame14 = procurarImagem "raposa_045_15" imagens
   frame15 = procurarImagem "raposa_045_16" imagens

animacaoSul :: Imagens -> [Picture]
animacaoSul imagens = [frame0,frame1,frame2,frame3,frame4,frame5,frame6,frame7,frame8,frame9,frame10,frame11,frame12,frame13,frame14,frame15,frame0,frame1,frame2,frame3,frame4,frame5,frame6,frame7,frame8,frame9,frame10,frame11,frame12,frame13,frame14,frame15,frame0,frame1,frame2,frame3,frame4,frame5,frame6,frame7,frame8,frame9,frame10,frame11,frame12,frame13,frame14,frame15]
  where
   frame0 = procurarImagem "raposa_225_01" imagens
   frame1 = procurarImagem "raposa_225_02" imagens
   frame2 = procurarImagem "raposa_225_03" imagens
   frame3 = procurarImagem "raposa_225_04" imagens
   frame4 = procurarImagem "raposa_225_05" imagens
   frame5 = procurarImagem "raposa_225_06" imagens
   frame6 = procurarImagem "raposa_225_07" imagens
   frame7 = procurarImagem "raposa_225_08" imagens
   frame8 = procurarImagem "raposa_225_09" imagens
   frame9 = procurarImagem "raposa_225_10" imagens
   frame10 = procurarImagem "raposa_225_11" imagens
   frame11 = procurarImagem "raposa_225_12" imagens
   frame12 = procurarImagem "raposa_225_13" imagens
   frame13 = procurarImagem "raposa_225_14" imagens
   frame14 = procurarImagem "raposa_225_15" imagens
   frame15 = procurarImagem "raposa_225_16" imagens

animacaoEste :: Imagens -> [Picture]
animacaoEste imagens = [frame0,frame1,frame2,frame3,frame4,frame5,frame6,frame7,frame8,frame9,frame10,frame11,frame12,frame13,frame14,frame15,frame0,frame1,frame2,frame3,frame4,frame5,frame6,frame7,frame8,frame9,frame10,frame11,frame12,frame13,frame14,frame15,frame0,frame1,frame2,frame3,frame4,frame5,frame6,frame7,frame8,frame9,frame10,frame11,frame12,frame13,frame14,frame15]
  where 
   frame0 = procurarImagem "raposa_135_01" imagens
   frame1 = procurarImagem "raposa_135_02" imagens
   frame2 = procurarImagem "raposa_135_03" imagens
   frame3 = procurarImagem "raposa_135_04" imagens
   frame4 = procurarImagem "raposa_135_05" imagens
   frame5 = procurarImagem "raposa_135_06" imagens
   frame6 = procurarImagem "raposa_135_07" imagens
   frame7 = procurarImagem "raposa_135_08" imagens
   frame8 = procurarImagem "raposa_135_09" imagens
   frame9 = procurarImagem "raposa_135_10" imagens
   frame10 = procurarImagem "raposa_135_11" imagens
   frame11 = procurarImagem "raposa_135_12" imagens
   frame12 = procurarImagem "raposa_135_13" imagens
   frame13 = procurarImagem "raposa_135_14" imagens
   frame14 = procurarImagem "raposa_135_15" imagens
   frame15 = procurarImagem "raposa_135_16" imagens

animacaoOeste :: Imagens -> [Picture]
animacaoOeste imagens = [frame0,frame1,frame2,frame3,frame4,frame5,frame6,frame7,frame8,frame9,frame10,frame11,frame12,frame13,frame14,frame15,frame0,frame1,frame2,frame3,frame4,frame5,frame6,frame7,frame8,frame9,frame10,frame11,frame12,frame13,frame14,frame15,frame0,frame1,frame2,frame3,frame4,frame5,frame6,frame7,frame8,frame9,frame10,frame11,frame12,frame13,frame14,frame15]
 where 
   frame0 = procurarImagem "raposa_315_01" imagens
   frame1 = procurarImagem "raposa_315_02" imagens
   frame2 = procurarImagem "raposa_315_03" imagens
   frame3 = procurarImagem "raposa_315_04" imagens
   frame4 = procurarImagem "raposa_315_05" imagens
   frame5 = procurarImagem "raposa_315_06" imagens
   frame6 = procurarImagem "raposa_315_07" imagens
   frame7 = procurarImagem "raposa_315_08" imagens
   frame8 = procurarImagem "raposa_315_09" imagens
   frame9 = procurarImagem "raposa_315_10" imagens
   frame10 = procurarImagem "raposa_315_11" imagens
   frame11 = procurarImagem "raposa_315_12" imagens
   frame12 = procurarImagem "raposa_315_13" imagens
   frame13 = procurarImagem "raposa_315_14" imagens
   frame14 = procurarImagem "raposa_315_15" imagens
   frame15 = procurarImagem "raposa_315_16" imagens

framesInimigo :: Inimigo -> Imagens -> [Picture]
framesInimigo i ima = case direcaoInimigo i of
  Norte -> animacaoNorte ima
  Sul -> animacaoSul  ima 
  Este ->  animacaoEste  ima 
  Oeste -> animacaoOeste  ima

desenhaInimigo :: ImmutableTowers -> [Inimigo] -> Picture
desenhaInimigo _ [] = Blank
desenhaInimigo it (i:is) = 
  let framesDoInimigo = framesInimigo i (imagens it) 
      frameAtual = framesDoInimigo !! indiceFrame it  
  in  case direcaoInimigo i of 
    Norte -> Pictures [Translate (x*60-60*y+5) (y*(-30)-30*x+35) $ Scale 0.45 0.45 $ frameAtual,Translate (x*60-60*y+5) (y*(-30)-30*x+45) $ Pictures [Color green $ rectangleSolid (vida*3) 5, Color (greyN 0.2) $ retanguloVazio 30 5] ,desenhaInimigo it is]
    Sul -> Pictures [Translate (x*60-60*y+15) (y*(-30)-30*x+55) $ Scale 0.45 0.45 $ frameAtual,Translate (x*60-60*y+5) (y*(-30)-30*x+45) $ Pictures [Color green $ rectangleSolid (vida*3) 5, Color (greyN 0.2) $ retanguloVazio 30 5] ,desenhaInimigo it is]
    Este -> Pictures [Translate (x*60-60*y-10) (y*(-30)-30*x+50) $ Scale 0.45 0.45 $ frameAtual,Translate (x*60-60*y+5) (y*(-30)-30*x+45) $ Pictures [Color green $ rectangleSolid (vida*3) 5, Color (greyN 0.2) $ retanguloVazio 30 5] ,desenhaInimigo it is]
    Oeste -> Pictures [Translate (x*60-60*y+5) (y*(-30)-30*x+35) $ Scale 0.45 0.45 $ frameAtual,Translate (x*60-60*y+5) (y*(-30)-30*x+45) $ Pictures [Color green $ rectangleSolid (vida*3) 5, Color (greyN 0.2) $ retanguloVazio 30 5] ,desenhaInimigo it is]
  where
    (x, y) = posicaoInimigo i 
    vida = vidaInimigo i

retanguloVazio :: Float -> Float -> Picture
retanguloVazio largura altura =
  Line [(x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)]
  where
    x1 = -largura / 2
    x2 = largura / 2
    y1 = -altura / 2
    y2 = altura / 2
    
desenhaMenuInicial :: ImmutableTowers -> IO Picture
desenhaMenuInicial im =
  let fundo = Scale 1.072 1.072 $ snd $  (imagens im) !! 58  -- Aumenta a imagem de fundo para ocupar o ecrã todo
      botaoIniciar =  Pictures [Translate 0 55 $ Scale 0.45 0.38 botaoAchatado, Scale 0.55 0.55 $ Translate 5 110 $ snd $ (imagens im) !! 59]
      botaoSair =  Pictures [Translate 0 (-30) $ Scale 0.45 0.38 botaoAchatado, Scale 0.55 0.55 $ Translate 5 (-43) $ snd $ (imagens im) !! 60]
  in return $ pictures [fundo, botaoIniciar, botaoSair]
 where botaoAchatado :: Picture
       botaoAchatado = procurarImagem "botao_achatado" (imagens im) 

desenhaMapa :: Mapa -> Imagens -> [Picture]
desenhaMapa m im = desenhaMapaAux (0,0) m im
 where
  desenhaMapaAux :: (Int,Int) -> Mapa -> Imagens -> [Picture]
  desenhaMapaAux _ [] _ = []
  desenhaMapaAux (x,y) m im
    | elementoDaMatriz (x,y) m == Just Terra && elementoDaMatriz (x,y+1) m == Just Agua  && elementoDaMatriz (x,y-1) m == Just Agua = (Translate (x'*60-60*y') (y'*(-30)-30*x') $ procurarImagem "ponte" im) : desenhaMapaAux (x+1,y) m im -- caso seja terra a atravessar agua 
    | elementoDaMatriz (x,y) m == Just Agua && elementoDaMatriz (x+1,y) m /= Just Agua && elementoDaMatriz (x,y-1) m == Just Terra = Translate (x'*60-60*y') (y'*(-30)-30*x') (curvaCima (Just Agua) im): desenhaMapaAux (x+1,y) m im
    | elementoDaMatriz (x,y) m == Just Agua && elementoDaMatriz (x+1,y) m == Just Agua && elementoDaMatriz (x,y+1) m == Just Terra = Translate (x'*60-60*y') (y'*(-30)-30*x') (curva3 (Just Agua) im): desenhaMapaAux (x+1,y) m im
    | elementoDaMatriz (x,y) m == elementoDaMatriz (x+1,y) m  && elementoDaMatriz (x,y) m == elementoDaMatriz (x-1,y) m = Translate (x'*60-60*y') (y'*(-30)-30*x') (retaHorizontal (elementoDaMatriz (x,y) m) im) : desenhaMapaAux (x+1,y) m im
    | elementoDaMatriz (x,y) m /= elementoDaMatriz (x+1,y) m && elementoDaMatriz (x,y) m /= elementoDaMatriz (x-1,y) m = Translate (x'*60-60*y') (y'*(-30)-30*x') (retaVertical (elementoDaMatriz (x,y) m) im) : desenhaMapaAux (x+1,y) m im
    | elementoDaMatriz (x,y) m == elementoDaMatriz (x,y-1) m && elementoDaMatriz (x,y) m == elementoDaMatriz (x-1,y) m = Translate (x'*60-60*y') (y'*(-30)-30*x') (curvaCima (elementoDaMatriz (x,y) m) im) : desenhaMapaAux (x+1,y) m im
    | elementoDaMatriz (x,y) m == elementoDaMatriz (x-1,y) m && elementoDaMatriz (x,y) m == elementoDaMatriz (x,y+1) m = Translate (x'*60-60*y') (y'*(-30)-30*x') (curvaEsquerda (elementoDaMatriz (x,y) m) im) : desenhaMapaAux (x+1,y) m im
    | elementoDaMatriz (x,y) m == Just Terra && elementoDaMatriz (x,y-1) m == Just Terra = (Translate (x'*60-60*y') (y'*(-30)-30*x') $ procurarImagem "curva_direita" im) : desenhaMapaAux (x+1,y) m im  -- este
    | elementoDaMatriz (x,y) m == Just Agua && elementoDaMatriz (x,y-1) m == Just Agua = (Translate (x'*60-60*y') (y'*(-30)-30*x') $ procurarImagem "curva__agua_direita" im) : desenhaMapaAux (x+1,y) m im  -- e este tao assim por que da mesma maneira que os outros nao tava a dar n sei por que
    | elementoDaMatriz (x,y) m == Just Terra && elementoDaMatriz (x,y+1) m == Just Terra = (Translate (x'*60-60*y') (y'*(-30)-30*x') $ procurarImagem "curva2" im) : desenhaMapaAux (x+1,y) m im -- mesma coisa para este
    | elementoDaMatriz (x,y) m == Just Agua && elementoDaMatriz (x,y+1) m == Just Agua = (Translate (x'*60-60*y') (y'*(-30)-30*x') $ procurarImagem "curva_agua_3" im) : desenhaMapaAux (x+1,y) m im  -- e este
    | elementoDaMatriz (x,y) m == Just Terra = (Translate (x'*60-60*y') (y'*(-30)-30*x') $ procurarImagem "reta2" im) : desenhaMapaAux (x+1,y) m im
    | elementoDaMatriz (x,y) m == Just Agua = (Translate (x'*60-60*y') (y'*(-30)-30*x') $ procurarImagem "reta_agua" im) : desenhaMapaAux (x+1,y) m im
    | elementoDaMatriz (x,y) m == Just Relva = (Translate (x'*60-60*y') (y'*(-30)-30*x') $ blocosRelva im) : desenhaMapaAux (x+1,y) m im 
    | elementoDaMatriz (x,y) m == Nothing && elementoDaMatriz (x-1,y+1) m == Nothing = []
    | elementoDaMatriz (x,y) m == Nothing = desenhaMapaAux (x-(length (head m)),y+1) m im
    where (x',y') = (fromIntegral x,fromIntegral y)

retaHorizontal :: Maybe Terreno -> Imagens -> Picture
retaHorizontal (Just Agua) ima = procurarImagem "reta_agua" ima
retaHorizontal (Just Terra) ima = procurarImagem "reta2" ima
retaHorizontal (Just Relva) ima = blocosRelva ima
retaHorizontal _ _ = Blank

retaVertical :: Maybe Terreno -> Imagens -> Picture
retaVertical (Just Agua) ima = procurarImagem "reta_agua_2" ima
retaVertical (Just Terra) ima = procurarImagem "reta" ima
retaVertical (Just Relva) ima = blocosRelva ima
retaVertical _ _ = Blank

curvaCima :: Maybe Terreno -> Imagens -> Picture
curvaCima (Just Terra) ima = procurarImagem "curva3" ima
curvaCima (Just Agua) ima = procurarImagem "curva_agua_2" ima
curvaCima (Just Relva) ima = blocosRelva ima
curvaCima _ _ = Blank

curva3 :: Maybe Terreno -> Imagens -> Picture
curva3 (Just Terra) ima = procurarImagem "curva2" ima
curva3 (Just Agua) ima = procurarImagem "curva_agua_3" ima
curva3 (Just Relva) ima = blocosRelva ima
curva3 _ _ = Blank

curvaEsquerda :: Maybe Terreno -> Imagens -> Picture
curvaEsquerda (Just Terra) ima = procurarImagem "curva_esquerda" ima
curvaEsquerda (Just Agua) ima = procurarImagem "curva_agua_esquerda" ima
curvaEsquerda (Just Relva) ima = blocosRelva ima
curvaEsquerda _ _ = Blank

desenhaPortais :: [Portal] -> Imagens -> Picture
desenhaPortais [] _ = Blank
desenhaPortais (p@(Portal {posicaoPortal = (x,y)}):ps) im = Pictures [Translate (x*60-60*y-1) (y*(-30)-30*x+55) $ procurarImagem "portalparado" im, desenhaPortais ps im]

desenhaPortal :: ImmutableTowers -> [Portal]  -> Picture
desenhaPortal _ [] = Blank
desenhaPortal it (p:ps)  =
  let framesPortal = framesDoPortal (imagens it)
      frameAtual = framesPortal !! indiceFrame it
  in Pictures [Translate (x*60-60*y-1) (y*(-30)-30*x+55) frameAtual , desenhaPortal it ps ]
  where
    (x, y) = posicaoPortal p
    
desenhaBase :: Base -> Imagens -> Picture
desenhaBase base ima =  Translate (x*60-60*y-8) (y*(-30)-30*x+50) $ procurarImagem "topo_resina_2_verde_pontiagudo" ima
 where (x,y) = posicaoBase base

blocosTerra :: Imagens -> Picture
blocosTerra ima = Pictures [  Translate (0) (-8) $  procurarImagem "BlocoTerra" ima, Translate (0) 8 $ procurarImagem "BlocoTerra" ima]

blocosRelva :: Imagens -> Picture
blocosRelva ima = Pictures [Translate (0) (-8) $ procurarImagem "BlocoTerra" ima, Translate (0) 8 $ procurarImagem "BlocoRelva" ima]

-- Função que devolve o elemento da matriz na posição (linha,coluna)
elementoDaMatriz :: (Int,Int) -> Mapa -> Maybe Terreno
elementoDaMatriz (linha,coluna) m  
 | linha < 0 || coluna < 0 = Nothing -- Caso a posição seja inválida
 | coluna >= length m  = Nothing -- Caso a linha esteja fora dos limites
 | linha >= length (m !! coluna) = Nothing -- Caso a coluna esteja fora dos limites
 | otherwise = Just ((m !! coluna) !! linha)
 
-- Para procurar uma imagem atraves da string e dar a propria imagem e nao maybe ou just
procurarImagem :: String -> Imagens -> Picture
procurarImagem n i = case lookup n i of
  Just pic -> pic
  Nothing -> Blank 

-- NIVEIS
jogoInicial :: Jogo
jogoInicial = Jogo (Base 20 (0,0) 9999) [] [] mapa01 [] []

jogo01 :: Jogo
jogo01 = Jogo {baseJogo = Base 20 (1,7) 400 ,
              portaisJogo = [Portal (0,0) [Onda [Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1] 2.5 0 10,
              Onda [Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1] 2.5 0 0,
              Onda [Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1] 2 0 0,
              Onda [Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1] 2 0 0,
              Onda [Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1] 2 0 0,
              Onda [Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1] 2 0 0,
              Onda [Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1] 1.5 0 0,
              Onda [Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1] 1.5 0 0,
              Onda [Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1] 1.5 0 0,
              Onda [Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1,Inimigo (0,0) Este 10 1 1 15 [] 1] 1.2 0 0
              ]],
              torresJogo = [] ,
              mapaJogo = mapa01 ,
              inimigosJogo = [] ,
              lojaJogo = [(150,torreFogo),(200,torreGelo),(250,torreResina)] }

jogo02 :: Jogo
jogo02 = Jogo { baseJogo = Base { posicaoBase = (1, 6), vidaBase = 20, creditosBase = 400 },
                portaisJogo = [Portal (0,0) [Onda [Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1] 2.5 0 10,
                Onda [Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1] 2 0 0,
                Onda [Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1] 2 0 0,
                Onda [Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1] 2 0 0,
                Onda [Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1] 1.5 0 0,
                Onda [Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1] 1.5 0 0,
                Onda [Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1] 1.5 0 0,
                Onda [Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1] 1.5 0 0,
                Onda [Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1] 1.2 0 0,
                Onda [Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1,Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1, Inimigo (0,0) Este 10 1 2 15 [] 1] 1.2 0 0
                ]],
                torresJogo = [],
                mapaJogo = mapa02,
                inimigosJogo = [],
                lojaJogo = [(150,torreFogo),(200,torreGelo),(250,torreResina)] }

jogo03 :: Jogo
jogo03 = Jogo { baseJogo = Base { posicaoBase = (0, 6), vidaBase = 20, creditosBase = 400 },
                portaisJogo = [Portal (0,1) [Onda [Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1] 2 0 10,
                Onda [Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1] 2 0 0,
                Onda [Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1] 2 0 0,
                Onda [Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1] 1.5 0 0,
                Onda [Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1] 1.5 0 0,
                Onda [Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1] 1.5 0 0,
                Onda [Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1] 1.2 0 0,
                Onda [Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1] 1.2 0 0,
                Onda [Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1] 1.2 0 0,
                Onda [Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1,Inimigo (0,1) Este 10 1 4 10 [] 1] 1.2 0 0
                ]],
                torresJogo = [],
                mapaJogo = mapa03,
                inimigosJogo = [],
                lojaJogo = [(150,torreFogo),(200,torreGelo),(250,torreResina)] }

torreFogo :: Torre
torreFogo = Torre { danoTorre = 6.5,
                    alcanceTorre = 3,
                    rajadaTorre = 1,
                    cicloTorre = 2,
                    tempoTorre = 0,
                    projetilTorre = Projetil Fogo (Finita 5)
                    }


torreGelo :: Torre
torreGelo = Torre { danoTorre = 2.5,
                    alcanceTorre = 3,
                    rajadaTorre = 1,
                    cicloTorre = 2,
                    tempoTorre = 0,
                    projetilTorre = Projetil Gelo (Finita 5)
                    }

torreResina :: Torre
torreResina = Torre { danoTorre = 5,
                    alcanceTorre = 3,
                    rajadaTorre = 1,
                    cicloTorre = 2,
                    tempoTorre = 0,
                    projetilTorre = Projetil Resina (Finita 5)
                    }


mapa01 :: Mapa
mapa01 =  
 [  [t, t, t, a, a, a,a,a],
    [r, r, t, a, r, r,r,r],
    [r, r, t, a, r, r,r,r], 
    [r, r, t, a, r, r,r,r],
    [r, r, t, t, t, t,r,r],
    [a, a, a, a, r, t,t,t], 
    [r, r, r, r, r, r,r,t],
    [r, t, t, t, t, t,t,t]
    -- base (5,2) portal (0,0)
  ]
 where
 t = Terra
 r = Relva
 a = Agua

mapa02 :: Mapa
mapa02 =  
 [  [t, t, t, t, a, a,a,a],
    [r, r, r, t, a, r,r,a],
    [r, r, r, t, t, t,t,a], 
    [a, a, a, a, a, r,t,a],
    [r, r, r, r, r, r,t,a],
    [r, r, r, r, r, r,t,a], 
    [r, t, t, t, t, t,t,a],
    [a, a, a, a, a, a,a,a]
    -- Portal (0,3) Base (1,6)
  ]
 where
 t = Terra
 r = Relva
 a = Agua

mapa03 :: Mapa
mapa03 =  
 [  [r, r, r, a, r, r,r,r],
    [t, t, t, t, t, t,t,t],
    [r, r, a, a, r, r,r,t], 
    [r, r, a, r, r, r,r,t],
    [r, r, a, r, r, r,r,t],
    [r, r, a, r, r, r,r,t], 
    [t, t, t, t, t, t,t,t],
    [a, a, a, r, r, r,r,r]
    -- Portal (0,1) Base (0,6)
  ]
 where
 t = Terra
 r = Relva
 a = Agua
module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import Desenhar
import Tarefa2 (terminouJogo, ganhouJogo)
import System.Exit (exitSuccess)
import LI12425 (Loja, Creditos, Base(..), Torre(..), lojaJogo, creditosBase, torresJogo,baseJogo,Jogo(..),Mapa,mapaJogo,Terreno(..))

reageEventos :: Event -> ImmutableTowers -> IO ImmutableTowers
reageEventos (EventKey (Char 'u') Down _ _) im = return im { noVitorias = (2,2) }
reageEventos (EventMotion (x, y)) im@(ImmutableTowers {estado = ColocarTorre}) = return im { posicaoCursor = (x, y) }
reageEventos (EventMotion (x, y)) im@(ImmutableTowers {estado = Jogando}) = return im { posicaoCursor = (x, y) }
reageEventos (EventKey (Char 'l') Down _ _) im = return im { desenhaLoja = not (desenhaLoja im) }
reageEventos (EventKey (Char 'p') Down _ _) im@(ImmutableTowers {estado = Jogando}) = return im { estado = Pausado }
reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) im@(ImmutableTowers {estado = Pausado}) = reageEventosPausado (x, y) im
reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) im = case estado im of
  MenuInicial -> reageEventosMenuInicial (x,y) im
  Jogando -> if terminouJogo (jogo im)
             then reageEventosTerminou (x, y) im (noVitorias im)
             else if desenhaLoja im
              then reageEventosLoja (x, y) im
              else return im
  ColocarTorre ->  if terminouJogo (jogo im) 
                    then reageEventosTerminou (x,y) im (noVitorias im)
                     else reageEventosColocarTorre (x, y) im
  EscolherNivel -> return $ reageEventosNivel (x, y) im (noVitorias im)
  _ -> return im
reageEventos _ im = return im

torrePosicaoDiferente :: Posicao -> [Torre] -> Bool
torrePosicaoDiferente pos [] = True
torrePosicaoDiferente pos (t:ts) = if (fromIntegral x, fromIntegral y) == posicaoTorre t then False else torrePosicaoDiferente pos ts
 where (x,y) = ecraParaMapa pos 130 98

reageEventosColocarTorre :: Posicao -> ImmutableTowers -> IO ImmutableTowers
reageEventosColocarTorre (x,y) im = if podeColocarTorre (x,y) (mapaJogo (jogo im)) && torrePosicaoDiferente (x,y) (torresJogo (jogo im)) then posicionaTorre (x,y) (torreComprada im) im else return im

reageEventosPausado :: Posicao -> ImmutableTowers -> IO ImmutableTowers
reageEventosPausado (x, y) im
  | clicarBotao (x, y) botaoRetomar = return $ im { estado = Jogando }
  | clicarBotao (x, y) botaoHome = return $ im { estado = MenuInicial }
  | otherwise = return im

botaoRetomar :: (Posicao, Posicao)
botaoRetomar = ((-170, -30), (180, 50))

botaoHome :: (Posicao, Posicao)
botaoHome = ((-25, -262), (30, -214))

reageEventosTerminou :: Posicao -> ImmutableTowers -> (Int,Int) -> IO ImmutableTowers
reageEventosTerminou (x, y) im (n1,n2)
  | clicarBotao (x, y) botaoInicio = if ganhouJogo (jogo im) && posicaoBase (baseJogo (jogo im)) == (1,7) then return $ im { estado = MenuInicial, noVitorias = (n1+ 1,n2 )} else if  ganhouJogo (jogo im) && posicaoBase (baseJogo (jogo im)) == (1,6) then return $ im { estado = MenuInicial, noVitorias = (n1,n2 +1 )} else return $ im 
  | otherwise = return im

botaoInicio :: (Posicao, Posicao)
botaoInicio = ((-170, -95), (175, -15))

botaoNivel1 = ((-520, -195), (-270, 160))
botaoNivel2 = ((-125, -195), (125, 160))
botaoNivel3 = ((270, -195), (520, 160))

reageEventosNivel :: Posicao ->  ImmutableTowers -> (Int,Int) -> ImmutableTowers
reageEventosNivel (x, y) im (n1,n2)
   |clicarBotao (x, y) botaoNivel1 = im { estado = Jogando ,jogo = jogo01 }
   |clicarBotao (x, y) botaoNivel2 = if n1  >= 1 then  im { estado = Jogando, jogo = jogo02 } else  im
   |clicarBotao (x, y) botaoNivel3 = if n2 >= 1 then  im { estado= Jogando, jogo = jogo03 } else im
   |otherwise = im

reageEventosMenuInicial :: Posicao -> ImmutableTowers -> IO ImmutableTowers
reageEventosMenuInicial (x,y) im
  | clicarBotao (x,y) botaoIniciarJogo = return $ im { estado = EscolherNivel }
  | clicarBotao (x,y) botaoSair = exitSuccess
  | otherwise = return $ im

reageLoja :: Event -> ImmutableTowers -> IO ImmutableTowers
reageLoja (EventKey (Char 'l') Down _ _) im = return im { desenhaLoja = not (desenhaLoja im) }
reageLoja _ im = return im

reageEventosLoja :: Posicao -> ImmutableTowers -> IO ImmutableTowers
reageEventosLoja (x, y) im = do
  let loja = lojaJogo (jogo im)
      creditos = creditosBase (baseJogo (jogo im))
      (novaBase,novaTorreComprada,bool) = verificaCompra (x,y) loja (baseJogo (jogo im))
      novoEstado = if bool then ColocarTorre else Jogando
  return im { jogo = (jogo im) { baseJogo = novaBase }, torreComprada = novaTorreComprada , estado = novoEstado} -- aqui adiconei um bool q era para ver se me facilitava mas nao funcionou mt

ecraParaMapa :: (Float, Float) -> Float -> Float -> (Int, Int)
ecraParaMapa (x, y) blocoLargura blocoAltura = (round xMapa, round yMapa)
    where
      xMapa = ( (x / (blocoLargura / 2) + y / (blocoAltura / 2)) / 2 )  - y * 0.03
      yMapa = ( (y / (blocoAltura / 2) - x / (blocoLargura / 2)) / 2 )  - y * 0.03

coordenadasIsometrica :: (Float,Float) -> (Int,Int)
coordenadasIsometrica (x,y) = (round (1+(x/120)-(y/45)),round  (1-(x/240)-(y/45)))

serRelva :: Mapa -> (Int,Int) -> Bool
serRelva mapa (a,b) = b >= 0 && b < length mapa && a>=0 && a < length (head mapa) && case mapa !! b !! a of
                                Relva -> True
                                _ -> False

posicionaTorre :: Posicao -> Maybe Torre -> ImmutableTowers -> IO ImmutableTowers
posicionaTorre (x,y) (Just t) im = do
  let (x',y') = ecraParaMapa (x,y) 130 98
      novaTorre = t { posicaoTorre =  (fromIntegral x',fromIntegral y' ) }
      novasTorres = novaTorre : torresJogo (jogo im)
  return im { jogo = (jogo im) { torresJogo = novasTorres }, torreComprada = Nothing ,estado= Jogando}
posicionaTorre' _ _ im = return im

verificaCompra :: Posicao -> Loja -> Base -> (Base,Maybe Torre,Bool)
verificaCompra (x, y) loja base
  | clicarBotao (x, y) botaoFogo && creditosBase base >= precoFogo =
      (base { creditosBase = creditosBase base - precoFogo}, Just torreFogo,True)
  | clicarBotao (x,y) botaoGelo && creditosBase base >= precoGelo =
      (base { creditosBase = creditosBase base - precoGelo}, Just torreGelo,True)
  | clicarBotao (x,y) botaoResina && creditosBase base >= precoResina =
     (base { creditosBase = creditosBase base - precoResina}, Just torreResina,True)
  | otherwise = (base,Nothing,False)

precoFogo :: Creditos
precoFogo = 150

precoGelo :: Creditos
precoGelo = 200

precoResina :: Creditos
precoResina = 250

botaoFogo :: (Posicao, Posicao)
botaoFogo = ((-860, 185), (-770, 210))

botaoGelo :: (Posicao, Posicao)
botaoGelo = ((-660, 185), (-570, 210))

botaoResina :: (Posicao, Posicao)
botaoResina = ((-460, 185), (-370, 210))

posBotao :: Int -> (Posicao, Posicao)
posBotao idx = ((x1, y1), (x2, y2))
  where
    x = fromIntegral (idx `mod` 3) * 200 - 815
    y = fromIntegral (idx `div` 3) * (-150) + 330
    x1 = x - 50
    y1 = y - 150
    x2 = x + 50
    y2 = y - 110

type Posicao = (Float, Float)

podeColocarTorre :: Posicao -> Mapa -> Bool
podeColocarTorre (x, y) mapa
 | elementoDaMatriz (ecraParaMapa (x,y) 130 98) mapa == Just Relva = True
 | otherwise = False

botaoIniciarJogo :: (Posicao, Posicao)
botaoIniciarJogo = ((-120, 35), (150, 85))

botaoSair :: (Posicao, Posicao)
botaoSair = ((-120, -50), (120, 0))

clicarBotao :: Posicao -> (Posicao, Posicao) -> Bool
clicarBotao (x,y) ((x1,y1), (x2,y2)) = x >= x1 && x <= x2 && y >= y1 && y <= y2
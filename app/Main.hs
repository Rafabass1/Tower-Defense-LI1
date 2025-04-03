module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import Graphics.Gloss.Interface.IO.Game

janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)

fundo :: Color
fundo = white

fr :: Int
fr = 60

upLoadImages :: ImmutableTowers -> IO ImmutableTowers
upLoadImages im = 
  do 
    fundoMenu <- loadBMP "Imagens/fundoMenu.bmp"
    fundo01 <- loadBMP "Imagens/fundo01.bmp"
    curva2 <- loadBMP "Imagens/curva2.bmp"
    curva3 <- loadBMP "Imagens/curva3.bmp"
    curva_agua_2 <- loadBMP "Imagens/curva_agua_2.bmp"
    curva_agua_3 <- loadBMP "Imagens/curva_agua_3.bmp"
    curva_agua_direita <- loadBMP "Imagens/curva_agua_direita.bmp"
    curva_agua_esquerda <- loadBMP "Imagens/curva_agua_esquerda.bmp"
    curva_direita <- loadBMP "Imagens/curva_direita.bmp"
    curva_esquerda <- loadBMP "Imagens/curva_esquerda.bmp"
    ponte <- loadBMP "Imagens/ponte.bmp"
    ponte2 <- loadBMP "Imagens/ponte2.bmp"
    relva <- loadBMP "Imagens/relva.bmp"
    reta <- loadBMP "Imagens/reta.bmp"
    reta_agua <- loadBMP "Imagens/reta_agua.bmp"
    reta_agua_2 <- loadBMP "Imagens/reta_agua_2.bmp"
    terra <- loadBMP "Imagens/terra.bmp"
    relva_e_1_arvore <- loadBMP "Imagens/relva_e_1_arvore.bmp"
    relva_e_2_arvores <- loadBMP "Imagens/relva_e_2_arvores.bmp"
    relva_e_3_arvores <- loadBMP "Imagens/relva_e_3_arvores.bmp"
    relva_e_1_pedra <- loadBMP "Imagens/relva_e_1_pedra.bmp"
    relva_e_2_pedras <- loadBMP "Imagens/relva_e_2_pedras.bmp"
    relva_e_2_cristais <- loadBMP "Imagens/relva_e_2_cristais.bmp"
    base_fogo_1 <- loadBMP "Imagens/base_fogo_1.bmp"
    base_fogo_2 <- loadBMP "Imagens/base_fogo_2.bmp"
    base_gelo_1 <- loadBMP "Imagens/base_gelo_1.bmp"
    base_gelo_2 <- loadBMP "Imagens/base_gelo_2.bmp"
    base_resina_1 <- loadBMP "Imagens/base_resina_1.bmp"
    base_resina_2 <- loadBMP "Imagens/base_resina_2.bmp"
    meio_fogo_1 <- loadBMP "Imagens/meio_fogo_1.bmp"
    meio_fogo_2 <- loadBMP "Imagens/meio_fogo_2.bmp"
    meio_gelo_1 <- loadBMP "Imagens/meio_gelo_1.bmp"
    meio_gelo_2 <- loadBMP "Imagens/meio_gelo_2.bmp"
    meio_resina_1 <- loadBMP "Imagens/meio_resina_1.bmp"
    meio_resina_2 <- loadBMP "Imagens/meio_resina_2.bmp"
    topo_fogo_1_branco <- loadBMP "Imagens/topo_fogo_1_branco.bmp"
    topo_fogo_2_branco <- loadBMP "Imagens/topo_fogo_2_branco.bmp"
    topo_fogo_1_verde <- loadBMP "Imagens/topo_fogo_1_verde.bmp"
    topo_fogo_2_verde <- loadBMP "Imagens/topo_fogo_2_verde.bmp"
    topo_fogo_1_semcor <- loadBMP "Imagens/topo_fogo_1_semcor.bmp"
    topo_fogo_2_semcor <- loadBMP "Imagens/topo_fogo_2_semcor.bmp"
    topo_gelo_1_verde <- loadBMP "Imagens/topo_gelo_1_verde.bmp"
    topo_gelo_2_verde <- loadBMP "Imagens/topo_gelo_2_verde.bmp"
    topo_gelo_1_vermelho <- loadBMP "Imagens/topo_gelo_1_vermelho.bmp"
    topo_gelo_2_vermelho <- loadBMP "Imagens/topo_gelo_2_vermelho.bmp"
    topo_gelo_1_semcor <- loadBMP "Imagens/topo_gelo_1_semcor.bmp"
    topo_gelo_2_semcor <- loadBMP "Imagens/topo_gelo_2_semcor.bmp"
    topo_resina_1_verde <- loadBMP "Imagens/topo_resina_1_verde.bmp"
    topo_resina_2_verde <- loadBMP "Imagens/topo_resina_2_verde.bmp"
    topo_resina_1_vermelho <- loadBMP "Imagens/topo_resina_1_vermelho.bmp"
    topo_resina_2_vermelho <- loadBMP "Imagens/topo_resina_2_vermelho.bmp"
    topo_resina_1_semcor <- loadBMP "Imagens/topo_resina_1_semcor.bmp"
    topo_resina_2_semcor <- loadBMP "Imagens/topo_resina_2_semcor.bmp"
    botao_quadrado <- loadBMP "Imagens/botao_quadrado.bmp"
    botao_achatado <- loadBMP "Imagens/botao_achatado.bmp"
    fundo_jogo_2 <- loadBMP "Imagens/fundo_jogo_2.bmp"
    fundo_3 <- loadBMP "Imagens/fundo_3.bmp"
    fundo_4 <- loadBMP "Imagens/fundo_4.bmp"
    fundo_5 <- loadBMP "Imagens/fundo_5.bmp"
    iniciar_jogo <- loadBMP "Imagens/iniciar_jogo.bmp"
    sair <- loadBMP "Imagens/sair.bmp"
    portal <- loadBMP "Imagens/portal.bmp"
    portalparado <- loadBMP "Imagens/portalparado.bmp"
    reta2 <- loadBMP "Imagens/reta2.bmp" 
    -- prespetiva 45 graus
    raposa_045_01 <- loadBMP "Imagens/raposa_045_01.bmp"
    raposa_045_02 <- loadBMP "Imagens/raposa_045_02.bmp"
    raposa_045_03 <- loadBMP "Imagens/raposa_045_03.bmp"
    raposa_045_04 <- loadBMP "Imagens/raposa_045_04.bmp"
    raposa_045_05 <- loadBMP "Imagens/raposa_045_05.bmp"
    raposa_045_06 <- loadBMP "Imagens/raposa_045_06.bmp"
    raposa_045_07 <- loadBMP "Imagens/raposa_045_07.bmp"
    raposa_045_08 <- loadBMP "Imagens/raposa_045_08.bmp"
    raposa_045_09 <- loadBMP "Imagens/raposa_045_09.bmp"
    raposa_045_10 <- loadBMP "Imagens/raposa_045_10.bmp"
    raposa_045_11 <- loadBMP "Imagens/raposa_045_11.bmp"
    raposa_045_12 <- loadBMP "Imagens/raposa_045_12.bmp"
    raposa_045_13 <- loadBMP "Imagens/raposa_045_13.bmp"
    raposa_045_14 <- loadBMP "Imagens/raposa_045_14.bmp"
    raposa_045_15 <- loadBMP "Imagens/raposa_045_15.bmp"
    raposa_045_16 <- loadBMP "Imagens/raposa_045_16.bmp"
    -- prespetiva 135 graus
    raposa_135_01 <- loadBMP "Imagens/raposa_135_01.bmp"
    raposa_135_02 <- loadBMP "Imagens/raposa_135_02.bmp"
    raposa_135_03 <- loadBMP "Imagens/raposa_135_03.bmp"
    raposa_135_04 <- loadBMP "Imagens/raposa_135_04.bmp"
    raposa_135_05 <- loadBMP "Imagens/raposa_135_05.bmp"
    raposa_135_06 <- loadBMP "Imagens/raposa_135_06.bmp"
    raposa_135_07 <- loadBMP "Imagens/raposa_135_07.bmp"
    raposa_135_08 <- loadBMP "Imagens/raposa_135_08.bmp"
    raposa_135_09 <- loadBMP "Imagens/raposa_135_09.bmp"
    raposa_135_10 <- loadBMP "Imagens/raposa_135_10.bmp"
    raposa_135_11 <- loadBMP "Imagens/raposa_135_11.bmp"
    raposa_135_12 <- loadBMP "Imagens/raposa_135_12.bmp"
    raposa_135_13 <- loadBMP "Imagens/raposa_135_13.bmp"
    raposa_135_14 <- loadBMP "Imagens/raposa_135_14.bmp"
    raposa_135_15 <- loadBMP "Imagens/raposa_135_15.bmp"
    raposa_135_16 <- loadBMP "Imagens/raposa_135_16.bmp"
    -- prespetiva 225 graus
    raposa_225_01 <- loadBMP "Imagens/raposa_225_01.bmp"
    raposa_225_02 <- loadBMP "Imagens/raposa_225_02.bmp"
    raposa_225_03 <- loadBMP "Imagens/raposa_225_03.bmp"
    raposa_225_04 <- loadBMP "Imagens/raposa_225_04.bmp"
    raposa_225_05 <- loadBMP "Imagens/raposa_225_05.bmp"
    raposa_225_06 <- loadBMP "Imagens/raposa_225_06.bmp"
    raposa_225_07 <- loadBMP "Imagens/raposa_225_07.bmp"
    raposa_225_08 <- loadBMP "Imagens/raposa_225_08.bmp"
    raposa_225_09 <- loadBMP "Imagens/raposa_225_09.bmp"
    raposa_225_10 <- loadBMP "Imagens/raposa_225_10.bmp"
    raposa_225_11 <- loadBMP "Imagens/raposa_225_11.bmp"
    raposa_225_12 <- loadBMP "Imagens/raposa_225_12.bmp"
    raposa_225_13 <- loadBMP "Imagens/raposa_225_13.bmp"
    raposa_225_14 <- loadBMP "Imagens/raposa_225_14.bmp"
    raposa_225_15 <- loadBMP "Imagens/raposa_225_15.bmp"
    raposa_225_16 <- loadBMP "Imagens/raposa_225_16.bmp"
    -- prespetiva 315 graus
    raposa_315_01 <- loadBMP "Imagens/raposa_315_01.bmp"
    raposa_315_02 <- loadBMP "Imagens/raposa_315_02.bmp"
    raposa_315_03 <- loadBMP "Imagens/raposa_315_03.bmp"
    raposa_315_04 <- loadBMP "Imagens/raposa_315_04.bmp"
    raposa_315_05 <- loadBMP "Imagens/raposa_315_05.bmp"
    raposa_315_06 <- loadBMP "Imagens/raposa_315_06.bmp"
    raposa_315_07 <- loadBMP "Imagens/raposa_315_07.bmp"
    raposa_315_08 <- loadBMP "Imagens/raposa_315_08.bmp"
    raposa_315_09 <- loadBMP "Imagens/raposa_315_09.bmp"
    raposa_315_10 <- loadBMP "Imagens/raposa_315_10.bmp"
    raposa_315_11 <- loadBMP "Imagens/raposa_315_11.bmp"
    raposa_315_12 <- loadBMP "Imagens/raposa_315_12.bmp"
    raposa_315_13 <- loadBMP "Imagens/raposa_315_13.bmp"
    raposa_315_14 <- loadBMP "Imagens/raposa_315_14.bmp"
    raposa_315_15 <- loadBMP "Imagens/raposa_315_15.bmp"
    raposa_315_16 <- loadBMP "Imagens/raposa_315_16.bmp"
    -- Loja
    titulo_loja <- loadBMP "Imagens/titulo_loja.bmp"
    torre_gelo <- loadBMP "Imagens/torre_gelo.bmp"
    torre_fogo <- loadBMP "Imagens/torre_fogo.bmp"
    torre_resina <- loadBMP "Imagens/torre_resina.bmp"
    comprar <- loadBMP "Imagens/comprar.bmp"
    _150 <- loadBMP "Imagens/_150.bmp"
    _200 <- loadBMP "Imagens/_200.bmp"
    _250 <- loadBMP "Imagens/_250.bmp"
    moeda_creditos <- loadBMP "Imagens/moeda_creditos.bmp"
    coracao_vidas <- loadBMP "Imagens/coracao_vidas.bmp"
    portalFrame1 <- loadBMP "Imagens/portalFrame1.bmp"
    portalFrame2 <- loadBMP "Imagens/portalFrame2.bmp"
    portalFrame3 <- loadBMP "Imagens/portalFrame3.bmp"
    portalFrame4 <- loadBMP "Imagens/portalFrame4.bmp"
    portalFrame5 <- loadBMP "Imagens/portalFrame5.bmp"
    portalFrame6 <- loadBMP "Imagens/portalFrame6.bmp"
    nivel_1 <- loadBMP "Imagens/nivel_1.bmp"
    nivel_2 <- loadBMP "Imagens/nivel_2.bmp"
    nivel_3 <- loadBMP "Imagens/nivel_3.bmp"
    escudo1 <- loadBMP "Imagens/escudo1.bmp"
    no1 <- loadBMP "Imagens/no1.bmp"
    no2 <- loadBMP "Imagens/no2.bmp"
    no3 <- loadBMP "Imagens/no3.bmp"
    selecionar_nivel <- loadBMP "Imagens/selecionar_nivel.bmp"
    bola_de_fogo <- loadBMP "Imagens/bola_de_fogo.bmp"
    escudo_2 <- loadBMP "Imagens/escudo_2.bmp"
    fundoParede2 <- loadBMP "Imagens/fundoParede2.bmp"
    vitoria <- loadBMP "Imagens/vitoria.bmp"
    derrota <- loadBMP "Imagens/derrota.bmp"
    inicio <- loadBMP "Imagens/inicio.bmp"
    botao_metal <- loadBMP "Imagens/botao_metal.bmp"
    quadro_madeira <- loadBMP "Imagens/quadro_madeira.bmp"
    vitoriafundo <- loadBMP "Imagens/vitoriafundo.bmp"
    derrotafundo <- loadBMP "Imagens/derrotafundo.bmp"
    simbolo_home <- loadBMP "Imagens/simbolo_home.bmp"
    pausado <- loadBMP "Imagens/pausado.bmp"
    retomar_jogo <- loadBMP "Imagens/retomar_jogo.bmp"
    cadeado <- loadBMP "Imagens/cadeado.bmp"

    return im { imagens =
      [ ("fundoMenu", fundoMenu) ,
        ("ImagemFundo", fundo01) ,
        ("curva2", curva2) , 
        ("curva3", curva3) ,
        ("curva_agua_2", curva_agua_2) ,
        ("curva_agua_3", curva_agua_3) ,
        ("curva_agua_direita", curva_agua_direita) ,
        ("curva_agua_esquerda", curva_agua_esquerda) ,
        ("curva_direita", curva_direita) ,
        ("curva_esquerda", curva_esquerda) ,
        ("ponte", ponte) ,
        ("ponte2", ponte2) ,
        ("BlocoRelva", relva) ,
        ("reta", reta) ,
        ("reta_agua", reta_agua) ,
        ("reta_agua_2", reta_agua_2) ,
        ("BlocoTerra", terra) ,
        ("relva_arvore", relva_e_1_arvore) ,
        ("relva_2arvores", relva_e_2_arvores) ,
        ("relva_3arvores", relva_e_3_arvores) ,
        ("relva_1pedra", relva_e_1_pedra) ,
        ("relva_2pedras", relva_e_2_pedras) ,
        ("relva_2cristais", relva_e_2_cristais),
        ("base_fogo_1", base_fogo_1) ,
        ("base_fogo_2", base_fogo_2) ,
        ("base_gelo_1", base_gelo_1) ,
        ("base_gelo_2", base_gelo_2) ,
        ("base_resina_1", base_resina_1) ,
        ("base_resina_2", base_resina_2) ,
        ("meio_fogo_1", meio_fogo_1) ,
        ("meio_fogo_2", meio_fogo_2) ,
        ("meio_gelo_1", meio_gelo_1) ,
        ("meio_gelo_2", meio_gelo_2) ,
        ("meio_resina_1", meio_resina_1) ,
        ("meio_resina_2", meio_resina_2) ,
        ("topo_fogo_1_branco", topo_fogo_1_branco) ,
        ("topo_fogo_2_branco_pontiagudo", topo_fogo_2_branco) ,
        ("topo_fogo_1_verde", topo_fogo_1_verde) ,
        ("topo_fogo_2_verde_pontiagudo", topo_fogo_2_verde) ,
        ("topo_fogo_1_semcor", topo_fogo_1_semcor) ,
        ("topo_fogo_2_semcor", topo_fogo_2_semcor) ,
        ("topo_gelo_1_verde", topo_gelo_1_verde) ,
        ("topo_gelo_2_verde_pontiagudo", topo_gelo_2_verde) ,
        ("topo_gelo_1_vermelho", topo_gelo_1_vermelho) ,
        ("topo_gelo_2_vermelho_pontiagudo", topo_gelo_2_vermelho) ,
        ("topo_gelo_1_semcor", topo_gelo_1_semcor) ,
        ("topo_gelo_2_semcor", topo_gelo_2_semcor) ,
        ("topo_resina_1_verde", topo_resina_1_verde) ,
        ("topo_resina_2_verde_pontiagudo", topo_resina_2_verde) ,
        ("topo_resina_1_vermelho", topo_resina_1_vermelho) ,
        ("topo_resina_2_vermelho_pontiagudo",topo_resina_2_vermelho),
        ("topo_resina_1_semcor", topo_resina_1_semcor) ,
        ("topo_resina_2_semcor", topo_resina_2_semcor),
        ("botao_quadrado", botao_quadrado),
        ("botao_achatado", botao_achatado) ,
        ("fundo_2", fundo_jogo_2) ,
        ("fundo_3", fundo_3) ,
        ("fundo_4", fundo_4) ,
        ("fundo_5", fundo_5),
        ("iniciar_jogo",iniciar_jogo),
        ("sair",sair),
        ("portal",portal),
        ("portalparado",portalparado) ,
        ("reta2",reta2) ,
        ("raposa_045_01",raposa_045_01) ,
        ("raposa_045_02",raposa_045_02) ,
        ("raposa_045_03",raposa_045_03) ,
        ("raposa_045_04",raposa_045_04) ,
        ("raposa_045_05",raposa_045_05) ,
        ("raposa_045_06",raposa_045_06) ,
        ("raposa_045_07",raposa_045_07) ,
        ("raposa_045_08",raposa_045_08) ,
        ("raposa_045_09",raposa_045_09) ,
        ("raposa_045_10",raposa_045_10) ,
        ("raposa_045_11",raposa_045_11) ,
        ("raposa_045_12",raposa_045_12) ,
        ("raposa_045_13",raposa_045_13) ,
        ("raposa_045_14",raposa_045_14) ,
        ("raposa_045_15",raposa_045_15) ,
        ("raposa_045_16",raposa_045_16) ,
        ("raposa_135_01",raposa_135_01) ,
        ("raposa_135_02",raposa_135_02) ,
        ("raposa_135_03",raposa_135_03) ,
        ("raposa_135_04",raposa_135_04) ,
        ("raposa_135_05",raposa_135_05) ,
        ("raposa_135_06",raposa_135_06) ,
        ("raposa_135_07",raposa_135_07) ,
        ("raposa_135_08",raposa_135_08) ,
        ("raposa_135_09",raposa_135_09) ,
        ("raposa_135_10",raposa_135_10) ,
        ("raposa_135_11",raposa_135_11) ,
        ("raposa_135_12",raposa_135_12) ,
        ("raposa_135_13",raposa_135_13) ,
        ("raposa_135_14",raposa_135_14) ,
        ("raposa_135_15",raposa_135_15) ,
        ("raposa_135_16",raposa_135_16) ,
        ("raposa_225_01",raposa_225_01) ,
        ("raposa_225_02",raposa_225_02) ,
        ("raposa_225_03",raposa_225_03) ,
        ("raposa_225_04",raposa_225_04) ,
        ("raposa_225_05",raposa_225_05) ,
        ("raposa_225_06",raposa_225_06) ,
        ("raposa_225_07",raposa_225_07) ,
        ("raposa_225_08",raposa_225_08) ,
        ("raposa_225_09",raposa_225_09) ,
        ("raposa_225_10",raposa_225_10) ,
        ("raposa_225_11",raposa_225_11) ,
        ("raposa_225_12",raposa_225_12) ,
        ("raposa_225_13",raposa_225_13) ,
        ("raposa_225_14",raposa_225_14) ,
        ("raposa_225_15",raposa_225_15) ,
        ("raposa_225_16",raposa_225_16) ,
        ("raposa_315_01",raposa_315_01) ,
        ("raposa_315_02",raposa_315_02) ,
        ("raposa_315_03",raposa_315_03) ,
        ("raposa_315_04",raposa_315_04) ,
        ("raposa_315_05",raposa_315_05) ,
        ("raposa_315_06",raposa_315_06) ,
        ("raposa_315_07",raposa_315_07) ,
        ("raposa_315_08",raposa_315_08) ,
        ("raposa_315_09",raposa_315_09) ,
        ("raposa_315_10",raposa_315_10) ,
        ("raposa_315_11",raposa_315_11) ,
        ("raposa_315_12",raposa_315_12) ,
        ("raposa_315_13",raposa_315_13) ,
        ("raposa_315_14",raposa_315_14) ,
        ("raposa_315_15",raposa_315_15) ,
        ("raposa_315_16",raposa_315_16) ,
        ("titulo_loja",titulo_loja) ,
        ("torre_gelo",torre_gelo) ,
        ("torre_fogo",torre_fogo) ,
        ("torre_resina",torre_resina) ,
        ("comprar",comprar) ,
        ("_150",_150) ,
        ("_200",_200) ,
        ("_250",_250) ,
        ("moeda_creditos", moeda_creditos) ,
        ("coracao_vidas", coracao_vidas) , 
        ("portalFrame1", portalFrame1) ,
        ("portalFrame2", portalFrame2) ,
        ("portalFrame3", portalFrame3) ,
        ("portalFrame4", portalFrame4) ,
        ("portalFrame5", portalFrame5) ,
        ("portalFrame6", portalFrame6) ,
        ("nivel_1", nivel_1) ,
        ("nivel_2", nivel_2) ,
        ("nivel_3", nivel_3) ,
        ("escudo1", escudo1) ,
        ("no1", no1) ,
        ("no2", no2) ,
        ("no3", no3) ,
        ("selecionar_nivel", selecionar_nivel) ,
        ("bola_fogo", bola_de_fogo) ,   
        ("escudo_2", escudo_2) ,
        ("fundoParede2", fundoParede2) ,
        ("vitoria", vitoria) , 
        ("derrota", derrota) , 
        ("inicio", inicio) ,
        ("botao_metal", botao_metal) ,
        ("quadro_madeira", quadro_madeira) ,
        ("vitoriafundo",vitoriafundo) ,
        ("derrotafundo",derrotafundo) ,
        ("simbolo_home",simbolo_home) ,
        ("pausado",pausado) ,
        ("retomar_jogo",retomar_jogo) ,
        ("cadeado",cadeado)

      ] 
      }

main :: IO ()
main = do
  putStrLn "Hello from Immutable Towers!"
  -- Carregar as imagens
  imagensCarregadas <- upLoadImages (ImmutableTowers MenuInicial jogoInicial [] False 0 0 Nothing (0,0) (0,0))
  let 
  playIO janela fundo fr imagensCarregadas desenha reageEventos reageTempo

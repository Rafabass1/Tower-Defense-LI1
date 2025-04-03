module ImmutableTowers where
import LI12425
import Graphics.Gloss

data ImmutableTowers = ImmutableTowers { 
  estado :: EstadoJogo,
  jogo :: Jogo,
  imagens :: Imagens ,
  desenhaLoja :: Bool,
  tempoAcc :: Float,
  indiceFrame :: Int ,
  torreComprada :: Maybe Torre ,
  posicaoCursor :: Posicao ,
  noVitorias :: (Int,Int) -- primeiro noVit do nivel 1 e segundo noVit do nivel 2
                                       }

data EstadoJogo = MenuInicial | Jogando | EscolherNivel | Pausado | ColocarTorre
  deriving (Eq, Show)

type Imagens = [(String,Picture)]
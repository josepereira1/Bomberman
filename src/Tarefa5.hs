{-| 
Module      : Main
Description : Este módulo contém as funções para a realização da __Tarefa 5__, do projeto __Bomberman__. 
Copyright   : José Pereira <https://github.com/josepereira1>;


Este módulo contém as __funções__, da __Tarefa 5__, que consiste na criação de uma __Interface Gráfica__ do jogo __Bomberman__ e a respetiva jogabilidade.
-}
module Main where

import Graphics.Gloss         
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game
import T4Lib
import T2Lib
import T1Lib

import Tarefa6_li1g135

{-|
O type __ActualTime__, corresponde ao tempo atual do jogo.
-}
type ActualTime = Int
{-|
O type __LastTimeReageT__, corresponde ao somatório de (1/FrameRate).
-}
type LastTimeReageT = Float
{-|
O type __Tamanho__, corresponde ao tamanho do mapa.
-}
type Tamanho = Int 
{-|
O type Semente, corresponde à semente do mapa.
-}
type Semente = Int
{-|
O type __Linha__, corresponde à __Linha__ do mapa.
-}
type Linha = Int
{-|
O type Coluna, corresponde à __Coluna__ do mapa.
-}
type Coluna = Int
{-|
O type __WindowAltura__, corresponde à altura do mapa em píxeis.
-}
type WindowAltura = Float
{-|
O type __WindowLargura__, corresponde à Largura do mapa em píxeis.
-}
type WindowLargura = Float
{-|
O type __Gamers__, corresponde ao número do jogador.
-}
type Gamers = Int

{-|
==Descrição:
O __Estado__, contém a informação necessária para executar todas as funções.

*Informação do Jogo (Tabuleiro,Power Ups,Bombas,Jogadores);
*Tamanho do Mapa;
*Semente do Mapa;
*Somatório de (1/Frame Rate);
*Imagens utilizadas na interface gráfica;
*Tempo atual do Jogo;
*Largura em Pixeis do Mapa;
*Altura em Pixeis do Mapa;
*Número do Jogador;
-}

type Estado = (InfGame,Tamanho,Semente,LastTimeReageT,Imagens,ActualTime,WindowLargura,WindowAltura,Gamers)

{-|
==Descrição:
O tipo Imagens contém todas as imagens usadas na interface gráfica.
-}
data Imagens = Imagens { pedra :: Picture,
                         bomba :: Picture, 
                         caixa :: Picture, 
                         jogador0 :: Picture, 
                         jogador1 :: Picture, 
                         jogador2 :: Picture, 
                         jogador3 :: Picture, 
                         powUpBomb :: Picture, 
                         powUpFlame :: Picture, 
                         fundo :: Picture, 
                         explosion :: Picture}

{-|
==Descrição:
A função __estadoInicial__, retorna o estado ínicial do jogo, com a informação do mapa, tamanho, semente, somatório do tempo do frame rate, as imagens, tempo atual de jogo , tamanho da janela (size*50), altura da janela(size*50), número do jogador.
-}
--  O estado inicial do jogo.
estadoInicial :: IO Estado
estadoInicial = do
    p1 <- loadBMP "pedra50x50.bmp"
    p2 <- loadBMP "bomba50x50.bmp"
    p3 <- loadBMP "caixa50x50.bmp"
    p4 <- loadBMP "jogador0.bmp"
    p5 <- loadBMP "jogador1.bmp"
    p6 <- loadBMP "jogador2.bmp"
    p7 <- loadBMP "jogador3.bmp"
    p8 <- loadBMP "pwUpbomb50x50.bmp"
    p9 <- loadBMP "pwUpflame50x50.bmp"
    p10 <- loadBMP "fundo50x50.bmp"
    p11 <- loadBMP "exp50x50.bmp"
    let ps = Imagens p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11
    {--
    putStrLn "insert size of map:"  --caso queira inserir os dados através do terminal, pode ser assim;(Código comentado)
    x <- getLine
    let size = read x :: Int
    putStrLn "insert seed of map:"
    y <- getLine
    let seed = read y :: Int
    {-putStrLn "Insira o número do jogador que vai jogar [0,1,2,3] respectivamente:"
    z <- getLine
    --}
    let jogador = read z :: Int-}
    let size = 13
    let seed = 0
    let jogador = 0
    return (filterInfGame ((mapa size seed)++["0 1 1","1 "++(show (size-2))++" 1","2 "++(show (size-2))++" "++(show (size-2)),"3 1 "++(show (size-2))]),size,seed,0,ps,2*(((length (head (mapa size seed)))-2)^2),(50*(fromIntegral size)),(50*(fromIntegral size)),0)
{-|
==Descrição:
A função __desenhaEstado__, recebe o estado de jogo, e cria a interface gráfica através desse estado de jogo. 
-}
--  Função que desenha o jogo.
desenhaEstado :: Estado -> Picture
desenhaEstado (infgame,size,semente,time,pict,actTime,wlargura,waltura,jogador) = Pictures (tabuleiro++powUps++bombs++jogadores++flamesBombs++[Color white (Scale 0.25 0.25 (Translate (-80) (2*(waltura)) (Text $ show actTime)))])
    where tabuleiro = desenhaTabuleiro pict (concat tab) 0 0 size ((-1)*((wlargura/2)-25)) ((wlargura/2)-25)
          (bombs,flamesBombs) = desenhaBombas pict bmbs size ((-1)*((wlargura/2)-25)) ((wlargura/2)-25) infgame
          jogadores = desenhaJogadores pict ply size 0 ((-1)*((wlargura/2)-25)) ((wlargura/2)-25)
          powUps = desenhaPowerUps pict pwUp infgame size ((-1)*((wlargura/2)-25)) ((wlargura/2)-25)
          (tab,pwUp,bmbs,ply) = infgame
{-|
==Descrição:
A função __determinaPixels1__, determina a lista dos píxeis por coluna para um dado mapa, no eixo dos xx, em que cada elemento da lista corresponde à coodenada x para colocar uma imagem, portanto o elemento na posição zero corresponde aos píxeis no mapa para a coluna zero.

==Exemplos:
>>>determinaPixels1 9 (-200)
[-200.0,-150.0,-100.0,-50.0,0.0,50.0,100.0,150.0,200.0]

==Notas:
*Como podemos ver no exemplo, para o mapa de tamanho 9, temos uma janela com 450 Pixeis, pois cada imagem tem 50x50 Pixeis, logo o tamanho do mapa na janela é dado por (50*9).
*Como o tamanho é 450 Pixeis, então a primeira imagem vai começar nos -200, e a partir desse valor a nossa função determina os restantes, em que cada posição da lista corresponde a uma coluna no mapa bomberman. 
-}
--  Esta função determina os píxeis no eixo dos xx, para cada quadrado do mapa, para saber onde tenho de colocar as imagens.
determinaPixels1 :: Int -> Float -> [Float] 
determinaPixels1  size dim | size > 0 = dim : determinaPixels1 (size-1) (dim+50)
                           | otherwise = []

{-|
==Descrição:
A função __determinaPixels1__, determina a lista dos píxeis por coluna para um dado mapa, no eixo dos yy, em que cada elemento da lista corresponde à coodenada y para colocar uma imagem, portanto o elemento na posição zero corresponde aos píxeis no mapa para a linha zero.

==Exemplos:
>>>determinaPixels2 9 (200)
[200.0,150.0,100.0,50.0,0.0,-50.0,-100.0,-150.0,-200.0]

==Notas:
*Como podemos ver no exemplo, para o mapa de tamanho 9, temos uma janela com 450 Pixeis, pois cada imagem tem 50x50 Pixeis, logo o tamanho do mapa na janela é dado por (50*9).
*Como o tamanho é 450 Pixeis, então a primeira imagem vai começar nos 200, e a partir desse valor a nossa função determina os restantes, em que cada posição da lista corresponde a uma linha no mapa bomberman. 
-}

--  Esta função determina os píxeis no eixo dos yy, para cada quadrado do mapa, para saber onde tenho de colocar as imagens.
determinaPixels2 :: Int -> Float -> [Float] 
determinaPixels2  size dim | size > 0 = dim : determinaPixels2 (size-1) (dim-50)
                           | otherwise = []
{-|
==Descrição:
A função __desenhaPowerUps__, coloca as imagens dos Power Ups destapados no mapa, através da sua linha e coluna.
-}
--  Esta função desenha os power Ups destapados.
desenhaPowerUps :: Imagens -> [PowUp] -> InfGame -> Size -> Float -> Float -> [Picture]
desenhaPowerUps pict [] infgame size wl wa = []
desenhaPowerUps pict ((Bomb (c,l)):t) infgame size wl wa 
  | blk == Vazio = (Translate x y (powUpBomb pict)):desenhaPowerUps pict t infgame size wl wa
  | otherwise = desenhaPowerUps pict t infgame size wl wa
        where (x,y) = (((determinaPixels1 size wl) !! c),((determinaPixels2 size wa) !! l))
              blk = bloco infgame (c,l) 
desenhaPowerUps pict ((Flame (c,l)):t) infgame size wl wa
  | blk == Vazio = (Translate x y (powUpFlame pict)):desenhaPowerUps pict t infgame size wl wa
  | otherwise = desenhaPowerUps pict t infgame size wl wa
        where (x,y) = (((determinaPixels1 size wl) !! c),((determinaPixels2 size wa) !! l))
              blk = bloco infgame (c,l)
{-|
==Descrição:
A função __desenhaBombas__, coloca as imagens das bombas na janela, através das posições das mesmas. -}
--  Esta função desenha as bombas que estão ativadas no mapa, e também desenha o flame das que vão explodir, ou seja, as que tem time == 1.
desenhaBombas :: Imagens -> [Bomba] -> Size -> Float -> Float -> InfGame -> ([Picture],[Picture])
desenhaBombas _ [] _ _ _ _ = ([],[])
desenhaBombas imgs bombs size wl wa infgame 
  | time /= 1 = ((Translate x y (bomba imgs)):bombas,flamesBombs)
  | otherwise = ((Translate x y (bomba imgs)):bombas,explosion++flamesBombs)                                             
    where (((c,l),j,f,time):t) = bombs
          (x,y) = (((determinaPixels1 size wl) !! c),((determinaPixels2 size wa) !! l))
          explosion = desenhaExplosionBomb imgs listaPos size wl wa  
          listaPos = detRangeCoords ((c,l),j,f,time) size infgame 
          (bombas,flamesBombs) = desenhaBombas imgs t size wl wa infgame

{-|
==Descrição:
A função __desenhaExplosionBomb__, coloca imagens no flame causado pela explosão das bombas.
-}
--  Esta função desenha o flame da explosão de uma bomba.
desenhaExplosionBomb :: Imagens -> [Posicao] -> Int -> Float -> Float -> [Picture]
desenhaExplosionBomb pict [] _ _ _ = []
desenhaExplosionBomb pict ((c,l):t) size wl wa = (Translate x y (explosion pict)):desenhaExplosionBomb pict t size wl wa
    where (x,y) = (((determinaPixels1 size wl) !! c),((determinaPixels2 size wa) !! l))
{-|
A função __desenhaJogadores__, coloca imagens dos jogadores através das suas posições no mapa.
-}
--  Esta função desenha os jogadores.
desenhaJogadores :: Imagens -> Players -> Size -> Int -> Float -> Float -> [Picture]
desenhaJogadores _ [] _ _ _ _ = []
desenhaJogadores imgs (h:t) size flg wl wa
  | h == Morto = desenhaJogadores imgs t size (flg+1) wl wa
  | flg == 0 = Translate x y (jogador0 imgs):desenhaJogadores imgs t size (flg+1) wl wa
  | flg == 1 = Translate x y (jogador1 imgs):desenhaJogadores imgs t size (flg+1) wl wa
  | flg == 2 = Translate x y (jogador2 imgs):desenhaJogadores imgs t size (flg+1) wl wa
  | flg == 3 = Translate x y (jogador3 imgs):desenhaJogadores imgs t size (flg+1) wl wa
    where (x,y) = (((determinaPixels1 size wl) !! c),((determinaPixels2 size wa) !! l))
          (Vivo (c,l) bmb flm) = h
{-|
==Descrição:
A função __desenhaTabuleiro__, coloca as imagens das Pedras, Tijolos, e vélulas Vazias na Janela, através das coordenadas das mesmas.
-}
--  Coloca as imagens do tabuleiro do jogo, ou seja, pedras, tijolos e células vazias.
desenhaTabuleiro :: Imagens -> [Bloco] -> Coluna -> Linha -> Size -> Float -> Float -> [Picture]
desenhaTabuleiro _ [] _ _ _ _ _ = []
desenhaTabuleiro imgs tab c l size wl wa  
  | c == (size-1) = (Translate  x y (pedra imgs)):desenhaTabuleiro imgs t 0 (l+1) size wl wa
  | h == Pedra = (Translate  x y (pedra imgs)):desenhaTabuleiro imgs t (c+1) l size wl wa
  | h == Tijolo = (Translate x y (caixa imgs)):desenhaTabuleiro imgs t (c+1) l size wl wa
  | h == Vazio = (Translate x y (fundo imgs)):desenhaTabuleiro imgs t (c+1) l size wl wa
  | otherwise = desenhaTabuleiro imgs t (c+1) l size wl wa
      where (x,y) = (((determinaPixels1 size (wl)) !! c),((determinaPixels2 size wa) !! l))
            (h:t) = tab
{-|
==Descrição:
A função __reageEvento__, modifica o estado de jogo, através de um evento, ou seja, um comando introduzido por um utilizador.
-}
--  Função que altera o estado do jogo quando acontece um evento.
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (Char 'w')    Down _ _) estado@(infgame,size,seed,time,pict,actTime,wlargura,waltura,jogador) = 
  (map3,size,seed,time,pict,actTime,wlargura,waltura,jogador)
    where mapa = decodeInfGameToString(filterInfGame (move (decodeInfGameToString infgame) jogador 'U'))
          key1 = bot mapa 1 actTime
          map1 = aux_reageEvento (filterInfGame mapa) key1 1
          key2 = bot (decodeInfGameToString map1) 2 actTime
          map2 = aux_reageEvento map1 key2 2
          key3 = bot (decodeInfGameToString map2) 3 actTime
          map3 = aux_reageEvento map2 key3 3 


reageEvento (EventKey (Char 's')  Down _ _) estado@(infgame,size,seed,time,pict,actTime,wlargura,waltura,jogador) = 
  (map3,size,seed,time,pict,actTime,wlargura,waltura,jogador)
    where mapa = decodeInfGameToString(filterInfGame (move (decodeInfGameToString infgame) jogador 'D'))
          key1 = bot mapa 1 actTime
          map1 = aux_reageEvento (filterInfGame mapa) key1 1
          key2 = bot (decodeInfGameToString map1) 2 actTime
          map2 = aux_reageEvento map1 key2 2
          key3 = bot (decodeInfGameToString map2) 3 actTime
          map3 = aux_reageEvento map2 key3 3

reageEvento (EventKey (Char 'a')  Down _ _) estado@(infgame,size,seed,time,pict,actTime,wlargura,waltura,jogador) = 
  (map3,size,seed,time,pict,actTime,wlargura,waltura,jogador)
    where mapa = decodeInfGameToString(filterInfGame (move (decodeInfGameToString infgame) jogador 'L'))
          key1 = bot mapa 1 actTime
          map1 = aux_reageEvento (filterInfGame mapa) key1 1
          key2 = bot (decodeInfGameToString map1) 2 actTime
          map2 = aux_reageEvento map1 key2 2
          key3 = bot (decodeInfGameToString map2) 3 actTime
          map3 = aux_reageEvento map2 key3 3

reageEvento (EventKey (Char 'd') Down _ _) estado@(infgame,size,seed,time,pict,actTime,wlargura,waltura,jogador) = 
  (map3,size,seed,time,pict,actTime,wlargura,waltura,jogador)
    where mapa = decodeInfGameToString(filterInfGame (move (decodeInfGameToString infgame) jogador 'R'))
          key1 = bot mapa 1 actTime
          map1 = aux_reageEvento (filterInfGame mapa) key1 1
          key2 = bot (decodeInfGameToString map1) 2 actTime
          map2 = aux_reageEvento map1 key2 2
          key3 = bot (decodeInfGameToString map2) 3 actTime
          map3 = aux_reageEvento map2 key3 3

reageEvento (EventKey (Char 'b') Down _ _) estado@(infgame,size,seed,time,pict,actTime,wlargura,waltura,jogador) = 
  (map3,size,seed,time,pict,actTime,wlargura,waltura,jogador)
    where mapa = decodeInfGameToString(filterInfGame (move (decodeInfGameToString infgame) jogador 'B'))
          key1 = bot mapa 1 actTime
          map1 = aux_reageEvento (filterInfGame mapa) key1 1
          key2 = bot (decodeInfGameToString map1) 2 actTime
          map2 = aux_reageEvento map1 key2 2
          key3 = bot (decodeInfGameToString map2) 3 actTime
          map3 = aux_reageEvento map2 key3 3
reageEvento _ estado = estado

{-
reageEvento _ estado@(infgame,size,seed,time,pict,actTime,wlargura,waltura,jogador) = 
  (map3,size,seed,time,pict,actTime,wlargura,waltura,jogador) -- ignora qualquer outro evento
      where mapa = decodeInfGameToString(infgame)
            key1 = bot mapa 1 actTime
            map1 = aux_reageEvento (filterInfGame mapa) key1 1
            key2 = bot (decodeInfGameToString map1) 2 actTime
            map2 = aux_reageEvento map1 key2 2
            key3 = bot (decodeInfGameToString map2) 3 actTime
            map3 = aux_reageEvento map2 key3 3
-}
aux_reageEvento :: InfGame -> Maybe Char -> Int -> InfGame 
aux_reageEvento infgame key jogador | key == Nothing = infgame
                                    | key == Just 'U' = filterInfGame (move (decodeInfGameToString infgame) jogador 'U')
                                    | key == Just 'D' = filterInfGame (move (decodeInfGameToString infgame) jogador 'D')
                                    | key == Just 'L' = filterInfGame (move (decodeInfGameToString infgame) jogador 'L')
                                    | key == Just 'R' = filterInfGame (move (decodeInfGameToString infgame) jogador 'R')
                                    | key == Just 'B' = filterInfGame (move (decodeInfGameToString infgame) jogador 'B')

{-|
==Descrição:
A função __reageTempo__, modifica o estado do jogo com a passagem do tempo.

==Notas:
*Aqui usamos o somatório do frame rate, quando este é superior a 1, assumimos que passou 1 segundo, portanto podemos verificar as alterações no estado de jogo, com a passagem de 1 segundo, através da função __avanca__, definida na T4Lib.
-}
--  Função que altera o estado do jogo quando o tempo avança @n@ segundos.
reageTempo :: Float -> Estado -> Estado
reageTempo f s | time >= 1 = (filterInfGame (avanca (decodeInfGameToString mapa) actTime),size,seed,0,pict,(actTime-1),wlargura,waltura,jogador)
               | otherwise = (mapa,size,seed,time+f,pict,actTime,wlargura,waltura,jogador)
                    where (mapa,size,seed,time,pict,actTime,wlargura,waltura,jogador) = s

-- | Frame rate

fr :: Int
fr = 50

-- | Display mode

dm :: Display
dm = InWindow "Bomberman" (800,600) (458,159) --InWindow "Novo Jogo" (800, 600) (0, 0) 
    
-- | Função principal que invoca o jogo.

main :: IO ()
main = do inicio <- estadoInicial
          play dm              -- display mode
               (greyN 0.2)     -- côr do fundo da janela  (greyN 0.4)
               fr              -- frame rate
               inicio          -- estado inicial
               desenhaEstado   -- desenha o estado do jogo
               reageEvento     -- reage a um evento
               reageTempo      -- reage ao passar do tempo

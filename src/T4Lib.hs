{-| 
Module      : T4Lib
Description : Este módulo contém as funções para a realização da __Tarefa 4__, do projeto __Bomberman__. 
Copyright   : José Pereira <https://github.com/josepereira1>;


Este módulo contém as __funções__, da __Tarefa 4__, para a reação do jogo à passagem do tempo. Nesta Tarefa, verifica-se as alterações causadas pela passagem do tempo, em especial atenção, à explosão de bombas e a respetiva destruição.
-}
module T4Lib where 

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe
import T1Lib

{-|
==Descrição:
O type __InfGame__, separa a informação do mapa, num tuplo, para facilitar o acesso a diferentes informações.
-}
type InfGame = (Tabuleiro,[PowUp],[Bomba],Players)

{-|
==Descrição:
O tipo __PowUp__, contém dois construtores para selecionar qual o tipo de Power Up, e a sua posição.
-}
data PowUp = Bomb Posicao | Flame Posicao 
        deriving Show

{-|
==Descrição:
O tipo __Player__, contém dois construtores, Vivo e Morto, em que no caso de Vivo, indicamos ainda a sua posição, número de Power Ups bombs e flames.
-}
data Player = Vivo Posicao NumbBomb NumbFlame
            | Morto
        deriving (Show,Eq)

{-|
==Descrição:
O tipo __Bloco__, contém três construtores, que são os diferentes tipos de Blocos (/Pedra,Tijolo,Vazio/), que se encontam no Mapa do Jogo Bomberman.
-}
data Bloco = Pedra 
           | Tijolo 
           | Vazio 
        deriving (Show,Eq)
{-|
==Descrição:
O tipo __Direction__, contém 4 construtores, das diferetes direções possíveis do flame das bombas no Bomberman.
-}
data Direction = Cima | Baixo | Esquerda | Direita
      deriving Show 
{-|
O type __Players__, corresponde à lista de Jogadores.
-}
type Players = [Player]
{-|
O type __Tabuleiro__, corresponde à lista de __Blocos__, ou seja, o mapa.
-}
type Tabuleiro = [[Bloco]]
{-|
O type __Posicao__, corresponde às coordenadas no mapa.
-}
type Posicao = (Int,Int)
{-|
O type __LinhaTabuleiro__, corresponde a uma linha no mapa __Bomberman__.
-}
type LinhaTabuleiro = [Bloco]
{-|
O type __Bomba__, corresponde a um tuplo, com toda a informação necessária de uma bomba.
-}
type Bomba = (Posicao,Jogador,NumbFlame,Time)
{-|
O type __NumbBomb__, corresponde ao número de power ups __Bombs__.
-}
type NumbBomb = Int 
{-|
O type __Jogador__, corresponde ao número do jogador.
-}
type Jogador = Int
{-|
O type __NumbFlame, corresponde ao número de power ups __Flames__.
-}
type NumbFlame = Int
{-|
O type __Time__, corresponde ao tempo atual do jogo.
-}
type Time = Int
{-|
O type __Size__, corresponde ao tamanho do mapa.
-}
type Size = Int

{-|
== Descrição:
A função __bloco__, retorna-me o bloco que está na posição que eu indicar.

==Exemplos:
>>>bloco (filterInfGame m1) (0,0)
Pedra
>>>bloco (filterInfGame m1) (1,1)
Vazio

==Notas:
* m1 = ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 10","0 1 1","1 7 1","2 7 7","3 1 7"]
* A função __filterInfGame__, converte de [String], para o data __InfGame__.
-}

-- Retorna-me o Tabuleiro.
bloco :: InfGame -> Posicao -> Bloco
bloco (m,_,_,_) (x,y) = (m !! y) !! x 
{-|
==Descrição:
A função __powerUps__ retorna-me a lista de power ups que está no InfGame.

==Exemplos:
>>>powerUps (filterInfGame m1)
[Bomb (5,2),Bomb (3,3),Flame (5,5)]

==Notas:
* m1 = ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 10","0 1 1","1 7 1","2 7 7","3 1 7"]
* A função __filterInfGame__, converte de [String], para o data __InfGame__.
-}
-- Retorna-me a lista de PowerUps
powerUps :: InfGame -> [PowUp]
powerUps (_,p,_,_) = p
{-|
==Descrição:
A função __bombs__ retorna-me a lista de bombas que está no InfGame.

==Exemplos:
>>>bombs (filterInfGame m1)
[((2,7),0,1,10)]

==Notas:
* m1 = ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 10","0 1 1","1 7 1","2 7 7","3 1 7"]
* A função __filterInfGame__, converte de [String], para o data __InfGame__.

-}
-- Retorna-me a lista de bombas existentes no mapa.
bombs :: InfGame -> [Bomba]
bombs (_,_,b,_) = b

---------------------------------------------------------------------
---------------------------------------------------------------------
{-|
==Descrição:
A função __filterTabuleiro__, recebe a informação do jogo em [String] e retorna apenas a informação do Tabuleiro.

==Exemplos:
>>>filterTabuleiro m1
["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########"]

==Notas:
* m1 = ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 10","0 1 1","1 7 1","2 7 7","3 1 7"]
-}
-- Filtra a informação do Tabuleiro.
filterTabuleiro :: [String] -> [String]
filterTabuleiro m = filter (\x -> (head x) == '#') m 

{-|
== Descrição:
A função __filterPowUp__, recebe a informação do jogo em [String] e retorna apenas a informação dos Power Ups.

==Exemplos:
>>>filterPowUp m1
["+ 5 2","+ 3 3","! 5 5"]

==Notas:
* m1 = ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 10","0 1 1","1 7 1","2 7 7","3 1 7"]
-}
-- Filtra os Power Ups.
filterPowUp :: [String] -> [String]
filterPowUp m = filter (\x -> (head x) == '+' || (head x) == '!') m
{-|
==Descrição:
A função __filterBomb__, recebe a informação do jogo em [String] e retorna apenas a informação das Bombas

==Exemplos:
>>>filterBomb ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 10","0 1 1","1 7 1","2 7 7","3 1 7"]
["* 2 7 0 1 10"]
-}
-- Filtra as Bombas.
filterBomb :: [String] -> [String]
filterBomb m = filter (\x -> (head x) == '*') m
{-|
==Descrição:
A função __filterPlayer__, recebe a informação do jogo em [String] e retorna apenas a informação dos Jogadores.

==Exemplos:
>>>filterPlayer ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 10","0 1 1","1 7 1","2 7 7","3 1 7"]
["0 1 1","1 7 1","2 7 7","3 1 7"]
-}
-- Filtra os jogadores.
filterPlayer :: [String] -> [String]
filterPlayer m = filter (\x -> isDigit (head x))  m
{-|
==Descrição:
A função __convToTabuleiro__, converte o mapa de [String], para o tipo Tabuleiro = [[Bloco]].

==Exemplos: 
>>>convToTabuleiro (mapa 5 0)
[[Pedra,Pedra,Pedra,Pedra,Pedra],[Pedra,Vazio,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Pedra],[Pedra,Pedra,Pedra,Pedra,Pedra]]
>>>mapa 5 0
["#####","#   #","# # #","#   #","#####"]

==Notas:
* A função __mapa__, está definida na biblioteca T1Lib, esta gera Tabuleiros do Bomberman, em [String].
-}
--  Converte o mapa para os construtores que eu defini.
convToTabuleiro :: [String] -> Tabuleiro
convToTabuleiro [] = []
convToTabuleiro (h:t) = (auxconvToTabuleiro h):convToTabuleiro t

auxconvToTabuleiro :: String -> LinhaTabuleiro
auxconvToTabuleiro [] = []
auxconvToTabuleiro (h:t) | h == '#' = Pedra:auxconvToTabuleiro t
                         | h == '?' = Tijolo:auxconvToTabuleiro t
                         | h == ' ' = Vazio:auxconvToTabuleiro t
{-|
==Descrição:
A função __convToPowUp__, converte a informação dos Power Ups de um mapa, para o tipo __PowUp__.

==Exemplos:
>>>convToPowUp ["+ 5 2","+ 3 3","! 5 5"]
[Bomb (5,2),Bomb (3,3),Flame (5,5)]
-}
-- Converte os Power Ups para a lista PowUp utilizando o tipo PowUp.
convToPowUp :: [String] -> [PowUp]
convToPowUp [] = []
convToPowUp (h:t) = (auxConvToPowUp h):convToPowUp t 

auxConvToPowUp :: String -> PowUp
auxConvToPowUp p | head x1 == '+' = Bomb (read x2 :: Int, read x3 :: Int)
                 | head x1 == '!' = Flame (read x2 :: Int, read x3 :: Int) 
                    where (x1:x2:x3:t) = words p 
{-|
==Descrição:
A função __convToBomb__, converte a informação das Bombas de um mapa, para o type __Bomba__, que facilita a pesquisa de informação das Bombas.

==Exemplos:
>>>convToBomb ["* 2 7 0 1 10"]
[((2,7),0,1,10)]
-}
--  Converte as Bombas para a lista de Bomb.
convToBomb :: [String] -> [Bomba]
convToBomb [] = []
convToBomb (h:t) = (auxConvToBomb h):convToBomb t

auxConvToBomb :: String -> Bomba
auxConvToBomb b = ((read x2 :: Int,read x3 :: Int),read x4 :: Int,read x5 :: Int,read x6 :: Int)
                    where (x1:x2:x3:x4:x5:x6:t) = words b
{-|
==Descrição:
A função __convToPlayer__, converte a informação dos Jogadores, para o tipo __Player__, que neste caso como é uma lista, usamos o type Players = [Player].

==Exemplos:
>>>convToPlayer ["0 1 1 +!","1 7 1","3 1 7 ++!!!"]
[Vivo (1,1) 1 1,Vivo (7,1) 0 0,Morto,Vivo (1,7) 2 3]

==Notas:
*Como podemos ver no exemplo, o jogador número 2, não existe, logo está Morto, aqui podemos ver a importância da utilização do data __Player__.
-}
--  Converte os Jogadores no tuplo de 4 elementos do tipo Player.
convToPlayer :: [String] -> Players
convToPlayer [] = [Morto,Morto,Morto,Morto]
convToPlayer (h:[]) | head h == '0' = [auxConvToPlayer h,Morto,Morto,Morto]
                    | head h == '1' = [Morto,auxConvToPlayer h,Morto,Morto]
                    | head h == '2' = [Morto,Morto,auxConvToPlayer h,Morto]
                    | head h == '3' = [Morto,Morto,Morto,auxConvToPlayer h]
convToPlayer (h:t) | head h == '0' = [px,p1,p2,p3]
                   | head h == '1' = [p0,px,p2,p3]
                   | head h == '2' = [p0,p1,px,p3]
                   | head h == '3' = [p0,p1,p2,px]
                      where [p0,p1,p2,p3] = convToPlayer t 
                            px = auxConvToPlayer h   

auxConvToPlayer :: String -> Player
auxConvToPlayer [] = Morto
auxConvToPlayer p = Vivo (read x2 :: Int, read x3 :: Int) bombs flames
                      where (x1:x2:x3:t) = words p
                            (bombs,flames) = counterBmbFlm (drop 3 p)
{-|
==Descrição:
A função __counterBmbFlm__, conta quantos Power Ups Bombs e Flames,um determinado, jogador tem.

==Exemplos:
>>>counterBmbFlm "3 1 7 ++!!!"
(2,3)
>>>counterBmbFlm "3 1 7"
(0,0)
>>>counterBmbFlm "3 1 7 !"
(0,1)

==Notas:
*Em que o primeiro elemento do tuplo, refere-se ao número de Power Up Bombs e o segundo aos Flames.
-}
--  Retorna-me o número de power ups bombs e flames de um jogador.
counterBmbFlm :: String -> (Int,Int)
counterBmbFlm [] = (0,0)
counterBmbFlm (h:t) | h == '!' = (bombs,1+flames)
                    | h == '+' = (1+bombs,flames) 
                    | otherwise = (bombs,flames)
                        where (bombs,flames) = counterBmbFlm t
{-|
== Descrição:
A função __filterInfGame__, conjuga, todas as funções (__convToTabuleiro__,__convToPowUp__,__convToBomb__,__convToPlayer__), para que seja possível, converter um mapa no tipo [String] para o tuplo de type InfGame.

@
filterInfGame m =
    (convToTabuleiro (filterTabuleiro m), convToPowUp (filterPowUp m), convToBomb (filterBomb m), convToPlayer (filterPlayer m))
@

==Exemplos:
>>>filterInfGame m1
([[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Tijolo,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra],[Pedra,Vazio,Tijolo,Tijolo,Vazio,Tijolo,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra]],[Bomb (5,2),Bomb (3,3),Flame (5,5)],[((2,7),0,1,10)],[Vivo (1,1) 0 0,Vivo (7,1) 0 0,Vivo (7,7) 0 0,Vivo (1,7) 0 0])
>>>filterInfGame (mapa 5 0)
([[Pedra,Pedra,Pedra,Pedra,Pedra],[Pedra,Vazio,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Pedra],[Pedra,Pedra,Pedra,Pedra,Pedra]],[],[],[Morto,Morto,Morto,Morto])

==Notas:
* m1 = ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 10","0 1 1","1 7 1","2 7 7","3 1 7"]
* A função __mapa__, está definida na biblioteca T1Lib, esta gera Tabuleiros do Bomberman, em [String].
* As funções __filterTabuleiro__, __filterPowUp__,__filterBomb__,__filterPlayer__, separam as respectivas partes do mapa inicial, ou seja, separa a informação do mapa, power ups, bombas e jogadores.
-}
--  Filtra toda a informação do mapa no type InfGame, com todas as informações necessárias separadas.
filterInfGame :: [String] -> InfGame
filterInfGame m =
    (convToTabuleiro (filterTabuleiro m), convToPowUp (filterPowUp m), convToBomb (filterBomb m), convToPlayer (filterPlayer m)) 
{-|
==Descrição:
A função __checkPosBloco__, substitui um bloco na posição que recebe como parâmetro.

==Exemplos:
>>>checkPosBloco (filterInfGame m1) (1,1) Tijolo
([[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra],[Pedra,Tijolo,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Tijolo,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra],[Pedra,Vazio,Tijolo,Tijolo,Vazio,Tijolo,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra]],[Bomb (5,2),Bomb (3,3),Flame (5,5)],[((2,7),0,1,10)],[Vivo (1,1) 0 0,Vivo (7,1) 0 0,Vivo (7,7) 0 0,Vivo (1,7) 0 0])
>>>checkPosBloco (filterInfGame m1) (0,0) Vazio
([[Vazio,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Tijolo,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra],[Pedra,Vazio,Tijolo,Tijolo,Vazio,Tijolo,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra]],[Bomb (5,2),Bomb (3,3),Flame (5,5)],[((2,7),0,1,10)],[Vivo (1,1) 0 0,Vivo (7,1) 0 0,Vivo (7,7) 0 0,Vivo (1,7) 0 0])

==Notas:
* m1 = ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 10","0 1 1","1 7 1","2 7 7","3 1 7"]
* A função __filterInfGame__, converte de [String], para o data __InfGame__.
-}
--  Esta função remove e adiciona um novo bloco ao mapa.
checkPosBloco :: InfGame -> Posicao -> Bloco -> InfGame
checkPosBloco (m,p,b,j) (c,l) bloco = ((removeBlocoMap m (c,l) bloco (0,0)),p,b,j)

removeBlocoMap :: Tabuleiro -> Posicao -> Bloco -> Posicao -> Tabuleiro
removeBlocoMap [] (c,l) bloco (caux,laux) = []
removeBlocoMap (h:t) (c,l) bloco (caux,laux) | l == laux = (removeBlocoLinhaMap h (c,l) (caux,laux) bloco):t
                                             | otherwise = h:removeBlocoMap t (c,l) bloco (caux,laux+1)

removeBlocoLinhaMap :: LinhaTabuleiro -> Posicao -> Posicao -> Bloco -> LinhaTabuleiro
removeBlocoLinhaMap (h:t) (c,l) (caux,laux) b | c == caux = b:t
                                              | otherwise = h:removeBlocoLinhaMap t (c,l) (caux+1,laux) b
{-|
==Descrição:
A função __checkPosPowerUp__, verifica se existe um Power Up na posição indicada e caso exista retorna True e a lista dos Power Ups sem esse Power Up, ou seja, remove-o, caso contrário, retorna False e a lista dos Power Ups completa. 

==Exemplos:
>>>checkPosPowerUp [Bomb (5,2),Bomb (3,3),Flame (5,5)] (5,2)
(True,[Bomb (3,3),Flame (5,5)])
>>>checkPosPowerUp [Bomb (5,2),Bomb (3,3),Flame (5,5)] (5,4)
(False,[Bomb (5,2),Bomb (3,3),Flame (5,5)])
-}
--  Esta função determina se existe um power up na posição, caso contrário, retorna Empty.
checkPosPowerUp :: [PowUp] -> Posicao -> (Bool,[PowUp])
checkPosPowerUp [] (_,_) = (False,[])
checkPosPowerUp ((Bomb (c,l)):t) (x,y) | c == x && l == y = (True,t) 
                                       | otherwise = (delete,(Bomb (c,l)):pwUps)
                                          where (delete,pwUps) = checkPosPowerUp t (x,y)
checkPosPowerUp ((Flame (c,l)):t) (x,y) | c == x && l == y = (True,t)
                                        | otherwise = (delete,(Flame (c,l)):pwUps)
                                          where (delete,pwUps) = checkPosPowerUp t (x,y)
{-|
==Descrição:
A função __checkPosPlayer__, verifica se existe um jogador na posição indicada, caso exista ele vai Morrer, porque foi atingido por uma bomba. Portanto se ele for atingido, retornamos True e a lista dos jogadores, com este Morto, caso contrário, retornamos False e a lista dos jogadores tal como a recebemos.

==Exemplos:
>>>checkPosPlayer [Vivo (1,1) 0 0,Vivo (7,1) 0 0,Vivo (7,7) 0 0,Vivo (1,7) 0 0] (1,1)
(True,[Morto,Vivo (7,1) 0 0,Vivo (7,7) 0 0,Vivo (1,7) 0 0])
>>>checkPosPlayer [Vivo (1,1) 0 0,Vivo (7,1) 0 0,Vivo (7,7) 0 0,Vivo (1,7) 0 0] (7,8)
(False,[Vivo (1,1) 0 0,Vivo (7,1) 0 0,Vivo (7,7) 0 0,Vivo (1,7) 0 0])

-}
--  Esta função determina se existe um jogador na posição e caso exista, ele "morre", pois quer dizer que foi atingido por uma bomba.
checkPosPlayer :: Players -> Posicao -> (Bool,Players)
checkPosPlayer [] _ = (False,[])
checkPosPlayer (h:t) (c,l) | h /= Morto && c == x && l == y = (True,Morto:ply)
                           | otherwise = (dead,h:ply)
                             where (Vivo (x,y) b f) = h
                                   (dead,ply) = checkPosPlayer t (c,l)  
{-|
==Descrição:
A função __checkPosBomb__, determina se existe uma bomba na posição indicada caso exista, retorna True e a bomba com o tempo de explosão = 1, para que possa expludir no próximo instante, se não for atingida, retorna False e a lista das bombas tal como a recebeu.

==Exemplos:
>>>checkPosBomb [((2,7),0,1,10)] (2,7)
(True,[((2,7),0,1,1)])
>>>checkPosBomb [((2,7),0,1,10)] (2,1)
(False,[((2,7),0,1,10)])

-}
--  Esta função verifica se existe uma bomba na respectiva posição e caso exista, retorna True e a bomba com o tempo de explosão = 1, para que possa expludir no próximo instante. 
checkPosBomb :: [Bomba] -> Posicao -> (Bool,[Bomba])
checkPosBomb [] (x,y) = (False,[])
checkPosBomb (((c,l),j,f,t):xs) (x,y) | c == x && l == y = (True,((c,l),j,f,1):xs)
                                      | otherwise = (active,((c,l),j,f,t):bmsMap)
                                        where (active,bmsMap) = checkPosBomb xs (x,y)

{-|
==Descrição:
A função __detRangeCoords__, determina o range de explosão de uma bomba, retornando apenas as coordenadas que são afetadas pelo flame da bomba.

==Exemplos:
>>>detRangeCoords ((2,7),0,3,1) 9 ([[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Tijolo,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra],[Pedra,Vazio,Tijolo,Tijolo,Vazio,Tijolo,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra]],[Bomb (5,2),Bomb (3,3),Flame (5,5)],[((2,7),0,3,1)],[Vivo (1,1) 0 2,Vivo (7,1) 0 0,Vivo (7,7) 0 0,Vivo (1,7) 0 0])
[(2,7),(1,7),(3,7),(4,7),(5,7)]

==Notas:
*O flame da Bomba não ultrapassa pedras, no caso dos Tijolos e Power Ups , não são ultrapassados, mas são destruídos, e tudo isso é avaliado nesta função.
-}
-- Esta função determina range de uma determinada bomba, ou seja, as coordenadas.
-- Aqui remove-se logo as coordenadas que tem pedras e as imediatamente a seguir, pois as pedras não permitem que o flame tansponha.Também quando a coordenada contém um tijolo, guardamos essa coordenada e removemos as próximas visto que o tijlo não permite a passagem do flame.
detRangeCoords :: Bomba -> Size -> InfGame -> [Posicao]
detRangeCoords ((c,l),j,f,t) size infgame = 
  (c,l):(direction ((c,l-1),j,f,t) f size infgame Cima )++(direction ((c,l+1),j,f,t) f size infgame Baixo)++(direction ((c-1,l),j,f,t) f size infgame Esquerda)++(direction ((c+1,l),j,f,t) f size infgame Direita)
   where 
          direction ((c,l),j,f,t) rangeFlame size infgame dir 
            | rangeFlame <= 0 = []
            | blc == Pedra && rangeFlame > 0 = []
            | blc == Tijolo && rangeFlame > 0 = (c,l):[] 
            | exist = (c,l):[]
            | rangeFlame > 0 = (c,l):(direction ((detCoords dir (c,l)),j,f,t) (rangeFlame-1) size infgame dir)
            | otherwise = []
              where blc = bloco infgame (c,l) 
                    (exist,pwUps) = checkPosPowerUp p (c,l)
                    p = powerUps infgame
{-|
==Descrição:
A função __detCoords__, determina através de um constructor de __Direction__ qual o tipo de direção a aplicar às coordenadas, retornando assim uma nova Posição.

==Exemplos:
>>>detCoords Cima (1,2)
(1,1)
>>>detCoords Baixo (1,2)
(1,3)
>>>detCoords Esquerda (1,2)
(0,2)
-}
--  Esta função determina através de um constructor de Direction qual o tipo de direção a aplicar.
detCoords :: Direction -> Posicao -> Posicao
detCoords Cima (c,l) = (c,l-1)
detCoords Baixo (c,l) = (c,l+1)
detCoords Esquerda (c,l) = (c-1,l)
detCoords Direita (c,l) = (c+1,l) 

{-|
==Descrição:
A função __checkConsequences__, recebe a lista das posições afetadas pelo flame de um bomba e determina as consequências da explosão, ou seja, a morte de jogadores, destruição de tijolos, power ups e ativação de bombas, retornando o mapa com as alterações feitas.
-}
--  Esta função determina as consequências da explosão de uma bomba, ou seja, a morte de jogadores, destruição de tijolos, power ups e ativação de bombas.
checkConsequences :: [Posicao] -> InfGame -> InfGame
checkConsequences [] infgame = infgame
checkConsequences ((c,l):t) infgame 
  | blc == Tijolo = checkConsequences t (checkPosBloco infgame (c,l) Vazio)
  | dead = checkConsequences t (m,p,b,ply)
  | delete = checkConsequences t (m,pwUps,b,j)
  | active = checkConsequences t (m,p,bmsMap,j)
  | otherwise = checkConsequences t infgame
    where blc = bloco infgame (c,l)
          (dead,ply) = checkPosPlayer j (c,l)
          (delete,pwUps) = checkPosPowerUp p (c,l)
          (active,bmsMap) = checkPosBomb b (c,l)
          (m,p,b,j) = infgame
{-|
==Descrição:
A função __collectsBombs__, separa as bombas que vão explodir neste instante de tempo das que vão explodir mais tarde, colocando estas com -1 segundo do tempo inicial.

==Exemplos:
>>>collectsBombs [((2,7),0,1,10),((1,7),1,1,1)]
([((1,7),1,1,1)],[((2,7),0,1,9)])
>>>collectsBombs [((2,7),0,1,10),((1,7),1,1,2)]
([],[((2,7),0,1,9),((1,7),1,1,1)])
>>>collectsBombs [((2,7),0,1,1),((1,7),1,1,1)]
([((2,7),0,1,1),((1,7),1,1,1)],[])

==Notas:
*As bombas que não explodem neste instante, ficam com menos um segundo do tempo inicial.
-}
--  Esta função separa-me as bombas que vão explodir neste instante de tempo das que vão explodir mais tarde, colocando estas com -1 segundo do tempo inicial.
collectsBombs :: [Bomba] -> ([Bomba],[Bomba])
collectsBombs [] = ([],[])
collectsBombs (x:xs) 
  | t == 1 = (x:exploded,active)
  | otherwise = (exploded,((c,l),j,f,(t-1)):active)
    where (exploded,active) = collectsBombs xs
          ((c,l),j,f,t) = x 

{-|
==Descrição:
A função __rangeExplosion__, determina todas as posições que irão ser afetadas por bombas.

==Exemplos:
>>>rangeExplosion (filterInfGame m1) [((2,7),0,1,1),((1,7),1,1,1)]
[(2,7),(1,7),(3,7),(1,7),(1,6),(2,7)]

==Notas:
* m1 = ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 1","* 1 7 1 1 1","0 2 7","1 7 1","2 7 7","3 1 7"]
* A função __filterInfGame__, converte de [String], para o data __InfGame__.
-}
--  Esta função retorna-me as coordenadas do range da explosão de todas as bombas.
rangeExplosion :: InfGame -> [Bomba] -> [Posicao]
rangeExplosion infgame [] = [] 
rangeExplosion infgame@(m,p,b,j) (h:t) = 
  (detRangeCoords h (length (head m)) infgame)++rangeExplosion infgame t

{-|
==Descrição:
A função __explosionBombs__, verifica e faz as alterações no mapa, causadas pela explosão de bombas.

==Exemplos:
>>>explosionBombs (filterInfGame m1)
([[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Tijolo,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra],[Pedra,Vazio,Tijolo,Tijolo,Vazio,Tijolo,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra]],[Bomb (5,2),Bomb (3,3),Flame (5,5)],[],[Morto,Vivo (7,1) 0 0,Vivo (7,7) 0 0,Morto])

==Notas:
*Como podemos ver no exemplo, as bombas causaram a morte ao jogador 0 e 3, são estas algumas das consequências.
* m1 = ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 1","* 1 7 1 1 1","0 2 7","1 7 1","2 7 7","3 1 7"]
* A função __filterInfGame__, converte de [String], para o data __InfGame__.
-}
explosionBombs :: InfGame -> InfGame
explosionBombs infgame@(m,p,b,j) = 
  checkConsequences (rangeExplosion (m,p,active,j) exploded) (m,p,active,j)
  where (exploded,active) = collectsBombs b
{-|
==Descrição:
A função __decodeInfGameToString__, converte o mapa do tipo __InfGame__, para [String].

==Exemplos:
>>>decodeInfGameToString (filterInfGame (mapa 9 0))
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]
>>>mapa 9 0
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]

==Notas:
* A função __filterInfGame__, converte de [String], para o data __InfGame__.

-}
--  Esta função converte a informação do jogo para [String].
decodeInfGameToString :: InfGame -> [String]
decodeInfGameToString (m,p,b,j) = 
  (decodeTabuleiro m)++(decodePowerUps p)++(decodeBombas b)++(decodePlayers j)
{-|
==Descrição:
A função __decodeTabuleiro__, converte a informação do tabuleiro do tipo data __Bloco__, para [String].
-}
--  Esta função converte a informação do mapa do tipo Data Bloco, para [String].
decodeTabuleiro :: Tabuleiro -> [String]
decodeTabuleiro [] = []
decodeTabuleiro (h:t) = 
  (decodeLinhaTabuleiro h):decodeTabuleiro t 
    where
      decodeLinhaTabuleiro [] = []
      decodeLinhaTabuleiro (h:t) | h == Pedra = '#':decodeLinhaTabuleiro t
                                 | h == Tijolo = '?':decodeLinhaTabuleiro t
                                 | h == Vazio = ' ':decodeLinhaTabuleiro t 
{-|
==Descrição:
A função __decodePowerUps__, converte a informação dos Power Ups do tipo data __PowUp__, para [String].
-}
--  Esta função descodifica a informação dos power ups do tipo data PowUp, para [String].
decodePowerUps :: [PowUp] -> [String]
decodePowerUps [] = []
decodePowerUps (h:t) =
  (decodePowerUp h):decodePowerUps t 
    where decodePowerUp (Bomb (c,l)) = "+ "++(show c)++" "++(show l)
          decodePowerUp (Flame (c,l)) = "! "++(show c)++" "++(show l) 
{-|
==Descrição:
A função __decodeBombas__, converte a informação das Bombas do tipo data __[Bomba]__, para [String].
-}
--  Esta função descodifica a informação das bombas para [String].
decodeBombas :: [Bomba] -> [String]
decodeBombas [] = []
decodeBombas (h:t) = (decodeBomba h):decodeBombas t 
  where decodeBomba ((c,l),gm,f,t) = "* "++(show c)++" "++(show l)++" "++(show gm)++" "++(show f)++" "++(show t)
{-|
==Descrição:
A função __decodePlayers__, converte a informação dos Jogadores do tipo data __Players__, para [String].
-}
--  Esta função descodifica a informação dos Jogadores do tipo Data Player, para [String].
decodePlayers :: Players -> [String]
decodePlayers [j0,j1,j2,j3] = removeMortos ((decodePlayer j0 "0"):(decodePlayer j1 "1"):(decodePlayer j2 "2"):(decodePlayer j3 "3"):[])

  where decodePlayer (Vivo (c,l) b f) num
          | b > 0 || f > 0 = num++" "++(show c)++" "++(show l)++" "++(replicate b '+')++(replicate f '!')
          | otherwise = num++" "++(show c)++" "++(show l)
        decodePlayer (Morto) _ = []
        removeMortos [] = []
        removeMortos (h:t) | h == "" = removeMortos t 
                           | otherwise = h:removeMortos t
{-|
==Descrição:
A função __drawSpiral__, desenha a expiral, consoante o tempo de jogo, colocando uma pedra na posição destinada, e destruindo tudo o que estiver nessa posição, retornado assim o mapa com todas as alterações causadas pela expiral.

==Exemplos:
>>>drawSpiral (filterInfGame m1) 49
([[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra],[Pedra,Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Tijolo,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra],[Pedra,Vazio,Tijolo,Tijolo,Vazio,Tijolo,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra]],[Bomb (5,2),Bomb (3,3),Flame (5,5)],[],[Morto,Vivo (7,1) 0 0,Vivo (7,7) 0 0,Vivo (1,7) 0 0])

==Notas:
*Como podemos ver no exemplo, caiu a primeira pedra da expiral, na posição (1,1) e matou o jogador 0.
*m1 = ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 1","* 1 7 1 1 1","0 2 7","1 7 1","2 7 7","3 1 7"]
* A função __filterInfGame__, converte de [String], para o data __InfGame__.
-}
drawSpiral :: InfGame -> Time -> InfGame 
drawSpiral infgame@(m,p,b,j) time = destructionSpiral ((posSpiral size) !! (startSpiral-time)) (checkPosBloco infgame ((posSpiral size) !! (startSpiral-time)) Pedra) 
  where size = length (head m)
        startSpiral = (size-2)^2
{-|
==Descrição:
A função __checkSpiral__, verifica se o tempo recebido, é menor ou igual ao tempo de ínicio da expiral.

==Exemplos:
>>>checkSpiral m1 49
True
>>>checkSpiral m1 50
False
>>>checkSpiral m1 25
True

==Notas:
*No mapa de tamanho 9, ou seja, como o mapa m1, o tempo do ínicio da Expiral é aos 49 segundos.
*m1 = ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 1","* 1 7 1 1 1","0 2 7","1 7 1","2 7 7","3 1 7"]
-}
--  Verifica se o tempo recebido, corresponde ao ínicio de espiral.
checkSpiral :: [String] -> Time -> Bool
checkSpiral (x:xs) t | t <= (((length x)-2)^2) = True 
                     | otherwise = False
{-|
==Descrição:
A função __destructionSpiral__, determina a destruição causada pela pedra que cai do efeito expiral, podendo destruir um tijolo, uma bomba, ou até matar um jogador.

==Exemplos:
>>>destructionSpiral (1,1) (filterInfGame m1)
([[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra],[Pedra,Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Tijolo,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra],[Pedra,Vazio,Tijolo,Tijolo,Vazio,Tijolo,Vazio,Vazio,Pedra],[Pedra,Vazio,Pedra,Vazio,Pedra,Tijolo,Pedra,Vazio,Pedra],[Pedra,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Pedra],[Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra,Pedra]],[Bomb (5,2),Bomb (3,3),Flame (5,5)],[],[Morto,Vivo (7,1) 0 0,Vivo (7,7) 0 0,Vivo (1,7) 0 0])

==Notas:
*Como podemos ver no exemplo, caiu a primeira pedra da expiral, na posição (1,1) e matou o jogador 0.
*m1 = ["#########","#       #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 2 7 0 1 1","* 1 7 1 1 1","0 2 7","1 7 1","2 7 7","3 1 7"]
* A função __filterInfGame__, converte de [String], para o data __InfGame__.
-}
destructionSpiral :: Posicao -> InfGame -> InfGame
destructionSpiral (c,l) infgame@(m,p,b,j)
  = (m,pwUps,listaBombas,ply)
  where (dead,ply) = checkPosPlayer j (c,l)
        (delete,pwUps) = checkPosPowerUp p (c,l)
        listaBombas = removeBomb b (c,l)
{-|
==Descrição:
A função __removeBomb__, remove uma bomba que esteja na posição indicada, que neste caso será a posição onde irá cair um pedra da expiral.

==Exemplos:
>>>removeBomb [((7,6),0,1,8),((7,7),1,3,1)] (7,6)
[((7,7),1,3,1)]
>>>removeBomb [((7,6),0,1,8),((7,7),1,3,1)] (1,1)
[((7,6),0,1,8),((7,7),1,3,1)]

==Notas:
*Se não existir nenhuma bomba na posição indicada, a função retorna a lista das Bombas tal e qual como recebeu.
-}
--  Esta função remove, caso exista a bomba que está na posição que entrar como parâmetro.
removeBomb :: [Bomba] -> Posicao -> [Bomba]
removeBomb [] _ = []
removeBomb (h:t) (c,l) | c == x && l == y = t
                       | otherwise = h:removeBomb t (c,l) 
                          where ((x,y),j,f,time) = h
{-|
==Descrição:
A função __posSpiral__, determina a lista das posições da expiral, pela ordem que ela ocorre, ou seja, com o efeito expiral, assim só será preciso saber onde é que a pedra tem que cair, como se trata de uma lista, utilizamos as posições da mesma.

==Exemplos:
>>>posSpiral 9
[(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7),(6,7),(5,7),(4,7),(3,7),(2,7),(1,7),(1,6),(1,5),(1,4),(1,3),(1,2),(2,2),(3,2),(4,2),(5,2),(6,2),(6,3),(6,4),(6,5),(6,6),(5,6),(4,6),(3,6),(2,6),(2,5),(2,4),(2,3),(3,3),(4,3),(5,3),(5,4),(5,5),(4,5),(3,5),(3,4),(4,4)]

==Notas:
*O único parâmetro da função é o tamanho do mapa.

-}
--  Esta função, através do tamanho do mapa, retorna-me todas as posições da espiral.
posSpiral :: Size -> [Posicao]
posSpiral size = aux_posSpiral (1,1) size 0 2 
aux_posSpiral (c,l) size flag prof
  | c == (size `div` 2) && l == (size `div` 2) = (c,l):[]
  | flag == 0 && c == (size-prof) = (c,l):(aux_posSpiral (c,l+1) size 1 prof)
  | flag == 0 = (c,l):aux_posSpiral (c+1,l) size flag prof
  | flag == 1 && l == (size-prof) = (c,l):(aux_posSpiral (c-1,l) size 2 prof)  
  | flag == 1 = (c,l):aux_posSpiral (c,l+1) size flag prof
  | flag == 2 && c == (prof-1) = (c,l):(aux_posSpiral (c,l-1) size 3 prof)
  | flag == 2 = (c,l):aux_posSpiral (c-1,l) size flag prof
  | flag == 3 && l == (prof-1) = aux_posSpiral (c+1,l+1) size 0 (prof+1)
  | flag == 3 = (c,l):(aux_posSpiral (c,l-1) size flag prof)
{-|
==Descrição:
A função __avanca__, recebe o estado de jogo e o tempo, e retorna o estado de jogo, com todas as alterações afetadas pela passagem tempo, ou seja, pode ocrrer explosão de bombas, ou simplesmente, a redução de 1 segunda nas bombas.

==Exemplo:
>>>avanca m1 49
["#########","##      #","# # #?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 4 1 0 1 1","1 7 1","2 7 7","3 1 7"]

==Notas:
*No exemplo podemos ver que a passagem de 1 segundo, matou um jogador, destrui-o um tijolo e caiu um pedra na posição (1,1).
*m1 = ["#########","#       #","# #?#?# #","#     ? #","# # # #?#","# ?? ?  #","# # #?# #","#       #","#########","+ 5 2","+ 3 3","! 5 5","* 3 1 1 1 1","* 4 1 0 1 10","0 2 1","1 7 1","2 7 7","3 1 7"]
-}
avanca :: [String] -> Int -> [String]
avanca infgame time = aux_avanca infgame time 0
aux_avanca infgame time flag 
   | flag == 0 && spiral = aux_avanca (decodeInfGameToString ((drawSpiral (filterInfGame infgame) time))) time 1
   | otherwise = decodeInfGameToString (explosionBombs (filterInfGame infgame))
      where spiral = checkSpiral infgame time
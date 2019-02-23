{-| 
Module      : T1Lib
Description : Este módulo contém as funções para a realização da __Tarefa 1__, do projeto __Bomberman__. 
Copyright   : José Pereira <https://github.com/josepereira1>;


Este módulo contém as __funções__, da __Tarefa 1__, para a criação de mapas do jogo __Bomberman__.
-}
module T1Lib where

import System.Environment
import Text.Read
import Data.Maybe
import System.Random

{-|
O type __Powerup__, corresponde à informação num tuplo de um __Power Up__.
-}
type Powerup = (Char,Int,Int)
{-|
O type __LinhaMapa__, corresponde a uma linha do mapa __Bomberman.
-}
type LinhaMapa = String
{-|
O type __Mapa__, corresponde ao mapa do __Bomberman__, que consiste numa lista de __LinhaMapa__.
-}
type Mapa = [LinhaMapa]
{-|
O type __Resto_InfRandom__, corresponde ao resto da informação do mapa.
-}
type Resto_InfRandom = String
{-|
O type __Inf_Mapa__, corresponde à informação do mapa.
-}
type Inf_Mapa = (LinhaMapa,[Powerup],Resto_InfRandom)

{-|
==Descrição:
A função __num_celulas__, determina o número de células que cada mapa irá ter para informação de power ups, tijolos ou simplesmente espaços vazios.

==Exemplo:
>>>num_celulas 9 
28

-}
num_celulas :: Int -> Int
num_celulas 5 = 8
num_celulas tam = (tam^2) - ((tam * 4) - 4) - 12 - (pilares_pedra tam)

{-|
==Descrição:
A função __pilares_pedra__ e a respectiva auxiliar, calculam o número de pilares de pedra, ou seja, as pedras que se encontram na linhas e colunas pares.

==Exemplos:
>>>pilares_pedra 9 
9
-}

pilares_pedra :: Int -> Int
pilares_pedra 5 = 1 
pilares_pedra tam = aux_pilares_pedra 5  1 tam
aux_pilares_pedra :: Int -> Int -> Int -> Int
aux_pilares_pedra tamanho_aux referencia tam | tamanho_aux == tam = referencia^2
                                             | otherwise = aux_pilares_pedra (tamanho_aux + 2) (referencia + 1) tam    

{-|
==Descrição:
A função __numeros_Random__, determina um número de forma aleatória para cada célula do mapa, excepto as células vazias e as que tem pedra.

==Exemplo:
>>>numeros_Random 9 0
[83,93,63,38,0,87,81,1,61,86,13,50,32,80,54,25,90,31,65,92,2,76,70,25,6,29,10,99]
-}
 
numeros_Random :: Int -> Int -> [Int]                                               
numeros_Random tam seed = take (num_celulas tam) $ (randomRs (0,99) (mkStdGen seed)) 


{-| 
A forma como se determina o conteúdo de uma célula a partir do respectivo número aleatório é a seguinte:

*0 a 1 -> /Power up Bombs/ escondido atrás de um tijolo.
*2 a 3 -> /Power up Flames/ escondido atrás de um tijolo.
*4 a 39 -> Tijolo.
*40 a 99 -> vazio 

@
determina_Char [] = [] 
determina_Char (h:t) | h >= 0 && h <= 1 = '+':determina_Char t 
                     | h >= 2 && h <= 3 = '!':determina_Char t 
                     | h >= 4 && h <= 39 = '?':determina_Char t 
                     | h >= 40 && h <= 99 = ' ':determina_Char t 
@

==Descrição:
A função __determina_Char__, recebe uma lista do tipo __Int__, e através dos intervalos propostos no Guião do Projecto, obtêm-se uma lista de caracteres, em que temos '+' -> powerUps Bombs // '!' -> powerUps Flames // ' ' -> espaço // '?' -> tijolo. 

==Exemplo:
>>>determina_Char [83,93,63,38,0,87,81,1,61,86,13,50,32,80,54,25,90,31,65,92,2,76,70,25,6,29,10,99]
"   ?+  +  ? ?  ? ?  !  ???? "

-}

determina_Char :: [Int] -> [Char]
determina_Char [] = [] 
determina_Char (h:t) | h >= 0 && h <= 1 = '+':determina_Char t 
                     | h >= 2 && h <= 3 = '!':determina_Char t 
                     | h >= 4 && h <= 39 = '?':determina_Char t 
                     | h >= 40 && h <= 99 = ' ':determina_Char t 

{-|
==Descrição:
A função __determina_PowerUp__, recebe um caracter e determina se este é um powerUp, ou seja, se é igual a '+' ou '!' e retorna True se for PowerUp e False caso contrário.

==Exemplo:
>>>determina_PowerUp '+'
True

-}

determina_PowerUp :: Char -> Bool
determina_PowerUp h | h == '+'|| h == '!' = True
                    | otherwise = False

{-|
==Descrição:
A função __linhaCimaBaixo__ gera a parede superior e inferior do mapa, ou seja, quando é tudo pedra.

==Exemplo:
>>>linhaCimaBaixo 9
"#########"

*Usamos como base o mapa de dimensão 5 em que temos "#####".

-}

linhaCimaBaixo :: Int -> String
linhaCimaBaixo 5 = "#####"
linhaCimaBaixo tam | tam > 5 = "##"++linhaCimaBaixo (tam-2)  

{-|
==Descrição:
A função __sPilares_normal__ constrói as linhas em que não temos pilares, ou seja, pedras no meio e que não tem as células vazias para os jogadores se movimentar no ínicio do jogo.

==Exemplo:
>>>sPilares_normal 9 3 0 "?+  +  ? ?  ? ?  !  ???? "
("#??     #",('+',2,3),"? ?  ? ?  !  ???? ")

* Usamos no exemplo uma lista de caracteres aleatória.
-}
sPilares_normal :: Int -> Int -> Int -> String -> Inf_Mapa
sPilares_normal tam linha coluna (h:t) 
  | coluna == 0 = ('#':c,d,p)
  | coluna == (tam -1) = ('#':[],[],(h:t)) 
  | otherwise = if determina_PowerUp h 
                then 
                ('?':a,(h,coluna,linha):b,x) 
                else 
                (h:a,b,x)
    where (a,b,x) = sPilares_normal tam linha (coluna +1) t
          (c,d,p) = sPilares_normal tam linha (coluna +1) (h:t)
{-|
==Descrição:
A função __sPilares_Cvazios__ contrói as linhas em que não temos pilares, ou seja, pedras no meio, mas temos 4 células vazias para o jogador se movimentar no ínico do jogo.

==Exemplo:
>>>sPilares_Cvazios 9 1 0 "?+  +  ? ?  ? ?  !  ???? "
"(#  ??   #",('+',4,1)," +  ? ?  ? ?  !  ???? ")

* Usamos no exemplo uma lista de caracteres aleatória.
-}
sPilares_Cvazios :: Int -> Int -> Int -> String -> Inf_Mapa
sPilares_Cvazios tam linha coluna  [] = ("  #",[],[])
sPilares_Cvazios tam linha coluna (h:t) 
  | coluna == 0 = ('#':c,d,p)
  | coluna == 1 || coluna == 2 || coluna == (tam -2) || coluna == (tam -3) = (' ':c,d,p) 
  | coluna == (tam -1) = ('#':[],[],(h:t))
  | otherwise = if determina_PowerUp h
                then 
                ('?':a,(h,coluna,linha):b,x) 
                else
                (h:a,b,x)         
    
    where (a,b,x) = sPilares_Cvazios tam linha (coluna +1) t
          (c,d,p) = sPilares_Cvazios tam linha (coluna +1) (h:t)
{-|
==Descrição:
A função __cPilares_Cvazios__ constrói as linhas em que temos pilares, ou seja, pedras no meio, e contém 2 células vazias para o jogador se movimentar no ínicio do jogo.

==Exemplo:
>>>cPilares_Cvazios 9 2 0 "?+  +  ? ?  ? ?  !  ???? "
("# #?#?# #",('+',5,2),"  +  ? ?  ? ?  !  ???? ")

* Usamos no exemplo uma lista de caracteres aleatória.
-}
cPilares_Cvazios :: Int -> Int -> Int -> String -> Inf_Mapa
cPilares_Cvazios tam linha coluna (h:t) 
  | coluna == 0 = ('#':c,d,p)
  | coluna == 1 || coluna == (tam -2) = (' ':c,d,p)
  | coluna == (tam -1) = ('#':[],[],(h:t))
  | mod coluna 2 == 0 = ('#':c,d,p)
  | otherwise = if determina_PowerUp h
                then 
                ('?':a,(h,coluna,linha):b,x)
                else
                (h:a,b,x)
    
    where (a,b,x) = cPilares_Cvazios tam linha (coluna +1) t
          (c,d,p) = cPilares_Cvazios tam linha (coluna +1) (h:t)
{-|
==Descrição:
A função __cPilares_normal__ constrói as linhas em que temos pilares, ou seja, pedras no meio, mas não contém as células vazias para o jogador se movimentar no ínicio.

==Exemplo:
>>>cPilares_normal 9 4 0 "?+  +  ? ?  ? ?  !  ???? "
(#?#?# # #,('+',3,4),"+  ? ?  ? ?  !  ???? ")

* Usamos no exemplo uma lista de caracteres aleatória.
-}

cPilares_normal :: Int -> Int -> Int -> String -> Inf_Mapa  --(String,[Powerup],String)
cPilares_normal tam linha coluna (h:t) 
  | coluna == 0 = ('#':c,d,p)   
  | coluna == (tam -1) = ('#':[],[],(h:t))
  | mod coluna 2 == 0 = ('#':c,d,p)
  | otherwise = if determina_PowerUp h 
                then 
                ('?':a,(h,coluna,linha):b,x)
                else 
                (h:a,b,x) 
    
    where (a,b,x) = cPilares_normal tam linha (coluna +1) t
          (c,d,p) = cPilares_normal tam linha (coluna +1) (h:t)
 

{-|
==Descrição:
A função __linha_tabuleiro__, dependendo da linha ela determina a função que deve ser usada para essa linhas.

* As funções determinadas, são as que definimos anteriormente.
-}
linha_tabuleiro :: Int -> Int -> String -> (Mapa,[Powerup])
linha_tabuleiro tam linha lista 
  | linha == tam = ([],[])
  | linha == 0 || linha==(tam-1) = 
      let (tab,powrs) = linha_tabuleiro tam (linha +1) lista
      in ((linhaCimaBaixo tam):tab,powrs)
  | linha == 1  || linha == (tam -2)= 
      let (lmapa,lpow,lrnd) = sPilares_Cvazios tam linha 0 lista
      in let (tab,powrs) = (linha_tabuleiro tam (linha+1) lrnd)
         in (lmapa:tab,lpow++powrs) 
  | linha == 2 || linha == (tam - 3) =
      let (lmapa,lpow,lrnd) = cPilares_Cvazios tam linha 0 lista
      in let (tab,powrs) = (linha_tabuleiro tam (linha +1) lrnd)
         in (lmapa:tab,lpow++powrs)
  | mod linha 2 == 0 =
      let (lmapa,lpow,lrnd) = cPilares_normal tam linha 0 lista
      in let (tab,powrs) = (linha_tabuleiro tam (linha +1) lrnd)
         in (lmapa:tab,lpow++powrs)
  | mod linha 2 /= 0 = 
      let (lmapa,lpow,lrnd) = sPilares_normal tam linha 0 lista
      in let (tab,powrs) = (linha_tabuleiro tam (linha +1) lrnd)
         in (lmapa:tab,lpow++powrs)

{-|
==Descrição:
A função __organizaPoweups__ organiza os power ups, primeiro os power ups bombs e depois os power ups flames.
-}
organizaPoweups :: [Powerup] -> ([Powerup], [Powerup])
organizaPoweups [] = ([],[])
organizaPoweups ((x,y,z):t) | x == '+' = ((x,y,z):a,b)
                            | otherwise = (a,(x,y,z):b)
                              where (a,b) = organizaPoweups t

{-|
==Descrição:
A função __converte__, converte a informação de um power up, de um tuplo para uma String.

==Exemplo:
>>>converte ('+',7,1)
"+ 7 1"

-}
converte :: (Char,Int,Int) -> String 
converte (c,a,b) = c:' ':((show a)++" "++(show b))

{-|
A função __mapa__, contrói o mapa e junta-o à informação dos power ups bombs e flames.
-}
mapa :: Int -> Int -> [String]
mapa tam seed = mapa++(map converte p_bombs)++(map converte p_flames)
                    where (mapa,p_ups) = linha_tabuleiro tam 0 (determina_Char (numeros_Random tam seed))
                          (p_bombs,p_flames) = organizaPoweups p_ups







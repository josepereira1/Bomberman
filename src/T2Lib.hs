{-| 
Module      : T2Lib
Description : Este módulo contém as funções para a realização da __Tarefa 2__, do projeto __Bomberman__. 
Copyright   : José Pereira <https://github.com/josepereira1>;


Este módulo contém as __funções__, da __Tarefa 2__, que determinam as alterações feitas no mapa, após inserido um determinado comando.
-}

module T2Lib where

import Data.Char (isDigit)
import System.Environment
import Data.Char
import T1Lib 


{-|A função __determina_pos__, recebe como parâmetros o comando que 
é um Char e as coordenadas atuais do jogador, e a partir desse comando 
descobrir qual a posição para onde o jogador quer, e descobrir 
através desse comando a direção e alterar o valor nas coordenadas.-}

determina_pos :: Char -> (Int,Int) -> (Int,Int)
determina_pos cmd (c,l) -- (c,l) (coluna,linha) 
  | cmd == 'U' || cmd == 'u' = (c,l-1) --x = linha 
  | cmd == 'D' || cmd == 'd' = (c,l+1) --y = coluna 
  | cmd == 'L' || cmd == 'l' = (c-1,l)
  | cmd == 'R' || cmd == 'r' = (c+1,l)
  | cmd == 'B' || cmd == 'b' = (c,l)
  | otherwise = (c,l)

{-| 
==Descrição:

A função __ocorrePedraTijolo__, tem como objectivo verificar se na posição c1 l1, 
existe uma pedra ou um tijolo, pois caso exista o jogador não se pode mover 
para essa posição, retornando nesse caso True, ou seja, existe uma pedra nessa posição.

==Exemplo:

>>>ocorrePedraTijolo (mapa 11 0) 2 2
True

-}
-------------------------------------------------
ocorrePedraTijolo :: [String] -> Int -> Int -> Bool 
ocorrePedraTijolo mapa c1 l1 = aux_ocorrePedraTijolo mapa c1 l1 0 0
  where
  aux_ocorrePedraTijolo :: [String] -> Int -> Int -> Int -> Int -> Bool 
  aux_ocorrePedraTijolo (h:t) c1 l1 c2 l2
    | l1 == l2 = ocorreLinha h c1 0
    | otherwise = aux_ocorrePedraTijolo t c1 l1 c2 (l2+1)  
{-|
==Descrição:

Utilizamos como função auxiliar __ocorreLinha__ que verifica através da 
coluna se existe um pedra num linha x com coluna y, caso exista retorna True,
caso contrário retorna False. Basicamente a função __ocorrePedraTijolo__ determina
a linha em que queremos verificar a existência de pedra ou tijolo, enquanto que a função 
__ocorreLinha verifica se existe uma pedra na coluna indicada, retornando assim um Bool.

-}
ocorreLinha :: String -> Int -> Int -> Bool
ocorreLinha [] c1 c = False
ocorreLinha (h:t) c1 c | c1 == c = if (h == '#' || h =='?') then True else ocorreLinha t c1 (c+1)
                       | otherwise = ocorreLinha t c1 (c+1) 
-------------------------------------------------

{-|
==Descrição:

A função __verifica_bomb__, recebe o mapa inicial e as coordenadas
para verificar se existe uma bomba nessa posição.

==Exemplo:

>>> verifica_bomb (mapa 11 0)++["* 7 7 1 1 0","0 3 1","1 7 7"] 7 7
True
-}
--Esta função verifica se existe uma bomba na posição recebida!
verifica_bomb :: [String] -> Int -> Int -> Bool 
verifica_bomb [] c l = False
verifica_bomb (h:t) c l | q == '*' && x1 == c && x2 == l = True
                        | otherwise = verifica_bomb t c l  
                          where (ch,x1,x2,x3,x4,x5) = converte_INFbomb (words h)
                                (q:qs) = h 
-------------------------------------------------
{-|
==Descrição:

A função __verificaBombPlayer__,verifica se um jogador tem powerups bombs para colocar 
mais uma bomba.

==Exemplo:

>>>verificaBombPlayer ((mapa 11 0)++["* 6 7 0 1 10","0 5 7 +"]) 0
True

*A função mapa está definida no T1Lib.
-}
verificaBombPlayer :: [String] -> Int -> Bool
verificaBombPlayer mapa gamer | ((det_pUPbombs mapa gamer) >= (detBomb_ativadas mapa gamer)) = True
                              | otherwise = False   

{-|
==Descrição:

A função __det_pUPbombs__ e a respectiva auxiliar, determinam a quantidade de power ups bombs que o jogador x tem.

==Exemplo
>>>det_pUPbombs ((mapa 11 0)++["0 3 1 ++"]) 0
2

*A função mapa está definida no T1Lib.
-}

det_pUPbombs :: [String] -> Int -> Int 
det_pUPbombs [] gamer = 0
det_pUPbombs (h:t) gamer | x == (chr (gamer + 48)) = aux_det_pUPbombs h 
                         | otherwise =  det_pUPbombs t gamer
                          where (x:xs) = h 

aux_det_pUPbombs :: String -> Int 
aux_det_pUPbombs [] = 0 
aux_det_pUPbombs (h:t) | h == '+' = 1 + aux_det_pUPbombs t
                       | otherwise =  aux_det_pUPbombs t

{-|
==Descrição:

A função __detBomb_ativadas__, determina quantas bombas ativadas, um jogador x tem. 

==Exemplo:

>>>detBomb_ativadas ((mapa 11 0)++["* 6 7 0 1 10","0 5 7 +"]) 0
1

*A função mapa está definida no T1Lib.

-}

detBomb_ativadas :: [String] -> Int  -> Int  
detBomb_ativadas [] gamer = 0 
detBomb_ativadas (h:t) gamer | (head h) == '*' && (read x3 :: Int) == gamer = 1+detBomb_ativadas t gamer  --pq o número do jogador está na posição 6.
                             | otherwise =  detBomb_ativadas t gamer
                          where (ch:x1:x2:x3:x4:x5:xs) = words h
-------------------------------------------------
{-|

==Descrição:
A função __validar_comando__, verifica se a jogada é válida.

==Nota:
Para isso usamos a recorrência da função __ocorrePedraTijolo__, para 
determinar se existe um pedra na posição para onde o jogador quer ir.

==Exemplo:
>>>validar_comando ((mapa 11 0)++["* 6 7 0 1 10","0 5 7 +"]) 'U' 0 (5,7)
False

*A função mapa está definida no T1Lib.
-}

validar_comando :: [String] -> Char -> Int -> (Int, Int) -> Bool 
validar_comando (h:t) cmd gamer (c,l) | l < 0 || l >= (length h) || c < 0 || c >= (length h) = False  --perguntar ao prof se vale apena fazer este caso
validar_comando mapa cmd gamer (c,l) 
  | cmd == 'U' || cmd == 'u' = if ocorrePedraTijolo mapa x1 y1 then False else True 
  | cmd == 'D' || cmd == 'd' = if ocorrePedraTijolo mapa x1 y1 then False else True 
  | cmd == 'L' || cmd == 'l' = if ocorrePedraTijolo mapa x1 y1 then False else True
  | cmd == 'R' || cmd == 'r' = if ocorrePedraTijolo mapa x1 y1 then False else True
  | cmd == 'B' || cmd == 'b' = if (verifica_bomb mapa x1 y1 == False) && (verificaBombPlayer mapa gamer == True) 
                                    then 
                                        True 
                                    else 
                                        False
    where (x1,y1) = determina_pos cmd (c,l)

-------------------------------------------------
{-|
==Descrição:
A função __verifica_pUp__, verifica se existe um power up na posição indicada.

==Exemplo:
>>>verifica_pUp (mapa 11 0) 7 1
('+',True)

*A função mapa está definida no T1Lib.
 -}
verifica_pUp :: [String] -> Int -> Int -> (Char, Bool)
verifica_pUp [] c1 l1 = (' ', False)
verifica_pUp (h:t) c1 l1 | ((q == '+' || q == '!') && (x1 == c1 && x2 == l1)) = (ch,True)
                         | otherwise = verifica_pUp t c1 l1
                          where (ch,x1,x2) = converte_INF_pUPtoInt (words h)
                                (q:qs) = h

-------------------------------------------------
{-|
==Descrição:
A função __add_powerUp__ e a respectiva auxiliar, adiciona um power up bomb ou flame, de forma ordenada, ou seja, primeiro bombs e depois flames

==Exemplo:
>>>add_powerUp ((mapa 11 0)++["0 3 1 !"]) 0 '+'
((mapa 11 0)++["0 3 1 +!"])

*A função mapa está definida no T1Lib.
-}
add_powerUp :: [String] -> Int -> Char -> [String] 
add_powerUp (h:t) gamer powerUP | x == chr (gamer +48) = aux_add_powerUp h powerUP:t
                                  | otherwise = h:add_powerUp t gamer powerUP
                                  where (x:xs) = h

aux_add_powerUp :: String -> Char -> String 
aux_add_powerUp (h:t) powerUP | h == '+' && t == [] = (h:t)++[powerUP]
                  | h == '!' && powerUP == '+' = powerUP:(h:t)
                  | h == '!' = (h:t)++[powerUP]
                  | t == [] = (h:t)++[' ']++[powerUP]
                  | otherwise = h:aux_add_powerUp t powerUP

-------------------------------------------------
{-|
==Descrição:
A função __det_flames__ e a respectiva auxiliar, determina quantos power ups flames um jogador tem.

==Exemplo:
>>>det_flames ((mapa 11 0)++["0 1 1 +!!"]) 0
2

*A função mapa está definida no T1Lib.
-}
det_flames :: [String] -> Int -> Int 
det_flames [] gamer = 0
det_flames (h:t) gamer | x == (chr (gamer + 48)) = aux_det_flames h 
                       | otherwise =  det_flames t gamer
                        where (x:xs) = h 

aux_det_flames :: String -> Int 
aux_det_flames [] = 0 
aux_det_flames (h:t) | h == '!' = 1 + aux_det_flames t
                     | otherwise =  aux_det_flames t

-------------------------------------------------
{-|
== Descrição:

A função __ativar_bomb__, ativa um bomba de um jogador, imprimindo-a 
depois dos power ups e antes da informação dos jogadores, caso já haja
uma bomba, a função coloca de forma ordenada, através das coordenadas.

==Exemplo:
>>>ativar_bomb ((mapa 11 0)++["0 3 1 !"]) 3 1 0 1
((mapa 11 0)++["* 3 1 0 2 10","0 3 1 !"])

*A função mapa está definida no T1Lib.
-}
ativar_bomb :: [String] -> Int -> Int -> Int -> Int -> [String]
ativar_bomb [] c l gamer flames = [] 
ativar_bomb (h:t) c l gamer flames 
  | isDigit x = (('*':' ':show c++" "++show l++" "++show gamer++" "++show flames++" "++[])++"3"):h:t  
  | x == '*' && l < x2 = (('*':' ':show c++" "++show l++" "++show gamer++" "++show flames++" "++[])++"3"):h:t
  | x == '*' && c < x1 && l == x2 = (('*':' ':show c++" "++show l++" "++show gamer++" "++show flames++" "++[])++"3"):h:t
  | x == '*' && l > x2 = [h]++(('*':' ':show c++" "++show l++" "++show gamer++" "++show flames++" "++[])++"3"):t
  | otherwise = h:(ativar_bomb t c l gamer flames)
    where (ch,x1,x2,x3,x4,x5) = converte_INFbomb (words h)
          (x:xs) = h

-------------------------------------------------
{-|
==Descrição:
A função __update_positions__ atualiza as posições do jogador.

==Exemplo:
>>>update_positions ((mapa 11 0)++["0 3 1"]) 0 'L' 2 1
((mapa 11 0)++["0 2 1"])

*A função mapa está definida no T1Lib.
-}
update_positions :: [String] -> Int -> Char -> Int -> Int -> [String]
update_positions [] gamer cmd c l = [] 
update_positions (h:t) gamer cmd c l 
  | q ==  chr (gamer+48) && resto == [] = (show x1++" "++show c++" "++show l++resto):t
  | q ==  chr (gamer+48) = (show x1++" "++show c++" "++show l++" "++resto):t
  | otherwise = h:update_positions t gamer cmd c l 
      where ((x1:x2:x3:xs),resto) = converte_INFplayer (words h)
            (q:qs) = h

{-|
==Descrição:

A função __remove_InfpowUp__ remove a informação de um power up que tenha sido apanhado por um jogador.

==Exemplo:
>>>remove_InfpowUp ((mapa 11 0)++["! 9 4"]) '+' 9 4
(mapa 11 0)

*A função mapa está definida no T1Lib.
-}
remove_InfpowUp :: [String] -> Char -> Int -> Int -> [String]
remove_InfpowUp (h:t) powUp c l | powUp == x && c == caux && l == laux = t
                                | otherwise = h:remove_InfpowUp t powUp c l  
                                  where (ch,caux,laux) = converte_INF_pUPtoInt (words h)
                                        (x:xs) = h 

-------------------------------------------------
{-|
==Descrição:

A função __det_pos_jogador__ determina através do número do jogador, a posição em que ele se encontra.

==Exemplo:
>>>det_pos_jogador ((mapa 11 0)++["0 3 1"]) 0
(3,1)

*A função mapa está definida no T1Lib.
-}  
det_pos_jogador :: [String] -> Int -> (Int,Int) 
det_pos_jogador [] gamer = (0,0)
det_pos_jogador (h:t) gamer | (chr (gamer + 48)) == (head h) = (x2,x3) 
                              | otherwise = det_pos_jogador t gamer 
                                where  (x1:x2:x3:xs) = converte_INFcoord (words h)

-------------------------------------------------
{-|
==Descrição:

A função __converte_INF_pUPtoInt__ converte a informação de um power up de string para um triplo.

==Exemplo:
>>>converte_INF_pUPtoInt ["+","7","1"]
('+',7,1)

-}
converte_INF_pUPtoInt :: [String] -> (Char,Int,Int)
converte_INF_pUPtoInt (x:y:z:t) = (head x,(read y :: Int),(read z :: Int))
 
{-|
==Descrição:

A função __converte_INFplayer__ converte a informação do jogador para um tuplo.

==Exemplo:
>>>converte_INFplayer ["0","3","1"] 
(0,3,1)

-} 
converte_INFplayer :: [String] -> ([Int],String)
converte_INFplayer [] = ([],[])
converte_INFplayer (x:t) | head x /= '+' && head x /= '!' = (((read x :: Int):coord),lpowUps)
                         | otherwise = ([], x)
                            where (coord,lpowUps) = converte_INFplayer t 

{-|
==Descrição:

A função __converte_INFcoord__ converte a informação das coordenadas para uma lista de inteiros.

==Exemplo:
>>>converte_INFcoord ["0","1"]
[0,1]
-}
converte_INFcoord :: [String] -> [Int]
converte_INFcoord [] = [] 
converte_INFcoord (x:t) = (read x :: Int):converte_INFcoord t

{-|
==Descrição:

A função __converte_INFbomb__ converte a informação da bomba, para um tuplo.

==Exemplo:
>>>converte_INFbomb (["*","7","1","0","1","10"])
('*',7,1,0,1,10)

-}
converte_INFbomb :: [String] -> (Char,Int,Int,Int,Int,Int) --inf. bomba
converte_INFbomb (x1:x2:x3:x4:x5:x6:xs) = (head x1, read x2 :: Int, read x3 :: Int, read x4 :: Int, read x5 :: Int, read x6 :: Int)

-------------------------------------------------
{-|
==Descrição:
A função __det_mapa_res__ determina através do comando (Char) introduzido, qual as funções que tem que utilizar, retornando assim o mapa com as alterações feitas pelo respectivo comando.

==Exemplo:
>>>det_mapa_res ((mapa 11 0)++["0 3 1","1 2 1"]) 'B' 0 3 1 
((mapa 11 9)++["* 3 1 0 1 10","0 3 1","1 2 1"])

*A função mapa está definida no T1Lib.

-}

det_mapa_res :: [String] -> Char -> Int -> Int -> Int -> [String]
det_mapa_res mapa cmd gamer c l 
  | cmd == 'U' || cmd == 'u' || cmd == 'D' || cmd == 'd' || cmd == 'L' || cmd == 'l' || cmd == 'R' || cmd == 'r' =
     if exist then add_powerUp up_Inf_mapa gamer powUp else update_positions mapa gamer cmd c l 
  | cmd == 'B' || cmd == 'b' = ativar_bomb mapa c l gamer flames 
    where (powUp,exist) = verifica_pUp mapa c l 
          flames = (1 + det_flames mapa gamer)
          up_Inf_mapa =  remove_InfpowUp (update_positions mapa gamer cmd c l) powUp c l 

-------------------------------------------------
{-|
==Descrição:
A função move valida a jogada, caso esta seja inválida, ele retorna o mapa inicial, caso contrário ele determina através da função __det_mapa_res__ as funções que tem que utilizar para comando.

==Notas Importantes:

*Caso o jogador não exista a minha função __det_pos_jogador__ retorna-me (0,0) e assim eu retorno o mapa inicial, visto que a jogada não é válida!

-}
move :: [String] -> Int -> Char -> [String]  
move mapa gamer cmd | c == 0 && l == 0 = mapa 
                    | validar_comando mapa cmd gamer (c,l) = det_mapa_res mapa cmd gamer c_cmd l_cmd 
                    | otherwise = mapa 
                     where (c,l) = det_pos_jogador mapa gamer
                           (c_cmd,l_cmd) = determina_pos cmd (c,l) 

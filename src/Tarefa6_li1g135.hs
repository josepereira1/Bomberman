{-| 
Module      : Tarefa6_li1g135
Description : Este módulo contém as funções para a realização da __Tarefa 6__, do projeto __Bomberman__. 
Copyright   : José Pereira <https://github.com/josepereira1>;


Este módulo contém as __funções__, da __Tarefa 6__, para a criação de um __Bot__.
-}
module Tarefa6_li1g135 where
import T4Lib
import T1Lib

{-|
==Descrição:
A __Flage__, é utilizada, para controlar situações.
-}
type Flag = Int
{-|
==Descrição:
O __Gamer__ é o número de um jogador, portanto, 0,1,2 e 3.
-}
type Gamer = Int
{-|
==Descrição:
A CoordX é a coordenada no eixo do xx, que no caso do __Bomberman__, corresponde à __Coluna__.
-}
type CoordX = Int
{-|
==Descrição:
A CoordY é a coordenada no eixo do yy, que no caso do __Bomberman__, corresponde à __Linha__.
-}
type CoordY = Int
{-|
==Descrição:
A __Distance__, é a distância entre dois Pontos.
-}
type Distance = Float

{-|
==Descrição:
O tipo de dado __Dir__, representa todas as direções possíveis do __Bot__, sendo que o Null, significa, que não faz comando.
-}
data Dir = Upp | Downn | Leftt | Rightt | Null
    deriving (Show,Eq)
{-|
==Descrição:
O tipo de dado __WhereIsBomb__, representa a localização de uma bomba. 

==Construtores:
*O construtor LinhaE, significa que a bomba está na mesma Linha que o jogador e que se encontra à Esquerda.
*O construtor LinhaD, significa que a bomba está na mesma Linha que o jogador e que se encontra à Direita.
*O construtor ColunaC, significa que a bomba está na mesma Coluna que o jogador e que se encontra em Cima.
*O construtor LinhaD, significa que a bomba está na mesma Coluna que o jogador e que se encontra em em Baixo.
*O construtor Bombsport, significa que a bomba está na mesma posição que o jogador.
-}
data WhereIsBomb = LinhaE | LinhaD | ColunaC | ColunaB | Bombspot | Nada
    deriving (Show,Eq)

{-| 
==Descrição:
A função __bot__, recebe o estado de jogo, o número do jogador e o tempo atual do jogo, e retorna o comando que o __bot__, determinou através da situação de Jogo.

==Descrição do Bot:
* Inicialmente o __Bot__, procura o jogador mais próximo e tenta matá-lo com bombas.
* Caso existam power ups, destapados, ele vai tentar capturá-los.
* Quando o efeito espiral estiver a começar o __bot__, foca uma posição, para se proteger da mesma.
* Quando estiver na posição ideal, ainda tenta matar jogadores quando o tempo for igual a 3 segundos.
* Este __Bot__, sabe escapar às suas bombas e de outros jogadores.
-}

bot :: [String] -> Int -> Int -> Maybe Char
bot mapa player ticks = chooseMove (filterInfGame mapa) player ticks
{-|
==Descrição:
A função __whatDir__, determina a direção que o __Bot__, deve seguir para atingir um determinado __Ponto__. Retornando assim duas possíveis Direções.

==Exemplos:
>>>whatDir (1,1) (3,3)
(Rightt,Downn)
>>>whatDir (1,1) (3,1)
(Rightt,Null)
>>>whatDir (1,1) (1,3)
(Null,Downn)
>>> whatDir (3,1) (1,2)
(Leftt,Downn)
>>>whatDir (3,1) (3,1)
(Null,Null)

==Notas:
* O primeiro elemento do par, contém as direções __Rightt__, __Leftt__ e __Null__, o segundo tem __Upp__, __Downn__, __Null__.
* Quando só existe uma direção possível ele retorna-a e no outro elemento do tuplo, ocorre __Null__.
* Quando a posição é a mesma, retorna em ambos os elemnentos do tuplo __Null__.
-}
--  Esta função diz-me a direção que eu devo seguir para atingir um certo ponto.
whatDir :: Posicao -> Posicao -> (Dir,Dir)
whatDir (c1,l1) (c2,l2) | c1 < c2 && l1 < l2 = (Rightt,Downn)
                        | c1 < c2 && l1 > l2 = (Rightt,Upp)
                        | c1 > c2 && l1 < l2 = (Leftt,Downn)
                        | c1 > c2 && l1 > l2 = (Leftt,Upp)
                        | c1 == c2 && l1 < l2 = (Null,Downn)
                        | c1 == c2 && l1 > l2 = (Null,Upp)
                        | l1 == l2 && c1 < c2 = (Rightt,Null)
                        | l1 == l2 && c1 > c2 = (Leftt,Null)
                        | c1 == c2 && l1 == l2 = (Null,Null)
{-|
==Descrição:
A função __whatComand__, recebe uma direção, e retorna qual o comando correspondente.

==Exemplos:
>>>whatComand Rightt
Just 'R'
>>>whatComand Leftt
Just 'L'
>>>whatComand Upp
Just 'U'
>>>whatComand Downn
Just 'D'

-}
-- Esta função retorna-me o comando a ser usado através da direção.
whatComand :: Dir -> Maybe Char 
whatComand dir 
    | dir == Upp = Just 'U'
    | dir == Downn = Just 'D'
    | dir == Leftt = Just 'L'
    | dir == Rightt = Just 'R'
{-|
==Descrição:
A função __whatPos__, e a respetiva auxiliar recebe duas direções e a posição, e determina, quais as coordenadas das direções, partindo da coordenada onde o __Bot__ se encontra.

==Exemplos:
>>>whatPos Leftt Downn (7,1)
((6,1),(7,2))
>>>whatPos Null Null (7,1)
((7,1),(7,1))
>>>whatPos Null Downn (7,1)
((7,1),(7,2))
-}
--  Esta função retorna-me as posições da direção que eu lhe colocar no primeiro elemento do tuplo, e as restantes direções possíveis no segundo elemento do tuplo.
whatPos :: Dir -> Dir -> Posicao -> (Posicao,Posicao)
whatPos dir1 dir2 (c,l) = (p1,p2)
          where p1 = whatCoords dir1 (c,l)
                p2 = whatCoords dir2 (c,l)
{-|
==Descrição:
A função __whatIsPlayer__, recebe a lista de jogadores, a flag inicializada a zero e o número do jogador, e retorna a posição, desse jogador, caso ele não exista, retorna a posição (0,0).

==Exemplos:
>>>whatIsPlayer [Vivo (1,1) 0 0,Vivo (9,1) 0 1,Morto,Morto] 0 0
(1,1)
>>>whatIsPlayer [Vivo (1,1) 0 0,Vivo (9,1) 0 1,Morto,Morto] 0 3
(0,0)
>>>whatIsPlayer [Vivo (1,1) 0 0,Vivo (9,1) 0 1,Morto,Morto] 0 1
(9,1)
-}
--  Esta função retorna-me a posição de um jogador.
whatIsPlayer :: Players -> Flag -> Gamer -> Posicao
whatIsPlayer [] flag n = (0,0) 
whatIsPlayer (Morto:t) flag n = whatIsPlayer t (flag+1) n 
whatIsPlayer ((Vivo (c,l) b f):t) flag n | flag == n = (c,l)
                                         | otherwise = whatIsPlayer t (flag+1) n   

{-|
==Descrição:
A função __chooseMove__, determina o movimento que o __Bot__, deve fazer, consoante a situação de jogo
-}
chooseMove :: InfGame -> Gamer -> Time -> Maybe Char
chooseMove infgame gamer time  
  | wibomb /= Nada = escapeBomb infgame wibomb (c,l)
  | myRange == False = Nothing
  | test == True && time > (((size-2)^2)+10) = followPlayer infgame (c,l) cPly lPly gamer -- -> esta linha é nova!
  | test == False && time > (((size-2)^2)+10) && cPu /= 0 && lPu /= 0 = escapeToPoint infgame (c,l) cPu lPu
  | b1 /= Pedra = escapeToPoint infgame (c,l) (div size 2) (div size 2)
  | otherwise = escapeToPoint infgame (c,l) ((div size 2)-1) (div size 2)
    where (m,p,b,j) = infgame
          b1 = bloco infgame ((div size 2),(div size 2))  -- bloco na posição do meio.
          size = length (head m)          -- size 
          wibomb = checkBomb b (c,l)      -- verifica se estou protegido de bombas.
          (c,l) = whatIsPlayer j 0 gamer  -- retorna as coordenadas do meu jogador.
          myRange = checkMyRange (c,l) b  -- verifica se existe bombas a atingir o meu range.
          (cPly,lPly) = selectPlayer j (c,l) (fromIntegral(3*size)) (0,0) 0 gamer  -- -> esta linha é nova!
          (cPu,lPu) = selectPowerUp p (c,l) (fromIntegral(3*size)) (0,0)
          test = selectPowerUpORPlayer (c,l) (cPly,lPly) (cPu,lPu)
{-|
==Descrição:
A função __playerPos__, recebe a lista de Jogadores, um posição, uma flag inicializada a zero e o número do jogador (__Bot__), e verifica se existe algum jogador na posição indicada, mas não verifica o nosso jogador (__Bot__).

==Exemplos:
>>>playerPos [Vivo (1,1) 0 0,Vivo (9,1) 0 1,Morto,Morto] (1,1) 0 1
True
>>>playerPos [Vivo (1,1) 0 0,Vivo (9,1) 0 1,Morto,Morto] (9,1) 0 1
False

==Notas:
* Retorna True, quando existe algum jogador na posição indicada, que não é o nosso jogador, e False, caso contrário.

-}
--  Verifica a existência de um jogador que não eu na posição indicada.
playerPos :: Players -> Posicao -> Flag -> Gamer -> Bool
playerPos [] _ _ _ = False
playerPos ((Morto):t) (c2,l2) flag gamer = playerPos t (c2,l2) (flag+1) gamer
playerPos ((Vivo (c1,l1) b f):t) (c2,l2) flag gamer | flag == gamer = playerPos t (c2,l2) (flag+1) gamer
                                                    | c1 == c2 && l1 == l2 = True
                                                    | otherwise = playerPos t (c2,l2) (flag+1) gamer
{-|
==Descrição:
A função __checkBomb__, recebe a lista das Bombas, uma posição e verifica se na posição indicada, existe alguma bomba capaz de atingir com o flame.

==Exemplos:
>>>checkBomb [((1,2),1,1,10)] (1,1)
ColunaB
>>>checkBomb [((1,1),1,1,10)] (1,1)
Bombspot
>>>[((1,1),1,1,10)] (2,1)
LinhaE

==Notas:
* Esta função retorna um tipo novo, que é o __WhereIsBomb__, o __ColunaB__, significa que a bomba esta na mesma Coluna que a posição indicada e que se encontra abaixo da posição.
* O __Bombspot__, significa que a bomba, econtra-se na mesma posição que a indicada.
* Pode-se consultar toda a informação sobre o tipo __WhereIsBomb__ nesta Documentação.
-}
--  Esta função verifica-me se existe alguma bomba que possa afetar o bot.
checkBomb :: [Bomba] -> Posicao -> WhereIsBomb
checkBomb [] _ = Nada
checkBomb (((c2,l2),j,f,time):t) (c1,l1) 
    | l1 == l2 && c1 == c2 = Bombspot
    | c1 == c2 && (l2-f) <= l1 && l1 < l2 = ColunaB
    | c1 == c2 && (l2+f) >= l1 && l1 > l2 = ColunaC
    | l1 == l2 && (c2-f) <= c1 && c1 < c2 = LinhaD
    | l1 == l2 && (c2+f) >= c1 && c1 > c2 = LinhaE
    | otherwise = checkBomb t (c1,l1) 

{-|
==Descrição:
A função __mvToUpDown__, recebe a posição do __Bot__, a informação do jogo, e a localização da bomba, e determina qual o melhor comando para a fuga, caso os comandos principais __Upp__, __Downn__, não sejam válidos, a função determina a melhor alternativa.
-}
--  Esta função função determina se é possível fazer o comando para cima ou para baixo, caso contrário verifica as alternativas.
mvToUpDown :: Posicao -> InfGame -> WhereIsBomb -> Dir
mvToUpDown (c,l) infgame dir
    | dir1 == Nada && b1 == Vazio = Upp
    | dir2 == Nada && b2 == Vazio = Downn
    | dir == LinhaE && b3 == Vazio = Rightt
    | dir == LinhaE && b3 /= Vazio = Leftt
    | dir == LinhaD && b4 == Vazio = Leftt
    | dir == LinhaD && b4 /= Vazio = Rightt
        where (x1,y1) = (c,l-1) -- Up 
              (x2,y2) = (c,l+1) -- Down
              (x3,y3) = (c+1,l) -- Right
              (x4,y4) = (c-1,l) -- Left
              b1 = bloco infgame (x1,y1)
              b2 = bloco infgame (x2,y2)
              b3 = bloco infgame (x3,y3)
              b4 = bloco infgame (x4,y4)
              (m,p,b,j) = infgame
              dir1 = checkBomb b (x1,y1)
              dir2 = checkBomb b (x2,y2)
{-|
==Descrição:
A função __mvToLeftRight__, recebe a posição do __Bot__, a informação do jogo, e a localização da bomba, e determina qual o melhor comando para a fuga, caso os comandos principais __Leftt__, __Rightt__, não sejam válidos, a função determina a melhor alternativa.
-}
--  Esta função função determina se é possível fazer o comando para esquerda ou para direita, caso contrário verifica as alternativas.
mvToLeftRight :: Posicao -> InfGame -> WhereIsBomb -> Dir
mvToLeftRight (c,l) infgame dir 
    | dir1 == Nada && b1 == Vazio = Leftt
    | dir2 == Nada && b2 == Vazio = Rightt
    | dir == ColunaC && b3 == Vazio = Downn
    | dir == ColunaC && b3 /= Vazio = Upp
    | dir == ColunaB && b4 == Vazio = Upp
    | dir == ColunaB && b4 /= Vazio = Downn 
        where (x1,y1) = (c-1,l) -- Left
              (x2,y2) = (c+1,l) -- Right
              (x3,y3) = (c,l+1) -- Down
              (x4,y4) = (c,l-1) -- Up
              b1 = bloco infgame (x1,y1)
              b2 = bloco infgame (x2,y2)
              b3 = bloco infgame (x3,y3)
              b4 = bloco infgame (x4,y4)
              (m,p,b,j) = infgame
              dir1 = checkBomb b (x1,y1)
              dir2 = checkBomb b (x2,y2)
{-|
==Descrição:
A função __mvToUpDownLeftRight__, recebe a posição do jogador e a informação do jogo, e determina qual das quatro direções possíveis é melhor, visto que este caso é o mais complexo, necessita-se de verificar todas as direções e escolher a melhor.
-}
--  Esta função função determina pela ordem contrária do relógio qual o movimento que deve fazer, ou seja, a ordem é Cima -> Esquerda -> Baixo -> Direita.
mvToUpDownLeftRight :: Posicao -> InfGame -> Dir
mvToUpDownLeftRight (c,l) infgame
    | d1 = Rightt
    | d2 = Downn
    | d3 = Leftt
    | d4 = Upp
    | b4 == Vazio = Upp
    | b1 == Vazio = Rightt
    | b2 == Vazio = Downn
    | b3 == Vazio = Leftt
        where (x1,y1) = (c,l-1)
              (x2,y2) = (c-1,l)
              (x3,y3) = (c,l+1)
              (x4,y4) = (c+1,l)
              (m,p,b,j) = infgame
              numFlame = checkFlmBomb b (c,l)
              d1 = goodWay (c+1,l) numFlame infgame Rightt
              d2 = goodWay (c,l+1) numFlame infgame Downn
              d3 = goodWay (c-1,l) numFlame infgame Leftt
              d4 = goodWay (c,l-1) numFlame infgame Upp 
              b1 = bloco infgame (x4,y4)
              b2 = bloco infgame (x3,y3)
              b3 = bloco infgame (x2,y2)
              b4 = bloco infgame (x1,y1)
{-|
==Descrição:
A função __goodway__, recebe a posição, o número de power ups flames, a informação do jogo e por fim a direção, esta função verfica se esta direção é a melhor para a fuga da bomba, pois necessita-se de verificar a existência de obstáculos ou de bombas. 
-}
--  Esta função determina se um caminho é bom ou não para seguir.
goodWay :: Posicao -> NumbFlame -> InfGame -> Dir -> Bool
goodWay (c,l) f infgame dir 
  | f > 0 && blck == Vazio = goodWay (x,y) (f-1) infgame dir
  | f == 0 && blck == Vazio && existBomb == Nada = True
  | otherwise = False 
    where blck = bloco infgame (c,l)
          (x,y) = whatCoords dir (c,l)
          (m,p,b,j) = infgame
          existBomb = checkBomb b (c,l) 
{-|
=Descrição:
A função __whatCoords__, recebe a direção e uma posição e determina, qual a posição originada por essa direção.
-}
-- Dada uma direção e uma posição, a função retorna-me as coordenadas, caso faça essa direção. 
whatCoords :: Dir -> Posicao -> Posicao
whatCoords dir (c,l) | dir == Rightt = (c+1,l)
                     | dir == Leftt = (c-1,l)
                     | dir == Upp = (c,l-1)
                     | dir == Downn = (c,l+1)
                     | dir == Null = (c,l)
{-|
==Descrição:
A função __checkFlmBomb__, recebe a lista de bombas e uma posição, e retorna o flame da bomba que se encontra nessa posição.

==Exemplos:
>>>checkFlmBomb [((1,2),1,4,10)] (1,2)
4
>>>checkFlmBomb [((1,2),1,4,10),((3,4),0,2,10)] (3,4)
2
-}
--  Esta função dá-me o flame de um determinada bomba.
checkFlmBomb :: [Bomba] -> Posicao -> NumbFlame
checkFlmBomb [] _ = 0
checkFlmBomb (((c1,l1),j,f,time):t) (c2,l2) 
  | c1 == c2 && l1 == l2 = f
  | otherwise = checkFlmBomb t (c2,l2)
                                            
{-|
==Descrição:
A função __escapeBomb__, determina através dos construtores do tipo __WhereIsBomb__, qual o melhor comando para a fuga do __Bot__.

==Exemplos:
>>>escapeBomb (filterInfGame ((mapa 11 0)++["* 1 1 1 1 10","0 1 1","1 9 1 !"]))  Bombspot (1,1)
Just 'R'
>>>escapeBomb (filterInfGame ((mapa 11 0)++["* 1 1 1 2 10","0 1 1","1 9 1 !"]))  ColunaC (1,2)
Just 'D'
>>>escapeBomb (filterInfGame ((mapa 11 0)++["* 1 1 1 1 10","0 1 1","1 9 1 !"]))  LinhaE (1,2)
Just 'D'

==Notas:
*A função __filterInfGame__ está definida na Biblioteca __T4Lib__, converte a informação do mapa de __[String]__ para o tipo InfGame.
*A função __mapa__ está definida na Biblioteca __T1Lib__, e gera mapas do jogo __Bomberman__.
-}
--  Esta função escapa do flame de uma bomba, ou tenta!
escapeBomb :: InfGame -> WhereIsBomb -> Posicao -> Maybe Char
escapeBomb infgame dir (c,l) 
    | dir == LinhaE = whatComand (mvToUpDown (c,l) infgame dir)  -- alt D
    | dir == LinhaD = whatComand (mvToUpDown (c,l) infgame dir)  -- alt E
    | dir == ColunaC = whatComand (mvToLeftRight (c,l) infgame dir)  -- alt B
    | dir == ColunaB = whatComand (mvToLeftRight (c,l) infgame dir) -- alt C
    | dir == Bombspot = whatComand (mvToUpDownLeftRight (c,l) infgame)
{-|
==Descrição:
A função __escapeToPoint__, recebe a informação do jogador, a posição do mesmo, e as coordenadas objectivo. A função determina qual o comando que o __Bot__, deve efetuar, para atingir a posição indicada.

==Exemplos:
>>>escapeToPoint (filterInfGame ((mapa 11 0)++["* 1 1 1 2 10","0 1 1","1 9 1 !"])) (1,1) 3 3
Just 'R'
>>>escapeToPoint (filterInfGame ((mapa 11 0)++["* 1 1 1 2 10","0 1 1","1 9 1 !"])) (1,1) 1 2
Just 'D'
>>>escapeToPoint (filterInfGame ((mapa 11 0)++["* 1 1 1 2 10","0 1 1","1 9 1 !"])) (9,1) 1 2
Just 'L'

==Notas:
*A função __filterInfGame__ está definida na Biblioteca __T4Lib__, converte a informação do mapa de [String] para o tipo InfGame.
*A função __mapa__ está definida na Biblioteca __T1Lib__, e gera mapas do jogo Bomberman.
-}
--  Esta função, faz com que o bot faça movimentos para um determinado ponto, que neste caso é a Spiral.
escapeToPoint :: InfGame -> Posicao -> CoordX -> CoordY -> Maybe Char
escapeToPoint infgame (c,l) objx objy
    | dirx /= Null && b1 == Vazio = dirxx
    | diry /= Null && b2 == Vazio = diryy
    | dirx /= Null && b1 == Tijolo = Just 'B'
    | diry /= Null && b2 == Tijolo = Just 'B'
    | otherwise = Nothing
        where (m,p,b,j) = infgame
              (dirx,diry) = whatDir (c,l) (objx,objy)
              ((x1,y1),(x2,y2)) = whatPos dirx diry (c,l)
              b1 = bloco infgame (x1,y1)
              b2 = bloco infgame (x2,y2)
              dirxx = whatComand dirx
              diryy = whatComand diry
{-|
==Descrição:
A função __checkMyRange__, verifica se existe alguma bomba no range do __Bot__, que o possa atingir.

@
checkMyRange :: Posicao -> [Bomba] -> Bool
checkMyRange (c,l) listaBmb = dir1 == Nada && dir2 == Nada && dir3 == Nada && dir4 == Nada
                                where dir1 = checkBomb listaBmb (c,l-1)
                                      dir2 = checkBomb listaBmb (c,l+1)
                                      dir3 = checkBomb listaBmb (c-1,l)
                                      dir4 = checkBomb listaBmb (c+1,l)
@

==Notas1:
* Como podemos ver no código, ele averigua todas as direções do range do __Bot__, para verficar a existência de Bombas.

==Exemplos:
>>>checkMyRange (1,1) [((3,1),1,1,10)]
False
>>>checkMyRange (1,1) [((4,1),1,1,10)]
True

==Notas2:
*Como os movimentos possíveis do __Bot__, são cima, baixo, esquerda e direita, a função verifica nessas posições, a existência de Bombas.
-}
--  Esta função verifica se à volta do Bot há perigo de bombas.
checkMyRange :: Posicao -> [Bomba] -> Bool
checkMyRange (c,l) listaBmb = dir1 == Nada && dir2 == Nada && dir3 == Nada && dir4 == Nada
                                where dir1 = checkBomb listaBmb (c,l-1)
                                      dir2 = checkBomb listaBmb (c,l+1)
                                      dir3 = checkBomb listaBmb (c-1,l)
                                      dir4 = checkBomb listaBmb (c+1,l)
{-|
==Descrição:
A função __followPlayer__, recebe a informação do jogo, a posição do __Bot__, as coordenadas objetivo e o jogador, e determina qual o comando para perseguir e tentar matar um jogador.
-}
followPlayer :: InfGame -> Posicao -> CoordX -> CoordY -> Gamer -> Maybe Char   -- -> esta função é nova!
followPlayer infgame (c,l) objx objy gamer
    | dirx /= Null && p1 = Just 'B'
    | diry /= Null && p2 = Just 'B'
    | dirx /= Null && b1 == Vazio = dirxx
    | diry /= Null && b2 == Vazio = diryy
    | dirx /= Null && b1 == Tijolo = Just 'B' -- -> mudei a ordem para testar uma coisa!
    | diry /= Null && b2 == Tijolo = Just 'B'
    | otherwise = Nothing
        where (m,p,b,j) = infgame
              (dirx,diry) = whatDir (c,l) (objx,objy)
              ((x1,y1),(x2,y2)) = whatPos dirx diry (c,l)
              b1 = bloco infgame (x1,y1)
              b2 = bloco infgame (x2,y2)
              p1 = playerPos j (x1,y1) 0 gamer
              p2 = playerPos j (x2,y2) 0 gamer
              dirxx = whatComand dirx
              diryy = whatComand diry
{-|
==Descrição:
A função __selectPlayer__, recebe a lista de jogadores, a posição do __Bot__, o valor da distância entre pontos, que é inicializado a 3*tamanho, uma posição inicializada a (0,0) (/o objetivo desta é guardar a posição do jogador mais próximo/), uma flag de controlo de jogadores, e por fim o número do jogador. Esta função seleciona o jogador mais próximo do __Bot__.

==Exemplos:
>>>selectPlayer [Vivo (1,1) 0 0,Vivo (3,3) 0 1,Vivo (7,7) 0 0,Vivo (4,3) 0 0] (1,1) 27 (0,0) 0 0
(3,3)
>>>selectPlayer [Vivo (5,5) 0 0,Vivo (3,3) 0 1,Vivo (7,7) 0 0,Vivo (4,3) 0 0] (5,5) 27 (0,0) 0 0
(4,3)

Notas:

*O método utilizado nesta função, foi a fórmula matemática da distância entre dois pontos.
*O tamanho do mapa utilizado nos exemplos é 9.
-}
--  Diz-me qual o jogador mais próximo.
selectPlayer :: Players -> Posicao -> Distance -> Posicao -> Flag -> Gamer -> Posicao    -- -> esta função é nova!
selectPlayer [] (c2,l2) lastDist (caux,laux) flag gamer = (caux,laux)
selectPlayer ((Morto):t) (c2,l2) lastDist (caux,laux) flag gamer = selectPlayer t (c2,l2) lastDist (caux,laux) (flag+1) gamer
selectPlayer ((Vivo (c1,l1) b f):t) (c2,l2) lastDist (caux,laux) flag gamer
  | flag == gamer = selectPlayer t (c2,l2) lastDist (caux,laux) (flag+1) gamer
  | dist < lastDist = selectPlayer t (c2,l2) dist (c1,l1) (flag+1) gamer
  | otherwise = selectPlayer t (c2,l2) lastDist (caux,laux) (flag+1) gamer
    where dist = sqrt(((fromIntegral c2)-(fromIntegral c1))^2 + ((fromIntegral l2)-(fromIntegral l1))^2)
{-|
==Descrição:
A função __selectPowerUp__, recebe a lista de power ups, a posição atual do __Bot__, a distância inicializada a zero, uma posição, para guardar a posição mais próxima, caso não exista power ups, retorna (0,0), pois esta foi inicializada. Esta função retorna a posição do power up mais próximo.

==Exemplos:
>>>selectPowerUp [] (5,5) (27) (0,0)
(0,0)
>>>selectPowerUp [Bomb (3,1)] (5,5) (27) (0,0)
(3,1)
>>>selectPowerUp [Bomb (3,1), Flame (3,2)] (5,5) (27) (0,0)
(3,2)
>>>selectPowerUp [Bomb (3,1), Flame (3,2), Flame (5,6)] (5,5) (27) (0,0)
(5,6)

==Notas:
* Caso não existam power ups, a função retorna a posição (0,0).
-}
selectPowerUp :: [PowUp] -> Posicao -> Distance -> Posicao -> Posicao
selectPowerUp [] (x,y) lastDist (caux,laux) = (caux,laux)
selectPowerUp ((Flame (c,l)):t) (x,y) lastDist (caux,laux)
    | dist < lastDist = selectPowerUp t (x,y) dist (c,l)
    | otherwise = selectPowerUp t (x,y) lastDist (caux,laux)
    where dist = sqrt(((fromIntegral x)-(fromIntegral c))^2 + ((fromIntegral y)-(fromIntegral l))^2)
selectPowerUp ((Bomb (c,l)):t) (x,y) lastDist (caux,laux)
    | dist < lastDist = selectPowerUp t (x,y) dist (c,l)
    | otherwise = selectPowerUp t (x,y) lastDist (caux,laux)
    where dist = sqrt(((fromIntegral x)-(fromIntegral c))^2 + ((fromIntegral y)-(fromIntegral l))^2)

selectPowerUpORPlayer :: Posicao -> Posicao -> Posicao -> Bool
selectPowerUpORPlayer (myx,myy) (x,y) (z,w) | dist1 <= dist2 = True
                                            | otherwise = False
                                              where dist1 = sqrt(((fromIntegral myx)-(fromIntegral x))^2 + ((fromIntegral myy)-(fromIntegral y))^2)
                                                    dist2 = sqrt(((fromIntegral myx)-(fromIntegral z))^2 + ((fromIntegral myy)-(fromIntegral w))^2)
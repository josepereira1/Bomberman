{-| 
Module      : T3Lib
Description : Este módulo contém as funções para a realização da __Tarefa e__, do projeto __Bomberman__. 
Copyright   : José Pereira <https://github.com/josepereira1>;


Este módulo contém as __funções__, da __Tarefa e__, para compressão do mapa de jogo a nível de caracteres.
-}
module T3Lib where

import Data.Char (isDigit)
import System.Environment
import Data.Char
import T1Lib


{-|
==Descrição:
A função __encode__ remove inicialmente as pedras e depois remove os espaços do mapa, o objetivo desta função é fazer uma compressão do mapa em caracteres.

==Exemplos:
>>>encode (mapa 9 0)
"9\n7\n1??1\n2?2?1\n?2?\n1?2?2\n1??1\n2??3\nf+ 5 2\n+ 3 3\n! 5 5\n"

==Notas:

*A função __mapa__ está definida na biblioteca da Tarefa1.hs denominada T1Lib.

-}
encode :: [String] -> String
encode mapa = remove_spaces (remove_pedras mapa)

{-|
==Descrição:
A função __remove_pedras__ remove todas as pedras do mapa, incluindo as interiores.

*Esta função ainda acrescenta um 'f' antes do ínicio da informação dos power ups.

-}

remove_pedras :: [String] -> String
remove_pedras mapa = aux_remove_pedras mapa 0 
aux_remove_pedras :: [String] -> Int -> String
aux_remove_pedras (h:t) l 
  | l == 0 && (length h) >= 10 = [chr (length h)]++['\n']++(aux_remove_pedras t (l+1))
  | l == 0 = show (length h)++['\n']++(aux_remove_pedras t (l+1)) 
  | l == ((length h) -1) = 'f':lista_infMapa t 
  | otherwise = (delete_stone h)++aux_remove_pedras t (l+1)
    where (x:xs) = h 

{-|
==Descrição:
A função __remove_spaces__ remove os espaços do mapa, mas guarda o número de espaços na String, para depois recuperá-los na descompressão.

==Notas:
*Quando a função encontra 'f' sabes que tem que parar de remover espaços.
-}

remove_spaces :: String -> String
remove_spaces mapa_comp = aux_remove_spaces mapa_comp 0
aux_remove_spaces :: String -> Int -> String
aux_remove_spaces [] numberSpc = []
aux_remove_spaces (h:t) numberSpc | h == 'f' = (h:t)
                                  | h /= ' ' && numberSpc > 0 = show (numberSpc)++[h]++aux_remove_spaces t 0
                                  | h /= ' ' = h:aux_remove_spaces t numberSpc 
                                  | otherwise = aux_remove_spaces t (numberSpc+1)

{-|
==Descrição:
A função __lista_infMapa__ separa as coordenadas dos power ups com um '\n'.
-}

lista_infMapa :: [String] -> String 
lista_infMapa [] = []
lista_infMapa (h:t) = h++"\n"++lista_infMapa t

{-|
==Descrição:
A função __delete_stone__ apaga todas as pedras do mapa

==Notas:
*Esta função é usada na função __remove_pedras__.
-}

delete_stone :: String -> String
delete_stone [] = '\n':[] --vai ser para depois identificar como um enter 
delete_stone (h:t) | h == '#' = delete_stone t
                   | otherwise = h:delete_stone t

{-|
==Descrição:
A função __decode__ recebe o mapa comprimido numa String, desenha o mapa inicial através dessa String.

==Exemplos:
>>>decode (encode (mapa 9 0))
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]

==Notas: 
* Como podemos observar a função __decode__ originou o mapa igual ao ínicial.

>>>mapa 9 0
["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]

*A função __mapa__ está definida na biblioteca da Tarefa1.hs denominada T1Lib.

-}
decode :: String -> [String]
decode mapa = aux_decode mapa 0
aux_decode mapa size = impMapa (draw_spaces (delete_inf (mapa))) (up_info mapa)

{-|
==Descrição:
A função __up_info__ retorna o tamanho do mapa comprimido, tal como podemos ver nas funções anteriores o tamanho do mapa é guardado no início da String.

==Exemplos:
>>>up_info "7\n5\n3\n1??2\n1?1\n5\nf+ 3 3\n+ 3 4\n"
7
>>>up_info "\r\n5??4\n1?2?1\n1?2?1?2?1\n1????1\n2?1????3\n?2?2\n9?1\n?1?2?\n3?1?2?2\n1?4\n2????2?2\nf+ 7 1\n+ 3 2\n+ 7 5\n+ 8 5\n! 10 3\n! 1 8\n! 4 9\n! 3 10\n! 6 11\n"
13

*Como podemos ver no segundo exemplo, a função encode colocou o tamanho do mapa em caracter, como estratégia de compressão, mas a função __up_info__ está preparada para esses casos.
-}

up_info :: String -> Int
up_info (h:t) | isDigit h = (read (take_Snumber (h:t)) :: Int) 
              | otherwise =  ord h         
{-|
==Descrição:
A função __take_Snumber__ isola-me a String com a informação do tamanho do mapa, para depois ser usada na função __up_info__.

==Exemplos:
>>>take_Snumber "7\n5\n3\n1??2\n1?1\n5\nf+ 3 3\n+ 3 4\n"
"7"
-}

take_Snumber :: String -> String
take_Snumber (h:t) | h == '\n' = [] 
                   | otherwise = h:take_Snumber t  


{-|
==Descrição:
A função __delete_inf__ elemina a informção do tamanho do mapa que se encontra no início da String comprimida.

==Exemplos:
>>>delete_inf "7\n5\n3\n1??2\n1?1\n5\nf+ 3 3\n+ 3 4\n"
"s\n5\n3\n1??2\n1?1\n5\nf+ 3 3\n+ 3 4\n"

==Notas:
*Como podemos ver no exemplo, a função __delete_inf__ coloca um /s/ no início da String para saber onde começa o mapa.
-}

delete_inf :: String -> String 
delete_inf (h:t) | h == '\n' = 's':(h:t) --este 's' é para sabermos quando temos que colocar a parede inicial
                 | otherwise = delete_inf t 

{-|
==Descrição:
A função __impMapa__ e a respectiva auxiliar, imprime o mapa através da String comprimida pela função __encode__, utilizando diversas funções.

==Notas:
*Esta função determina as funções que tem de utilizar, através da linha que é inicializada a 0, e posteriormente incrementada.
-}

impMapa :: String -> Int -> [String]
impMapa mapa size  = aux_impMapa mapa size 0
aux_impMapa :: String -> Int -> Int -> [String]
aux_impMapa [] size l = []
aux_impMapa (h:t) size l 
  | h == '\n' = aux_impMapa t size (l+1)
  | h == 's' = (linhaCimaBaixo size):(aux_impMapa t size l) 
  | h == 'f' = (linhaCimaBaixo size):(aux_impMapa t size (l+1))
  | l == 1 || l == (size-2) = (linhaSpilares linha):(aux_impMapa resto size l) 
  | l == 2 || l == (size-3) = (linhaCpilares linha size 0):(aux_impMapa resto size l) 
  | l >= size = linha:aux_impMapa resto size l 
  | even l = (linhaCpilares linha size 0):(aux_impMapa resto size  l)
  | odd l = (linhaSpilares linha):(aux_impMapa resto size  l) 
    where (linha,resto) = need_list (h:t)   

{-|
==Descrição:
A função __need_list__ determina a String que tem que ser enviada para as funções responsáveis por desenhar as linhas do mapa.
-}

need_list :: String -> (String,String)
need_list (h:t) | h == '\n' = ("",(h:t)) 
                | otherwise = ((h:a),b) 
                  where (a,b) = need_list t  

{-|
==Descrição:
A função __linhaSpilares__ desenha as linhas que não tem pedras no meio, recebendo a lista com os caracteres especiais, ou seja, tijolos ou espaços vazios.

==Exemplos:
>>>linhaSpilares " ? ??  ?"
"# ? ??  ?#"

-}

linhaSpilares :: String -> String
linhaSpilares s = '#':s++"#"

{-|
==Descrição:
A função __linhaCpilares__  desenha as linhas que tem pedras no meio, recebendo a lista com os caracteres especiais, ou seja, tijolos ou espaços vazios.

==Exemplos:
>>>linhaCpilares " ? ? " 11 0 
"# #?# #?# #"
-}

linhaCpilares :: String -> Int -> Int -> String 
linhaCpilares "" size c = "#"
linhaCpilares xs size c | even c = '#' : linhaCpilares xs size (succ c)
linhaCpilares (h:t) size c = h : linhaCpilares t size (succ c)

{-|
==Descrição:
A função __draw_spaces__ e __desenhaLinha__ desenham os espaços vazios no mapa, através dos números que aparecem na String comprimida.
-}

draw_spaces :: String -> String
draw_spaces []  = [] 
draw_spaces (h:t) | head t == 'f' = (h:t)
                  | h == 'f' = (h:t)
                  | h == '\n' = h:(desenhaLinha (x:xs))++draw_spaces resto 
                  | otherwise =  h:(draw_spaces t)
                     where (x:xs) = separaElem linha
                           (linha,resto) = agarraLinhas t

desenhaLinha :: [String] -> String
desenhaLinha [] = [] 
desenhaLinha (h:t) | h == "" = []
                   | isDigit (head h) = (desenha_spaces (read h :: Int))++desenhaLinha t 
                   | otherwise = h++desenhaLinha t

{-|
==Descrição:
A função __desenha_spaces__ recebe um número inteiro e retorna uma String com o número de espaços igual a esse inteiro.

==Exemplo:
>>>desenha_spaces 4
"    "
-}

desenha_spaces :: Int -> String
desenha_spaces 0 = []
desenha_spaces flag | flag > 0 = ' ':desenha_spaces (flag-1)

{-|
==Descrição:
A função __agarraLinhas__ seleciona uma linha entre new lines, que corresponde a uma linha do mapa.

==Exemplos:
>>>agarraLinhas "#       #\n# # # # #\n"
("#       #","\n# # # # #\n")

==Notas:
*A String usada no exemplo é aleatória.

-}

agarraLinhas :: String -> (String,String)
agarraLinhas [] = ([],[])
agarraLinhas (h:t) | h == '\n' = ([],(h:t))
                   | otherwise = (h:linha,resto)
                      where (linha,resto) = agarraLinhas t

{-|
==Descrição:
A função __separaElem__ separa numa lista de Strings, todos os elementos de uma lista, mas usando a função __isola_num__.

==Exemplo:
>>>separaElem "3??1"
["3","??","1",""]

==Notas:
*A String utilizada é aleatória.
-}

separaElem :: String -> [String]
separaElem [] = []
separaElem (h:t) | h /= '?'  = lista:tijolos:separaElem resto 
                 | otherwise = tijolos:separaElem resto 
                  where (lista,resto,tijolos) = isola_num (h:t) 

{-|
==Descrição:
A função __isola_num__ isola um número de um String, ou seja, recolhe o número, guardando depois o resto e tijolos.

==Exemplos:
>>>isola_num "13??  "
("13","  ","??")

==Notas:
*A String utilizada é aleatória.
-}

isola_num :: String -> (String,String,String) 
isola_num [] = ([],[],[])
isola_num (h:t) | h == '?' = ([],rest,intrr)
                | otherwise = (h:lista,resto,tijolos)
      where (lista,resto,tijolos) = isola_num t
            (rest,intrr) = getIntrr (h:t)

{-|
==Descrição:
A função __getIntrr__ junta os pontos de interrogação que encontrar seguidos e depois retorna o resto da lista que não foi usada.

==Exemplos:
>>>getIntrr "???    ?"
("    ?","???")

==Notas:
*A String utilizada é aleatória.
-}

getIntrr :: String -> (String, String)
getIntrr [] = ([],[]) 
getIntrr (h:t) | h == '?' = (rest,h:intrr)
               | otherwise = (h:t,[])
                  where (rest,intrr) = getIntrr t

{- |
Module        : Tarefa3_2018li1g127
Description   : Implementação do bot
Copyright     : Luís Sobral <a89474@alunos.uminho.pt>;
                João Guedes <a89588@alunos.uminho.pt>

= Introdução Tarefa 3:
Neste módulo trabalhamos com 2 funções principais : __comprime__ e __descomprime__
* O objetivo desta tarefa é comprimir um estado, o máximo possível com a função __comprime__ e em seguida, conseguir descomprimi-lo com sucesso.
* A maior parte das dificuldades desta tarefa, foi à volta de arranjar forma de dividir o mapa comprimido, de modo a facilitar a descompressão.
= Estratégias Utilizadas:

== Compressão
1. Começamos por comprimir o mapa, a lista de jogadores e a lsita de disparos separadamente.
2. Juntamos tudo, usando a função principal __comprime__ , separando o mapa da lista de jogadores por um "!" e a lista de jogadores por "&".

=== Compressão Mapa
* Começamos por substituir um Bloco Destrutível por um "d", um Bloco Indestrutível por um "i" e um Bloco Vazio por um "v". As linhas são separadas por "/".
* Apesar da redução ser significativa, nós quisemos ir à busca de mais, e por isso, se houvesse 2 ou mais "i"/"v"/"d" seguidos, subsituíriamos isso por __número de blocos seguidos__ seguida da letra correspondente ao bloco. Assim, em mapas significativamente grandes, a nossa taxa de compressão iria ser alta.

=== Compressão Lista de Jogadores
* Optámos por separar os vários jogadores por "|", a posição iria estar de forma a ser x-y seguida da direção seguida das vidas. para separar as vidas, dos lasers, dos choques, como eram todos ints, escolhemos usar "/" para separá-los.

=== Compressão Lista de Disparos
* Escolhemos começar por identificar o tipo de disparo - "c" -> canhão; "q" -> choque ; "l" -> laser.
* Seguido do tipo de disparo (laser ou canhão), vem o identificador (que apenas é 1 número) e logo a seguir vem a Linha, separada pela Coluna por um "-"; a seguir à coluna, vem uma letra - a direção.
* No caso do disparo Choque, a seguir ao "q" vem o identificador (apenas 1 int) e logo a seguir vêm o numero de ticks.
* A separar os vários disparos, escolhemos uma "/".

== Descompressão
1. A descompressão foi onde tivemos mais dificuldades, pois tivémos de fazer funções para separar, além do mapa, da lista de jogadores e da lista de disparos, também os vários disparos e os vários jogadores.
2. Dividimos a descompressão em 3 fases: __descompilaMapa__ , __descompjog__ e __descompldisparos__ .

=== Descompressão  Mapa
* Uma função importante foi a __divstringm__ que dava logo a string correspondente ao mapa.
* A partir dessa string, a __descompilaMapa__ descompilava linha a linha de forma a que descompilasse tudo corretamente.

=== Descompressão Lista de Jogadores
* Nesta fase, há uma outra função __divstringj__ que retira a string correspondente à lista de jogadores.
* Depois a função __descompjog__ , com a ajuda das várias auxiliares que obtinham a posição das linhas, das colunas, a direção, as vidas, os lasers e os choques, comprime a lista de jogadores toda.

=== Descompressão Lista de Disparos
* Em semelhança ás outras funções, esta usa uma função para obter apenas a lista de disparos : __descompldisparos__ .
* Como a descompressão da lista de jogadores, a dificuldade aqui passou em obter os identificadores, a posição e a direção ou os ticks - dependendo do tipo de disparo. Utilizou-se as várias auxiliares para ajudarem no "trabalho".

= Conclusão: 
Nesta tarefa a comprime/descomprime obteve uma boa compressão , mas sentimos algumas dificuldades na funcão desconprime.

Havia várias alternativas na comprime e tentamos que esta tivesse a maior taxa de compressão , tendo em conta a dificuldade que depois seria descomprimir.
-}
module Tarefa3_2018li1g127 where

import LI11819
import Tarefa1_2018li1g127

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [(Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]][(Jogador (2,2) D 1 1 1)] [(DisparoCanhao 0 (2,3) D)]),(Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (2,2) D 1 1 1),(Jogador (10,10) C 3 4 5)] [(DisparoCanhao 0 (2,3) D)]),(Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (2,2) D 1 1 1),(Jogador (8,10) C 3 4 5),(Jogador (6,6) B 3 1 2)] [(DisparoCanhao 0 (6,3) D),(DisparoLaser 2 (10,3) D),(DisparoChoque 1 5)]),Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]], jogadoresEstado = [Jogador {posicaoJogador = (2,11), direcaoJogador = D, vidasJogador = 2, lasersJogador = 11, choquesJogador = 0},Jogador {posicaoJogador = (8,18), direcaoJogador = E, vidasJogador = 0, lasersJogador = 2, choquesJogador = 0},Jogador {posicaoJogador = (16,9), direcaoJogador = D, vidasJogador = 1, lasersJogador = 0, choquesJogador = 4},Jogador {posicaoJogador = (9,17), direcaoJogador = B, vidasJogador = 2, lasersJogador = 1, choquesJogador = 1}], disparosEstado = [DisparoChoque {jogadorDisparo = 0, tempoDisparo = 4},DisparoLaser {jogadorDisparo = 2, posicaoDisparo = (16,12), direcaoDisparo = D},DisparoCanhao {jogadorDisparo = 3, posicaoDisparo = (10,17), direcaoDisparo = B}]},(Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (1,1) B 10 2 3)] [(DisparoCanhao 0 (2,1) B)])]


-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.


comprime :: Estado -> String
comprime (Estado m l d)= compilaMapa m ++"!"++comprimejog l++"&"++compldisparo d



----------------------------------------------------------------------------------------------------------
-- | se o mapa for constituido só por 1 linha, junta os blocos dessa linha, caso seja constituido por mais de uma linha, comprime os blocos dessa linha com a auxiliar "juntabc" e repete o processo para o restante mapa acrescentando um "/" no meio das linhas
compilaMapa :: Mapa -> String
compilaMapa [] = []
compilaMapa [h] = juntabc h
compilaMapa (h:t) = juntabc h ++"/" ++ compilaMapa t



-- | junta blocos consecutivos e iguais numa linha
juntabc :: [Peca] -> String
juntabc [] = []
juntabc [h] = mudabloco h
juntabc (h:s:t) | h == s && t == [] = "2" ++ mudabloco h
                | h == s = auxjuntabc 2 (s:t)
                | h == Bloco Indestrutivel && t == [] = "i" ++ mudabloco s
                | h == Bloco Destrutivel && t == [] = "d" ++ mudabloco s
                | h == Vazia && t == [] = "v" ++ mudabloco s
                | h == Bloco Indestrutivel = "i" ++ juntabc (s:t)
                | h == Bloco Destrutivel = "d" ++ juntabc (s:t)
                | h == Vazia = "v" ++ juntabc (s:t)

-- | se tiver pelo menos 2 blocos consecutivos, a função vai contar quantos e adicioná-los
auxjuntabc :: Int -> [Peca] -> String
auxjuntabc n [] = []
auxjuntabc n (h:s:t) | (h == s) && (h == Bloco Indestrutivel) && (t == []) = show (n+1) ++ "i"
                     | (h == s) && (h == Bloco Destrutivel) && (t == []) = show (n+1) ++ "d"
                     | (h == s) && (h == Vazia) && (t == []) = show (n+1) ++ "v"
                     | h == Bloco Indestrutivel && (t == []) = (show n) ++ "i" ++ mudabloco s
                     | h == Bloco Destrutivel && (t == []) = (show n) ++ "d" ++ mudabloco s
                     | h == Vazia && (t == []) = (show n) ++ "v" ++ mudabloco s
                     | (h == s) && (h == Bloco Indestrutivel) = auxjuntabc (n+1) (s:t)
                     | (h == s) && (h == Bloco Destrutivel) = auxjuntabc (n+1) (s:t)
                     | (h == s) && (h == Vazia) = auxjuntabc (n+1) (s:t)
                     | h == Bloco Indestrutivel = (show n) ++ "i" ++ juntabc (s:t)
                     | h == Bloco Destrutivel = (show n) ++ "d" ++ juntabc (s:t)
                     | h == Vazia = (show n) ++ "v" ++ juntabc (s:t)

-- | recebe uma peça e converte-a numa String
mudabloco :: Peca -> String
mudabloco p | p == Bloco Indestrutivel = "i"
            | p == Bloco Destrutivel = "d"
            | otherwise = "v"
-------------------------------------------------------------------------------------------------------------

-- | comprime peca para string
compbloco :: Peca -> String
compbloco (Vazia) = "v"
compbloco (Bloco Indestrutivel) = "i"
compbloco (Bloco Destrutivel) = "d"

-- | comprime lista de disparos para string
compldisparo :: [Disparo] -> String
compldisparo [] = ""
compldisparo [x] =compdisparo x
compldisparo (h:t) = compdisparo h ++"/"++ compldisparo t


-- | comprime disparo para string
compdisparo :: Disparo -> String
compdisparo (DisparoCanhao i (x,y) d) = "c" ++ show i ++ show x ++"-"++ show y ++ auxdirecao d
compdisparo (DisparoLaser i (x,y) d) = "l" ++ show i ++ show x ++"-"++ show y ++ auxdirecao d
compdisparo (DisparoChoque i x) = "q" ++ show i ++ show x

-- | Passa cada Jogador para uma String
comprimejog :: [Jogador] -> String
comprimejog [] = ""
comprimejog [Jogador (a,b) d v l c] = show a ++"-"++ show b ++ auxdirecao d ++ show v ++ "/" ++ show l ++ "/" ++ show c
comprimejog ((Jogador (a,b) d v l c):t) = show a ++ "-" ++ show b ++ auxdirecao d ++ show v ++ "/" ++ show l ++ "/" ++ show c ++ "|" ++ comprimejog t


-- | Dada uma direção, transforma-a numa String
auxdirecao :: Direcao -> String
auxdirecao d | d == C = "C"
             | d == D = "D"
             | d == B = "B"
             | d == E = "E"
-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -> Estado
descomprime m= (Estado (descompilaMapa(divstringm m)) (descompjog(divstringj m)) ((descompldisparos(divstringd m))))

-- | encontra a parte da string pertencente ao mapa
divstringm :: String -> String
divstringm (a:t) | a == '!' = ""
                 | otherwise = a:divstringm t

-- | divide string para lista de jogadores, eliminando a parte pertencente ao mapa, e depois a parte pertencente aos disparos
divstringj :: String -> String
divstringj ('&':t) = ""
divstringj (a:t) | a=='!' = a:auxdivj t
                 | otherwise = divstringj t
-- | elimina toda a parte da string pertencente aos disparos
auxdivj :: String -> String
auxdivj "" = ""
auxdivj (h:t) | h=='&' = ""
              | otherwise = h:auxdivj t

-- | divide string para lista de disparos eliminando tudo até à parte dos disparos
divstringd :: String -> String
divstringd [] = []
divstringd (a:t) | a=='&' = t
                 | otherwise = divstringd t



-- | descompila Mapa
-- | inicia quando o primeiro caractere for parte do mapa
descompilaMapa :: String -> Mapa
descompilaMapa [] = []
descompilaMapa ('/':t) = descompilaMapa t
descompilaMapa (h:t) = descompilaLinha (escolhelinhas (h:t)) : descompilaMapa (drop (length (escolhelinhas (h:t))) (h:t))


-- | recebe a lista do mapa e devolve apenas uma linha do mapa
escolhelinhas :: String -> String
escolhelinhas [] = []
escolhelinhas (h:t) | h /= '/'  = [h] ++ escolhelinhas t
                    | otherwise = []

-- | se for só 1 bloco, mete o bloco
-- | caso apareça um número, vai "ler" o número e juntar esse número de elementos numa lista e continuar a função para os seguintes elementos da lista
-- | chama a função para fazer uma linha, tendo como primeiro argumento uma linha do mapa e como segundo argumento, o caractere a repetir

descompilaLinha :: String -> [Peca]
descompilaLinha [] = []
descompilaLinha (h:t) | h == 'i' = Bloco Indestrutivel : descompilaLinha t
                      | h == 'd' = Bloco Destrutivel : descompilaLinha t
                      | h == 'v' = Vazia : descompilaLinha t
                      | otherwise = ajudadescomp (auxlinha (h:t)) (head(drop (length (auxlinha (h:t))) (h:t))) ++ descompilaLinha (drop (length (auxlinha (h:t)) + 1 ) (h:t))
-- | junta os números
auxlinha :: String -> String
auxlinha [] = []
auxlinha (h:t) | (h /= 'v') && (h /= 'i') && (h /= 'd') = [h] ++ auxlinha t
               | otherwise = []

-- | transforma a string num número; recebe o caracter pretendido para descompilar e chama a função para repetir esse elemento x vezes, descompilando assim uma linha do mapa
ajudadescomp :: String -> Char -> [Peca]
ajudadescomp l c = fazxlinha (read(l)) c

-- | repete peça x vezes para construir uma linha
fazxlinha :: Int -> Char -> [Peca]
fazxlinha 0 _ = []
fazxlinha n c | c == 'i' = Bloco Indestrutivel : fazxlinha (n-1) c
              | c == 'd' = Bloco Destrutivel : fazxlinha (n-1) c
              | otherwise = Vazia : fazxlinha (n-1) c







-- | descomprime string para lista de jogadores
descompjog :: String -> [Jogador]
descompjog "" = []
descompjog ('!':t)= descompjog t
descompjog ('|':t)= descompjog t
descompjog l@(h:t) = (Jogador (read (takepos1 l) ,read (takepos2 (takewhilepos l))) (desdir t) (read (encontravida l)) (read (encontralaser l)) (read (encontrachoque l))): descompjog (drop (length ((takepos1 l)++(takepos2(takewhilepos l)) ++ (encontravida l) ++ (encontralaser l) ++ (encontrachoque l)) + 4 ) l)



-- | vê quantas vidas o Jogador possui
encontravida :: String -> String
encontravida [] = []
encontravida (h:s:t)    | (h == 'C' || h == 'D' || h == 'B' || h == 'E') && (s /= '/') = [s] ++ encontravida (h:t)
                        | s == '/' = []
                        | otherwise = encontravida (s:t)



-- | encontra a partir de quando começam todos os argumentos pertencentes ao laser (depois do '/')
encontralaser :: String -> String
encontralaser [] = []
encontralaser (h:s:t) | h == '/' = [s] ++ auxencontralaser t
                      | otherwise = encontralaser (s:t)
-- | junta todos os argumentos pertencentes ao laser
auxencontralaser :: String -> String
auxencontralaser [] = []
auxencontralaser (h:t) | h /= '/' = [h] ++ auxencontralaser t
                       | otherwise = []



-- | vê quantos choques o Jogador possui
-- | elimina parte das vidas
encontrachoque :: String -> String
encontrachoque [] = []
encontrachoque (h:t) | h /= '/' = encontrachoque t
                     | otherwise = aux1encontrachoque t

-- | elimina número de lasers
aux1encontrachoque :: String -> String
aux1encontrachoque [] = []
aux1encontrachoque (h:t) | h /= '/' = aux1encontrachoque t
                         | otherwise = aux2encontrachoque t


-- | encontra número de choques até que haja novo jogador ("|") ou até que acabe a lista de jogadores
aux2encontrachoque :: String -> String
aux2encontrachoque [] = []
aux2encontrachoque (h:t) | h /= '|' = [h] ++ aux2encontrachoque t
                         | otherwise = []







-- | descomprime string para linha do mapa
descomplista :: String -> [Peca]
descomplista "" = []
descomplista (h:t) = (descomppeca [h]):descomplista t

-- | descomprime string para peca
descomppeca :: String -> Peca
descomppeca "v" = (Vazia)
descomppeca "i" = (Bloco Indestrutivel)
descomppeca "d" = (Bloco Destrutivel)

-- | descomprime string para lista de disparos
-- | se for '&' ou '/' remove da lista
-- | descomprime o 1º disparo da lista, e repete o processo para os seguintes
descompldisparos :: String -> [Disparo]
descompldisparos "" = []
descompldisparos ('&':t)= descompldisparos t
descompldisparos ('/':t)= descompldisparos t
descompldisparos l@(h:t) = descdisparo (takewhiledisp (h:t)) : descompldisparos (drop (length (takewhiledisp (h:t))) l )

-- | 1º disparo da lista
takewhiledisp :: String -> String
takewhiledisp "" = ""
takewhiledisp (h:t) | h /= '/' = h: takewhiledisp t
                    | otherwise = ""



-- | descomprime string para disparo
descdisparo :: String -> Disparo
descdisparo ('c':s:t) = (DisparoCanhao (read [s]) (read (takepos1 (takewhilepos t)), read (takepos2 (takewhilepos t))) (desdir t))
descdisparo ('l':s:t) = (DisparoLaser (read [s]) (read (takepos1 (takewhilepos t)), read (takepos2 (takewhilepos t))) (desdir t))
descdisparo ('q':s:a:t) = (DisparoChoque (read [s]) (read (a:t)))

-- | tira posição no formato x-y
takewhilepos :: String -> String
takewhilepos "" = ""
takewhilepos (h:t) | h == 'C' || h == 'D' || h == 'B' || h == 'E' = ""
                   | otherwise = [h] ++ takewhilepos t


-- | retira a primeira posição
takepos1 :: String -> String
takepos1 "" = ""
takepos1 (h:t) | h == '-' = ""
                  | otherwise = [h] ++ takepos1 t


-- | retira a segunda posição
takepos2 :: String -> String
takepos2 (h:t) = drop (length (takepos1 (h:t)) + 1) (h:t)







-- | descobre Direção
desdir :: String -> Direcao
desdir (h:t) | h == 'C' = C
             | h == 'D' = D
             | h == 'B' = B
             | h == 'E' = E
             | otherwise = desdir t

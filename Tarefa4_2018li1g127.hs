-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g127 where

import LI11819
import Tarefa0_2018li1g127
import Tarefa1_2018li1g127
import Data.List
-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [(Estado (mapaInicial (8,8)) [(Jogador (2,3) B 5 4 3),(Jogador (5,3) E 2 2 2)] [DisparoLaser 0 (4,3) B]),
            (Estado (mapaInicial (8,8)) [(Jogador (2,3) B 5 4 3),(Jogador (5,3) E 2 2 2)] [DisparoCanhao 0 (4,3) B]),
            (Estado (mapaInicial (8,8)) [(Jogador (2,3) B 5 4 3),(Jogador (5,3) E 2 2 2)] [DisparoChoque 0 4]),
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
            [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
            [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
            [Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],
            [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
            [(Jogador (2,3) B 5 4 3),(Jogador (2,5) B 2 2 2)] [DisparoLaser 0 (4,3) B]),
            (Estado (mapaInicial (8,8)) [(Jogador (2,3) B 5 4 3),(Jogador (5,3) E 2 2 2)] [DisparoCanhao 0 (4,3) B,DisparoLaser 0 (3,3) B,DisparoChoque 1 4]),
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
            [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
            [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
            [Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],
            [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
            [(Jogador (2,2) D 5 4 3),(Jogador (2,5) B 2 2 2)] [DisparoLaser 0 (2,4) D]),
            (Estado (mapaInicial (8,8)) [(Jogador (2,3) B 5 4 3),(Jogador (5,3) E 1 2 2)] [DisparoLaser 0 (4,3) B]),
            (Estado (mapaInicial (8,8)) [(Jogador (4,3) C 5 4 3),(Jogador (2,3) B 1 2 2)] [DisparoLaser 0 (2,3) C]),
            (Estado (mapaInicial (8,8)) [(Jogador (2,3) E 5 4 3),(Jogador (5,3) E 1 2 2)] [DisparoLaser 0 (4,3) E]),
            (Estado (mapaInicial (8,8)) [(Jogador (2,3) E 5 4 3),(Jogador (5,3) E 1 2 2)] [DisparoChoque 0 0]),
            (Estado (mapaInicial (8,8)) [(Jogador (2,3) E 5 4 3),(Jogador (5,3) E 1 2 2)] [DisparoCanhao 0 (1,1) C]),
            (Estado (mapaInicial (8,8)) [(Jogador (2,3) E 5 4 3),(Jogador (5,3) E 1 2 2)] [DisparoCanhao 0 (0,1) C]),
            (Estado (mapaInicial (8,8)) [(Jogador (2,3) E 5 4 3),(Jogador (5,3) E 1 2 2)] [DisparoCanhao 0 (5,2) E]),
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (3,2) D 5 4 3)] [DisparoCanhao 0 (3,3) D]),
            (Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (3,2) D 5 4 3)] [DisparoCanhao 0 (3,3) D]),
            (Estado (mapaInicial (8,8)) [(Jogador (5,3) E 5 4 3)] [(DisparoCanhao 0 (2,2) D),(DisparoCanhao 0 (2,3) E)]),
            (Estado (mapaInicial (8,8)) [(Jogador (5,3) E 5 4 3)] [(DisparoCanhao 0 (2,3) D),(DisparoCanhao 0 (2,2) E)]),
            (Estado (mapaInicial (8,8)) [(Jogador (5,3) E 5 4 3)] [(DisparoCanhao 0 (2,3) D),(DisparoCanhao 0 (2,2) D)])
            ]

-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers (Estado m j d) = (Estado (mudamapa m (esclasers d)) (listjogadores (esclasers d) j m) (listdisparos (esclasers d) d m))

-- | cria lista apenas com os lasers que estao na lista de disparos
esclasers :: [Disparo] -> [Disparo]
esclasers [] = []
esclasers ((DisparoLaser a b c):t) = (DisparoLaser a b c):esclasers t
esclasers (_:t) = esclasers t

-- | altera lista de jogadores de acordo com os efeitos dos lasers
listjogadores :: [Disparo] -> [Jogador] -> Mapa -> [Jogador]
listjogadores [] l _ = l
listjogadores (h:t) l m= listjogadores t (afetajogador h l m) m

-- | altera jogador de acordo com o efeito de um laser
afetajogador :: Disparo -> [Jogador] -> Mapa -> [Jogador]
afetajogador _ [] _ = []
afetajogador (DisparoLaser 0 p d) (h:t) m = h:afetajogador (DisparoLaser 3 p d) t m
afetajogador (DisparoLaser j p1 d1) ((Jogador p2 d2 v l c):t) m | veriftank m p1 p2 d1 && v>0 = (Jogador p2 d2 (v-1) l c):afetajogador (DisparoLaser (j-1) p1 d1) t m
                                                                | otherwise = (Jogador p2 d2 v l c):afetajogador (DisparoLaser (j-1) p1 d1) t m

-- | altera lista disparos de acordo com o  efeito todos os lasers
---------------lista lasers----------------------------
listdisparos :: [Disparo] -> [Disparo] -> Mapa -> [Disparo]
listdisparos [] [] _ = []
listdisparos [] l m = l
listdisparos (a:b) l m = listdisparos b (afetadisparos a l m) m

-- | altera lista disparos de acordo com o  efeito de um laser e elimina lasers da lista
afetadisparos :: Disparo -> [Disparo] -> Mapa -> [Disparo]
afetadisparos _ [] _ = []
afetadisparos a ((DisparoCanhao x y z):t) m | verifcanhao m (posicaoDisparo a) y (direcaoDisparo a) = afetadisparos a t m
                                            | otherwise = (DisparoCanhao x y z):afetadisparos a t m
afetadisparos a ((DisparoChoque x y):t) m = (DisparoChoque x y):afetadisparos a t m
afetadisparos a (_:t) m = afetadisparos a t m

-- | altera mapa de acordo com o efeitos dos lasers
mudamapa :: Mapa -> [Disparo] -> Mapa
mudamapa m [] = m
mudamapa m (h:t) = mudamapa (alteramapa m h) t

-- | altera mapa de acordo com o efeito de um laser
alteramapa :: Mapa -> Disparo -> Mapa
alteramapa m (DisparoLaser j (x,y) d) | d == C && blocosmapa m (x,y) d = alteramapa m (DisparoLaser j (x-1,y) d)
                                      | d == B && blocosmapa m (x,y) d = alteramapa m (DisparoLaser j (x+1,y) d)
                                      | d == E && blocosmapa m (x,y) d = alteramapa m (DisparoLaser j (x,y-1) d)
                                      | d == D && blocosmapa m (x,y) d = alteramapa m (DisparoLaser j (x,y+1) d)
                                      | (d == C || d == E) && encontraPosicaoMatriz (x,y) m == Bloco Destrutivel = alteramapa (atualizaPosicaoMatriz (x,y) (Vazia) m) (DisparoLaser j (x,y) d)
                                      | (d == C || d == D) && encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel = alteramapa (atualizaPosicaoMatriz (x,y+1) (Vazia) m) (DisparoLaser j (x,y) d)
                                      | (d == B || d == E) && encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel = alteramapa (atualizaPosicaoMatriz (x+1,y) (Vazia) m) (DisparoLaser j (x,y) d)
                                      | (d == B || d== D) && encontraPosicaoMatriz (x+1,y+1) m == Bloco Destrutivel = alteramapa (atualizaPosicaoMatriz (x+1,y+1) (Vazia) m) (DisparoLaser j (x,y) d)
                                      | verifmapa m (x,y) d == False = m

-- | verifica se há blocos na direcao do lasers
blocosmapa :: Mapa -> Posicao -> Direcao -> Bool
blocosmapa m (x,y) C = encontraPosicaoMatriz (x,y) m == Vazia && encontraPosicaoMatriz (x,y+1) m == Vazia
blocosmapa m (x,y) B = encontraPosicaoMatriz (x+1,y) m == Vazia && encontraPosicaoMatriz (x+1,y+1) m == Vazia
blocosmapa m (x,y) E = encontraPosicaoMatriz (x,y) m == Vazia && encontraPosicaoMatriz (x+1,y) m == Vazia
blocosmapa m (x,y) D = encontraPosicaoMatriz (x,y+1) m == Vazia && encontraPosicaoMatriz (x+1,y+1) m == Vazia

-- | verifica se ha blocos indestrutiveis no caminho do lasers
verifmapa :: Mapa -> Posicao -> Direcao -> Bool
verifmapa m (x,y) C = (encontraPosicaoMatriz (x,y) m /= Bloco Indestrutivel && encontraPosicaoMatriz (x,y+1) m /= Bloco Indestrutivel)
verifmapa m (x,y) B = (encontraPosicaoMatriz (x+1,y) m /= Bloco Indestrutivel && encontraPosicaoMatriz (x+1,y+1) m /= Bloco Indestrutivel)
verifmapa m (x,y) E = (encontraPosicaoMatriz (x,y) m /= Bloco Indestrutivel && encontraPosicaoMatriz (x+1,y) m /= Bloco Indestrutivel)
verifmapa m (x,y) D = (encontraPosicaoMatriz (x,y+1) m /= Bloco Indestrutivel && encontraPosicaoMatriz (x+1,y+1) m /= Bloco Indestrutivel)

-- | verifica se a posicao do tank afetada pelo laser
veriftank :: Mapa -> Posicao -> Posicao -> Direcao -> Bool
veriftank m (x,y) (a,b) C | verifmapa m (x,y) C && x>a && (y == b || y+1 == b || y-1 == b) = veriftank m (x-1,y) (a,b) C
                          | x == a && (y == b || y+1 == b || y-1 ==b) = True
                          | otherwise = False
veriftank m (x,y) (a,b) B | verifmapa m (x,y) B && x<a && (y == b || y+1 == b || y-1 == b) = veriftank m (x+1,y) (a,b) B
                          | x == a && (y == b || y+1 == b || y-1 == b) = True -- && verifmapa m (x,y) B = True
                          | otherwise = False
veriftank m (x,y) (a,b) E | verifmapa m (x,y) E && y > b && (x == a || x+1 == a || x-1 == a) = veriftank m (x,y-1) (a,b) E
                          | y == b && (x == a || x+1 == a || x-1 == a) = True
                          | otherwise = False
veriftank m (x,y) (a,b) D | verifmapa m (x,y) D && y < b && (x == a || x+1 == a || x-1 == a) = veriftank m (x,y+1) (a,b) D
                          | y == b && (x == a || x+1 == a || x-1 == a) = True
                          | otherwise = False

-- | verifica se a posicao da balacanhao é afetada pelo laser
verifcanhao :: Mapa -> Posicao -> Posicao -> Direcao -> Bool
verifcanhao m (x,y) (a,b) d | x == a && y == b && verifmapa m (x,y) d = True
                            | d == C && verifmapa m (x,y) d && x>a && y == b = verifcanhao m (x-1,y) (a,b) d
                            | d == B && verifmapa m (x,y) d && x<a && y == b = verifcanhao m (x+1,y) (a,b) d
                            | d == E && verifmapa m (x,y) d && y>b && x == a = verifcanhao m (x,y-1) (a,b) d
                            | d == D && verifmapa m (x,y) d && y<b && x == a = verifcanhao m (x,y+1) (a,b) d
                            | otherwise = False


-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado-------------------------------------------------------------------------------------------------------------------------------------FALTA ELIMINAR BALAS COM COSTAS VIRADAS
tickCanhoes (Estado m j d) = (Estado (alteramapaC m (elimina (eliminacostas (separacanhao d)))) (mataj j (contrajogador j (elimina (eliminacostas (separacanhao d))))) ((mantemdisp d) ++ (modificadisparo m j (elimina (eliminacostas (separacanhao d))))))


--------------------------MAPA --------------------------------------------------------------------------
-- | recebendo o mapa e uma lista com disparos que chocam contra paredes, e , dependendo da direção, chama uma auxiliar para alterar o mapa corretamente
alteramapaC :: Mapa -> [Disparo] -> Mapa
alteramapaC m [] = m
alteramapaC m (h:t) = let (a,b) = (posicaoDisparo h)
                      in if direcaoDisparo h == D then alteramapaC (auxalteramapaCH m (a,b+1) (a+1)) t
                        else if direcaoDisparo h == E then alteramapaC (auxalteramapaCH m (a,b) (a+1)) t
                        else if direcaoDisparo h == B then alteramapaC (auxalteramapaCV m (a+1,b) (b+1)) t
                            else alteramapaC (auxalteramapaCV m (a,b) (b+1)) t

-- | recebe o mapa, a posição onde os blocos se localizam, e um inteiro, que vai servir para destruir os 2 blocos que a bala tem contacto
-- | se o bloco for destrutivel, então substitui o bloco por um bloco vazio e repete o processo para o bloco a baixo
-- | se o bloco for indestrutivel, apenas repete o processo para o bloco a baixo
auxalteramapaCH :: Mapa -> Posicao -> Int -> Mapa
auxalteramapaCH m (a,b) x | a > x = m
                          | encontraPosicaoMatriz (a,b) m == Bloco Destrutivel = auxalteramapaCH (atualizaPosicaoMatriz (a,b) Vazia m) (a+1,b) x
                          | otherwise = auxalteramapaCH m (a+1,b) x

-- | recebe o mapa, a posição onde os blocos se localizam, e um inteiro, que vai servir para destruir os 2 blocos que a bala tem contacto
-- | se o bloco for destrutivel, então substitui o bloco por um bloco vazio e repete o processo para o bloco ao lado
-- | se o bloco for indestrutivel, apenas repete o processo para o bloco ao lado
auxalteramapaCV :: Mapa -> Posicao -> Int -> Mapa
auxalteramapaCV m (a,b) y | b > y = m
                          | encontraPosicaoMatriz (a,b) m == Bloco Destrutivel = auxalteramapaCV (atualizaPosicaoMatriz (a,b) Vazia m) (a,b+1) y
                          | otherwise = auxalteramapaCV m (a,b+1) y

-- | Separa lista de disparos que chocam só contra uma parede
contraparede :: Mapa -> [Disparo] -> [Disparo]
contraparede m [] = []
contraparede m (h:t) | (encontraPosicaoMatriz (posicaoDisparo h) m) == Vazia = contraparede m t
                     | otherwise = h : contraparede m t

---------------------------------------------LISTA DE JOGADORES---------------------------------------------------------------------

-- | recebe uma lista de jogadores e uma lista de disparos
-- | subtrai todas as vidas dos jogadores que intersetam disparos com a auxiliar subtraivida
mataj :: [Jogador] -> [Disparo] -> [Jogador]
mataj j [] = j
mataj [] _ = []
mataj (j:js) d  | vidasJogador j == 0 = j : mataj js d
                | otherwise = subtraivida j d : mataj js d


-- | Recebe um jogador e uma lista de disparos; se o jogador interceta com algum disparo, diminui 1 vida
subtraivida :: Jogador -> [Disparo] -> Jogador
subtraivida j [] = j
subtraivida j@(Jogador (a,b) z v l c) (d:ds) | (posicaoDisparo d == (a,b+1) && direcaoDisparo d == D) || (posicaoDisparo d == (a,b-1) && direcaoDisparo d == E) || (posicaoDisparo d == (a+1,b) && direcaoDisparo d == B) || (posicaoDisparo d == (a-1,b) && direcaoDisparo d == C) = subtraivida j ds
                                             | (posicaoDisparo d == (a,b) || posicaoDisparo d == (a+1,b) || posicaoDisparo d == (a-1,b) || posicaoDisparo d == (a,b+1) || posicaoDisparo d == (a,b-1) || posicaoDisparo d == (a+1,b-1) || posicaoDisparo d == (a+1,b+1) || posicaoDisparo d == (a-1,b+1) || posicaoDisparo d == (a-1,b-1)) = subtraivida (Jogador (a,b) z (v-1) l c) ds
                                             | otherwise = subtraivida j ds

-- | reduz a lista de disparos apenas aos disparos que acertam em jogadores
contrajogador :: [Jogador] -> [Disparo] -> [Disparo]
contrajogador _ [] = []
contrajogador j (h:t) | verificabalatanquel h j = h : (contrajogador j t)
                      | otherwise = contrajogador j t


-- | verifica se uma bala acerta em algum jogador de uma lista
verificabalatanquel :: Disparo -> [Jogador] -> Bool
verificabalatanquel d [] = False
verificabalatanquel d ((Jogador (a,b) z v l c):t) | v == 0 = verificabalatanquel d t
                                                  | (posicaoDisparo d == (a,b+1) && direcaoDisparo d == D) || (posicaoDisparo d == (a,b-1) && direcaoDisparo d == E) || (posicaoDisparo d == (a+1,b) && direcaoDisparo d == B) || (posicaoDisparo d == (a-1,b) && direcaoDisparo d == C) = verificabalatanquel d t
                                                  | (posicaoDisparo d == (a,b) || posicaoDisparo d == (a+1,b) || posicaoDisparo d == (a-1,b) || posicaoDisparo d == (a,b+1) || posicaoDisparo d == (a,b-1) || posicaoDisparo d == (a+1,b-1) || posicaoDisparo d == (a+1,b+1) || posicaoDisparo d == (a-1,b+1) || posicaoDisparo d == (a-1,b-1)) = True
                                                  | otherwise = verificabalatanquel d t

----------------------------------------DISPAROS-------------------------------------------------------------------------------
-- | recebe a lista de disparos iniciais e mantêm apenas os que não são canhão
mantemdisp :: [Disparo] -> [Disparo]
mantemdisp [] = []
mantemdisp ((DisparoCanhao i p d):t) = mantemdisp t
mantemdisp (h:t) = h : mantemdisp t

-- | Fica com uma lista só com disparos de canhão
separacanhao :: [Disparo] -> [Disparo]
separacanhao [] = []
separacanhao (h@(DisparoCanhao i p d):t) = h : (separacanhao t)
separacanhao (_:t) = separacanhao t

-- | recebendo um mapa e uma lista de disparos, altera a lista de disparos, avançando aqueles em que a sua posição é vazia e retirando aqueles em que não é (choca com alguma coisa)
modificadisparo :: Mapa -> [Jogador] -> [Disparo] -> [Disparo]
modificadisparo m j [] = []
modificadisparo m j (h@(DisparoCanhao i (a,b) dir):t) | verificabalatanquel h j = modificadisparo m j t
                                                      | dir == D && ((encontraPosicaoMatriz (a,b+1) m) == Vazia) && ((encontraPosicaoMatriz (a+1,b+1) m) == Vazia) = avancabala h : modificadisparo m j t
                                                      | dir == E && ((encontraPosicaoMatriz (a,b) m) == Vazia) && ((encontraPosicaoMatriz (a+1,b) m) == Vazia) = avancabala h : modificadisparo m j t
                                                      | dir == B && ((encontraPosicaoMatriz (a+1,b) m) == Vazia) && ((encontraPosicaoMatriz (a+1,b+1) m) == Vazia) = avancabala h : modificadisparo m j t
                                                      | dir == C && ((encontraPosicaoMatriz (a,b) m) == Vazia) && ((encontraPosicaoMatriz (a,b+1) m) == Vazia) = avancabala h : modificadisparo m j t
                                                      | otherwise = modificadisparo m j t
modificadisparo m j (h:t) = h : modificadisparo m j t
------------------------------------------------------------------------------------------------------------------

-- | recebe uma lista de disparos e remove todos os disparos em que a posição é a mesma
-- | se a lista for vazia, devolve a mesma, caso contrário, verifica se o primeiro disparo tem algum outro com a mesma posição; se sim, então chama uma auxiliar para eliminar todos os disparos com essa posição; caso contrário, mantêm o disparo e verifica o mesmo para todos os disparos seguintes
elimina :: [Disparo] -> [Disparo]
elimina [] = []
elimina (h@(DisparoCanhao i p dir):t) | elemdisparos h t = eliminarepetidas h t
                                      | otherwise = h : elimina t
elimina (h:t) = h : elimina t

-- | recebe um disparo e uma lista de disparos
-- | se a posição do disparo for igual ao do primeiro da lista, então retorna Verdade; caso contrário, verifica para o resto da lista
elemdisparos :: Disparo -> [Disparo] -> Bool
elemdisparos d [] = False
elemdisparos d (h@(DisparoCanhao i p dir):t) | posicaoDisparo d == posicaoDisparo h = True
                                             | otherwise = elemdisparos d t
elemdisparos d (h:t) = elemdisparos d t

-- | elimina todos os disparos com a mesma posição do disparo dado
-- | se a posição do primeiro disparo for igual ao do primeiro da lista, então elimina o primeiro; caso contrário, verifica-o para o resto da lista
eliminarepetidas :: Disparo -> [Disparo] -> [Disparo]
eliminarepetidas d [] = []
eliminarepetidas d (h@(DisparoCanhao i p dir):t) | posicaoDisparo d == posicaoDisparo h = eliminarepetidas d t
                                                 | otherwise = h : eliminarepetidas d t
eliminarepetidas d (h:t) = h : eliminarepetidas d t

------------------------------------------------------------------------------------------------------------------

-- | recebendo um disparocanhao, avança esse disparo consoante a direção dada
-- | Dependendo da direção, move a bala de canhão um bloco
avancabala :: Disparo -> Disparo
avancabala (DisparoCanhao i (a,b) d) | d == C = (DisparoCanhao i (a-1,b) d)
                                     | d == B = (DisparoCanhao i (a+1,b) d)
                                     | d == D = (DisparoCanhao i (a,b+1) d)
                                     | otherwise = (DisparoCanhao i (a,b-1) d)


-- | recebe um disparo e uma lista de disparos e verifica se existe algum disparo nessa lista que esteja virado de costas para o mesmo
verificadisparocostas :: Disparo -> [Disparo] -> Bool
verificadisparocostas d [] = False
verificadisparocostas s@(DisparoCanhao i (a,b) d) ((DisparoCanhao id (x,y) dir):t) | a == x && d == E && dir == D && b+1 == y = True
                                                                                   | a == x && d == D && dir == E && b-1 == y = True
                                                                                   | b == y && d == C && dir == B && x-1 == a = True
                                                                                   | b == y && d == B && dir == C && x+1 == a = True
                                                                                   | otherwise = verificadisparocostas s t

-- | Recebe uma lista de disparos e, usando uma auxiliar que elimina, numa lista de disparos, os disparos que estão nas costas, elimina todos os disparos numa lista que se encontram nas costas
eliminacostas :: [Disparo] -> [Disparo]
eliminacostas [] = []
eliminacostas (h:t) | verificadisparocostas h t = eliminacostas ((h:t) \\ (auxeliminacostas h t))
                    | otherwise = h : eliminacostas t



-- | recebe um disparo e uma lista de disparos
-- | se o disparo tem direção oposta, e está nas costas de outro disparo, ambos os disparos são eliminados
auxeliminacostas :: Disparo -> [Disparo] -> [Disparo]
auxeliminacostas e [] = []
auxeliminacostas e@(DisparoCanhao i (a,b) d) (s@(DisparoCanhao id (x,y) dir):t) | a == x && d == E && dir == D && b+1 == y = [e]++[s]
                                                                                | a == x && d == D && dir == E && b-1 == y = [e]++[s]
                                                                                | b == y && d == C && dir == B && x-1 == a = [e]++[s]
                                                                                | b == y && d == B && dir == C && x+1 == a = [e]++[s]
                                                                                | otherwise = auxeliminacostas e t


--------------------------------------------------------------------------------------------------------------------------------
-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques (Estado m l d)= (Estado m l (alterachoques d))

-- | altera os choques que estao na lista de disparos
alterachoques :: [Disparo] -> [Disparo]
alterachoques [] = []
alterachoques ((DisparoChoque i ti):t) | ti > 0 = (DisparoChoque i (ti-1)):alterachoques t
                                      | ti == 0 = alterachoques t
alterachoques (h:t) = h : alterachoques t

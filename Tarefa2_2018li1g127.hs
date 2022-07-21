-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g127 where

import LI11819
import Tarefa0_2018li1g127
import Tarefa1_2018li1g127
-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0,(Movimenta B), Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (1,1) B 1 1 1)] []),(1,(Dispara Laser), Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]][(Jogador (1,1) B 1 1 1),(Jogador (3,3) B 1 1 1),(Jogador (1,3) B 1 1 1)] [(DisparoChoque 2 5)]),(2,(Movimenta E), Estado [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]] [(Jogador (1,1) B 1 1 1),(Jogador (1,3) B 1 1 1),(Jogador (3,3) B 1 1 1)] [(DisparoChoque 1 5)]),
            (0 , Dispara Choque, (Estado (mapaInicial (6,6)) [Jogador (1,1) C 5 3 3, Jogador (3,3) B 5 2 2] [DisparoChoque 0 5])),
            (1 , Movimenta B, (Estado (mapaInicial (6,6)) [Jogador (1,1) C 5 3 3, Jogador (3,3) B 5 2 2] [])),
            (0 , Dispara Laser, (Estado (mapaInicial (6,6)) [Jogador (2,2) C 5 3 2, Jogador (1,3) D 4 2 2] [] )),
            (2 , Movimenta D, (Estado (mapaInicial (8,8)) [Jogador (1,4) C 5 1 1, Jogador (3,1) E 2 3 3, Jogador (3,5) B 4 3 1] [] )),
            (0 , Dispara Laser, (Estado (mapaInicial (7,5)) [Jogador (1,1) B 5 3 3, Jogador (3,1) C 5 2 2] [])),
            (1 , Dispara Canhao, (Estado (mapaInicial (7,7)) [Jogador (2,2) C 5 3 3, Jogador (3,3) E 4 2 2] [] )),
            (0 , Movimenta C, (Estado (mapaInicial (10,10)) [Jogador (5,3) C 1 2 3, Jogador (3,3) B 1 2 3] [])),
            (0 , Movimenta C, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) B 1 2 3] [])),
            (1 , Movimenta C, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) B 0 2 3] [])),
            (1 , Dispara Laser, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) B 1 0 3] [])),
            (1 , Dispara Laser, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) B 0 2 3] [])),
            (1 , Movimenta B, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) B 0 2 3] [])),
            (1 , Dispara Laser, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) B 1 1 0] [])),
            (1 , Dispara Choque, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) B 1 0 0] [])),
            (1 , Movimenta E, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) B 0 2 3] [])),
            (1 , Movimenta E, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) E 0 2 3] [])),
            (1 , Movimenta D, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) B 0 2 3] [])),
            (1 , Movimenta D, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) D 0 2 3] [])),
            (1 , Dispara Canhao, (Estado (mapaInicial (7,7)) [Jogador (2,2) C 5 3 3, Jogador (3,3) C 4 2 2] [] )),
            (1 , Dispara Canhao, (Estado (mapaInicial (7,7)) [Jogador (2,2) C 5 3 3, Jogador (3,3) B 4 2 2] [] )),
            (1 , Dispara Canhao, (Estado (mapaInicial (7,7)) [Jogador (2,2) C 5 3 3, Jogador (3,3) D 4 2 2] [] )),
            (1 , Dispara Laser, (Estado (mapaInicial (7,5)) [Jogador (1,1) B 5 3 3, Jogador (3,1) E 5 2 2] [])),
            (1 , Dispara Laser, (Estado (mapaInicial (7,5)) [Jogador (1,1) B 5 3 3, Jogador (3,1) D 5 2 2] [])),
            (1 , Movimenta D, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) D 1 2 3] [DisparoChoque 0 5])),
            (1 , Movimenta D, (Estado (mapaInicial (10,10)) [Jogador (3,4) C 1 2 3, Jogador (3,3) D 1 2 3] [])),
            (0 , Movimenta D, (Estado (mapaInicial (10,10)) [Jogador (5,5) C 1 2 3, Jogador (3,3) D 1 2 3] [])),
            (0 , Movimenta D, (Estado (mapaInicial (10,10)) [Jogador (5,5) D 1 2 3, Jogador (3,3) D 1 2 3] [])),
            (0 , Movimenta E, (Estado (mapaInicial (10,10)) [Jogador (5,5) E 1 2 3, Jogador (5,3) D 1 2 3] [])),
            (0 , Movimenta E, (Estado (mapaInicial (10,10)) [Jogador (5,5) E 1 2 3, Jogador (5,3) D 1 2 3] [])),
            (0 , Movimenta D, (Estado (mapaInicial (10,10)) [Jogador (5,5) D 1 2 3, Jogador (6,7) D 1 2 3] [])),
            (0 , Movimenta B, (Estado (mapaInicial (10,10)) [Jogador (5,5) B 1 2 3, Jogador (7,4) D 1 2 3] [])),
            (1 , Movimenta E, (Estado (mapaInicial (10,10)) [Jogador (3,2) B 1 2 3, Jogador (4,4) E 1 2 3] [])),
            (1 , Movimenta D, (Estado (mapaInicial (10,10)) [Jogador (6,6) C 1 2 3, Jogador (3,3) D 1 2 3] [DisparoChoque 0 5])),
            (1 , Movimenta D, (Estado (mapaInicial (10,10)) [Jogador (6,6) C 1 2 3, Jogador (3,3) E 1 2 3] [DisparoLaser 1 (2,2) E,DisparoChoque 0 5])),
            (1 , Movimenta D, (Estado (mapaInicial (10,10)) [Jogador (3,3) C 1 2 3, Jogador (6,7) D 1 2 3] [])),
            (1 , Movimenta C, (Estado (mapaInicial (10,10)) [Jogador (3,3) C 1 2 3, Jogador (2,3) D 1 2 3] [])),
            (1 , Movimenta E, (Estado (mapaInicial (10,10)) [Jogador (7,4) C 1 2 3, Jogador (6,6) E 1 2 3] [])),
            (1 , Movimenta E, (Estado (mapaInicial (10,10)) [Jogador (2,2) C 1 2 3, Jogador (6,6) E 1 2 3] [])),
            (0 , Movimenta E, (Estado (mapaInicial (11,11)) [Jogador (3,9) E 1 2 3, Jogador (6,6) E 1 2 3] [DisparoChoque 1 5]))
            ]








-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada x (Movimenta q) (Estado m l d) | l==[] || vidasJogador (jogcerto x l) ==0 = (Estado m l d)
                                      | verchoque d && choqueffect x l d = (Estado m (auxjogadorchoque x l q) d)
                                      | otherwise = (Estado m (auxjogcerto l x (Movimenta q) m l) d)
jogada x (Dispara q) (Estado m l d) | l == [] || vidasJogador (jogcerto x l)==0 || verfdisp (jogcerto x l) q = (Estado m l d)
                                    | otherwise = (Estado m (dispjog x q l) ((auxdispara q x (jogcerto x l)):d))



-- | pegando no identificador do jogador, na lista de jogadores e na lista de disparos, verifica se o jogador x se encontra dentro da área de um choque senão o dele
choqueffect :: Int -> [Jogador] -> [Disparo] -> Bool
choqueffect x j [] = False
choqueffect x j ((DisparoChoque i t):s) | x == i = choqueffect x j s 
                                        | auxchoque (posicaoJogador (jogcerto x j)) (posicaoJogador (jogcerto i j)) = True 
                                        | otherwise = choqueffect x j s 
choqueffect x j (h:t) = choqueffect x j t 



-- | devolve lista com jogador alterado
auxjogcerto :: [Jogador] -> Int -> Jogada -> Mapa -> [Jogador] -> [Jogador]
auxjogcerto l 0 (Movimenta x) m (h:t) = (auxjogador l (Movimenta x) m h):t
auxjogcerto l x (Movimenta d) m (h:t) = h:auxjogcerto l (x-1) (Movimenta d) m t

-- | altera direcao jogador afetado com choques
auxjogadorchoque :: Int -> [Jogador] -> Direcao -> [Jogador]
auxjogadorchoque i [] d = []
auxjogadorchoque 0 ((Jogador p dir v l c):t) d = (Jogador p d v l c):t
auxjogadorchoque i (h:t) d = h : auxjogadorchoque (i-1) t d

-- | altera jogador
auxjogador :: [Jogador] -> Jogada -> Mapa -> Jogador -> Jogador
auxjogador j (Movimenta d) m (Jogador (x,y) a v l c) | d /= a = (Jogador (x,y) d v l c)
                                                     | d==C && (posvalida (x,y) a m) && postanks (x-2,y) (listpjog j) && postanks (x-2,y-1) (listpjog j) && postanks (x-2,y+1) (listpjog j) = (Jogador (x-1,y) a v l c)
                                                     | d==B && (posvalida (x,y) a m) && postanks (x+2,y) (listpjog j) && postanks (x+2,y-1) (listpjog j) && postanks (x+2,y+1) (listpjog j) = (Jogador (x+1,y) a v l c)
                                                     | d==E && (posvalida (x,y) a m) && postanks (x,y-2) (listpjog j) && postanks (x+1,y-2) (listpjog j) && postanks (x-1,y-2) (listpjog j) = (Jogador (x,y-1) a v l c)
                                                     | d==D && (posvalida (x,y) a m) && postanks (x,y+2) (listpjog j) && postanks (x+1,y+2) (listpjog j) && postanks (x-1,y+2) (listpjog j) = (Jogador (x,y+1) a v l c)
                                                     | otherwise = (Jogador (x,y) a v l c)
-- | cria disparo
auxdispara :: Arma -> Int -> Jogador -> Disparo
auxdispara a b (Jogador (x,y) d v l c) | a == Canhao && d == C = (DisparoCanhao b (x-1,y) d)
                                       | a == Canhao && d == B = (DisparoCanhao b (x+1,y) d)
                                       | a == Canhao && d == E = (DisparoCanhao b (x,y-1) d)
                                       | a == Canhao && d == D = (DisparoCanhao b (x,y+1) d)
                                       | a == Laser && d == C = (DisparoLaser b (x-1,y) d)
                                       | a == Laser && d == B = (DisparoLaser b (x+1,y) d)
                                       | a == Laser && d == E = (DisparoLaser b (x,y-1) d)
                                       | a == Laser && d == D = (DisparoLaser b (x,y+1) d)
                                       | a == Choque = (DisparoChoque b 5)

-- | verifica se o jogador se pode mover para a direçao que pretende
posvalida :: Posicao -> Direcao -> Mapa -> Bool
posvalida (x,y) d m |(d==C && ((encontraPosicaoMatriz (x-1,y) m)==Vazia) && ((encontraPosicaoMatriz (x-1,y+1) m)==Vazia))=True
                    |(d==B && ((encontraPosicaoMatriz (x+2,y) m)==Vazia) && ((encontraPosicaoMatriz (x+2,y+1) m)==Vazia))=True
                    |(d==E && ((encontraPosicaoMatriz (x,y-1) m)==Vazia) && ((encontraPosicaoMatriz (x+1,y-1) m)==Vazia))=True
                    |(d==D && ((encontraPosicaoMatriz (x,y+2) m)==Vazia) && ((encontraPosicaoMatriz (x+1,y+2) m)==Vazia))=True
                    | otherwise = False

-- | jogador que efetua a jogada
jogcerto :: Int -> [Jogador] -> Jogador
jogcerto 0 (h:t) = h
jogcerto n (h:t) = jogcerto (n-1) t

-- | verifica se o jogador pode efetuar o disparo
verfdisp :: Jogador -> Arma -> Bool
verfdisp h Laser = lasersJogador h==0
verfdisp h Choque = choquesJogador h==0
verfdisp h Canhao = False

-- | altera muniçoes
dispjog :: Int -> Arma -> [Jogador] -> [Jogador]
dispjog  0 a ((Jogador p d v l c):t) | a == Laser = ((Jogador p d v (l-1) c):t)
                                     | a == Choque = ((Jogador p d v l (c-1)):t)
                                     | a == Canhao = ((Jogador p d v l c):t)
dispjog x a (h:t) = h:dispjog (x-1) a t

-- | lista posiçoes jogadores
listpjog :: [Jogador] -> [PosicaoGrelha]
listpjog [] = []
listpjog (h:t)  | vidasJogador h == 0 = listpjog t
                | otherwise = ((posicaoJogador h):listpjog t)

-- | posiçoes tanks
postanks :: PosicaoGrelha -> [PosicaoGrelha] -> Bool
postanks p [] = True
postanks p (h:t) | p == h = False
                 | otherwise = postanks p t
-- | choque
-- |verifica se ha choques no mapa
verchoque :: [Disparo] -> Bool
verchoque [] = False
verchoque ((DisparoChoque j t):s) = True
verchoque (d:s) = verchoque s


-- | posicao jogador disparou choque
{-poschoque :: [Disparo] -> [Jogador] -> Posicao
poschoque ((DisparoChoque j t):s) l = posicaoJogador (jogcerto j l)
poschoque (h:s) l= poschoque s l
-}

-- -- | verifica se o jogador é afetado pelo choque
-- afchoque :: Int -> Int -> Posicao -> Posicao -> Bool
-- afchoque 0 0 (x,y) (a,b) = False
-- afchoque l 0 (x,y) (a,b) = afchoque (l-1) 7 (x,y) (a+1,b-6)
-- afchoque l c (x,y) (a,b) | (x,y)==(a,b) = True
--                          | otherwise = afchoque l (c-1) (x,y) (a,b+1)
-- -- | corre a funcao funcao afchoque que comeca no pontos mais a cima e mais à esquerda que é
-- -- | afetado pelo choque e verifica se há algum jogador nas sete posições à direita diminuindo depois a linha para verificar o mesmo para as sete linhas de baixo
auxchoque :: Posicao -> Posicao -> Bool
auxchoque (a,b) (x,y) = a > x-4 && a < x+4 && b > y-4 && b < y+4  -- = afchoque 7 7 p (x-3,y-3)

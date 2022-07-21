-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g127 where

import LI11819
import Tarefa0_2018li1g127

import Data.List
-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[Move D,Move B,Move B,Move B,Roda,MudaParede,Move D,Move D,Move D,Desenha],[Move B,Move B, Roda,MudaParede,Desenha] ,[Move D, Move D ,Move D,Move D,Move B, MudaTetromino,Roda],
            [Move B,Move D,Move B,Move D,Move B,Move D,Move B,Move D,Move B,Move D,Move C,Move E,Move C,Move E,Move C,Move E,Move C,Move E,Desenha,Move D,Move D,Move D,Roda,Desenha,Move B,Move B,Move B,Roda,Desenha,Move E,Move E,Roda,Move E,Move E,Desenha,Move B,MudaParede,Move D,Roda,Move D,Move D,Move E,Desenha,Move C,Move C,Move C,Move C,Roda,Move C,Move E,Move E,Move E,Desenha]




            



            ]


-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.
instrucao (Move x) (Editor (a,b) d t par m) | x==C = (Editor (a-1,b) d t par m)
                                            | x==B = (Editor (a+1,b) d t par m)
                                            | x==E = (Editor (a,b-1) d t par m)
                                            | otherwise = (Editor (a,b+1) d t par m)

instrucao (Roda) (Editor p d t par m) | d == C = (Editor p D t par m)
                                      | d == D = (Editor p B t par m)
                                      | d == B = (Editor p E t par m)
                                      | otherwise = (Editor p C t par m)

instrucao (MudaTetromino) (Editor p d t par m) | t == I = (Editor p d J par m)
                                               | t == J = (Editor p d L par m)
                                               | t == L = (Editor p d O par m)
                                               | t == O = (Editor p d S par m)
                                               | t == S = (Editor p d T par m)
                                               | t == T = (Editor p d Z par m)
                                               | otherwise = (Editor p d I par m)

instrucao (MudaParede) (Editor p d t par m) | par == Indestrutivel = (Editor p d t (Destrutivel) m)
                                            | otherwise = (Editor p d t (Indestrutivel) m)

instrucao (Desenha) (Editor (a,b) d t par m) = (Editor (a,b) d t par (auxdesenha (0,0) (a,b) (desenhaMapa (rodaT d (tetrominoParaMatriz t)) par) m))


-- | recebe uma auxiliar de posição (0,0) , a posição a iniciar o desenho (a,b), o mapa do tetromino e o mapa a substituir
-- | se a linha coincidir com a auxiliar, então manda para uma auxiliar para encontrar a coluna e começar a desenhar
-- | se a linha não coincidir com a auxiliar, escreve a linha do mapa, e repete o processo para as restantes linhas até achar a linha a substituir
auxdesenha :: Posicao -> Posicao -> Mapa -> Mapa -> Mapa
auxdesenha (z,w) (a,b) [] [] = [] 
auxdesenha (z,w) (a,b) [] m = m 
auxdesenha (z,w) (a,b) ((x:y):xs) ((h:hs):t) | z < a = (h:hs) : auxdesenha (z+1,w) (a,b) ((x:y):xs) t
                                             | z >= a = auxdesenhalinha b (x:y) (h:hs) : auxdesenha (z+1,0) (a,b) xs t 
                                            

-----------------------------T---------M
-- | Recebe a quantidade de peças que vai ter de substituir, uma linha do tetromino, e a linha do mapa a substituir
-- | Se a peça do tetromino a substituir for vazia, não substitui e passa à peça seguinte
-- | Se a peça do tetromino a substituir não for vazia, então substitui no mapa, independentemente do tipo de peça que está no mapa
auxdesenhalinha :: Int -> [Peca] -> [Peca] -> [Peca]
auxdesenhalinha y [] l = l
auxdesenhalinha y (x:xs) (h:t) | y /= 0 = h: auxdesenhalinha (y-1) (x:xs) t
                               | (y == 0 && x /= Vazia) = x: auxdesenhalinha y xs t
                               | (y == 0 && x== Vazia) = h : auxdesenhalinha y xs t
                               


-- | Dada a matriz de Bools do tetromino e o tipo de parede, transforma a Matriz de Bool num Mapa
desenhaMapa :: Matriz Bool -> Parede -> Mapa
desenhaMapa [] _ = []
desenhaMapa (x:xs) par = desenhaLinha x par : desenhaMapa xs par

-- | recebe uma linha de bool, o tipo de parede, e transforma a linha numa linha de Peças
desenhaLinha :: [Bool] -> Parede -> [Peca]
desenhaLinha [] _ = []
desenhaLinha (x:xs) par | x = Bloco par : desenhaLinha xs par
                        | otherwise = Vazia : desenhaLinha xs par

-- | Recebe uma direção, e a Matriz de Bools do tetromino
-- | Se for cima, dá a matriz; Se for Direita, roda uma vez; Se for Baixo, roda 2 vezes; Se for Esquerda, roda 1 vez para a esquerda.
rodaT :: Direcao -> Matriz Bool -> Matriz Bool
rodaT d [] = []
rodaT d t | d == C = t
          | d == D = rodaMatriz t
          | d == B = reverse (map reverse t)
          | otherwise = reverse (transpose t)











-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes [] (Editor p d t par m) = (Editor p d t par m)
instrucoes (h:s) (Editor p d t par m)=instrucoes s (instrucao h (Editor p d t par m))

-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (a,b) = auxauxMapaI a (a,b)

-- | recebe uma variável auxiliar e a dimensão do mapa (a,b)
-- | se for só 1 linha, então repete b vezes a peça Bloco Indestrutivel
-- | se a variável auxiliar for igual ao número de linhas, então faz uma linha só de Blocos Indestrutiveis
-- | caso contrário, escreve um Bloco Indestrutivel, seguido de (b-2) blocos vazios e mais um bloco indestrutivel, acabando assim a linha e repetindo para o resto das linhas
auxauxMapaI :: Int -> Posicao -> Mapa
auxauxMapaI 1 (a,b) = [replicate b (Bloco Indestrutivel)]
auxauxMapaI x (a,b) | x==a = replicate b (Bloco Indestrutivel):auxauxMapaI (x-1) (a,b)
                    | otherwise = ([Bloco Indestrutivel]++(replicate (b-2) (Vazia))++[Bloco Indestrutivel]) : auxauxMapaI (x-1) (a,b)

-- | recebe uma auxiliar de posição
--auxmapaI :: Posicao -> Dimensao -> [Peca]
--auxmapaI (c,d) (a,b) | d > b = []
--                     | c == 1 || d == 1 || c == a || d == b = Bloco Indestrutivel : auxmapaI (c,d+1) (a,b)
--                     | otherwise = Vazia : auxmapaI (c,d+1) (a,b)


-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial e = (Editor (posicaoInicial e) C I Indestrutivel (mapaInicial (dimensaoInicial e)))

-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi is = constroiM (instrucoes is (editorInicial is))
-- | recebe um editor e devolve o mapa
constroiM :: Editor -> Mapa
constroiM (Editor p d t pr m) = m

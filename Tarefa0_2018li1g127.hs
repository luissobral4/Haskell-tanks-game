-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g127 where

import LI11819
import Data.List

-- * Funções não-recursivas.

-- | Um 'Vetor' é uma 'Posicao' em relação à origem.
type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** Funções sobre vetores

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetores'.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (x1,y1) (x2,y2) = (x1 + x2 , y1 + y2)

-- | Subtrai dois 'Vetores'.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (x1,y1) (x2,y2) = (x1 - x2 , y1 - y2)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor n (x1,y1) = (n * x1 , n * y1)

--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distância à origem).
rodaVetor :: Vetor -> Vetor
rodaVetor (x,y) = (y,-x)

--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
inverteVetorH :: Vetor -> Vetor
inverteVetorH (x,y) = (x,-y)
--
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
inverteVetorV :: Vetor -> Vetor
inverteVetorV (x,y) = (-x,y)
-- *** Funções do trabalho sobre 'Vetor'es.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor d = case d of
                 C ->(-1,0)
                 D ->(0,1)
                 B ->(1,0)
                 E ->(0,-1)


-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido _ [] = False
eIndiceListaValido n l = (n >= 0) && (n <= ((length l)-1))

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz [[]] = (0,0)
dimensaoMatriz ([]:t) = dimensaoMatriz t
dimensaoMatriz l | length l > 0 = (length l , length (head l))
                 | otherwise = (0,0)


-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida p [[]] = False
ePosicaoMatrizValida (x1,x2) l = (x1 >= 0) &&(x1 <= (length l)-1) && (x2 >= 0) && (x2 <= (length (head l))-1)

-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz p [[]] = False
eBordaMatriz (x1,x2) l = (x1 == 0) || (x1 == (length l)-1) || (x2 == 0) || (x2 == (length (head l))-1)

-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz t = case t of
                                            I -> [[False,True,False,False],[False,True,False,False],[False,True,False,False],[False,True,False,False]]
                                            J -> [[False,True,False],[False,True,False],[True,True,False]]
                                            L -> [[False,True,False],[False,True,False],[False,True,True]]
                                            O -> [[True,True],[True,True]]
                                            S -> [[False,True,True],[True,True,False],[False,False,False]]
                                            T -> [[False,False,False],[True,True,True],[False,True,False]]
                                            Z -> [[True,True,False],[False,True,True],[False,False,False]]



-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista x l =  l !! x
{-}
encontraIndiceLista 0 (h:t) = h
encontraIndiceLista x (h:t) = encontraIndiceLista (x-1) t
-}

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista i e [] = []
atualizaIndiceLista 0 e (h:t) = (e:t)
atualizaIndiceLista i e (h:t) = h:atualizaIndiceLista (i-1) e t

-- ** Funções sobre matrizes.

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
--
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>
rodaMatriz :: Matriz a -> Matriz a
rodaMatriz m = transpose (reverse m)

-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH [[]] = [[]]
inverteMatrizH l = map reverse l


-- | Inverte uma 'Matriz' na vertical.
--
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV l = reverse l

-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (0,0) v = [[]]
criaMatriz (x,y) v = replicate x (replicate y v)


-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (0,0) m = head (head m)
encontraPosicaoMatriz (x,y) m@(h:t) | ((ePosicaoMatrizValida (x,y) m) == False) =  head (head m)
                                    | (x /= 0) = encontraPosicaoMatriz (x-1, y) t
                                    | otherwise = h !! y
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.

-- | Verifica se a posição inserida é valida; Se for, modifica um elemento numa dada 'Posicao', caso contrário retorna a matriz dada
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (x,y) v l = if ePosicaoMatrizValida (x,y) l then auxatualizaPosicaoMatriz (x,y) v l
                                   else l


-- | Recebe uma posição, um elemento, uma matriz, e altera o elemento da matriz na posição dada pelo elemento inserido
auxatualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
auxatualizaPosicaoMatriz (0,0) v ((h1:t1):t) = ((v:t1):t)
auxatualizaPosicaoMatriz (0,y) v ((h1:t1):t) = ([h1] ++ head (atualizaPosicaoMatriz (0,y-1) v (t1:t))):t
auxatualizaPosicaoMatriz (x,0) v ((h1:t1):t) = (h1:t1) : atualizaPosicaoMatriz (x-1,0) v t
auxatualizaPosicaoMatriz (x,y) v ((h1:t1):t) = (h1:t1) : atualizaPosicaoMatriz (x-1,y) v t

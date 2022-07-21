{- |
Module        : Main
Description   : Componente gráfica do jogo
Copyright     : Luís Sobral <a89474@alunos.uminho.pt>;
                João Guedes <a89588@alunos.uminho.pt>
= Introdução Tarefa 5:
* Nesta tarefa tinhamos como objetivo desenvolver a componente gráfica do nosso jogo utilizando a biblioteca Gloss.
* É importante reconhecer que todas as tarefas foram desenvolvidas de acordo com a lógica da implementação do jogo.
* Nesta tarefa tinhamos várias opções de desenhar e desenvolver a parte grafica do nosso jogo.

= Estratégias utilizadas:
* Começamos por tentar desenhar um mapa de jogo fixo , tendo primeiro criadado a função linhamapa que cria uma linha do mapa e a função mapa que usa a linhamapa para desenhar o mapa de jogo.
* Despois criamos as funções desenhadisp e desenhajog e juntamos as 3 funções na funcao desenhaestado3 para conseguirmos desenhar todo um estado de jogo.
* A partir daqui definimos as funções estadoinicialg , onde definimos o estado inicial do nosso jogo, reageEventoB que aplica as jogadas de cada jogador a um estado (função jogada definida na tarefa 2) e a função reageTempog que reage à passagem do tempo (usando a funçâo ticks definida na tarefa4).
* Uma vez que já conseguiamos correr o nosso jogo começamos a criar os menus. Primeiro criamos um menu que desenhava um editor de mapas (função da Tarefa 1) que permite criar um mapa e usamos a função scale1 para redimensionar e posicionar o mapa à medida que este ficava maior. De seguida fomos redefinindo o nosso estadogloss de forma a poder receber mais informações relativas ao tipo de jogo pretendido pelo utilizador , tais como o número de jogador,o tipode jogo , e os mapas (definidos na tarefa 2).
* Para concluir implementamos no nosso jogo a funçâo bot definida na Tarefa 6

=Conclusão:
* Ao longo desta tarefa sentimos algumas dificuldades principalmente por nunca termos trabalhado com gloss, mas conseguimos cria uma boa implementação gráfica do nosso jogo, criar menos varias funcionalidades.
-}
module Main where

import LI11819
import Tarefa1_2018li1g127
import Tarefa0_2018li1g127
import Graphics.Gloss

import Tarefa4_2018li1g127
import Graphics.Gloss.Interface.Pure.Game
import Tarefa2_2018li1g127
import Tarefa6_2018li1g127
import Graphics.Gloss.Data.Bitmap

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo,
-- e reutilizar __de forma completa__ as funções das tarefas anteriores.
main :: IO ()
main = do
          jog1 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/nave1.bmp"
          jog2 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/nave2.bmp"
          jog3 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/nave3.bmp"
          jog4 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/nave4.bmp"
          blocoV <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/blocoV.bmp"
          blocoI <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/blocoI.bmp"
          blocoD <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/blocoD.bmp"
          canhao <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/Canhao.bmp"
          laser <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/laser.bmp"
          choque<- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/choque.bmp"
          inicio <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/inicio.bmp"
          jogadores <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/jogadores.bmp"
          mapas <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/mapas.bmp"
          mapa1 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/mapa1.bmp"
          mapa2 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/mapa2.bmp"
          mapa3 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/mapa3.bmp"
          tId <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/Ides.bmp"
          tJd <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/Jdes.bmp"
          tLd <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/Ldes.bmp"
          tOd <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/4des.bmp"
          tSd <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/Sdes.bmp"
          tZd <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/5des.bmp"
          tTd <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/Vdes.bmp"
          tIi <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/Iind.bmp"
          tJi <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/Jind.bmp"
          tLi <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/Lind.bmp"
          tOi <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/4ind.bmp"
          tSi <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/Sind.bmp"
          tZi <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/5ind.bmp"
          tTi <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/Vind.bmp"
          fundo <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/fundo.bmp"
          tabela <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/tabela.bmp"
          a0 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/0.bmp"
          a1 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/1.bmp"
          a2 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/2.bmp"
          a3 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/3.bmp"
          a4 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/4.bmp"
          a5 <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/5.bmp"
          teditor <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/teditor.bmp"
          vencedor <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/vencedor.bmp"
          menu <- loadBMP "/Users/luissobral/Desktop/LEI/1ºano/LI1/2018li1g127/src/imagem/menu.bmp"
          let i = [inicio,jogadores,mapas,mapa1,mapa2,mapa3,menu,vencedor]
          let j = [jog1,jog2,jog3,jog4]
          let b = [blocoV,blocoI,blocoD]
          let d = [canhao,laser,choque]
          let td = [tId,tJd,tLd,tOd,tSd,tZd,tTd]
          let ti = [tIi,tJi,tLi,tOi,tSi,tZi,tTi]
          let f = [fundo,tabela,a0,a1,a2,a3,a4,a5,teditor]
          play dm         -- janela onde irá correr o jogo
               (black)     -- côr do fundo da janela
               fr              -- frame rate
               (estadoinicialg [i,j,b,d,td,ti,f])   -- estado inicial
               desenhaestadog   -- desenha o estado do jogo
               reageEventog     -- reage a um evento
               reageTempog      -- reage ao passar do tempo

-- | funcao que devolve a lista de jogadores posicionados em cada canto mapa
listajogadores :: Int -> Mapa -> [Jogador]
listajogadores n m | n == 2 = [Jogador (1,1) B 5 5 5,Jogador (length(m)-3,length (head (m))-3) C 5 5 5]
                   | n == 3 = [Jogador (1,1) B 5 5 5,Jogador (1,length (head (m))-3) B 5 5 5,Jogador (length(m)-3,1) C 5 5 5]
                   | n == 4 = [Jogador (1,1) B 5 5 5,Jogador (1,length (head (m))-3) B 5 5 5,Jogador (length(m)-3,1) C 5 5 5,Jogador (length(m)-3,length (head (m))-3) C 5 5 5]

-- | mapas guardados
mapaI :: Int -> Instrucoes
mapaI m | m == 1 = [Move D,Move B,Move D,Move B,Move D,Move B,Move D,Move B,Move D,Move B,Move D,Move B,Move D,Move B,Move D,
                   Move E,Move E,Move E,Move E,Move D,Desenha,Move C,Move C,Move C,Move C,Move C,Move C,Move C,Desenha,Roda,
                   Move B,Move B,Move B,Move B,Move D,Move D,Move D,Move D,Desenha,Move E,Move E,Move E,Move E,Move E,Move E,
                   Move E,Move E,Move E,Desenha,MudaParede,Move C,Desenha,Move B,Move B,Desenha,Move D,Move D,Move D,Move D,
                   Desenha,Move D,Move D,Move D,Desenha,Move D,Move D,Desenha,Move C,Move C,Desenha,Move E,Move E,Move E,
                   Desenha,Move E,Move E,Move E,Desenha,Move D,Move B,Desenha,Move D,Desenha,Move E,Move E,Move E,Move E,
                   Move E,Move E,Move B,Desenha,Move C,Move C,Desenha,MudaParede,Move B,Desenha,Move D,Move D,Move D,Move D,
                   Move D,Move D,Move D,Move D,Move D,Move D,Move D,Desenha,MudaParede,Move C,Desenha,Move B,Move B,Desenha]

        | m == 2 = [Move D,Move D,Move B,Move B,Move D,Move B,Move D,Roda,Roda,Roda,Roda,MudaTetromino,MudaTetromino,
                   MudaTetromino,MudaTetromino,Move D,Desenha,Move E,Move E,Roda,Roda,Move E,Move E,Move E,Move C,Desenha,
                   Move C,Move C,Move C,Move C,Move B,Move B,Move B,Move C,Move C,Move C,Move C,Move C,Move E,Move E,Move E,
                   Move B,Move B,Move C,Desenha,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Desenha,Roda,Move E,
                   Move E,Move E,Move C,Move E,MudaTetromino,MudaTetromino,MudaTetromino,Roda,Desenha,Move B,Move B,Move B,
                   Move B,Move B,Move B,Move B,Desenha,MudaParede,MudaTetromino,MudaTetromino,Move E,Move E,Move E,Move E,
                   Move C,Desenha,Move C,Move C,Move C,Move D,Move D,Move D,Move C,Move C,Move B,Desenha,Move B,Move D,Move E,
                   Move E,Move E,Move D,Desenha,Move B,Move D,Move D,Move D,Move D,Move C,Move C,Desenha,Move B,Move B,Move B,
                   Desenha,Move E,Move B,Move B,Move C,Desenha,Move B,Move C]

        | m == 3 = [Move B,Move D,Move D,Move B,Move D,Move B,Move D,Move D,Move B,Move B,Move D,Move B,Move B,Move D,Move C,Move C,Move E,Move E,Move E,Move C,Move C,Move E,Move E,Move C,Move C,Move C,MudaParede,Desenha,
                   MudaParede,Move D,Desenha,Move D,Move D,Move D,MudaParede,Move D,Desenha,Move E,MudaParede,Desenha,Move B,Move B,Move B,Move B,Move B,Roda,Move D,Desenha,Move E,Move E,Move E,Desenha,Move E,Move E,Move E,
                   Move E,Move E,Move E,Desenha,Move B,Move B,Move D,Move D,Move D,Move D,Move B,Roda,Desenha]


-- | escolhe imagem
imagem :: [Picture] -> Int -> Picture
imagem im x = (im !! x)

------------- estado inicial ---------------------------------
-- | dá o estado inicial
estadoinicial :: Estado
estadoinicial = (Estado (mapaInicial (12,12)) [Jogador (1,1) C 1 20 20,Jogador (1,9) C 1 1 1,Jogador (9,1) B 1 1 1,Jogador (9,9) B 1 1 1] [])

------------- desenha estados ----------------------

-- | desenha menu inicial
desenhaestado1 :: [Picture] -> Picture
desenhaestado1 (h:t) = h

-- | desenha menu inicial
desenhaestado7 :: [Picture] -> Picture
desenhaestado7 a = (a !! 6)

-- | desenha menu numero de jogadores
desenhaestado4 :: [Picture] -> Picture
desenhaestado4 (h:a:t) = a

-- | desenha menu mapas
desenhaestado5 :: [Picture] -> Int -> Picture
desenhaestado5 (h:a:b:m1:m2:m3:t) m | m == 1 = Pictures [b,m1]
                                    | m == 2 = Pictures [b,m2]
                                    | m == 3 = Pictures [b,m3]

-- | desenha vencedor
desenhaestado6 :: [[Picture]] -> Estado -> Picture
desenhaestado6 (i:j:t) (Estado m js@(j1:j2:s) ds) | vidasJogador j1 /= 0 = Pictures [(i !! 7),scale 3 3 (head j)]
                                                  | vidasJogador j2 /= 0 = Pictures [(i !! 7),scale 3 3 (j !! 1)]
                                                  | vidasJogador (js !! 2) /= 0 = Pictures [(i !! 7),scale 3 3 (j !! 2)]
                                                  | vidasJogador (last js) /= 0 = Pictures [(i !! 7),scale 3 3 (last j)]

-- | desenha criador de mapas
desenhaestado2 :: [[Picture]] -> Editor -> Picture
desenhaestado2 im@(i:j:b:d:td:ti:e)
               (Editor (x,y) dir te par m) | te == I = Pictures ([(Translate (500) 0 (last(last im)))]++[head(last im)]++[scale1 (Pictures((mapa m b 245)++[Translate ((fromInteger (toInteger y))*(30)-200) ((fromInteger (toInteger x))*(-30)+200) (rodaimagem (f par !! t te) dir)]))m])
                                           | te == O = Pictures ([(Translate (500) 0 (last(last im)))]++[head(last im)]++[scale1 (Pictures((mapa m b 245)++[Translate ((fromInteger (toInteger y))*(30)-230) ((fromInteger (toInteger x))*(-30)+230) (rodaimagem (f par !! t te) dir)]))m])
                                           | otherwise = Pictures ([(Translate (500) 0 (last(last im)))]++[head(last im)]++[scale1 (Pictures((mapa m b 245)++[Translate ((fromInteger (toInteger y))*(30)-215) ((fromInteger (toInteger x))*(-30)+215) (rodaimagem (f par !! t te) dir)]))m])
                                          where t I=0
                                                t J=1
                                                t L=2
                                                t O=3
                                                t S=4
                                                t Z=5
                                                t T=6
                                                f Destrutivel = td
                                                f Indestrutivel = ti
-- | desenha jogo
desenhaestadog3 :: Estadogloss -> Picture
desenhaestadog3 (e,im,(ed,ins),i,j,m,n,ev,ti) = Pictures [desenhaestado3 e im,tempo ti]

-- | desenha jogo
desenhaestado3 :: Estado -> [[Picture]] -> Picture
desenhaestado3 es@(Estado m js ds) im@(i:j:b:d:e) = (Pictures ([head(last im)]++([(Translate (500) 0 ((last im) !! 1))]++desenhatabela js (last im) 0)++[scale1 (Pictures((mapa m b 245)++(desenhajog js j)++desenhadisp es d)) m]))

------------------------ MAPA ----------------------------
-- | desenha mapa
mapa :: Mapa -> [Picture] -> Float -> [Picture]
mapa [] b x = []
mapa (h:s) b x = (linhamapa h b x (-245)) ++ mapa s b (x-30)

-- | desenha linha do mapa
linhamapa :: [Peca] -> [Picture] -> Float -> Float -> [Picture]
linhamapa [] a b c = []
linhamapa (h:t) b@(v:i:d:a) x y   | h == Bloco Destrutivel = (Translate y x d):linhamapa t b x (y+30)
                                  | h == Bloco Indestrutivel = (Translate y x i):linhamapa t b x (y+30)
                                  | h == Vazia = (Translate y x v):linhamapa t b x (y+30)

---------------- JOGADORES -----------------------------------------
-- | desenha jogadores
desenhajog :: [Jogador] -> [Picture] -> [Picture]
desenhajog [] a = []
desenhajog (a:e) (h:t) | vidasJogador a == 0 = desenhajog e t
                       | otherwise = (posjog (posicaoJogador a) (rodaimagem (h) (direcaoJogador a))):desenhajog e t

-- | roda imagem de acordo com a sua direcao
rodaimagem :: Picture -> Direcao -> Picture
rodaimagem i d  | d == C = i
                | d == E = rotate 270 i
                | d == B = rotate 180 i
                | d == D = rotate 90 i

-- | posiciona o jogador
posjog :: Posicao -> Picture -> Picture
posjog (x,y) i = Translate ((fromInteger (toInteger y))*(30)-230) ((fromInteger (toInteger x))*(-30)+230) i


----------------- DISPAROS --------------------------------------
-- | desenha disparos
desenhadisp :: Estado -> [Picture] -> [Picture]
desenhadisp (Estado m a []) b = []
desenhadisp (Estado m js ((DisparoCanhao i (x,y) d):t)) a@(c:l:ch:z) | d == E || d == D = (Translate ((fromInteger (toInteger y))*(30)-230) ((fromInteger (toInteger x))*(-30)+230) c):desenhadisp (Estado m js t) a
                                                                    | otherwise = (Translate ((fromInteger (toInteger y))*(30)-230) ((fromInteger (toInteger x))*(-30)+230) (rodaimagem c E)):desenhadisp (Estado m js t) a
desenhadisp (Estado m js ((DisparoLaser i p d):t)) a@(c:l:ch:s) =  (expandelaser p d m l)++desenhadisp (Estado m js t) a
desenhadisp (Estado m js ((DisparoChoque i ti):t)) a@(c:l:ch:s) = (Translate ((fromInteger (toInteger y))*(30)-230) ((fromInteger (toInteger x))*(-30)+230) ch):desenhadisp (Estado m js t) a
                                                                 where (x,y) = posicaoJogador (js !! i)
desenhadisp (Estado m js (_:t)) b = desenhadisp (Estado m js t) b

-- | expande o laser ate encontrar um bloco indestrutivel
expandelaser :: Posicao -> Direcao -> Mapa -> Picture -> [Picture]
expandelaser (x,y) d m l | verifmapa m (x,y) d && (d == E || d == D)= (Translate ((fromInteger (toInteger y))*(30)-230) ((fromInteger (toInteger x))*(-30)+230) l):expandelaser (somaVetores (x,y) (direcaoParaVetor d)) d m l
                         | verifmapa m (x,y) d = (Translate ((fromInteger (toInteger y))*(30)-230) ((fromInteger (toInteger x))*(-30)+230) (rodaimagem l E)):expandelaser (somaVetores (x,y) (direcaoParaVetor d)) d m l
                         | otherwise = []

-- | posiciona numeros na tabela de vidas, lasers e choques
desenhatabela :: [Jogador] -> [Picture] -> Float -> [Picture]
desenhatabela [] im i = []
desenhatabela (j:t) im@(f:tab:s) i = (posicoes j s i) ++ desenhatabela t im (i-120)

-- | posiciona numero de vidas, lasers e choques
posicoes :: Jogador -> [Picture] -> Float -> [Picture]
posicoes (Jogador o d v l c) im i = [(Translate (490) (120+i) (im !! v)),(Translate (580) (160+i) (im !! l)),(Translate (580) (120+i) (im !! c))]

-- | desenha tempo
tempo :: Int -> Picture
tempo x = scale 0.18 0.18 (Translate 2600 1250 (Color white( Text(show x))))

-- | funcao que posiciona e define o tamanho do mapa
scale1 :: Picture -> Mapa -> Picture
scale1 p m = translate ((fromInteger(toInteger((length (head m)))))* (-4)) ((fromInteger(toInteger((length m))))* (4)) (scale (1-(fromInteger(toInteger((length (head m)))))* (0.011)) (1-(fromInteger(toInteger((length m))))* (0.011)) p)

---------------------- Eventos -----------------------------


-- | funcao que define os controlos do menu inicial
reageEventomenu :: Event -> Estadogloss -> Estadogloss
reageEventomenu (EventKey (_) Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),7,j,m,n,ev,ti)
reageEventomenu _ e = e

-- | funcao que define os controlos para voltar ao menu inicial no final do jogo
reageEventotipojogo :: Event -> Estadogloss -> Estadogloss
reageEventotipojogo (EventKey (Char 'a') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),4,j,m,1,ev,ti)
reageEventotipojogo (EventKey (Char 'b') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),4,j,m,2,ev,ti)
reageEventotipojogo (EventKey (SpecialKey KeySpace) Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),1,j,m,n,ev,ti)
reageEventotipojogo _ e = e

-- | funcao que define o numero de jogadores
reageEventojogadores :: Event -> Estadogloss -> Estadogloss
reageEventojogadores (EventKey (Char '2') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),5,2,m,n,ev,ti)
reageEventojogadores (EventKey (Char '3') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),5,3,m,n,ev,ti)
reageEventojogadores (EventKey (Char '4') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),5,4,m,n,ev,ti)
reageEventojogadores (EventKey (SpecialKey KeySpace) Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),7,j,m,n,ev,ti)
reageEventojogadores _ e = e

-- | funcao que define o mapa de jogo
reageEventoMapa :: Event -> Estadogloss -> Estadogloss
reageEventoMapa (EventKey (SpecialKey KeyLeft) Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) | m == 1 = (e,im,(ed,ins),i,j,3,n,ev,ti)
                                                                                       | otherwise = (e,im,(ed,ins),i,j,m-1,n,ev,ti)
reageEventoMapa (EventKey (SpecialKey KeyRight) Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) | m == 3 = (e,im,(ed,ins),i,j,1,n,ev,ti)
                                                                                        | otherwise = (e,im,(ed,ins),i,j,m+1,n,ev,ti)
reageEventoMapa (EventKey (SpecialKey KeyEnter) Down _ _) (Estado mapa js ds,im,(ed,ins),i,j,m,n,ev,ti) = (Estado (constroi (mapaI m)) (listajogadores j (constroi (mapaI m))) ds,im,(ed,ins),3,j,m,n,ev,ti)
reageEventoMapa (EventKey (Char 'm') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),2,j,m,n,ev,ti)
reageEventoMapa (EventKey (SpecialKey KeySpace) Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),4,j,m,n,ev,ti)
reageEventoMapa _ e = e

-- | funcao que define os controlos para voltar ao menu inicial no final do jogo
reageEventoVencedor :: Event -> Estadogloss -> Estadogloss
reageEventoVencedor (EventKey (SpecialKey KeyEnter) Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = estadoinicialg im--(e,im,(ed,ins),1,j,m,n,[],ti)
reageEventoVencedor _ e = e

-- | funcao que define os controlos do criador de mapas
reageEventoEditor :: Event -> Estadogloss -> Estadogloss
reageEventoEditor (EventKey (SpecialKey KeyUp)    Down _ _)
                  eg@(e,im,(ed@(Editor (x,y) d t p ma),ins),i,j,m,n,ev,ti) | length (constroi (ins++[Move C])) > 60 = eg
                                                                     | x == 1 = (e,im,(ed,ins++[Move C]),i,j,m,n,ev,ti)
                                                                     | otherwise = (e,im,(instrucao (Move C) ed,ins++[Move C]),i,j,m,n,ev,ti)
reageEventoEditor (EventKey (SpecialKey KeyDown)  Down _ _)
                  eg@(e,im,(ed@(Editor (x,y) d t p ma),ins),i,j,m,n,ev,ti) | length (constroi (ins++[Move B])) > 60 = eg
                                                                   | x == (length (constroi (ins++[Move B])))-1 = (e,im,(ed,ins++[Move B]),i,j,m,n,ev,ti)
                                                                   | otherwise = (e,im,(instrucao (Move B) ed,ins++[Move B]),i,j,m,n,ev,ti)
reageEventoEditor (EventKey (SpecialKey KeyLeft)  Down _ _)
                  eg@(e,im,(ed@(Editor (x,y) d t p ma),ins),i,j,m,n,ev,ti) | length (head (constroi (ins++[Move E]))) > 60 = eg
                                                                     | y == 1 = (e,im,(ed,ins++[Move E]),i,j,m,n,ev,ti)
                                                                     | otherwise = (e,im,(instrucao (Move E) ed,ins++[Move E]),i,j,m,n,ev,ti)
reageEventoEditor (EventKey (SpecialKey KeyRight) Down _ _)
                  eg@(e,im,(ed@(Editor (x,y) d t p ma),ins),i,j,m,n,ev,ti) | length (head (constroi (ins++[Move D]))) > 60 = eg
                                                                     | y == (length (head (constroi (ins++[Move D]))))-1 = (e,im,(ed,ins++[Move D]),i,j,m,n,ev,ti)
                                                                     | otherwise = (e,im,(instrucao (Move D) ed,ins++[Move D]),i,j,m,n,ev,ti)
reageEventoEditor (EventKey (Char 'd') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(instrucao (Desenha) ed,ins++[Desenha]),i,j,m,n,ev,ti)
reageEventoEditor (EventKey (Char 'p') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(instrucao (MudaParede) ed,ins++[MudaParede]),i,j,m,n,ev,ti)
reageEventoEditor (EventKey (Char 't') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(instrucao (MudaTetromino) ed,ins++[MudaTetromino]),i,j,m,n,ev,ti)
reageEventoEditor (EventKey (Char 'r') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(instrucao (Roda) ed,ins++[Roda]),i,j,m,n,ev,ti)
reageEventoEditor (EventKey (SpecialKey KeyEnter)    Down _ _) ((Estado m1 js ds),im,(ed,ins),i,j,m,n,ev,ti) = ((Estado (constroi ins) (listajogadores j (constroi ins)) ds),im,(ed,ins),3,j,m,n,ev,ti)
reageEventoEditor (EventKey (SpecialKey KeySpace)    Down _ _) ((Estado m1 js ds),im,(ed,ins),i,j,m,n,ev,ti) = ((Estado m1 js ds),im,(ed,ins),5,j,m,n,ev,ti)
reageEventoEditor _ e = e


-- | aplica afuncao reageEvento para jogar multiplayer e a funcao reageEventoB para jogar contra bots
reageEventojogo :: Event -> Estadogloss -> Estadogloss
reageEventojogo (EventKey (SpecialKey KeySpace)    Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (Estado (mapaInicial (10,10)) [] [],im,(ed,ins),5,j,m,n,ev,ti)
reageEventojogo ev a@(e@(Estado m js ds),im,(ed,ins),i,j,m1,n,ev1,ti) | length (verificavencedor js) == 1 = (Estado m js ds,im,(ed,ins),6,j,m1,n,ev1,ti)
                                                                      | n == 2 = reageEvento ev a
                                                                      | n == 1 = reageEventoB ev a

-- | verifica se existe apenas um jogador com vida dirente de 0
verificavencedor :: [Jogador] -> [Jogador]
verificavencedor [] = []
verificavencedor ((Jogador p d 0 l c):t) = verificavencedor t
verificavencedor (a:t) = a:verificavencedor t

-- | funcao que controla as jogadas de cada jogador
reageEvento :: Event -> Estadogloss -> Estadogloss
----------------- JOGADOR 1 -----------------------
reageEvento (EventKey a@(SpecialKey KeyUp)    Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(SpecialKey KeyDown)  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(SpecialKey KeyLeft)  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(SpecialKey KeyRight) Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(SpecialKey KeyUp)    Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey a@(SpecialKey KeyDown)  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey a@(SpecialKey KeyLeft)  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey a@(SpecialKey KeyRight) Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey (Char ',') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 0 (Dispara Canhao) e,im,(ed,ins),i,j,m,n,ev,ti)
reageEvento (EventKey (Char '.') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 0 (Dispara Laser)  e,im,(ed,ins),i,j,m,n,ev,ti)
reageEvento (EventKey (Char '-') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 0 (Dispara Choque) e,im,(ed,ins),i,j,m,n,ev,ti)

----------------- JOGADOR 2 -----------------------
reageEvento (EventKey a@(Char 'w')  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(Char 's')  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(Char 'a')  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(Char 'd')  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(Char 'w')  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey a@(Char 's')  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey a@(Char 'a')  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey a@(Char 'd')  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey (Char '1') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 1 (Dispara Canhao) e,im,(ed,ins),i,j,m,n,ev,ti)
reageEvento (EventKey (Char '2') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 1 (Dispara Laser) e,im,(ed,ins),i,j,m,n,ev,ti)
reageEvento (EventKey (Char '3') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 1 (Dispara Choque) e,im,(ed,ins),i,j,m,n,ev,ti)

----------------- JOGADOR 3 -----------------------
reageEvento (EventKey a@(Char 'i')  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(Char 'k')  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(Char 'j')  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(Char 'l')  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(Char 'i')  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey a@(Char 'k')  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey a@(Char 'j')  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey a@(Char 'l')  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey (Char '7') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 2 (Dispara Canhao) e,im,(ed,ins),i,j,m,n,ev,ti)
reageEvento (EventKey (Char '8') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 2 (Dispara Laser)  e,im,(ed,ins),i,j,m,n,ev,ti)
reageEvento (EventKey (Char '9') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 2 (Dispara Choque) e,im,(ed,ins),i,j,m,n,ev,ti)

----------------- JOGADOR 4 ----------------------
reageEvento (EventKey a@(Char 'g')  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(Char 'b')  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(Char 'v')  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(Char 'n')  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEvento (EventKey a@(Char 'g')  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey a@(Char 'b')  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey a@(Char 'v')  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey a@(Char 'n')  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEvento (EventKey (Char 'z') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 3 (Dispara Canhao) e,im,(ed,ins),i,j,m,n,ev,ti)
reageEvento (EventKey (Char 'x') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 3 (Dispara Laser) e,im,(ed,ins),i,j,m,n,ev,ti)
reageEvento (EventKey (Char 'c') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 3 (Dispara Choque) e,im,(ed,ins),i,j,m,n,ev,ti)

reageEvento _ e = e


-- | funcao para controlar bots e um jogador
reageEventoB :: Event -> Estadogloss -> Estadogloss
----------------- JOGADOR 1 -----------------------
reageEventoB (EventKey a@(SpecialKey KeyUp)    Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEventoB (EventKey a@(SpecialKey KeyDown)  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEventoB (EventKey a@(SpecialKey KeyLeft)  Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEventoB (EventKey a@(SpecialKey KeyRight) Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,a:ev,ti)
reageEventoB (EventKey a@(SpecialKey KeyUp)    Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEventoB (EventKey a@(SpecialKey KeyDown)  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEventoB (EventKey a@(SpecialKey KeyLeft)  Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEventoB (EventKey a@(SpecialKey KeyRight) Up _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (e,im,(ed,ins),i,j,m,n,retiraevento a ev,ti)
reageEventoB (EventKey (Char ',') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 0 (Dispara Canhao) e,im,(ed,ins),i,j,m,n,ev,ti)
reageEventoB (EventKey (Char '.') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 0 (Dispara Laser) e,im,(ed,ins),i,j,m,n,ev,ti)
reageEventoB (EventKey (Char '-') Down _ _) (e,im,(ed,ins),i,j,m,n,ev,ti) = (jogada 0 (Dispara Choque) e,im,(ed,ins),i,j,m,n,ev,ti)
reageEventoB _ e = e

-- | retira evento a uma lista de eventos
retiraevento ::Key -> [Key] -> [Key]
retiraevento e [] = []
retiraevento ev (h:t) | ev == h = t
                      | otherwise = h : retiraevento ev t

-- | aplica jogado do bot
transjogada :: Maybe Jogada -> Estado -> Int-> Estado
transjogada Nothing e i = e
transjogada (Just a) e i = jogada i a e


----------  reage ao tempo  ------------
-- | reague à passagem do tempo alterando o estado
reageTempo :: Float -> Estadogloss -> Estadogloss
reageTempo t a@(e@(Estado m js ds),im,(ed,ins),i,j,m1,n,ev,ti) | length (verificavencedor js) == 1 = (Estado m js ds,im,(ed,ins),6,j,m1,n,ev,0)
                                                               | ti == 500 = (Estado m js ds,im,(ed,ins),6,j,m1,n,ev,0)
                                                               | n == 1 && j == 2 = (tick (event1 ev (transjogada (bot 1 e) e 1)),im,(ed,ins),i,j,m1,n,ev,ti+1)
                                                               | n == 1 && j == 3 = (tick (event1 ev (transjogada (bot 2 e) (transjogada (bot 1 e) e 1) 2)),im,(ed,ins),i,j,m1,n,ev,ti+1)
                                                               | n == 1 && j == 4 = (tick (event1 ev (transjogada (bot 3 e) (transjogada (bot 2 e) (transjogada (bot 1 e) e 1) 2) 3)),im,(ed,ins),i,j,m1,n,ev,ti+1)
                                                               | n == 2 = (tick (event1 ev e),im,(ed,ins),i,j,m1,n,ev,ti+1)

-- | aplica a jogada correspondente a cada key
event1 :: [Key] -> Estado -> Estado
event1 [] e = e
event1 ((SpecialKey KeyUp):t) e = event1 t (jogada 0 (Movimenta C) e)
event1 ((SpecialKey KeyDown):t) e = event1 t (jogada 0 (Movimenta B) e)
event1 ((SpecialKey KeyLeft):t) e = event1 t (jogada 0 (Movimenta E) e)
event1 ((SpecialKey KeyRight):t) e = event1 t (jogada 0 (Movimenta D) e)
event1 ((Char 'w'):t) e = event1 t (jogada 1 (Movimenta C) e)
event1 ((Char 's'):t) e = event1 t (jogada 1 (Movimenta B) e)
event1 ((Char 'a'):t) e = event1 t (jogada 1 (Movimenta E) e)
event1 ((Char 'd'):t) e = event1 t (jogada 1 (Movimenta D) e)
event1 ((Char 'i'):t) e = event1 t (jogada 2 (Movimenta C) e)
event1 ((Char 'k'):t) e = event1 t (jogada 2 (Movimenta B) e)
event1 ((Char 'j'):t) e = event1 t (jogada 2 (Movimenta E) e)
event1 ((Char 'l'):t) e = event1 t (jogada 2 (Movimenta D) e)
event1 ((Char 'g'):t) e = event1 t (jogada 3 (Movimenta C) e)
event1 ((Char 'b'):t) e = event1 t (jogada 3 (Movimenta B) e)
event1 ((Char 'v'):t) e = event1 t (jogada 3 (Movimenta E) e)
event1 ((Char 'n'):t) e = event1 t (jogada 3 (Movimenta D) e)


------------------ ESTADO GLOSS ------------------------------------
-- | define um novo tipo de estado gloss ( tipo de estado | nº jogadores | mapas | tipo de jogo )
type Estadogloss = (Estado,[[Picture]],(Editor,Instrucoes),Int,Int,Int,Int,[Key],Int)

-- tipos de estado
-- 1 - menu menu Inicial
-- 2 - editor de mapas
-- 3 - jogo
-- 4 - menu jogadores
-- 5 - menu mapas
-- 6 - menu vencedor
-- 7 - menu tipo do jogo

-- | estado inicial do jogo
estadoinicialg :: [[Picture]] -> Estadogloss
estadoinicialg im = (estadoinicial,im,(Editor (1,1) C I Indestrutivel (mapaInicial (6,6)),[]),1,4,1,1,[],0)

-- | desenha estado do jogo
desenhaestadog :: Estadogloss -> Picture
desenhaestadog a@(e,im,((Editor p d t par ma),instrucoes),i,j,m,n,ev,ti) | i == 1 = desenhaestado1 (head im)
                                                                         | i == 2 = desenhaestado2 im (Editor p d t par (constroi instrucoes))
                                                                         | i == 3 = desenhaestadog3 a
                                                                         | i == 4 = desenhaestado4 (head im)
                                                                         | i == 5 = desenhaestado5 (head im) m
                                                                         | i == 6 = desenhaestado6 im e
                                                                         | i == 7 = desenhaestado7 (head im)

-- | reage a um evento
reageEventog :: Event -> Estadogloss -> Estadogloss
reageEventog ev a@(e,im,(ed,ins),i,j,m,n,ev1,ti) | i == 3 = reageEventojogo ev a
                                                 | i == 1 = reageEventomenu ev a
                                                 | i == 2 = reageEventoEditor ev a
                                                 | i == 4 = reageEventojogadores ev a
                                                 | i == 5 = reageEventoMapa ev a
                                                 | i == 6 = reageEventoVencedor ev a
                                                 | i == 7 = reageEventotipojogo ev a

-- | reage à passagem do tempo
reageTempog :: Float -> Estadogloss -> Estadogloss
reageTempog s a@(e,im,(ed,ins),i,j,m,n,ev,ti) | i == 3 = (reageTempo s a)
                                              | i == 1 || i == 2 || i == 4 || i == 5 || i == 6 || i == 7 = a

-- | frame rate
fr = 10

-- | display
dm :: Display
dm = InWindow "Tanks" (1400, 800) (0,0)

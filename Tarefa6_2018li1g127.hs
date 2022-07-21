{- | 
Module        : Tarefa6_2018li1g127
Description   : Implementação do bot
Copyright     : Luís Sobral <a89474@alunos.uminho.pt>;
                João Guedes <a89588@alunos.uminho.pt>
= Introdução Tarefa 6:
Neste módulo trabalhamos com 2 funções principais, bot e reagedir que recebe já as informações do bot.

O objetivo deste módulo passou por criar um bot que, recebendo o estado, conseguisse mover-se no mapa, disparar contra os jogadores e defender-se.


= Estratégias utilizadas:
* As jogadas que são feitas, são todas graças à função __reagedir__ que recebe o identificador do bot, o Estado e o estado do Bot, e que dependendo das várias posições, lista de disparos, etc... , escolhe a melhor opção para essa situação específica.
* Uma função importante utilizada, senão a mais importante, é a __verificaBloco__ que, dando 2 posições, verifica se existe blocos insdestrutiveis entre essas posições. Esta função é das mais importantes, pois se existir um bloco indestrutivel entre o bot e um jogador/disparo, o bot não vai se "preocupar" com eles.
* Existe ainda outras duas funções essenciais: __eixox__ e __eixoy__ - O objetivo destas funções é verificar se existe algum jogador que esteja em linha em relação ao Bot seja numa linha (eixox) ou numa coluna (eixoy). Caso não exista, o Bot não se deve preocupar com esse Jogador.

== Defesa:
* Inicialmente começamos por implementar no bot, funções que fariam com que ele de certa forma se defendesse. Sempre que se deparasse com alguém que possuisse lasers, ele iria fugir para onde havia caminho livre.
* Pode-se dizer que o coração da defesa está na função __reagedir__ , porque é lá que tem as condições necessárias para que o Bot consiga fugir dos jogadores com sucesso.
* Como já foi referido em cima, as funções __eixox__ e __eixoy__ têm um papel fundamental na Defesa.
* Além disso, se houver disparos de canhão em direção ao bot, ele vai virar-se para os disparos e disparar também de forma a defender-se.
* Uma função importante para esta situação é a __reagedis__ que, conforme as condições impostas, exporta a melhor opção e a __suscdisparo__ que verifica se existe algum disparo canhão em direção ao Bot.

=== Exemplo Defesa:
* Um jogador que esteja virado horizontalmente para o bot com lasers, o bot iria fugir para cima, caso o caminho estivesse livre, senão iria para baixo. Em ultimo caso, iria virar-se para o jogador e disparar.

== Ataque:
* Depois dessas funções estarem implementadas, decidimos acrescentar algumas funções em que o bot iria atacar. Estas foram inseridas na função já feita __reagedir__ .
* A maior parte destas funções acontecem quando o Jogador em causa, não possui lasers. Caso isto se verifique, sempre que um Jogador estiver na linha/coluna do bot, o bot vai virar-se na direção em que se encontra o Jogador, e começar a disparar Lasers (caso os tenha) ou Canhões.
* Mais tarde, acrescentamos a função em que, se o bot for afetado por um choque, ele vai usar o seu choque também para nenhum dos Jogadores se puder mexer, e assim o bot não fica em desvantagem.

=== Exemplos Ataque:
* Um jogador que se encontre na mesma coluna/linha que o bot, caso o bot tenha lasers, ele vai virar-se para o jogador e disparar os seus lasers. Quando já não tiver lasers, ele dispara Canhões só, e só apenas se o Jogador não estiver virado para o Bot, ou se o Jogador estiver virado para ele, mas tiver 0 lasers.
* Um jogador que utilize o choque que afete o bot, o Bot vai recontribuir com um choque caso os tenha.

== Caso Neutro:
*Este caso acontece quando não há jogadores a quem reagir, nem disparos. Neste caso, inicialmente pensámos em mantê-lo quieto, mas face aos resultados dos torneios, decidimos colocá-lo a andar pelas bordas no sentido dos ponteiros do relógio à procura de Jogadores.

=== Exemplos Neutro:
1. Caso não haja nenhum jogador para reagir, o Bot vai começar a andar para cima enquanto possível, depois para a direita, para baixo e para a esquerda conforme as condições.
2. Caso esteja com blocos destrutiveis à volta, vai destrui-los.

= Conclusão:
* No fim, pensamos que o Bot não ficou perfeito, mas consegue complicar a vida aos outros jogadores. Ele consegue esquivar-se dos lasers, atacar os jogadores, defender-se dos canhões, contra-atacar os choques e adaptar-se a mapas adversos.
* Por isso, penso que a tarefa ficou completa, e apesar do Bot não jogar como um ser-humano, penso que ficou bem construido, adaptando-se tanto à posição dos vários jogadores, como à lista de disparos.
-}
module Tarefa6_2018li1g127 where

import LI11819
import Tarefa2_2018li1g127
import Tarefa0_2018li1g127
import Tarefa1_2018li1g127

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot i e@(Estado m j d) = reagedir i e (jogcerto i j)

-- | recebe o identificador , o estado, o estado do Bot e aplica uma jogada consoante as condições
reagedir :: Int -> Estado -> Jogador -> Maybe Jogada
reagedir i (Estado m [] d) bot@(Jogador (a,b) dir v l c) | suscdisparo (a,b-1) d || suscdisparo (a,b+1) d || suscdisparo (a-1,b) d || suscdisparo (a+1,b) d = Nothing
                                                         | (encontraPosicaoMatriz (a+1,b) m == Bloco Destrutivel && dir == B) || (encontraPosicaoMatriz (a,b-1) m == Bloco Destrutivel && dir == E) = Just (Dispara Canhao)
                                                         | (encontraPosicaoMatriz (a+1,b) m == Bloco Destrutivel && dir == B) || (encontraPosicaoMatriz (a,b+1) m == Bloco Destrutivel && dir == D) = Just (Dispara Canhao)
                                                         | (encontraPosicaoMatriz (a-1,b) m == Bloco Destrutivel && dir == C) || (encontraPosicaoMatriz (a,b-1) m == Bloco Destrutivel && dir == E) = Just (Dispara Canhao)
                                                         | (encontraPosicaoMatriz (a-1,b) m == Bloco Destrutivel && dir == C) || (encontraPosicaoMatriz (a,b+1) m == Bloco Destrutivel && dir == D) = Just (Dispara Canhao)
                                                         | encontraPosicaoMatriz (a+1,b) m == Bloco Destrutivel || encontraPosicaoMatriz (a,b-1) m == Bloco Destrutivel = Just (Movimenta B)
                                                         | encontraPosicaoMatriz (a+1,b) m == Bloco Destrutivel || encontraPosicaoMatriz (a,b+1) m == Bloco Destrutivel = Just (Movimenta D)
                                                         | encontraPosicaoMatriz (a-1,b) m == Bloco Destrutivel || encontraPosicaoMatriz (a,b-1) m == Bloco Destrutivel = Just (Movimenta E)
                                                         | encontraPosicaoMatriz (a-1,b) m == Bloco Destrutivel || encontraPosicaoMatriz (a,b+1) m == Bloco Destrutivel = Just (Movimenta C)
                                                         | (posvalida (a,b) C m) && not(posvalida (a,b) E m) = Just (Movimenta C)
                                                         | not(posvalida (a,b) C m) && (posvalida (a,b) D m) = Just (Movimenta D)
                                                         | not(posvalida (a,b) D m) && (posvalida (a,b) B m) = Just (Movimenta B)
                                                         | not(posvalida (a,b) B m) && (posvalida (a,b) E m) = Just (Movimenta E)
                                                         | otherwise = Just (Movimenta C)
reagedir i (Estado m@(h:t) (j@(Jogador (x,y) dir vid las cho):js) dis) bot@(Jogador (a,b) d v l c)| vid == 0 = reagedir i (Estado m js dis) bot
                                                                                                  | x == a && y == b = reagedir i (Estado m js dis) bot
--                                                                                                       existe bloco indestrutivel?
                                                                                                  | verificaBloco (a,b) (x,y) m = reagedir i (Estado m js dis) bot
                                                                                                  | c /= 0 && verchoque dis && not(botchoque i dis) && choqueffect i (j:js) dis = Just (Dispara Choque)
                                                                                          --                Movimento
                                                                                                  | ((eixox x a && ((y > b && d == D) || (y < b && d == E))) || (eixoy y b && ((x > a && d == B) || (x < a && d == C)))) && l /= 0 = Just (Dispara Laser) 
                                                                                                  | ((x == a)|| (y==b)) && l/= 0 = Just (Movimenta (oposta dir))


--                                                                                                           LINHAS
                                                                                                  | eixox x a && not(posvalida (a,b) C m) && not (posvalida (a,b) B m) && d == oposta dir = Just (Dispara Canhao)
                                                                                                  | eixox x a && not(posvalida (a,b) C m) && not (posvalida (a,b) B m) = Just (Movimenta (oposta dir))
                                                                                                  | x == a && y > b && dir == E && (not(posvalida (a,b) C m) || not(posvalida (a-1,b) C m)) && las /= 0 && (posvalida (a,b) B m) && (posvalida (a+1,b) B m) = Just (Movimenta B)
                                                                                                  | x == a && y > b && dir == E && (not(posvalida (a,b) B m) || not(posvalida (a+1,b) B m)) && las /= 0 && (posvalida (a,b) C m) && (posvalida (a-1,b) C m) = Just (Movimenta C)                                                                                       
                                                                                                  | (x == a || x == a + 1) && y > b && d == C && dir == E && (posvalida (a,b) C m) && las /= 0 = Just (Movimenta C)
                                                                                                  | (x == a || x == a - 1) && y > b && d == B && dir == E && (posvalida (a,b) B m) && las /= 0 = Just (Movimenta B)
                                                                                                  | (x == a || x == a + 1) && y > b && dir == E && (posvalida (a,b) C m) && las /= 0 = Just (Movimenta C)
                                                                                                  | (x == a || x == a - 1) && y > b && dir == E && (posvalida (a,b) B m) && las /= 0 = Just (Movimenta B)
                                                                                                  | (x == a || x == a + 1) && y > b && dir == E && (posvalida (a,b) B m) && las /= 0 && d /= D = Just (Movimenta B)
                                                                                                  | (x == a || x == a - 1) && y > b && dir == E && (posvalida (a,b) C m) && las /= 0 && d /= D = Just (Movimenta C)  
                                                                                                  | (x == a || x == a + 1) && y > b && dir == E && not(posvalida (a,b) C m) && las /= 0 && d == D = Just (Movimenta B)
                                                                                                  | (x == a || x == a - 1) && y > b && dir == E && not(posvalida (a,b) B m) && las /= 0 && d == D = Just (Movimenta C)                                                                                      
                                                                                                  
                                                                                                  
                                                                                                  | eixox x a && y < b && dir == D && (not(posvalida (a,b) C m) || not(posvalida (a-1,b) C m)) && las /= 0 && (posvalida (a,b) B m) && (posvalida (a+1,b) B m) = Just (Movimenta B)
                                                                                                  | eixox x a && y < b && dir == D && (not(posvalida (a,b) B m) || not(posvalida (a+1,b) B m)) && las /= 0 && (posvalida (a,b) C m) && (posvalida (a-1,b) C m) = Just (Movimenta C)
                                                                                                  | (x == a || x == a + 1) && y < b && d == C && dir == D && (posvalida (a,b) C m) && las /= 0 = Just (Movimenta C)
                                                                                                  | (x == a || x == a - 1) && y < b && d == B && dir == D && (posvalida (a,b) B m) && las /= 0 = Just (Movimenta B)
                                                                                                  | (x == a || x == a + 1) && y < b && dir == D && (posvalida (a,b) C m) && las /= 0 = Just (Movimenta C)
                                                                                                  | (x == a || x == a - 1) && y < b && dir == D && (posvalida (a,b) B m) && las /= 0 = Just (Movimenta B)
                                                                                                  | (x == a || x == a + 1) && y < b && dir == D && (posvalida (a,b) B m) && las /= 0 && d /= E = Just (Movimenta B)
                                                                                                  | (x == a || x == a - 1) && y < b && dir == D && (posvalida (a,b) C m) && las /= 0 && d /= E = Just (Movimenta C)
                                                                                                  | (x == a || x == a + 1) && y < b && dir == D && not(posvalida (a,b) C m) && las /= 0 && d == E = Just (Movimenta B)
                                                                                                  | (x == a || x == a - 1) && y < b && dir == D && not(posvalida (a,b) B m) && las /= 0 && d == E = Just (Movimenta C)  
                                                                                                  
--                                                                                                              COLUNAS

                                                                                                  | y == b && x > a && dir == C && (not(posvalida (a,b) D m) || not(posvalida (a,b+1) D m)) && las /= 0 && (posvalida (a,b) E m) && (posvalida (a,b-1) E m) = Just (Movimenta E)
                                                                                                  | y == b && x > a && dir == C && (not(posvalida (a,b) E m) || not(posvalida (a,b-1) E m)) && las /= 0 && (posvalida (a,b) D m) && (posvalida (a,b+1) D m) = Just (Movimenta D)
                                                                                                  | x > a && (y == b || y == b - 1) && d == D && dir == C && (posvalida (a,b) D m) && las /= 0 = Just (Movimenta D)
                                                                                                  | x > a && (y == b || y == b + 1) && d == E && dir == C && (posvalida (a,b) E m) && las /= 0 = Just (Movimenta E)
                                                                                                  | x > a && (y == b || y == b - 1) && dir == C && (posvalida (a,b) D m) && las /= 0 = Just (Movimenta D)
                                                                                                  | x > a && (y == b || y == b + 1) && dir == C && (posvalida (a,b) E m) && las /= 0 = Just (Movimenta E)
                                                                                                  | x > a && (y == b || y == b - 1) && dir == C && (posvalida (a,b) E m) && las /= 0 && d /= B = Just (Movimenta E)
                                                                                                  | x > a && (y == b || y == b + 1) && dir == C && (posvalida (a,b) D m) && las /= 0 && d /= B = Just (Movimenta D)
                                                                                                  | x > a && (y == b || y == b - 1) && dir == C && not(posvalida (a,b) D m) && las /= 0 && d == B = Just (Movimenta E)
                                                                                                  | x > a && (y == b || y == b + 1) && dir == C && not(posvalida (a,b) E m) && las /= 0 && d == B = Just (Movimenta D)
                                                                                                  

                                                                                                  | y == b && x < a && dir == B && (not(posvalida (a,b) D m) || not(posvalida (a,b+1) D m)) && las /= 0 && (posvalida (a,b) E m) && (posvalida (a,b-1) E m) = Just (Movimenta E)
                                                                                                  | y == b && x < a && dir == B && (not(posvalida (a,b) E m) || not(posvalida (a,b-1) E m)) && las /= 0 && (posvalida (a,b) D m) && (posvalida (a,b+1) D m) = Just (Movimenta D)
                                                                                                  | x < a && (y == b || y == b - 1) && d == D && dir == B && (posvalida (a,b) D m) && las /= 0 = Just (Movimenta D)
                                                                                                  | x < a && (y == b || y == b + 1) && d == E && dir == B && (posvalida (a,b) E m) && las /= 0 = Just (Movimenta E)
                                                                                                  | x < a && (y == b || y == b - 1) && dir == B && (posvalida (a,b) D m) && las /= 0 = Just (Movimenta D)
                                                                                                  | x < a && (y == b || y == b + 1) && dir == B && (posvalida (a,b) E m) && las /= 0 = Just (Movimenta E)
                                                                                                  | x < a && (y == b || y == b - 1) && dir == B && (posvalida (a,b) E m) && las /= 0 && d /= C = Just (Movimenta E)
                                                                                                  | x < a && (y == b || y == b + 1) && dir == B && (posvalida (a,b) D m) && las /= 0 && d /= C = Just (Movimenta D)
                                                                                                  | x < a && (y == b || y == b - 1) && dir == B && not(posvalida (a,b) D m) && las /= 0 && d == C = Just (Movimenta E)
                                                                                                  | x < a && (y == b || y == b + 1) && dir == B && not(posvalida (a,b) E m) && las /= 0 && d == C = Just (Movimenta D)
                                                                                                  | suscdisparo (a,b) dis = reagedis d (a,b) dis m


                                                                                                  | eixox x a && y > b && d == D && las == 0 = Just (Dispara Canhao)
                                                                                                  | eixox x a && y > b && d /= D && las == 0 = Just (Movimenta D)
                                                                                                  | eixox x a && y < b && d == E && las == 0 = Just (Dispara Canhao)
                                                                                                  | eixox x a && y < b && d /= E && las == 0 = Just (Movimenta E)
                                                                                                  | eixoy y b && x > a && d == B && las == 0 = Just (Dispara Canhao)
                                                                                                  | eixoy y b && x > a && d /= B && las == 0 = Just (Movimenta B)
                                                                                                  | eixoy y b && x < a && d == C && las == 0 = Just (Dispara Canhao)
                                                                                                  | eixoy y b && x < a && d /= C && las == 0 = Just (Movimenta C) 



                                                                                          -----                 ATAQUE
                                                                   
                                                                                                  | eixox x a && y > b && dir /= E && d /= D = Just (Movimenta D)                                                             
                                                                                                  | eixox x a && y < b && dir /= D && d /= E = Just (Movimenta E)                                                                                    
                                                                                                  | x < a && eixoy y b && dir /= B && d /= C = Just (Movimenta C)                                                                               
                                                                                                  | x > a && eixoy y b && dir /= C && d /= B = Just (Movimenta B)
                                                                                                  | eixox x a && y > b && dir /= E && d == D = Just (Dispara Canhao)
                                                                                                  | eixox x a && y > b && dir == E && d == D && las == 0 = Just (Dispara Canhao)
                                                                                                  | eixox x a && y < b && dir /= D && d == E = Just (Dispara Canhao)
                                                                                                  | x < a && eixoy y b && dir /= B && d == C = Just (Dispara Canhao)
                                                                                                  | x > a && eixoy y b && dir /= C && d == B = Just (Dispara Canhao)



                                                                                                  | otherwise = reagedir i (Estado m js dis) bot
-- | recebe o identificador do bot, uma lista de disparos, e verifica que existe algum choque pertencente ao bot
botchoque :: Int -> [Disparo] -> Bool
botchoque i [] = False
botchoque i ((DisparoChoque j tick):t) | i == j = True
                                       | otherwise = botchoque i t
botchoque i (h:t) = botchoque i t


-- | verifica se existe Bloco Indestrutivel entre duas posições
verificaBloco :: Posicao -> Posicao -> Mapa -> Bool
verificaBloco (a,b) (x,y) m | eixox x a && y > b && (not(verificaLinhax (a,b) (x,y) m) || not(verificaLinhax (a+1,b) (x,y) m)) = True
                            | eixox x a && y < b && (not(verificaLinhaxi (a,b) (x,y) m) || not(verificaLinhaxi (a+1,b) (x,y) m)) = True
                            | eixoy y b && x > a && (not(verificaColunay (a,b) (x,y) m) || not(verificaColunay (a,b+1) (x,y) m)) = True
                            | eixoy y b && x < a && (not(verificaColunayi (a,b) (x,y) m) || not(verificaColunayi (a,b+1) (x,y) m)) = True
                            | otherwise = False

-- | verifica se existe algum bloco insdestrutivel entre a posição do bot e outra posição ( para y > b - Bot à esquerda)
verificaLinhax :: Posicao -> Posicao -> Mapa -> Bool 
verificaLinhax (a,b) (x,y) m | y == b = True
                             | encontraPosicaoMatriz (a,b) m == Bloco Indestrutivel = False
                             | otherwise = verificaLinhax (a,b+1) (x,y) m
-- | verifica se existe algum bloco insdestrutivel entre a posição do bot e outra posição ( para y < b - Bot à direita)
verificaLinhaxi :: Posicao -> Posicao -> Mapa -> Bool 
verificaLinhaxi (a,b) (x,y) m | y == b = True
                              | encontraPosicaoMatriz (a,b) m == Bloco Indestrutivel = False
                              | otherwise = verificaLinhaxi (a,b-1) (x,y) m


-- | verifica se existe algum bloco insdestrutivel entre a posição do bot e outra posição ( para x > a - Bot em cima)
verificaColunay :: Posicao -> Posicao -> Mapa -> Bool                        
verificaColunay (a,b) (x,y) m | a == x = True
                              | encontraPosicaoMatriz (a,b) m == Bloco Indestrutivel = False
                              | otherwise = verificaColunay (a+1,b) (x,y) m

-- | verifica se existe algum bloco insdestrutivel entre a posição do bot e outra posição ( para x < a - Bot em baixo)
verificaColunayi :: Posicao -> Posicao -> Mapa -> Bool                        
verificaColunayi (a,b) (x,y) m | a == x = True
                               | encontraPosicaoMatriz (a,b) m == Bloco Indestrutivel = False
                               | otherwise = verificaColunayi (a-1,b) (x,y) m

-- | recebe a posição do Bot, uma lista de disparos, e verifica se existe algum disparo canhão a ir na direção do bot
suscdisparo :: Posicao -> [Disparo] -> Bool
suscdisparo _ [] = False
suscdisparo p (h:t) | verificaDisparos p h = True
                    | otherwise = suscdisparo p t

-- | compara a posição do bot e a posição do disparo e verifica se está a dirigir-se na direção do bot
verificaDisparos :: Posicao -> Disparo -> Bool
verificaDisparos (x,y) (DisparoCanhao i (a,b) d) = ((x == a + 1 || x == a - 1 || x == a) && ((b > y && d == E) || (b < y && d == D))) || ((y == b + 1 || y == b - 1 || y == b) && ((a > x && d == C) || (a < x && d == B)))    
verificaDisparos p h = False

-- | recebe a direção a direção do Bot, a sua posição, a lista de disparos e o mapa e , consoante condições, executa uma jogada
-- | se existir um bloco indestrutível entre as 2 posições ou se o disparo não estiver na direção do bot, executa a função para o resto da lsita de disparos
reagedis :: Direcao -> Posicao -> [Disparo] -> Mapa -> Maybe Jogada
reagedis dir p [] m = Nothing
reagedis dir (x,y) (dis@(DisparoCanhao i (a,b) d):t) m  | not(verificaDisparos (x,y) dis) || verificaBloco (x,y) (a,b) m = reagedis dir (x,y) t m 
                                                        | oposta dir == d && (x == a || y == b) = Just (Dispara Canhao) 
                                                        | (x == a || y == b) = Just (Movimenta (oposta d))
                                                        | (x == a || x == a + 1) && posvalida (x,y) B m = Just (Movimenta B)
                                                        | x == a - 1 && posvalida (x,y) C m = Just (Movimenta C)
                                                        | (x == a || x == a + 1) && (posvalida (x,y) C m) && (posvalida (x-1,y) C m) = Just (Movimenta C)
                                                        | x == a - 1 && (posvalida (x,y) B m) && (posvalida (x+1,y) B m) = Just (Movimenta B)
                                                        | (y == b || y == b + 1) && posvalida (x,y) D m = Just (Movimenta D)
                                                        | y == b - 1 && posvalida (x,y) E m = Just (Movimenta E)
                                                        | (y == b || y == b + 1) && (posvalida (x,y) E m) && (posvalida (x,y-1) E m) = Just (Movimenta E)
                                                        | y == b - 1 && (posvalida (x,y) D m) && (posvalida (x,y+1) D m) = Just (Movimenta D)
                                                        | eixox x a && not (posvalida (x,y) C m) && not(posvalida (x,y) B m) && (dir == oposta d) = Just (Dispara Canhao)
                                                        | otherwise = Just (Movimenta (oposta d))
reagedis dir p (h:t) m = reagedis dir p t m                                    

-- | função que transforma uma direção na direção oposta
oposta :: Direcao -> Direcao
oposta C = B
oposta B = C
oposta D = E
oposta E = D


--          Jogador   Bot
-- | dando a linha do jogador e a linha do bot, verifica se está em zona de perigo
eixox :: Int -> Int -> Bool
eixox x a = x == a || x == a + 1 || x == a - 1
-- | dando a coluna do jogador e a coluna do bot, verifica se está em zona de perigo
eixoy :: Int -> Int -> Bool
eixoy y b = y == b || y == b + 1 || y == b - 1



--auxreagedir :: [Jogador] -> Jogador
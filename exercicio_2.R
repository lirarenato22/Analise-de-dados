install.packages("tidyverse")
library(tidyverse)

install.packages("poliscidata")
library(poliscidata)

install.packages("dplyr")
library(dplyr)

install.packages("magrittr")
library(magrittr)

#
# Suponha que tenhamos o dataframe df abaixo
#
# x     y
# A     5
# A     3
# B     8
# B    12
#
# Complete o c�digo que obt�m o seguinte resultado:
#
#        z
#        7
#
df_q1 <- data.frame(x = c("A", "A", "B", "B"), y = c(5, 3, 8, 12))

df_q1 %>%
  summarise(z = mean(y))

         
#######################################################################

# Suponha que tenhamos o dataframe df abaixo
#
# y1    y2    y3    y4
# 8.04  9.14  7.46  6.58
# 6.95  8.14  6.77  5.76
# 7.58  8.74  12.74 7.71
#
# Complete o c�digo que obt�m o seguinte resultado:
#
# y1    
# 8.04  
# 6.95  
# 7.58  

df_q2 <- data.frame(y1 = c(8.04, 6.95, 7.58), y2 = c(9.14, 8.14, 8.74), 
                 y3 = c(7.46, 6.77, 12.74), y4 = c(6.58, 5.76, 7.71))

df_q2_selecionado <- df_q2 %>% select(y1)
  
#######################################################################

# Suponha que tenhamos o dataframe df abaixo
#
#    x  y
#   1  10
#   6  8
#   2  3
#   4  5
#
# Complete o c�digo que obt�m o seguinte resultado, fazendo uma opera��o
# entre x e y
#
#    x  y   z
#   1  10  -9
#   6  8   -2
#   2  3   -1
#   4  5   -1
#

df_q3 <- data.frame(x = c(1, 6, 2, 4), y = c(10, 8, 3, 5))

df_q3 %>% mutate(z = x - y)
  
########################################################################

#
# Suponha que tenhamos o dataframe df abaixo
#
#    city sales
# Boston   220
# Boston   125
#    NYC   150
#    NYC   250
#
# Complete o c�digo que obt�m o seguinte resultado:
#
# city   avg_sales
# Boston      172
# NYC         200 

df_q4 <- data.frame(city = c("Boston", "Boston", 
                             "NYC", "NYC"), sales = c(220, 125, 150, 250))

df_q4 %>% group_by(city) %>% summarise(avg_sales = mean(sales))
  
########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
#week   min   max
#  3    55    60
#  2    52    56
#  1    60    63
#  4    65    67
#
# Complete o c�digo que obt�m o seguinte resultado:
#
#week   min   max
#  1    60    63
#  2    52    56
#  3    55    60
#  4    65    67

df_q5 <- data.frame(week = c(3, 2, 1, 4),
                    min = c(55, 52, 60, 65), max = c(60, 56, 63, 67))

df_q5 %>% arrange(week)

########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
# x_b_1  x_b_2  y_c_1  y_c_2
#  A      2      W1     25
#  A      4      W2     21
#  B      6      W1     26
#  B      8      W2     30
#
# Complete o c�digo que obt�m o seguinte resultado:
#
# y_c_1  y_c_2
#  W1     25
#  W2     21
#  W1     26
#  W2     30

df_q6 <- data.frame(x_b_1 = c("A", "A", "B", "B"),
                    x_b_2 = c(2, 4, 6, 8),
                    y_c_1 = c("W1", "W2", "W1", "W2"), 
                    y_c_2 = c(25, 21, 26, 30))


df_q6 %>%
select(starts_with("y"))

#########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
# Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# 78           6.7         3.0          5.0         1.7 versicolor
# 121          6.9         3.2          5.7         2.3  virginica
# 11           5.4         3.7          1.5         0.2     setosa
# 92           6.1         3.0          4.6         1.4 versicolor
# 146          6.7         3.0          5.2         2.3  virginica
# 62           5.9         3.0          4.2         1.5 versicolor
# 50           5.0         3.3          1.4         0.2     setosa
# 17           5.4         3.9          1.3         0.4     setosa
# 69           6.2         2.2          4.5         1.5 versicolor
# 143          5.8         2.7          5.1         1.9  virginica
#
# Complete o c�digo que obt�m o seguinte resultado:
#
#Species      Sepal.Area
#versicolor      20.10
#virginica       22.08
#setosa          19.98
#versicolor      18.30
#virginica       20.10
#versicolor      17.70
#setosa          16.50
#setosa          21.06
#versicolor      13.64
#virginica      15.66

df_q7 <- data.frame(Sepal.Length = c(6.7, 6.9, 5.4, 6.1, 6.7, 5.9, 5.0, 5.4, 6.2, 5.8),
                    Sepal.Width = c(3.0, 3.2, 3.7, 3.0, 3.0, 3.0, 3.3, 3.9, 2.2, 2.7),
                    Petal.Length = c(5.0, 5.7, 1.5, 4.6, 5.2, 4.2, 1.4, 1.3, 4.5, 5.1), 
                    Petal.Width = c(1.7, 2.3, 0.2, 1.4, 2.3, 1.5, 0.2, 0.4, 1.5, 1.9),
                    Species = c("versicolor", "virginica", "setosa", "versicolor", 
                                "virginica", "versicolor", "setosa", "setosa",
                                "versicolor", "virginica"))

df_q7 %>% transmute(Species, Sepal.Area = Sepal.Length*Sepal.Width)

########################################################################

# Suponha que tenhamos o dataframe df abaixo
#
#name         start       end         party     
#Eisenhower   1953-01-20  1961-01-20  Republican
#Kennedy      1961-01-20  1963-11-22  Democratic
#Johnson      1963-11-22  1969-01-20  Democratic
#Nixon        1969-01-20  1974-08-09  Republican
#Ford         1974-08-09  1977-01-20  Republican
#Carter       1977-01-20  1981-01-20  Democratic
#Reagan       1981-01-20  1989-01-20  Republican
#Bush         1989-01-20  1993-01-20  Republican
#Clinton      1993-01-20  2001-01-20  Democratic
#Bush         2001-01-20  2009-01-20  Republican
#Obama        2009-01-20  2017-01-20  Democratic
#
#Crie um c�digo abaixo para que se altere a vari�vel party
#deixando apenas a primeira letra dos partidos

df_q8 <- data.frame(name = c("Eisenhower", "Kennedy", "Johnson", 
                             "Nixon","Ford", "Carter", 
                             "Reagan", "Bush", "Clinton", 
                             "Bush", "Obama"),
                    start = c("1953-01-20", "1961-01-20", "1963-11-22", 
                              "1969-01-20", "1974-08-09", "1977-01-20", 
                              "1981-01-20", "1989-01-20", "1993-01-20", 
                              "2001-01-20", "2009-01-20"),
                    end = c("1961-01-20", "1963-11-22", "1969-01-20", 
                            "1974-08-09", "1977-01-20", "1981-01-20", 
                            "1989-01-20", "1993-01-20", "2001-01-20", 
                            "2009-01-20", "2017-01-20"), 
                    party = c("Republican", "Democratic", "Democratic", 
                              "Republican", "Republican", "Democratic",
                              "Republican", "Republican", "Democratic",
                              "Republican", "Democratic"))

df_q8 %>% mutate(party = recode(party, Republican = "R", Democratic = "D"))

###############################################################################

# No pacote poliscidata existe um banco de dados chamado nes, com informa��es 
# do American National Election Survey. Para os exer�cicios a seguir, instale 
# o pacote poliscidata e tidyverse, carregue-os e crie um objeto chamado
# df com os dados do nes. 

install.packages("tidyverse")
library(tidyverse)

install.packages("poliscidata")
library(poliscidata)

df <- nes

# Fa�a uma primeira explora��o do banco de dados com todos os comandos
# passados at� aqui que possuem esse objetivo

glimpse(nes)

?nes

str(df)

head(df)

tail(df)

summary(df)

# Quantos respondentes possui na pesquisa?

str(df)

#5.916 respondentes.

# Caso queiram ter mais informa��es sobre as vari�veis do nes, basta rodar
# o c�digo `?nes`, que no canto inferior direito aparecer� uma descri��o.
# Como temos muitas vari�veis, deixe apenas as colunas
# ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote
# income5, gender.

?nes

df_selecionado <- df %>% select(ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote,
                               income5, gender)


# Se quisermos ter informa��es apenas de pessoas que votaram na
# elei��o de 2012, podemos usar a vari�vel voted2012. Tire do banco
# os respondentes que n�o votaram

df_filtrado <- df_selecionado %>%
  filter(voted2012 == "Voted")

# Quantos respondentes sobraram?

str(df_filtrado)

#Sobraram 4.404 respondentes.


# Crie uma vari�vel chamada white que indica se o respondente � branco
# ou n�o a partir da vari�vel dem_raceeth, crie a vari�vel ideology a
# partir da vari�vel ftgr_cons (0-33 como liberal, 34 a 66 como centro,
# e 67 a 100 como conservador), ao mesmo tempo em que muda
# a vari�vel obama_vote para trocar o 1 por "Sim" e 2 por "n�o"

df_adicionado_white_ideology_renomeado_obama <- df_selecionado %>%
  mutate(white = dem_raceeth == "1. White non-Hispanic", ideology = case_when(ftgr_cons <= 33 ~ "liberal", ftgr_cons >= 34 & ftgr_cons <= 66 ~ "centro", ftgr_cons >= 67 & ftgr_cons <= 100 ~ "conservador"), obama_vote = recode(obama_vote, "1" = "Sim", "0" = "n�o"))


# Demonstre como observar a quantidade de pessoas em cada uma das
# categorias de science_use

df_adicionado_ideology %>% count(science_use)

# Demonstre como observar a m�dia de conservadorismo (vari�vel 
# ftgr_cons) para cada categoria de science_use

df_adicionado_ideology %>% summarise(media = mean(ftgr_cons, na.rm = TRUE))
  
###############################################################################

# Responder as quest�es te�ricas da aula abaixo

#EXERC�CIO TE�RICO - 2

#Selecione o principal artigo do campo de estudos relacionado ao seu trabalho e responda as seguintes quest�es: 
  
  #Q1) Qual � a quest�o da pesquisa? 
  
  #Resposta/Renato: Se as institui��es devem ter uma esp�cie de capacidade de perman�ncia, como as mesmas institui��es podem explicar tanto a estabilidade quanto a mudan�a?
  
  #Q2) Qual � a teoria? 
  
  #Resposta/Renato: Immergut (1992) contribui com a discuss�o neoinstitucionalista hist�rica apresentando, a partir de casos de pa�ses europeus (Su�cia, Fran�a e Su��a), como as mesmas institui��es podem servir para explicar tanto a mudan�a como a perman�ncia de pol�ticas. Ela seleciona o seguro nacional de sa�de como pol�tica analisada e observa como inputs e outputs se relacionam de formas diferentes a depender do sistema pol�tico de cada pa�s, o que, segundo a autora, rompe com a no��o usual de explica��o por correla��es. 
#Em "As tr�s vers�es do neo-institucionalismo", Peter Hall e Rosemary Taylor tecem coment�rios sobre a perspectiva calculadora utilizada na pesquisa de Ellen Immergut:
  #"Ellen Immergut, por exemplo, explica as diferen�as entre pa�ses em mat�ria de reforma do sistema de sa�de pelo grau em que os agrupamentos de m�dicos est�o dispostos a compor com os partid�rios da reforma, e liga isso ao modo como a estrutura institucional do sistema pol�tico afeta as expectativas desses grupos no tocante �s possibilidades de sucesso no caso de contestarem uma decis�o que n�o lhes conviesse. Sua an�lise repousa sobre um procedimento calculador cl�ssico."  (HALL, TAYLOR, 2003, p.199)
#Nesse �nterim, seguindo a l�gica calculadora, os indiv�duos sempre buscar�o maximizar os seus ganhos de acordo com as suas prefer�ncias, enquanto as institui��es afetar�o a informa��o e, consequentemente, o comportamento dos indiv�duos, mudando as expectativas que os atores t�m sobre a rea��o dos outros atores �s suas a��es (HALL, TAYLOR, 2003). Dependendo de onde se encontram as oportunidades de veto em um sistema pol�tico, ser� permitido modificar decis�es em diversas etapas no processo de formula��o de pol�ticas p�blicas. 

#Q3) Qual � o desenho de pesquisa?
  
  #Resposta/Renato: Estudo observacional de corte transversal, a partir das unidades espaciais "Su�cia", "Fran�a" e "Su��a".

#Q4) Como o artigo se sai nos 4 quesitos de avalia��o de causalidade? 
  
  #Resposta/Renato: 1 - S, o mecanismo consiste na varia��o das formata��es das regras do jogo incidindo na varia��o dos resultados em rela��o aos seguros nacionais de sa�de dos casos; 2 - S, considerando que os resultados n�o t�m como causar as regras do jogo; 3 - S, existe covaria��o entre X e Y; 4 - S, a autora, durante a pesquisa, elencou at� a exaust�o a quantidade de vari�veis que poderiam afetar, de forma a control�-las para garantir que a rela��o entre X e Y n�o fosse esp�ria.

#Q5) O que ele conclui? 
  
  #Resposta/Renato: Formata��es diferentes das regras do jogo causaram resultados diferentes em rela��o aos seguros nacionais de sa�de da Su�cia, da Fran�a e da Su��a.

#Q6) Como a sua pesquisa d� um passo a mais para o desenvolvimento te�rico presente neste artigo?
  
  #Resposta/Renato: Pretendo ampliar o n�mero de casos analisados, com o intuito de observar se a teoria de Immergut (1992) � generaliz�vel, ou seja, analisar se as formata��es das regras do jogo causam resultados diferentes quando a amostra � maior.

#Q7) Elabore qual � a pergunta da sua pesquisa em apenas uma frase.

#Resposta/Renato: Considerando que a pesquisa pretendida � uma amplia��o da pesquisa de Immergut (1992), a pergunta ser� a mesma, ou seja, "Se as institui��es devem ter uma esp�cie de capacidade de perman�ncia, como as mesmas institui��es podem explicar tanto a estabilidade quanto a mudan�a?".

#Q8) Pense no seu trabalho e avalie em que medida ele passa pelas 4 avalia��es de rela��o causal, e quais problemas ele pode ter em cada uma delas.

#Resposta/Renato: 1 - S; 2 - S; 3 - S; 4 - S. A maior dificuldade vai ser reunir as vari�veis que podem estar relacionadas, levando em considera��o um N maior, que ter� tamb�m modelos de institui��es diferentes e possivelmente de dif�cil compara��o, que implicar�o em vari�veis Z diversas de acordo com as institui��es. Nesse sentido, dever� ser balanceada a rela��o "generaliza��o vs. parcim�nia", na medida em que se queira controlar as vari�veis Z para garantir que a rela��o entre X e Y n�o seja esp�ria.

#REFER�NCIAS
#HALL, Peter A.; TAYLOR, Rosemary C. R.. As tr�s vers�es do neo-institucionalismo. Lua Nova [online]. 2003, n.58, pp.193-223. ISSN 1807-0175. https://doi.org/10.1590/S0102-64452003000100010.

#IMMERGUT, Ellen M. As Regras do Jogo: A L�gica da Pol�tica de Sa�de na Fran�a, na Su��a e na Su�cia. Nova York, Cambridge University Press. 1992.

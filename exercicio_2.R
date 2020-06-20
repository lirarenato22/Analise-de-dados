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
# Complete o código que obtém o seguinte resultado:
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
# Complete o código que obtém o seguinte resultado:
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
# Complete o código que obtém o seguinte resultado, fazendo uma operação
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
# Complete o código que obtém o seguinte resultado:
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
# Complete o código que obtém o seguinte resultado:
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
# Complete o código que obtém o seguinte resultado:
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
# Complete o código que obtém o seguinte resultado:
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
#Crie um código abaixo para que se altere a variável party
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

# No pacote poliscidata existe um banco de dados chamado nes, com informações 
# do American National Election Survey. Para os exerícicios a seguir, instale 
# o pacote poliscidata e tidyverse, carregue-os e crie um objeto chamado
# df com os dados do nes. 

install.packages("tidyverse")
library(tidyverse)

install.packages("poliscidata")
library(poliscidata)

df <- nes

# Faça uma primeira exploração do banco de dados com todos os comandos
# passados até aqui que possuem esse objetivo

glimpse(nes)

?nes

str(df)

head(df)

tail(df)

summary(df)

# Quantos respondentes possui na pesquisa?

str(df)

#5.916 respondentes.

# Caso queiram ter mais informações sobre as variáveis do nes, basta rodar
# o código `?nes`, que no canto inferior direito aparecerá uma descrição.
# Como temos muitas variáveis, deixe apenas as colunas
# ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote
# income5, gender.

?nes

df_selecionado <- df %>% select(ftgr_cons, dem_raceeth, voted2012, science_use, preknow3, obama_vote,
                               income5, gender)


# Se quisermos ter informações apenas de pessoas que votaram na
# eleição de 2012, podemos usar a variável voted2012. Tire do banco
# os respondentes que não votaram

df_filtrado <- df_selecionado %>%
  filter(voted2012 == "Voted")

# Quantos respondentes sobraram?

str(df_filtrado)

#Sobraram 4.404 respondentes.


# Crie uma variável chamada white que indica se o respondente é branco
# ou não a partir da variável dem_raceeth, crie a variável ideology a
# partir da variável ftgr_cons (0-33 como liberal, 34 a 66 como centro,
# e 67 a 100 como conservador), ao mesmo tempo em que muda
# a variável obama_vote para trocar o 1 por "Sim" e 2 por "não"

df_adicionado_white <- df_selecionado %>%
  mutate(white = dem_raceeth == "1. White non-Hispanic")

df_adicionado_ideology <- df_adicionado_white %>% mutate(ideology = case_when(ftgr_cons <= 33 ~ "liberal", ftgr_cons >= 34 & ftgr_cons <= 66 ~ "centro", ftgr_cons >= 67 & ftgr_cons <= 100 ~ "conservador"), obama_vote = recode(obama_vote, "1" = "Sim", "0" = "não"))


# Demonstre como observar a quantidade de pessoas em cada uma das
# categorias de science_use

df_adicionado_ideology %>% count(science_use)

# Demonstre como observar a média de conservadorismo (variável 
# ftgr_cons) para cada categoria de science_use

df_adicionado_ideology %>% summarise(media = mean(ftgr_cons, na.rm = TRUE))
  
###############################################################################

# Responder as questões teóricas da aula abaixo

#EXERCÍCIO TEÓRICO - 2

#Selecione o principal artigo do campo de estudos relacionado ao seu trabalho e responda as seguintes questões: 
  
  #Q1) Qual é a questão da pesquisa? 
  
  #Resposta/Renato: Se as instituições devem ter uma espécie de capacidade de permanência, como as mesmas instituições podem explicar tanto a estabilidade quanto a mudança?
  
  #Q2) Qual é a teoria? 
  
  #Resposta/Renato: Immergut (1992) contribui com a discussão neoinstitucionalista histórica apresentando, a partir de casos de países europeus (Suécia, França e Suíça), como as mesmas instituições podem servir para explicar tanto a mudança como a permanência de políticas. Ela seleciona o seguro nacional de saúde como política analisada e observa como inputs e outputs se relacionam de formas diferentes a depender do sistema político de cada país, o que, segundo a autora, rompe com a noção usual de explicação por correlações. 
#Em "As três versões do neo-institucionalismo", Peter Hall e Rosemary Taylor tecem comentários sobre a perspectiva calculadora utilizada na pesquisa de Ellen Immergut:
  #"Ellen Immergut, por exemplo, explica as diferenças entre países em matéria de reforma do sistema de saúde pelo grau em que os agrupamentos de médicos estão dispostos a compor com os partidários da reforma, e liga isso ao modo como a estrutura institucional do sistema político afeta as expectativas desses grupos no tocante às possibilidades de sucesso no caso de contestarem uma decisão que não lhes conviesse. Sua análise repousa sobre um procedimento calculador clássico."  (HALL, TAYLOR, 2003, p.199)
#Nesse ínterim, seguindo a lógica calculadora, os indivíduos sempre buscarão maximizar os seus ganhos de acordo com as suas preferências, enquanto as instituições afetarão a informação e, consequentemente, o comportamento dos indivíduos, mudando as expectativas que os atores têm sobre a reação dos outros atores às suas ações (HALL, TAYLOR, 2003). Dependendo de onde se encontram as oportunidades de veto em um sistema político, será permitido modificar decisões em diversas etapas no processo de formulação de políticas públicas. 

#Q3) Qual é o desenho de pesquisa?
  
  #Resposta/Renato: Estudo observacional de corte transversal, a partir das unidades espaciais "Suécia", "França" e "Suíça".

#Q4) Como o artigo se sai nos 4 quesitos de avaliação de causalidade? 
  
  #Resposta/Renato: 1 - S, o mecanismo consiste na variação das formatações das regras do jogo incidindo na variação dos resultados em relação aos seguros nacionais de saúde dos casos; 2 - S, considerando que os resultados não têm como causar as regras do jogo; 3 - S, existe covariação entre X e Y; 4 - S, a autora, durante a pesquisa, elencou até a exaustão a quantidade de variáveis que poderiam afetar, de forma a controlá-las para garantir que a relação entre X e Y não fosse espúria.

#Q5) O que ele conclui? 
  
  #Resposta/Renato: Formatações diferentes das regras do jogo causaram resultados diferentes em relação aos seguros nacionais de saúde da Suécia, da França e da Suíça.

#Q6) Como a sua pesquisa dá um passo a mais para o desenvolvimento teórico presente neste artigo?
  
  #Resposta/Renato: Pretendo ampliar o número de casos analisados, com o intuito de observar se a teoria de Immergut (1992) é generalizável, ou seja, analisar se as formatações das regras do jogo causam resultados diferentes quando a amostra é maior.

#Q7) Elabore qual é a pergunta da sua pesquisa em apenas uma frase.

#Resposta/Renato: Considerando que a pesquisa pretendida é uma ampliação da pesquisa de Immergut (1992), a pergunta será a mesma, ou seja, "Se as instituições devem ter uma espécie de capacidade de permanência, como as mesmas instituições podem explicar tanto a estabilidade quanto a mudança?".

#Q8) Pense no seu trabalho e avalie em que medida ele passa pelas 4 avaliações de relação causal, e quais problemas ele pode ter em cada uma delas.

#Resposta/Renato: 1 - S; 2 - S; 3 - S; 4 - S. A maior dificuldade vai ser reunir as variáveis que podem estar relacionadas, levando em consideração um N maior, que terá também modelos de instituições diferentes e possivelmente de difícil comparação, que implicarão em variáveis Z diversas de acordo com as instituições. Nesse sentido, deverá ser balanceada a relação "generalização vs. parcimônia", na medida em que se queira controlar as variáveis Z para garantir que a relação entre X e Y não seja espúria.

#REFERÊNCIAS
#HALL, Peter A.; TAYLOR, Rosemary C. R.. As três versões do neo-institucionalismo. Lua Nova [online]. 2003, n.58, pp.193-223. ISSN 1807-0175. https://doi.org/10.1590/S0102-64452003000100010.

#IMMERGUT, Ellen M. As Regras do Jogo: A Lógica da Política de Saúde na França, na Suíça e na Suécia. Nova York, Cambridge University Press. 1992.


# Entre no seguinte link:
# https://pt.wikipedia.org/wiki/Eleição_presidencial_no_Brasil_em_2002
# Vá até o tópico RESUMO DAS ELEICOES
# Crie um vetor com o nome dos seis candidatos a presidência

candidatos <- 

# Crie um vetor com a sigla do partido de cada candidato

partido <- 

# Crie um vetor com o total de votos de cada candidato
  
votos_candidatos <- 

# Crie um objeto calculando a soma do votos dos candidatos no 1o turno
  
total_votos <- 

# Crie um vetor com a porcentagem de votos de cada candidato
# fazendo uma operação aritmética entre o objeto votos_candidatos
# e o objeto total_votos

porcentagem_votos <- 

# Crie uma matriz que conste uma coluna com o total de votos de cada candidato
# e outra com a porcentagem dos votos de cada candidato

matriz_votos <- 

# Nomeie as linhas da matriz com o nome dos candidatos


# Nomeie também as colunas


# Crie um dataframe com o nome dos candidatos, o partido,
# a quantidade de votos e o percentual


# Crie um vetor lógico, indicado TRUE ou FALSE, com a informacao se
# o candidato foi para o segundo turno


# Adicione esta coluna no dataframe


# Calcule a soma da porcentagem dos dois candidatos que obtiveram mais votos


# Exiba as informações do dataframe dos dois candidatos com mais votos


###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# [1] 24 18 31

q <- c(47, 24, 18, 33, 31, 15)
q[?]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# Out Nov
#  24   2

x <- c(5, 4, 24, 2)
y <- c("Ago", "Set", "Out", "Nov")
names(x) <- y

x[?]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# 'data.frame':	2 obs. of  2 variables:
# $ x: Factor w/ 2 levels "d","e": 1 2
# $ y: num  1 4

df <- data.frame(?,)
y = c(1,4)

str(df)

###############################################################################

# Crie a seguinte matriz
#
#       [,1] [,2] [,3]
# [1,]   19   22   25
# [2,]   20   23   26
# [3,]   21   24   27




###############################################################################

# Se Z é uma matriz 4 por 4, qual é o resultado de Z[1,4] ?

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
#  W3 W4 W1 W2 
#  20 69  5 88 

y <- c(20, 69, 5, 88)
q <- c("W3", "W4", "W1", "W2")
names(?) <- ?
  
  ###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
#       [,1] [,2]
# [1,]    4    6
# [2,]    3    7
# [3,]    1    8


cbind(c(4, 3, 1), ?)



###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#       [,1] [,2] [,3] [,4]
# [1,]    1    3   13   15
# [2,]    2    4   14   16

x <- 1:4
y <- 13:26

matrix(?,
       nrow = 2,
       byrow = FALSE)

###############################################################################

# Crie o seguinte dataframe df
#
# df
#    x  y    z
# 1 17  A  Sep
# 2 37  B  Jul
# 3 12  C  Jun
# 4 48  D  Feb
# 5 19  E  Mar




# Ainda utilizando o dataframe df,
# qual código produziria o seguinte resultado?
#
#    x  y
# 1 17  A
# 2 37  B
# 3 12  C



###############################################################################

# Responder o exercício teórico abaixo

candidatos <- c("Luiz Inacio Lula da Silva", "Jose Serra", "Anthony Garotinho", "Ciro Gomes", "Jose Maria de Almeida", "Rui Costa Pimenta")

partido <- c("PT", "PSDB", "PSB", "PPS", "PSTU", "PCO")

votos_candidatos <- c(39455233,19705445,15180097,10170882,402236,38619)

total_votos <- sum(39455233,19705445,15180097,10170882,402236,38619)

porcentagem_votos <- (votos_candidatos/total_votos)*100

matriz_votos <- matrix(c(votos_candidatos, porcentagem_votos), byrow = FALSE, nrow = 6)

rownames(matriz_votos) <- c(candidatos)

colnames(matriz_votos) <- c("Votos", "Percentual de votos")

Resumo_Eleicoes_2002 <- data.frame(candidatos, partido, votos_candidatos, porcentagem_votos)

Segundo_Turno <- c(T, T, F, F, F, F)

Resumo_Eleicoes_2002 <- cbind(Resumo_Eleicoes_2002, Segundo_Turno)

sum(c(porcentagem_votos [1]), c(porcentagem_votos [2]))

Resumo_Eleicoes_2002[1:2,]

q <- c(47, 24, 18, 33, 31, 15)

q[c(2,3,5)]

x <- c(5, 4, 24, 2)

y <- c("Ago", "Set", "Out", "Nov")

names(x) <- y

x[c(3:4)]

df <- data.frame(x = factor(c("d", "e")), y = c(1,4))

str(df)

vetor_1 <- c(19,22,25)

vetor_2 <- c(20,23,26)

vetor_3 <- c(21,24,27)

matriz <- matrix(c(vetor_1, vetor_2, vetor_3), byrow = TRUE, nrow = 3)

print(matriz)

# Pergunta/Rodrigo - Se Z é uma matriz 4 por 4, qual é o resultado de Z[1,4] ?

# Resposta/Renato - É o elemento que está na primeira linha e na última (quarta) coluna da matriz

y <- c(20, 69, 5, 88)

q <- c("W3", "W4", "W1", "W2")

names(y) <- q

print(y)  

cbind(c(4, 3, 1), c(6, 7, 8))

x <- 1:4

y <- 13:26

matrix(c(x, y[1:4]), nrow = 2, byrow = FALSE)

x <- c(17, 37, 12, 48, 19)

y <- c("A","B","C","D","E")

z <- c("Sep", "Jul", "Jun", "Feb", "Mar")

df <- data.frame(x, y, z)

print(df)

df[1:3,1:2]

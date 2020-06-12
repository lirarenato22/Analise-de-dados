
# Entre no seguinte link:
# https://pt.wikipedia.org/wiki/Eleição_presidencial_no_Brasil_em_2002
# Vá até o tópico RESUMO DAS ELEICOES
# Crie um vetor com o nome dos seis candidatos a presidência

candidatos <- c("Luiz Inacio Lula da Silva", "Jose Serra", "Anthony Garotinho", "Ciro Gomes", "Jose Maria de Almeida", "Rui Costa Pimenta")

# Crie um vetor com a sigla do partido de cada candidato

partido <- c("PT", "PSDB", "PSB", "PPS", "PSTU", "PCO")

# Crie um vetor com o total de votos de cada candidato
  
votos_candidatos <- c(39455233,19705445,15180097,10170882,402236,38619)

# Crie um objeto calculando a soma do votos dos candidatos no 1o turno
  
total_votos <- sum(39455233,19705445,15180097,10170882,402236,38619)

# Crie um vetor com a porcentagem de votos de cada candidato
# fazendo uma operação aritmética entre o objeto votos_candidatos
# e o objeto total_votos

porcentagem_votos <- (votos_candidatos/total_votos)*100

# Crie uma matriz que conste uma coluna com o total de votos de cada candidato
# e outra com a porcentagem dos votos de cada candidato

matriz_votos <- matrix(c(votos_candidatos, porcentagem_votos), byrow = FALSE, nrow = 6)

# Nomeie as linhas da matriz com o nome dos candidatos

rownames(matriz_votos) <- c(candidatos)

# Nomeie também as colunas

colnames(matriz_votos) <- c("Votos", "Percentual de votos")

# Crie um dataframe com o nome dos candidatos, o partido,
# a quantidade de votos e o percentual

Resumo_Eleicoes_2002 <- data.frame(candidatos, partido, votos_candidatos, porcentagem_votos)

# Crie um vetor lógico, indicado TRUE ou FALSE, com a informacao se
# o candidato foi para o segundo turno

Segundo_Turno <- c(T, T, F, F, F, F)

# Adicione esta coluna no dataframe

Resumo_Eleicoes_2002 <- cbind(Resumo_Eleicoes_2002, Segundo_Turno)

# Calcule a soma da porcentagem dos dois candidatos que obtiveram mais votos

sum(c(porcentagem_votos [1]), c(porcentagem_votos [2]))

# Exiba as informações do dataframe dos dois candidatos com mais votos

Resumo_Eleicoes_2002[1:2,]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# [1] 24 18 31

q <- c(47, 24, 18, 33, 31, 15)

q[c(2,3,5)]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# Out Nov
#  24   2

x <- c(5, 4, 24, 2)
y <- c("Ago", "Set", "Out", "Nov")
names(x) <- y

x[c(3:4)]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# 'data.frame':	2 obs. of  2 variables:
# $ x: Factor w/ 2 levels "d","e": 1 2
# $ y: num  1 4

df <- data.frame(x = factor(c("d", "e")), y = c(1,4))
y = c(1,4)

str(df)

###############################################################################

# Crie a seguinte matriz
#
#       [,1] [,2] [,3]
# [1,]   19   22   25
# [2,]   20   23   26
# [3,]   21   24   27


vetor_1 <- c(19,22,25)

vetor_2 <- c(20,23,26)

vetor_3 <- c(21,24,27)

matriz <- matrix(c(vetor_1, vetor_2, vetor_3), byrow = TRUE, nrow = 3)

###############################################################################

# Se Z é uma matriz 4 por 4, qual é o resultado de Z[1,4] ?

# Resposta/Renato - É o elemento que está na primeira linha e na última (quarta) coluna da matriz

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
#  W3 W4 W1 W2 
#  20 69  5 88 

y <- c(20, 69, 5, 88)
q <- c("W3", "W4", "W1", "W2")
names(y) <- q
  
  ###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
#       [,1] [,2]
# [1,]    4    6
# [2,]    3    7
# [3,]    1    8


cbind(c(4, 3, 1), c(6, 7, 8))



###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#       [,1] [,2] [,3] [,4]
# [1,]    1    3   13   15
# [2,]    2    4   14   16

x <- 1:4
y <- 13:26

matrix(c(x, y[1:4]), nrow = 2, byrow = FALSE)

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

x <- c(17, 37, 12, 48, 19)

y <- c("A","B","C","D","E")

z <- c("Sep", "Jul", "Jun", "Feb", "Mar")

df <- data.frame(x, y, z)


# Ainda utilizando o dataframe df,
# qual código produziria o seguinte resultado?
#
#    x  y
# 1 17  A
# 2 37  B
# 3 12  C

df[1:3,1:2]

###############################################################################

# Responder o exercício teórico abaixo

#EXERCÍCIO TEÓRICO

#Q1) Elaborar uma explicação causal teórica.
#Resposta/Renato: 

#Pergunta de pesquisa -> A securitização dos setores cibernéticos dos Estados explica o nível de comprometimento com segurança cibernética dos mesmos?

#Faz-se mister uma conceituação breve de securitização para melhor elucidar qual o seu papel. Uma das principais contribuições da Escola de Copenhague, que é representada por nomes como Barry Buzan, Ole Waever e Jaap de Wilde (ACÁCIO, 2019), para os estudos de Segurança Internacional foi entender as questões de segurança como essencialmente discursivas, ou seja, a securitização de um setor dá-se uma vez que há a construção discursiva daquele setor como algo que representa ameaça existencial para os Estados (TANNO, 2003). O comprometimento com segurança cibernética é resultante da pontuação dos Estados em um índice baseado em 25 indicadores, o Índice Global de Segurança Cibernética.

#Q2) Elaborar hipóteses.
#Resposta/Renato: 

#Hipótese nula: Não há relação entre a variável independente (securitização) e a variável dependente (comprometimento com segurança cibernética).

#Hipótese alternativa: Há relação entre a variável independente (securitização) e a variável dependente (comprometimento com segurança cibernética).

#Q3) Pensar em como operacionalizar os conceitos teóricos em variáveis empíricas.
#Resposta/Renato: 

#Como observado em Lopes (2014), o conceito de securitização, variável independente, pode ser operacionalizado a partir de três categorias: a) não politizado; b) politizado; c) securitizado. Dessa forma, os casos analisados, que são os Estados, serão classificados a partir desses três grupos. 
#Já para a variável dependente, comprometimento com segurança cibernética, será utilizado o Índice Global de Segurança Cibernética (GCI), que, por sua vez, é baseado em 25 indicadores divididos entre os pilares: a) Legal; b) Técnico; c) Organizacional; d) Capacitação; e) Cooperação. De acordo com o GCI, o comprometimento dos Estados apresenta valores entre 0 e 1. O Brasil, por exemplo, pontuou 0,577 no GCI em 2018, ficando em 70º lugar no ranque global (ITU, 2019).

#Q4) Estabelecer o tipo de relação entre as variáveis operacionalizadas.
#Resposta/Renato:

#Consistindo a hipótese alternativa na ideia de que há relação entre securitização e comprometimento com segurança cibernética, a ponderação que pode ser feita visando o estabelecimento do tipo de relação entre a variável independente, securitização, e a variável dependente, comprometimento com segurança cibernética, é que espera-se, a partir do referencial teórico (LOPES, 2014; ACÁCIO, 2012), que a securitização afete positivamente o comprometimento dos Estados com segurança cibernética, de maneira que espera-se que Estados em que haja a securitização do setor cibernético apresentem um maior desempenho no GCI.

#REFERÊNCIAS

#ACÁCIO, Igor Daniel Palhares. Segurança cibernética: estudo de caso sobre a política de defesa brasileira. Orientador: Marcial Alécio Garcia Suarez. Trabalho de Conclusão de Curso (Bacharelado em Relações Internacionais) - Universidade Federal Fluminense, Rio de Janeiro, 2012.
#ITU. Global Cybersecurity Index (GCI). Studies & research. ITU Publications: Geneva, Switzerland. 2019.
#LOPES, Gills. Análise Exploratória da Securitização Militar do Ciberespaço nos EUA, Brasil e Canadá. Security and Defense Studies Review. Volume 15, p.116-138. 2014.
#TANNO, Grace. A Contribuição da Escola de Copenhague aos Estudos de Segurança Internacional. Contexto Internacional. Rio de Janeiro, vol. 25, nº 1, jan/jun. p.47-80. 2003.

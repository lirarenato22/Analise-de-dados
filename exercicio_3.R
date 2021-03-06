# Utilizando o banco world do pacote poliscidata, fa�a um  
# histograma que tamb�m indique a m�dia  e um boxplot 
# da vari�vel gini10
# Descreva o que voc� pode observar a partir deles.

library(tidyverse)

library(poliscidata)

banco <- world

ggplot(banco, aes(x = gini10)) +
geom_histogram() + 
geom_vline(aes(xintercept = mean(gini10, na.rm = T))) +
geom_boxplot()

?world

#Informa��o sobre a vari�vel gini10: Income Gini coefficient, 2000-2010 (UN)

# O histograma ilustra a frequ�ncia dos valores do 
# coeficiente de gini (2000-2010). Dos 167 casos, 14 n�o apresentaram
# informa��es para essa vari�vel. A m�dia foi em torno de 40, enquanto a
# mediana foi sutilmente menor do que a m�dia, em torno de 37, n�o 
# coincidindo com a mesma no boxplot (pelo contr�rio, a mediana tende a
# se direcionar para o primeiro quartil, indicando assimetria positiva na 
# distribui��o dos dados). O boxplot representa o lugar na
# distribui��o onde est�o 50% dos casos, ou seja, entre os valores 34 e 46
# no coeficiente de gini. Uma parte consider�vel dos casos est� abaixo da
# m�dia. O valor mais frequente na distribui��o foi em torno de 35 no 
# coeficiente de gini, havendo 15 observa��es para o mesmo. No entanto, 
# pode haver valores bem distantes da m�dia, sem ao menos constar 
#no boxplot, como � o caso que pontua quase 75 no coeficiente de gini.Em 
# situa��es como essas, a mediana � a medida de tend�ncia mais "confi�vel",
# j� que a m�dia normalmente � influenciada pelos outliers.





# Utilizando as fun��es de manipula��o de dados da aula passada,
# fa�a uma tabela que sumarize a media (fun��o mean), 
# mediana (funcao median) e o desvio padr�o (fundao sd) da 
# renda per capta (vari�vel gdppcap08), agrupada por tipo de regime 
# (vari�vel democ).
# Explique a diferen�a entre valores das m�dias e medianas.
# Ilustre com a explica��o com gr�fico de boxplot.
# Os dados corroboram a hip�tese da rela��o entre democracia
# e desempenho economico?

tabela_01 <- banco %>% group_by(democ) %>%
  summarise(media = mean(gdppcap08, na.rm = TRUE), 
            mediana = median(gdppcap08, na.rm = TRUE), 
            desvio_padrao = sd(gdppcap08, na.rm = TRUE))

#Informa��o sobre a vari�vel gdppcap08: GDP per capita in 2008 (World Bank)

# A m�dia, ou m�dia aritm�tica simples, de um conjunto �
# o resultado da soma das informa��es dividida pela quantidade de 
# informa��es que foram somadas. J� a mediana, em um conjunto num�rico
# crescente ou decrescente, representa o n�mero que ocupa o valor central
# no conjunto.
# A m�dia do PIB per capita em 2008 para democracias foi de 16.351,178,
# enquanto para casos considerados como n�o-democr�ticos a m�dia foi de 
# 9.243,377. A mediana do PIB per capita em 2008 para democracias foi de 
# 11.660, enquanto para casos considerados como n�o-democr�ticos a mediana
# foi de 4.388. A diferen�a entre os valores das m�dias e das medianas ocorre
# principalmente devido � assimetria na distribui��o dos casos, tanto para
# os pa�ses n�o-democr�ticos quanto para os democr�ticos, uma vez que nos
# dois grupos h� casos com valores muito mais altos (PIB maior) que os
# outros, o que faz com que a m�dia apresente um valor superdimensionado
# em rela��o � mediana, sendo essa �ltima a melhor medida de tend�ncia
# central para analisar casos assim.

ggplot(banco, aes(x = democ, y = gdppcap08)) +
  geom_boxplot()

# Os dados sobre PIB e forma de governo (democr�tico ou n�o-democr�tico),
# principalmente com a explana��o atrav�s do boxplot, podem influenciar
# o pesquisador a corroborar a hip�tese de que h� rela��o entre democracia
# e desempenho econ�mico, o que consistiria numa extrapola��o da explica��o,
# uma vez que n�o foram consideradas na an�lise todas as vari�veis que 
# podem estar relacionadas a essas duas e nem foram esgotadas as explica��es
# alternativas de como essas duas vari�veis podem se relacionar. Os dados
# esmiu�ados no boxplot ignoram um mundo de possibilidades de explica��o e 
# de vari�veis que n�o foram contabilizadas para ent�o serem controladas de
# forma a s� observar a poss�vel rela��o entre democracia e desempenho 
# econ�mico. Portanto, Os dados n�o corroboram a hip�tese da rela��o 
# entre democracia e desempenho economico.



# Carregue o banco states que est� no pacote poliscidata 
# Mantenha apenas as vari�veis obama2012, conpct_m, hs_or_more,
# prcapinc, blkpct10, south, religiosity3, state

banco_02 <- states

banco_02_selecionado <- banco_02 %>%
  select(obama2012, conpct_m, hs_or_more, prcapinc, blkpct10, 
         south, religiosity3, state)

# Carregue o banco nes que est� no pacote poliscidata
# Mantenha apenas as vari�veis obama_vote, ftgr_cons, dem_educ3,
# income5, black, south, relig_imp, sample_state

banco_03 <- nes

banco_03_selecionado <- banco_03 %>%
  select(obama_vote, ftgr_cons, dem_educ3, income5, black, 
         south, relig_imp, sample_state)


# As vari�veis medem os mesmos conceitos, voto no obama em 2012, 
# conservadorismo, educa��o, renda, cor, norte-sul, 
# religiosidade e estado. A diferen�a � que o nes � um banco de
# dados com surveys individuais e o states � um banco de dados
# com informa��es por estado
#
# Fa�a um gr�fico para cada banco representando o n�vel de
# conservadorismo. Comente as conclus�es que podemos ter
# a partir deles sobre o perfil do eleitorado estadunidense.
# Para ajudar, voc�s podem ter mais informa��es sobre os bancos
# de dados digitando ?states e ?nes, para ter uma descri��o das
# vari�veis

?states
#conpct_m = Percent mass public Conservative

ggplot(banco_02, aes(conpct_m)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = mean(conpct_m, na.rm = T))) +
  geom_boxplot()

?nes
#ftgr_cons = Feeling thermometer: Conservatives

ggplot(banco_03, aes(ftgr_cons)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = mean(ftgr_cons, na.rm = T)))

# Aparentemente, observando apenas esses dois gr�ficos referentes
# aos bancos states e nes, o eleitorado estadunidense tem uma parcela
# consider�vel de conservadores. H� uma leve diferen�a entre os resultados
# dos bancos, uma vez que o gr�fico baseado no banco states apresenta 
# m�dia e mediana pr�ximas de 35 (havendo uma ligeira diferen�a entre o
# valor das duas), enquanto o gr�fico baseado no banco nes apresenta m�dia
# pr�xima de 50 (sendo necess�rio observar se os respondentes do survey
# foram devidamente selecionados respeitando os estratos dos estados). 
# Al�m disso, no survey, houveu uma grande quantidade de "n�o-respostas"
# e tamb�m deve ser considerada a tend�ncia dos indiv�duos a "fugir dos
# extremos.
# De toda maneira, os dois gr�ficos de ambos os bancos indicam uma 
# expressiva exist�ncia de eleitores estadunidenses conservadores.


# Qual � o tipo de gr�fico apropriado para descrever a vari�vel
# de voto em obama nos dois bancos de dados?
# Justifique e elabore os gr�ficos

?states
# obama2012 = Obama vote share in 2012

ggplot(banco_02, aes(obama2012)) +
  geom_histogram() + geom_vline(aes(xintercept = mean(obama2012, na.rm = T)))


?nes
# obama_vote = Respondent vote for Obama?

install.packages(scales)

library(scales)

ggplot(banco_03, aes(obama_vote, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

# O gr�fico mais adequado para descrever a vari�vel de voto em Obama
# a partir do banco states � o histograma, com a observa��o da m�dia, 
# considerando que trata-se de uma vari�vel num�rica. J� o gr�fico mais
# adequado para descrever a vari�vel de voto em Obama a partir do banco
# nes � o gr�fico de barras, uma vez que ele � indicado para explorar
# vari�veis categ�ricas.




# Crie dois bancos de dados a partir do banco nes, um apenas com
# respondentes negros e outro com n�o-negros. A partir disso, fa�a
# dois gr�ficos com a propor��o de votos no obama.
# O que voc� pode afirmar a partir dos gr�ficos?
# Voc� diria que existe uma rela��o entre voto em Obama e cor?

banco_03_black <- banco_03_selecionado %>%
  mutate(black = recode(black, Yes = "negro", 
                        No = "n�o-negro"))

banco_03_black_negro <- banco_03_black %>%
  filter(black == "negro")

banco_03_black_n�o_negro <- banco_03_black %>%
  filter((black == "n�o-negro"))



ggplot(banco_03_black_negro, aes(obama_vote, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

ggplot(banco_03_black_n�o_negro, aes(obama_vote, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

# A propor��o dos votos em Obama entre os n�o-negros � em torno de 50%,
# estando equilibrada entre o sim (1) e o n�o (0).
# J� a propor��o dos votos em Obama entre os negros � de quase 100% para
# sim (1), havendo uma porcentagem pequen�ssima de votos n�o (0).
# Dessa forma, mesmo sem estar amparado em quantidades maiores de dados,
# o que poderia afetar a an�lise, arrisco afirmar que os dados tendem a
# indicar fortemente uma rela��o entre voto em Obama e cor.




# A partir do banco de dados states, fa�a uma compara��o semelhante.
# Fa�a um gr�fico com as porcentagens de votos em Obama para estados
# que est�o acima da mediana da porcentagem de popula��o negra nos estados,
# e outro gr�fico com as porcentagens de votos em Obama para os estados
# que est�o abaixo da mediana de popula��o negra.
# O que voc� pode afirmar a partir dos gr�ficos?
# Podemos chegar a mesma conclus�o anterior?

?states
#blkpct10 = Percent black (2010)

tabela_02 <- banco_02_selecionado %>%
  summarise(
  mediana = median(blkpct10, na.rm = TRUE))

banco_02_filtrado_acima_mediana <- banco_02_selecionado %>%
  filter(blkpct10 > 8.25)

banco_02_filtrado_abaixo_mediana <- banco_02_selecionado %>%
  filter(blkpct10 < 8.25)

ggplot(banco_02_filtrado_acima_mediana, aes(obama2012)) +
  geom_histogram() + geom_vline(aes(xintercept = mean(obama2012, na.rm = T)))

ggplot(banco_02_filtrado_abaixo_mediana, aes(obama2012)) +
  geom_histogram() + geom_vline(aes(xintercept = mean(obama2012, na.rm = T)))

# Mesmo com distribui��es um pouco diferentes entre si, os dois gr�ficos
# apresentam m�dias semelhantes. Dessa forma, � poss�vel notar que os
# Estados com porcentagem de popula��o negra acima da mediana obtiveram, 
# em m�dia, os mesmos resultados dos Estados com porcentagem de popula��o
# negra abaixo da mediana. Tal achado denota a import�ncia de sempre
# recorrermos a uma quantidade maior de informa��es, uma vez que, 
# mesmo tendo sido influenciado anteriormente a indicar uma rela��o entre
# voto em Obama e cor no banco nes, com base no banco states, e nas
# informa��es dos percentuais de popula��o negra dos Estados, pude 
# observar fortes ind�cios de que, no geral, n�o h� rela��o entre
# voto em Obama e cor.

# A partir da var�avel X do banco df abaixo
df <- data.frame(x = cos(seq(-50,50,0.5)))
# Fa�a os tipos de gr�ficos que representem esse tipo de vari�vel
# comentando as diferen�as entre elas e qual seria a mais adequada

ggplot(df, aes(x)) +
  geom_histogram() + geom_vline(aes(xintercept = mean(x, na.rm = T)))

ggplot(df, aes(x)) +
  geom_density()

ggplot(df, aes(x)) +
  geom_histogram() + geom_vline(aes(xintercept = mean(x, na.rm = T))) + geom_boxplot()

ggplot(df, aes(x = x)) + 
  geom_boxplot()

ggplot(df, aes(x = x, y = "")) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

install.packages("ggbeeswarm")

library("ggbeeswarm")

ggplot(df, aes("",x)) +
  geom_beeswarm()

ggplot(df, aes("",x)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_beeswarm()

# O histograma � uma forma de observar a distribui��o de dados
# de vari�veis cont�nuas. Outra forma v�lida � a curva de densidade.
# A diferen�a entre elas � que a segunda apresenta uma linha curva que
# descreve e representa a distribui��o dos dados para os valores. O 
# boxplot tamb�m � uma forma relevante e consideravelmente mais 
# informativa para que observemos a varia��o de dados cont�nuos. Ele
# nos informa os valores da mediana, do terceiro quartil, 
# do primeiro quartil e os valores m�ximos e m�nimos, no entanto,
# falta uma melhor explana��o sobre como os dados s�o distribu�dos, 
# que pode ser observada a partir do violinplot. O violinplot junta ao 
# mesmo tempo as informa��es estilo boxplot e uma curva de distribui��o
# dos dados com os valores.Por �ltimo, � v�lido ressaltar a import�ncia
# do gr�fico de pontos, uma vez que o mesmo mostra a distribui��o dos
# dados a partir da concentra��o dos pontos nos valores correlatos.
# No meu entendimento, a mais adequada � o violinplot com o gr�fico de
# pontos, j� que associa as informa��es de como os dados est�o 
# distribu�dos (violinplot) associando-os � concentra��o dos pontos
# nos valores correlatos (gr�fico de pontos).

# responsa as quest�es te�ricas abaixo
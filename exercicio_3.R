# Utilizando o banco world do pacote poliscidata, faça um  
# histograma que também indique a média  e um boxplot 
# da variável gini10
# Descreva o que você pode observar a partir deles.

library(tidyverse)

library(poliscidata)

banco <- world

ggplot(banco, aes(x = gini10)) +
geom_histogram() + 
geom_vline(aes(xintercept = mean(gini10, na.rm = T))) +
geom_boxplot()

?world

#Informação sobre a variável gini10: Income Gini coefficient, 2000-2010 (UN)

# O histograma ilustra a frequência dos valores do 
# coeficiente de gini (2000-2010). Dos 167 casos, 14 não apresentaram
# informações para essa variável. A média foi em torno de 40, enquanto a
# mediana foi sutilmente menor do que a média, em torno de 37, não 
# coincidindo com a mesma no boxplot (pelo contrário, a mediana tende a
# se direcionar para o primeiro quartil, indicando assimetria positiva na 
# distribuição dos dados). O boxplot representa o lugar na
# distribuição onde estão 50% dos casos, ou seja, entre os valores 34 e 46
# no coeficiente de gini. Uma parte considerável dos casos está abaixo da
# média. O valor mais frequente na distribuição foi em torno de 35 no 
# coeficiente de gini, havendo 15 observações para o mesmo. No entanto, 
# pode haver valores bem distantes da média, sem ao menos constar 
#no boxplot, como é o caso que pontua quase 75 no coeficiente de gini.Em 
# situações como essas, a mediana é a medida de tendência mais "confiável",
# já que a média normalmente é influenciada pelos outliers.





# Utilizando as funções de manipulação de dados da aula passada,
# faça uma tabela que sumarize a media (função mean), 
# mediana (funcao median) e o desvio padrão (fundao sd) da 
# renda per capta (variável gdppcap08), agrupada por tipo de regime 
# (variável democ).
# Explique a diferença entre valores das médias e medianas.
# Ilustre com a explicação com gráfico de boxplot.
# Os dados corroboram a hipótese da relação entre democracia
# e desempenho economico?

tabela_01 <- banco %>% group_by(democ) %>%
  summarise(media = mean(gdppcap08, na.rm = TRUE), 
            mediana = median(gdppcap08, na.rm = TRUE), 
            desvio_padrao = sd(gdppcap08, na.rm = TRUE))

#Informação sobre a variável gdppcap08: GDP per capita in 2008 (World Bank)

# A média, ou média aritmética simples, de um conjunto é
# o resultado da soma das informações dividida pela quantidade de 
# informações que foram somadas. Já a mediana, em um conjunto numérico
# crescente ou decrescente, representa o número que ocupa o valor central
# no conjunto.
# A média do PIB per capita em 2008 para democracias foi de 16.351,178,
# enquanto para casos considerados como não-democráticos a média foi de 
# 9.243,377. A mediana do PIB per capita em 2008 para democracias foi de 
# 11.660, enquanto para casos considerados como não-democráticos a mediana
# foi de 4.388. A diferença entre os valores das médias e das medianas ocorre
# principalmente devido à assimetria na distribuição dos casos, tanto para
# os países não-democráticos quanto para os democráticos, uma vez que nos
# dois grupos há casos com valores muito mais altos (PIB maior) que os
# outros, o que faz com que a média apresente um valor superdimensionado
# em relação à mediana, sendo essa última a melhor medida de tendência
# central para analisar casos assim.

ggplot(banco, aes(x = democ, y = gdppcap08)) +
  geom_boxplot()

# Os dados sobre PIB e forma de governo (democrático ou não-democrático),
# principalmente com a explanação através do boxplot, podem influenciar
# o pesquisador a corroborar a hipótese de que há relação entre democracia
# e desempenho econômico, o que consistiria numa extrapolação da explicação,
# uma vez que não foram consideradas na análise todas as variáveis que 
# podem estar relacionadas a essas duas e nem foram esgotadas as explicações
# alternativas de como essas duas variáveis podem se relacionar. Os dados
# esmiuçados no boxplot ignoram um mundo de possibilidades de explicação e 
# de variáveis que não foram contabilizadas para então serem controladas de
# forma a só observar a possível relação entre democracia e desempenho 
# econômico. Portanto, Os dados não corroboram a hipótese da relação 
# entre democracia e desempenho economico.



# Carregue o banco states que está no pacote poliscidata 
# Mantenha apenas as variáveis obama2012, conpct_m, hs_or_more,
# prcapinc, blkpct10, south, religiosity3, state

banco_02 <- states

banco_02_selecionado <- banco_02 %>%
  select(obama2012, conpct_m, hs_or_more, prcapinc, blkpct10, 
         south, religiosity3, state)

# Carregue o banco nes que está no pacote poliscidata
# Mantenha apenas as variáveis obama_vote, ftgr_cons, dem_educ3,
# income5, black, south, relig_imp, sample_state

banco_03 <- nes

banco_03_selecionado <- banco_03 %>%
  select(obama_vote, ftgr_cons, dem_educ3, income5, black, 
         south, relig_imp, sample_state)


# As variáveis medem os mesmos conceitos, voto no obama em 2012, 
# conservadorismo, educação, renda, cor, norte-sul, 
# religiosidade e estado. A diferença é que o nes é um banco de
# dados com surveys individuais e o states é um banco de dados
# com informações por estado
#
# Faça um gráfico para cada banco representando o nível de
# conservadorismo. Comente as conclusões que podemos ter
# a partir deles sobre o perfil do eleitorado estadunidense.
# Para ajudar, vocês podem ter mais informações sobre os bancos
# de dados digitando ?states e ?nes, para ter uma descrição das
# variáveis

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

# Aparentemente, observando apenas esses dois gráficos referentes
# aos bancos states e nes, o eleitorado estadunidense tem uma parcela
# considerável de conservadores. Há uma leve diferença entre os resultados
# dos bancos, uma vez que o gráfico baseado no banco states apresenta 
# média e mediana próximas de 35 (havendo uma ligeira diferença entre o
# valor das duas), enquanto o gráfico baseado no banco nes apresenta média
# próxima de 50 (sendo necessário observar se os respondentes do survey
# foram devidamente selecionados respeitando os estratos dos estados). 
# Além disso, no survey, houveu uma grande quantidade de "não-respostas"
# e também deve ser considerada a tendência dos indivíduos a "fugir dos
# extremos.
# De toda maneira, os dois gráficos de ambos os bancos indicam uma 
# expressiva existência de eleitores estadunidenses conservadores.


# Qual é o tipo de gráfico apropriado para descrever a variável
# de voto em obama nos dois bancos de dados?
# Justifique e elabore os gráficos

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

# O gráfico mais adequado para descrever a variável de voto em Obama
# a partir do banco states é o histograma, com a observação da média, 
# considerando que trata-se de uma variável numérica. Já o gráfico mais
# adequado para descrever a variável de voto em Obama a partir do banco
# nes é o gráfico de barras, uma vez que ele é indicado para explorar
# variáveis categóricas.




# Crie dois bancos de dados a partir do banco nes, um apenas com
# respondentes negros e outro com não-negros. A partir disso, faça
# dois gráficos com a proporção de votos no obama.
# O que você pode afirmar a partir dos gráficos?
# Você diria que existe uma relação entre voto em Obama e cor?

banco_03_black <- banco_03_selecionado %>%
  mutate(black = recode(black, Yes = "negro", 
                        No = "não-negro"))

banco_03_black_negro <- banco_03_black %>%
  filter(black == "negro")

banco_03_black_não_negro <- banco_03_black %>%
  filter((black == "não-negro"))



ggplot(banco_03_black_negro, aes(obama_vote, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

ggplot(banco_03_black_não_negro, aes(obama_vote, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

# A proporção dos votos em Obama entre os não-negros é em torno de 50%,
# estando equilibrada entre o sim (1) e o não (0).
# Já a proporção dos votos em Obama entre os negros é de quase 100% para
# sim (1), havendo uma porcentagem pequeníssima de votos não (0).
# Dessa forma, mesmo sem estar amparado em quantidades maiores de dados,
# o que poderia afetar a análise, arrisco afirmar que os dados tendem a
# indicar fortemente uma relação entre voto em Obama e cor.




# A partir do banco de dados states, faça uma comparação semelhante.
# Faça um gráfico com as porcentagens de votos em Obama para estados
# que estão acima da mediana da porcentagem de população negra nos estados,
# e outro gráfico com as porcentagens de votos em Obama para os estados
# que estão abaixo da mediana de população negra.
# O que você pode afirmar a partir dos gráficos?
# Podemos chegar a mesma conclusão anterior?

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

# Mesmo com distribuições um pouco diferentes entre si, os dois gráficos
# apresentam médias semelhantes. Dessa forma, é possível notar que os
# Estados com porcentagem de população negra acima da mediana obtiveram, 
# em média, os mesmos resultados dos Estados com porcentagem de população
# negra abaixo da mediana. Tal achado denota a importância de sempre
# recorrermos a uma quantidade maior de informações, uma vez que, 
# mesmo tendo sido influenciado anteriormente a indicar uma relação entre
# voto em Obama e cor no banco nes, com base no banco states, e nas
# informações dos percentuais de população negra dos Estados, pude 
# observar fortes indícios de que, no geral, não há relação entre
# voto em Obama e cor.

# A partir da varíavel X do banco df abaixo
df <- data.frame(x = cos(seq(-50,50,0.5)))
# Faça os tipos de gráficos que representem esse tipo de variável
# comentando as diferenças entre elas e qual seria a mais adequada

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

# O histograma é uma forma de observar a distribuição de dados
# de variáveis contínuas. Outra forma válida é a curva de densidade.
# A diferença entre elas é que a segunda apresenta uma linha curva que
# descreve e representa a distribuição dos dados para os valores. O 
# boxplot também é uma forma relevante e consideravelmente mais 
# informativa para que observemos a variação de dados contínuos. Ele
# nos informa os valores da mediana, do terceiro quartil, 
# do primeiro quartil e os valores máximos e mínimos, no entanto,
# falta uma melhor explanação sobre como os dados são distribuídos, 
# que pode ser observada a partir do violinplot. O violinplot junta ao 
# mesmo tempo as informações estilo boxplot e uma curva de distribuição
# dos dados com os valores.Por último, é válido ressaltar a importância
# do gráfico de pontos, uma vez que o mesmo mostra a distribuição dos
# dados a partir da concentração dos pontos nos valores correlatos.
# No meu entendimento, a mais adequada é o violinplot com o gráfico de
# pontos, já que associa as informações de como os dados estão 
# distribuídos (violinplot) associando-os à concentração dos pontos
# nos valores correlatos (gráfico de pontos).

# responsa as questões teóricas abaixo
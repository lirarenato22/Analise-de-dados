
## Fa�a todos os gr�ficos utilizando um tema que voc� ache mais adequado
## e nomeie os eixos x e y da maneira adequada

## Carregue o banco world do pacote poliscidata

library(poliscidata)

library(tidyverse)

banco <- world %>% filter(!is.na(democ_regime08),
                          !is.na(muslim), !is.na(gdppcap08))

## Observe o banco de dados com as fun��es adequadas

glimpse(world)

?world

str(banco)

head(banco)

tail(banco)

summary(banco)

## A vari�vel democ_regime08 indica se um pa�s � democr�tico.
## Usando as ferramentas de manipulacao de bancos de dados, verifique
## quantos paises sao democraticos ou nao, e apresente esta vari�vel 
## graficamente

banco %>% count(democ_regime08)

install.packages(scales)
library(scales)

ggplot(banco, aes(democ_regime08, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

ggplot(banco, aes(democ_regime08)) +
  geom_bar()

## Teste a rela��o entre a vari�vel democ_regime08 e a vari�vel
## muslim (que indica se um pa�s � mu�ulmano ou n�o). E represente
## visualmente as vari�veis para visualizar se esta religi�o
## aumenta ou diminui a chance de um pa�s ser democr�tico

## Qual seria sua conclus�o com rela��o a associa��o destas duas
## vari�veis?

 banco_recodificado <- banco %>%
  mutate(democ_regime08  = recode(democ_regime08,
                            Yes = "Democr�tico",
                            No = "N�o Democr�tico",
                            ))

tabela_grafico_01 <- table(banco_recodificado$democ_regime08, banco_recodificado$muslim)

chisq.test(tabela_grafico_01)


library(grid)

library(vcd)

assoc(tabela_grafico_01, shade = TRUE)

ggplot(banco_recodificado, aes(democ_regime08, fill = muslim)) +
  geom_bar(position = "fill")

ggplot(banco_recodificado, aes(x = democ_regime08, y = muslim)) + 
  geom_count(aes(group = democ_regime08, size = after_stat(prop))) +
  scale_size_area(max_size = 10)

# As vari�veis democ_regime08 e muslim,
# testadas a partir do teste do qui-quadrado, apresentam grande
# probabilidade de estarem associadas, uma vez que o X-squared � 
# relativamente alto e o p-valor � baixo. Dessa forma, demonstra-se 
# a signific�ncia estat�stica. Considerando esses dados, por�m, 
# n�o h� ainda base suficiente para defender causalidade nessa
# associa��o. O que se pode afirmar, em resumo, � que h� uma rela��o
# negativa entre religi�o e regime pol�tica, indicando que os pa�ses
# isl�micos est�o, em parte consider�vel dos casos, associados a 
# regimes n�o democr�ticos.




## A vari�vel gdppcap08 possui informa��o sobre o PIB per capta
## dos pa�ses. Fa�a uma representa��o gr�fica desta vari�vel

ggplot(banco, aes(x = gdppcap08)) +
  geom_boxplot()




## Fa�a um sumario com a m�dia, mediana e desvio padr�o do pib per capta
## para cada tipo de regime politico, represente a associa��o destas
## vari�veis graficamente, e fa�a o teste estat�stico adequado para
## chegar a uma conclus�o. 
#Existe associa��o entre as vari�veis?

tabela_banco_01 <- banco_recodificado %>% group_by(democ_regime08) %>%
  summarise(media = mean(gdppcap08, na.rm = TRUE), 
            mediana = median(gdppcap08, na.rm = TRUE), 
            desvio_padrao = sd(gdppcap08, na.rm = TRUE))

ggplot(banco_recodificado, aes(gdppcap08, fill = democ_regime08)) +
  geom_density(alpha = 0.3)

ggplot(banco_recodificado, aes(democ_regime08, gdppcap08)) +
  geom_boxplot()

ggplot(banco_recodificado, aes(democ_regime08, gdppcap08)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 

t.test(gdppcap08 ~ democ_regime08, data = banco_recodificado)


# Atrav�s do Teste T, podemos observar que as vari�veis gdppcap08
# e democ_regime08 est�o associadas, uma vez que o intervalo de 
# confian�a para a diferen�a entre as m�dias n�o inclui o valor 0.
# A partir disso, podemos concluir que h� diferen�a com signific�ncia
# estat�stica entre as m�dias de PIB per capita em pa�ses democr�ticos
# e pa�ses n�o democr�ticos. Em suma, pa�ses democr�ticos apresentam, 
# em m�dia, valores de PIB per capita consideravelmente maiores que
# pa�ses n�o democr�ticos.




## Por fim, ao inv�s de utilizar uma vari�vel bin�ria de democracia,
## utilize a vari�vel dem_score14 para avaliar se existe uma associa��o
## entre regime pol�tico e desenvolvimento econ�mico. Represente
## a associa��o graficamente, fa�a o teste estat�stico e explica sua
## conclus�o

ggplot(banco_recodificado, aes(dem_score14, gdppcap08)) +
  geom_jitter()

cor.test(banco_recodificado$dem_score14, banco_recodificado$gdppcap08)

# A partir do Teste r de Pearson, podemos observar a exist�ncia de 
# correla��o entre as vari�veis dem_score14 e gdppcap08, que representam
# regime pol�tico e desenvolvimento econ�mico, respectivamente. Dessa 
# maneira, considerando o p-valor baixo e tamb�m que o intervalo de 
# confian�a da correla��o n�o inclui o valor 0, podemos concluir que 
# a correla��o entre regime pol�tico e desenvolvimento econ�mico possui
# signific�ncia estat�stica.



## Teste a associa��o entre renda perca capta e religiao (com a vari�vel
## muslim) e represente graficamente. Qual � sua conclus�o? 

t.test(gdppcap08 ~ muslim, data = banco_recodificado)

ggplot(banco_recodificado, aes(muslim, gdppcap08)) +
  geom_boxplot()

ggplot(banco_recodificado, aes(muslim, gdppcap08)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

ggplot(banco_recodificado, aes(gdppcap08, fill = muslim)) +
  geom_density(alpha = 0.3) +
  labs(title = "Gr�fico - Densidade",
       subtitle = "Desenvolvimento Econ�mico x Religi�o (Isl�)",
       x = "Renda Per Capita",
       y = "Distribui��o dos casos")

# Atrav�s do Teste T, podemos observar uma associa��o entre as vari�veis
# gdppcap08 e muslim, representando Renda Per Capita e Religi�o,
# respectivamente. Como o intervalo de confian�a n�o inclui o valor 0,
# a associa��o entre as vari�veis � estatisticamente significante.
# O p-valor pode ser considerado baixo (para literatura que aceita 
# at� 0,05) ou n�o (para literatura que aceita at� 0,01), a depender do 
# referencial te�rico-metodol�gico. Em resumo, pa�ses isl�micos apresentam,
# em m�dia, PIB per capita consideravelmente mais baixos que pa�ses 
# n�o-isl�micos, o que demonstra uma rela��o entre renda per capita e 
# religi�o.


## Comparando suas conclus�es anteriores, � poss�vel afirmar qual
## das duas vari�veis possui maior impacto no desenvolvimento economico?
## Por que? 

t.test(gdppcap08 ~ democ_regime08, data = banco_recodificado)

cor.test(banco_recodificado$dem_score14, banco_recodificado$gdppcap08)

t.test(gdppcap08 ~ muslim, data = banco_recodificado)

# Considerando os Testes T realizados entre as vari�veis categ�ricas 
# de regime pol�tico (democ_regime08) e religi�o (muslim) e a vari�vel
# cont�nua de desenvolvimento econ�mico (gdppcap08), podemos observar que
# a diferen�a entre as m�dias � maior quando se est� observando a 
# associa��o entre regime pol�tico e desenvolvimento. Al�m disso, o 
# p-valor do Teste T que avalia muslim e gdppcap08, em an�lises que 
# considerassem p-valor baixo < 0,01, n�o seria considerado baixo. 
# Como mencionado, o Teste r de Pearson, com duas vari�veis cont�nuas,
# tamb�m apontou correla��o entre regime pol�tico e desenvolvimento 
# econ�mico, de maneira que h� uma quantidade consider�vel de dados 
# indicando a associa��o entre regime pol�tico e desenvolvimento econ�mico,
# enquanto o resultado da an�lise da associa��o entre religi�o e renda
# per capita depende do rigor em rela��o ao p-valor. Al�m disso,
# em an�lises mais detalhistas e profundas, deveria ser observado o quanto
# a vari�vel que mensura religi�o est� relacionada com a 
# as vari�veis que mensuram regime pol�tico, como foi feito em parte em 
# quest�o anterior, para, ent�o, saber se a associa��o de uma delas com
# desenvolvimento econ�micon�o seria tamb�m resultado da associa��o da 
# outra, de maneira que � dif�cil isolar os impactos das duas em rela��o
# ao desenvolvimento econ�mico e compar�-los sem observar essas quest�es.

##########################################################################

## Exerc�cio te�rico
## Levando em considera��o as vari�veis de seu trabalho final,
## qual dos 3 testes estat�sticos utilizados seria adequado utilizar?


## Faça todos os gráficos utilizando um tema que você ache mais adequado
## e nomeie os eixos x e y da maneira adequada

## Carregue o banco world do pacote poliscidata

library(poliscidata)

library(tidyverse)

banco <- world %>% filter(!is.na(democ_regime08),
                          !is.na(muslim), !is.na(gdppcap08))

## Observe o banco de dados com as funções adequadas

glimpse(world)

?world

str(banco)

head(banco)

tail(banco)

summary(banco)

## A variável democ_regime08 indica se um país é democrático.
## Usando as ferramentas de manipulacao de bancos de dados, verifique
## quantos paises sao democraticos ou nao, e apresente esta variável 
## graficamente

banco %>% count(democ_regime08)

install.packages(scales)
library(scales)

ggplot(banco, aes(democ_regime08, ..count../sum(..count..) )) +
  geom_bar(na.rm = T) +
  scale_y_continuous(labels = percent)

ggplot(banco, aes(democ_regime08)) +
  geom_bar()

## Teste a relação entre a variável democ_regime08 e a variável
## muslim (que indica se um país é muçulmano ou não). E represente
## visualmente as variáveis para visualizar se esta religião
## aumenta ou diminui a chance de um país ser democrático

## Qual seria sua conclusão com relação a associação destas duas
## variáveis?

 banco_recodificado <- banco %>%
  mutate(democ_regime08  = recode(democ_regime08,
                            Yes = "Democrático",
                            No = "Não Democrático",
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

# As variáveis democ_regime08 e muslim,
# testadas a partir do teste do qui-quadrado, apresentam grande
# probabilidade de estarem associadas, uma vez que o X-squared é 
# relativamente alto e o p-valor é baixo. Dessa forma, demonstra-se 
# a significância estatística. Considerando esses dados, porém, 
# não há ainda base suficiente para defender causalidade nessa
# associação. O que se pode afirmar, em resumo, é que há uma relação
# negativa entre religião e regime política, indicando que os países
# islâmicos estão, em parte considerável dos casos, associados a 
# regimes não democráticos.




## A variável gdppcap08 possui informação sobre o PIB per capta
## dos países. Faça uma representação gráfica desta variável

ggplot(banco, aes(x = gdppcap08)) +
  geom_boxplot()




## Faça um sumario com a média, mediana e desvio padrão do pib per capta
## para cada tipo de regime politico, represente a associação destas
## variáveis graficamente, e faça o teste estatístico adequado para
## chegar a uma conclusão. 
#Existe associaçào entre as variáveis?

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


# Através do Teste T, podemos observar que as variáveis gdppcap08
# e democ_regime08 estão associadas, uma vez que o intervalo de 
# confiança para a diferença entre as médias não inclui o valor 0.
# A partir disso, podemos concluir que há diferença com significância
# estatística entre as médias de PIB per capita em países democráticos
# e países não democráticos. Em suma, países democráticos apresentam, 
# em média, valores de PIB per capita consideravelmente maiores que
# países não democráticos.




## Por fim, ao invés de utilizar uma variável binária de democracia,
## utilize a variável dem_score14 para avaliar se existe uma associação
## entre regime político e desenvolvimento econômico. Represente
## a associação graficamente, faça o teste estatístico e explica sua
## conclusão

ggplot(banco_recodificado, aes(dem_score14, gdppcap08)) +
  geom_jitter()

cor.test(banco_recodificado$dem_score14, banco_recodificado$gdppcap08)

# A partir do Teste r de Pearson, podemos observar a existência de 
# correlação entre as variáveis dem_score14 e gdppcap08, que representam
# regime político e desenvolvimento econômico, respectivamente. Dessa 
# maneira, considerando o p-valor baixo e também que o intervalo de 
# confiança da correlação não inclui o valor 0, podemos concluir que 
# a correlação entre regime político e desenvolvimento econômico possui
# significância estatística.



## Teste a associação entre renda perca capta e religiao (com a variável
## muslim) e represente graficamente. Qual é sua conclusão? 

t.test(gdppcap08 ~ muslim, data = banco_recodificado)

ggplot(banco_recodificado, aes(muslim, gdppcap08)) +
  geom_boxplot()

ggplot(banco_recodificado, aes(muslim, gdppcap08)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))

ggplot(banco_recodificado, aes(gdppcap08, fill = muslim)) +
  geom_density(alpha = 0.3) +
  labs(title = "Gráfico - Densidade",
       subtitle = "Desenvolvimento Econômico x Religião (Islã)",
       x = "Renda Per Capita",
       y = "Distribuição dos casos")

# Através do Teste T, podemos observar uma associação entre as variáveis
# gdppcap08 e muslim, representando Renda Per Capita e Religião,
# respectivamente. Como o intervalo de confiança não inclui o valor 0,
# a associação entre as variáveis é estatisticamente significante.
# O p-valor pode ser considerado baixo (para literatura que aceita 
# até 0,05) ou não (para literatura que aceita até 0,01), a depender do 
# referencial teórico-metodológico. Em resumo, países islâmicos apresentam,
# em média, PIB per capita consideravelmente mais baixos que países 
# não-islâmicos, o que demonstra uma relação entre renda per capita e 
# religião.


## Comparando suas conclusões anteriores, é possível afirmar qual
## das duas variáveis possui maior impacto no desenvolvimento economico?
## Por que? 

t.test(gdppcap08 ~ democ_regime08, data = banco_recodificado)

cor.test(banco_recodificado$dem_score14, banco_recodificado$gdppcap08)

t.test(gdppcap08 ~ muslim, data = banco_recodificado)

# Considerando os Testes T realizados entre as variáveis categóricas 
# de regime político (democ_regime08) e religião (muslim) e a variável
# contínua de desenvolvimento econômico (gdppcap08), podemos observar que
# a diferença entre as médias é maior quando se está observando a 
# associação entre regime político e desenvolvimento. Além disso, o 
# p-valor do Teste T que avalia muslim e gdppcap08, em análises que 
# considerassem p-valor baixo < 0,01, não seria considerado baixo. 
# Como mencionado, o Teste r de Pearson, com duas variáveis contínuas,
# também apontou correlação entre regime político e desenvolvimento 
# econômico, de maneira que há uma quantidade considerável de dados 
# indicando a associação entre regime político e desenvolvimento econômico,
# enquanto o resultado da análise da associação entre religião e renda
# per capita depende do rigor em relação ao p-valor. Além disso,
# em análises mais detalhistas e profundas, deveria ser observado o quanto
# a variável que mensura religião está relacionada com a 
# as variáveis que mensuram regime político, como foi feito em parte em 
# questão anterior, para, então, saber se a associação de uma delas com
# desenvolvimento econômiconão seria também resultado da associação da 
# outra, de maneira que é difícil isolar os impactos das duas em relação
# ao desenvolvimento econômico e compará-los sem observar essas questões.

##########################################################################

## Exercício teórico
## Levando em consideração as variáveis de seu trabalho final,
## qual dos 3 testes estatísticos utilizados seria adequado utilizar?

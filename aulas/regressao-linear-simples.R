
# Regressão Linear Simples ------------------------------------------------

# R.L simples é o modelo de regressão que contém apenas uma única v. independente

# Vamos ter, então, uma v. independente, uma v. que queremos prever e uma v.
# dependente

# Carregando pacotes ------------------------------------------------------

library(pacman)

# Essa função faz o download dos pacotes que não temos e depois carrega eles
# (se já tivermos os pacotes baixados, essa função já carrega os pacotes)
pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr)

# Importando a base de dados ----------------------------------------------

dados <- read.csv2("dados/Banco de Dados 11.csv")

# Essa base contém informações de 200 CDs comercializados por uma gravadora. A
# ideia é verificar se o gasto em publicidade é capaz de prever a venda de CDs

glimpse(dados)

# Verificando os pressupostos para a regressão linear ---------------------

### Relação linear entre a VD e a VI ----
# VD: v. que queremos prever (venda)
# VI: v. que imaginamos que influencia a venda (publicidade)

plot(dados$Publicidade, dados$Vendas)

### Construção do modelo ----

mod <- lm(Vendas ~ Publicidade, data = dados)

### Análise gráfica ----

# Plota os quatro gráficos numa mesma imagem
par(mfrow = c(2,2)) # 2,2 = 2 linhas e 2 colunas
plot(mod)

# Interpretação: https://library.virginia.edu/data/articles/diagnostic-plots

# Vamos abaixo ver a interpertação de cada grafico. O link acima detalha mais
# essas interpretações

# Esses gráficos sõa muito importantes para compreender os pressupostos

#### Residuals vs Fitted ----

# O que é? 
# Gráfico dos resíduos pelos valores previstos

# Permite analisar a linearidade e a homocedasticidade

# Linearidade: verificarmos a lineralidade pela linha vermelha. Se ela estiver 
# aprox. horizontal, temos uma relação linear

# Homocedasticidade: homogeneidade de variâncias. Existe homocedasticidade se a 
# dispersão de pontos no gráfico é constante ao longo dos valores previstos (eixo x
# do gráfico, fitted values)

# Se não existisse homocedasticidade das variancia, teríamos heterocedasticidade

#### Normal Q-Q ----

# O que é?
# Permite ver se os residuos tem dist. normal

# Eixo y: os residuos padronizados
# Eixo x: os resuiduos teóricos (residuos esperados caso a dist. fosse normal)

# Tem dist. normal se os pontos estão em cima da reta do grafico

#### Scale-Location ----

# Gráfico mais recomendado para verificar a homocedasticidade
# Caso exista homocedasticidade, a linha vermelha deve ser horizontal

#### Residuals vs Leverage

# Permite pensar se existe residuos outliers
# E se existe pontos de alavangagem

# Pressuposto da reg. linear: não deve existir residuos outliers (mas até tudo 
# bem se tiver) 

# O que não pode acontecer é existir residuos outliers que são pontos influentes
# ou ponto de alavancagem (pontos que são tão outliers que estão influenciando
# a estimação)

# Existe outlier se existirem residuos abaixo de -3 ou acima de 3

# Existe pontos de alavancagem se existirem residuos padronizados fora da linha
# vermleha pontilhada. Além disso, o R vai colocar um numero de id em cima do 
# residuo

# Testes ------------------------------------------------------------------

par(mfrow = c(1,1)) # a partir de agora cada plot vait er apenas um grafico

### Normalidade dos residuos ----

shapiro.test(mod$residuals)

# H0: dist. = normal    -> p > 0.05
# H1: dist. != normal   -> p <= 0.05 

### Outliers nos resíduos ----

# Faz para cada um dos casos
rstandard(mod)

# Faz um resumo geral (mais interessante!)
summary(rstandard(mod))

### Independencia dos resíduos (Durbin-Watson) ----

# Existe uma literatura que vai dizer que satisfazer esse pressuposto é necessario
# somente se vamos fazer uma anailse longitudinal

durbinWatsonTest(mod)

# D-W Statistic: deve estar prox de 2 para existir independencia dos residuos
# tem que estar entre 1 e 3

# p-value: ta pensando na correlação entre os residuos

# H0: corr = 0
# H1: corr != 0

# p-value > 0.05: não rejeitar a hipótese nula
# p-value < 0.05: rejeitar a hipótese nula

# Para satisfazer esse pressuposto queremos p-value > 0.05

### Homocedasticidade (Breusch-Pagan) ----

bptest(mod)

# H0: existe homocedasticidade      ->   p > 0.05
# H1: NÃO existe homocedasticidade  ->   p <= 0.05

# Análise do modelo -------------------------------------------------------

summary(mod)

#### Intercept ----

# Valor de Y quando X é zero
# Interpretando: Mesmo que eu invista 0 em publicidade eu posso esperar uma 
# venda de CDs de 125

#### Publicidade (beta) ----

# Estimate: a cada 1 real gasto em publicidade temos um aumento de 0.10495 em
# vendas (em média)

# Pr(>|t|): baseado no teste t value
# - t value:
#   - H0: coef   = 0  ->  p > 0.05 (VI não influencia VD)
#   - H1: coef. != 0  ->  p <= 0.05  (VI influencia VD)

#### R-squared ----

# Adjusted R-squared é mais importante para reg. linear multipla

# R-squared
# Interpretamos como uma porcentagem
# 0.3634 significa 36%

# O investimento em publicade explica 36% das vendas

#### F-statistic ----

# Compara o modelo que criamos com um modelo nulo (sem previsor nenhum)

# 105.6 on 1 and 185 DF
# p-value: < 2.2e-16

# H0: modelo criado preve tao bem quanto o modelo nulo  -> p > 0.05
# H1: existe uma diferenca enrte esses modelos          -> p <= 0.05

# p < 0.05 existe diferença entre os modelos

# Gráfico de dispersão ----------------------------------------------------

# Com equação da reta

dados |> 
  ggplot() +
  # É padrão que a VD esteja no eixo X e a VI no eixo Y
  aes(x = Publicidade, y = Vendas) +
  geom_point() +
  # Plot a linha do modelo
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(
    label = paste(..eq.label.., ..adj.rr.label.., sep = "*plain(\",\")~~")
    ),
    label.x = 0, label.y = 400
    ) +
  theme_classic()

# O sombreado é o erro padrão

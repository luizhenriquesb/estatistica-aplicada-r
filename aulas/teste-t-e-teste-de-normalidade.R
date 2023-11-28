
library(tidyverse)

# Teste-t para uma amostra e teste de normalidade no R -----------------------

# Esse teste é feito quando temos um único grupo e queremos comparar a média 
# desse grupo a um valor de referência

# Pergunta a ser respondida: No banco de dados abaixo, temos a altura em cm de
# 30 pessoas. Sabendo que a altura média do brasileiro é de 1,67 m, verifique
# se a altura média da nossa amostra é diferente da altura média nacional

dados <- read.csv("dados/Banco de Dados 2.csv",
                  sep = ";",
                  dec = ",",
                  fileEncoding = "latin1")

glimpse(dados)

# Verificar a normalidade dos dados ----

dados |> # Fiz esse gráfico de densidade só para ver a distribuição
  ggplot() +
  aes(x = Altura) +
  geom_density()

shapiro.test(dados$Altura)

# p-value: interpretação ----

# Vamos interpretar o resultado do teste tendo em mente as hipóteses nula e 
# alternativa

# H0: distribuição dos dados = normal --> p > 0,05
# H1: distribuição dos dados != normal --> p <= 0,05

# Nosso teste retornou um p-value de 0.214, ou seja, dados$Altura segue uma
# distribuição normal

# Teste-t para uma amostra ----

t.test(dados$Altura, 
       mu = 167 # valor de referência
       )

# t: 
# df: grau de liberdade
# p-value = 0.4883

# H0: média da amostra = valor de referência --> p > 0,05
# H1: média da amostra != valor de referência --> p < 0,05

# Interpretação ----

# O teste-t para uma amostra mostrou que a nédia de altura da amostra não é
# diferente da média de altura nacional (t(29) = 0.702; p = 0.488)

# Observação ----

# O teste default do R é bicaudal (verifica se a média da amostra é maior ou 
# menor do valor de referência)

# Para fazer um teste unicaudal:

t.test(dados$Altura, mu = 167,
       alternative = "greater", # verifica se a média da amostra é maior
       #alternative = "less"    # verifica se a média da amostra é menor
       )

# Visualização gráfica: Box-plot ----

boxplot(dados$Altura, ylab = "Altura (cm)")












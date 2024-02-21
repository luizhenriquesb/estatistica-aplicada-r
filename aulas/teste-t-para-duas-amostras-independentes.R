 
# Teste-t para duas amostras independentes no R ---------------------------

# Com base no banco de dados abaixo, verifique se a posição que o aluno 
# ocupa na sala tem efeito sobre as suas notas em biologia, física e 
# história

# Para fazer um teste-t, é preciso se verificar se os dados temos a normalidade
# a homogeneidade de variâncias

# Pacotes ----
library(tidyverse)
library(RVAideMemoire)
library(car)

# Importando o banco de dados ----

dados <- read.csv("dados/Banco de Dados 3.csv",
                  sep = ";",
                  dec = ",")

dados <- dados |> 
  mutate(across(
    .cols = Genero:Posicao_Sala,
    .fns = factor
  ))

# Passo 1: Verificação da normalidade dos dados ----

           
byf.shapiro(
  Nota_Biol ~     # v. dependente e verifica a normalidade
    Posicao_Sala, # v. independente e agrupa
  dados)

byf.shapiro(Nota_Fis ~ Posicao_Sala, dados)
byf.shapiro(Nota_Hist ~ Posicao_Sala, dados)

### Interpretação ----

# Vamos pensar em termos de hipótese nula e alternativa

# H0: distribuição dos dados = normal  --> p > 0.05
# H1: distribuição dos dados != normal --> p <= 0.05

# Shapiro-Wilk normality tests
# 
# data:  Nota_Biol by Posicao_Sala 
# 
#             W p-value  
# Frente 0.9852 0.99312 
# Fundos 0.9003 0.06865 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# O p-value do grupo "frente" e grupo "fundo" deu maior que 0.05, entao a dist.
# é normal nos dois grupos

# Passo 2: Verificação da homogeneidade de variâncias ----

leveneTest(Nota_Fis        # v. dependente
           ~ Posicao_Sala, # v. independente
           dados,
           center = mean) # Sem definir o center, é feito com base na mediana

leveneTest(Nota_Fis ~ Posicao_Sala, dados, center = mean) 
leveneTest(Nota_Hist ~ Posicao_Sala, dados, center = mean) 

### Interpretação ----

# Levene's Test for Homogeneity of Variance (center = mean)
#       Df F value    Pr(>F)    
# group  1  13.658 0.0008749 ***
#       30                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Vamos pensar em termos de hipótese nula e alternativa

# H0: As variÂncias dps grupos são homogêneas      --> p > 0.05
# H1: As variÂncias dps grupos NÃO são homogêneas  --> p <= 0.05

# Passo 3: Realização do teste-t para amostras independentes ----

t.test(Nota_Biol ~ Posicao_Sala, dados, 
       var.test = TRUE # Acima, vimos que as variancias são homogeneas 
       )

t.test(Nota_Fis ~ Posicao_Sala, dados,
       var.test = FALSE # Acima, vimos que as variancias NÃO são homogeneas 
       )
t.test(Nota_Hist ~ Posicao_Sala, dados, 
       var.test = FALSE # Acima, vimos que as variancias NÃO são homogeneas 
       )

### Interpretação ----

# Welch Two Sample t-test
# 
# data:  Nota_Hist by Posicao_Sala
# t = 1.5737, df = 19.909, p-value = 0.1313
# alternative hypothesis: true difference in means between group Frente and group Fundos is not equal to 0
# 95 percent confidence interval:
#   -0.3860238  2.7546513
# sample estimates:
#   mean in group Frente mean in group Fundos 
# 5.466667             4.282353 

# Vamos pensar em termos de hipótese nula e alternativa

# H0: Média do grupo A = Média do grupo B   --> p > 0.05
# H1: Média do grupo A != Média do grupo B  --> p <= 0.05

# OBS.: Por default, o teste-t faz o teste bicaudal, para fazer unicaudal basta
# passar o argumento alternative = "less" ou "greater"

# Passo 4 (opcional): Visualização da distribuição dos dados

# Este código estabelece que os gráficos saiam na mesma linha
par(mfrow = c(1, # Uma única linha 
              3) # Três colunas
    )

boxplot(data = dados, 
        Nota_Biol ~ Posicao_Sala, 
        ylab = "Notas de Biologia",
        xlab = "Posição na Sala")

boxplot(data = dados, 
        Nota_Fis ~ Posicao_Sala, 
        ylab = "Notas de Física",
        xlab = "Posição na Sala")

boxplot(data = dados, 
        Nota_Hist ~ Posicao_Sala, 
        ylab = "Notas de História",
        xlab = "Posição na Sala")

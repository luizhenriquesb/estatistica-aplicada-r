# Regressão Logística Binária ---------------------------------------------

# Passo 1: Carregar os pacotes que serão usados ---------------------------

if (!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, psych, car, MASS, DescTools, QuantPsyc, ggplot2)

# Passo 2: Carregar o banco de dados --------------------------------------

dados <- read.csv2("datasets/Banco de Dados 13.csv",
  stringsAsFactors = TRUE,
  fileEncoding = "latin1"
)

# Passo 3: Análise das frequências das categorias da VD -------------------

table(dados$Cancer)

summary(dados)

# Passo 4: Checagem das categorias de referencia ---------------------------

levels(dados$Cancer) # Não = categoria de refer?ncia

levels(dados$Hab_Fumar) # Não = categoria de refer?ncia

# Passo 5: Checagem dos pressupostos ---------------------------------------

### 1. Variável dependente dicotomica (categorias mutuamente exclusivas)

### 2. Independencia das observações (sem medidas repetidas)
## Não estamos medindo o mesmo sujeito experimental ao longo do tempo

## Construção do modelo:

mod <- glm(Cancer ~ Estresse + Hab_Fumar,
           family = binomial(link = 'logit'), data = dados)

### 3. Ausência de outliers/ pontos de alavancagem

plot(mod, which = 5)

# Não pode ser menor que -3 nem maior que 3
summary(stdres(mod))

### 4. Ausência de multicolinearidade

# Problema: correlação entre duas ou mais VIs
pairs.panels(dados)
cor(dados)
# Existe multicolinearidade se r > 0.9 (ou 0.8)

vif(mod)
## Existe multicolinearidade se VIF > 10

### 5. Relação linear entre cada VI cont?nua e o logito da VD

# Teste Box-Tidwell
# A interação entre a VI contínua e o seu log não deve ser significativa
# Field, A. P., Miles, J., & Field, Z. (2012). Discovering statistics using R/Andy Field, Jeremy Miles, Zoë Field.

intlog <- dados$Estresse * log(dados$Estresse)

dados$intlog <- intlog

modint <- glm(Cancer ~ Hab_Fumar + Estresse + intlog,
              family = binomial(link = 'logit'), data = dados)

# OBS: Se tivessemos varias v. continuas, bastaria adicionar todas as interações
# no modelo acima (intelog1, intelog2, intelog3, ...). Não é necessario criar
# varios modelos

summary(modint)

### Outra opção:

# Cálculo do logito
logito <- mod$linear.predictors

dados$logito <- logito

# Outra opção para o calculo do logito:
# prob <- predict(mod, type = "response")
# logito <- log(prob/(1-prob))
# dados$logito <- logito

### Análise da relação linear

# Grafico do logito x VD
ggplot(dados, aes(logito, Estresse)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_classic()

# Pela analise visual do grafico acima, o pressuposto de linearidade nao foi
# atendido. Como o teste de Box-Tidwell mostrou que é linear a relação, vamos
# prosseguir com a analise



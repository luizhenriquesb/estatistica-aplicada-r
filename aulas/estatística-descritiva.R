 
# Estatística descritiva no R - Tabelas -----------------------------------

# Pacotes ----

library(tidyverse)

# Base de dados ----

dados <- read.csv2("dados/Banco de Dados 2.csv", fileEncoding = "latin1")

# 1. Tabelas de frequências - variáveis vategóricas ----

# Frequências absolutas ----

table(dados$Genero)

table(dados$Grau_de_Instruçao)

# Tabela cruzada com freq. absoluta ----

table(dados$Genero, dados$Grau_de_Instruçao)

# Frequências relativas ----

prop.table(table(dados$Genero))

prop.table(table(dados$Grau_de_Instruçao))

# Tabela cruzada com freq. relativa ----

prop.table(table(dados$Genero, dados$Grau_de_Instruçao))

# 2. Tabelas de frequências - variáveis quantitativas ----

# Tabela de frequências ----

# Variáveis discretas ----

table(dados$N_Filhos)
prop.table(table(dados$N_Filhos))

# Variáveis contínuas ----

# Necessário criar categorias que correspondam a faixas de valores

# Passo 1: analisar a amplitude
range(dados$Salario)

# Passo 2 (opcional): avaliar a qtde. de categorias adequada (método Sturges)
nclass.Sturges(dados$Salario)

# Passo 3: criação da tabela com as faixas 
table(cut # divide a varia´vel em faixas de valores
      (dados$Salario, seq(0, 6, l = 7))
      )

# Função summary ----

summary(dados$Salario)

# Funções describe e describeBy ----

install.packages("psych")
library(psych)

describe(dados$Salario)

describeBy(dados$Salario, group = dados$Genero)
describeBy(dados$Salario, group = dados$Genero:dados$Grau_de_Instruçao)

# Usando group_by

dados |> 
  group_by(Genero, Grau_de_Instruçao) |> 
  summarise(
    media = mean(Salario),
    dp = sd(Salario),
    mediana = median(Salario)
  )
  













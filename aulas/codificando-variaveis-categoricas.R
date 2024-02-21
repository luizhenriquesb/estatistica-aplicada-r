 
# Codificando variáveis categóricas no R ----------------------------------

# Link para o vídeo: <https://www.youtube.com/watch?v=0afvtQyTxjc&list=PLOw62cBQ5j9VE9X4cCCfFMjW_hhEAJUhU&index=3>

library(tidyverse)

# Importando base de dados ----

df <- read.csv("dados/Banco_de_Dados_2_Codificado.csv", 
                  sep = ";",
                  dec = ",",
                  fileEncoding = "latin1")

dados <- read.csv("dados/Banco_de_Dados_2_Codificado.csv", 
                  sep = ";",
                  dec = ",",
                  fileEncoding = "latin1")

glimpse(dados)

# Ajustando as variáveis ----

# Transformando gênero em fator

dados$Genero <- factor(dados$Genero, label = c("M", "F"), levels = c(0,1))

# Transformando grau de instrução em fator

dados$Grau_de_Instruçao <- factor(dados$Grau_de_Instruçao,
                                  label = c("Fundamental",
                                            "Medio",
                                            "Superior"),
                                  levels = 0:2,
                                  # Especifica que a variável é ordinal
                                  order = TRUE)

# Codificando valores ausentes

dados[dados==-999] <- NA






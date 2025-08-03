# Instalando pacotes
library("tidyverse")
library("read.dbc")

# Abrindo as bases do SINAN dos últimos 10 anos

sinan23 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/sinan/VIOLBR23.dbc")

sinan22 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/sinan/VIOLBR22.dbc")

sinan21 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/sinan/VIOLBR21.dbc")

sinan20 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/sinan/VIOLBR20.dbc")

sinan19 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/sinan/VIOLBR19.dbc")

sinan18 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/sinan/VIOLBR18.dbc")

sinan17 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/sinan/VIOLBR17.dbc")

# Arrumando o ano de 2020

sinan20$NU_ANO <- "2020"

sinan20$NU_ANO <- as.factor(sinan20$NU_ANO)

# Unindo as bases

sinan <- bind_rows(sinan17, sinan18, sinan19, sinan20, sinan21, sinan22, sinan23)


sinan$NU_ANO <- as.character(sinan$NU_ANO)

sinan$NU_ANO[is.na(sinan$NU_ANO)] <- "2020"

sinan %>% 
  group_by(NU_ANO) %>%
  summarise(n = n())


# Salvando em um arquivo único

write.csv(sinan, "C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/sinan/sinan_17a23.csv")

# Filtrando casos de Recife
sinanrec <- sinan %>% filter(ID_MUNICIP == "261160")


sinanrec %>% 
  group_by(NU_ANO) %>%
  summarise(n = n())


# Salvando
write.csv(sinanrec, "C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/sinan/sinan_17a23_rec.csv")

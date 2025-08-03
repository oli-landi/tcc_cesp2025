# Instalando pacotes
library("tidyverse")
library("read.dbc")

# Abrindo as bases do SINASC 2017-2023

sinasc23 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/SINASC/DNPE2023.dbc")

sinasc22 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/SINASC/DNPE2022.dbc")

sinasc21 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/SINASC/DNPE2021.dbc")

sinasc20 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/SINASC/DNPE2020.dbc")

sinasc19 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/SINASC/DNPE2019.dbc")

sinasc18 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/SINASC/DNPE2018.dbc")

sinasc17 <- read.dbc("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/SINASC/DNPE2017.dbc")

# Filtrando Recife

sinasc23_rec <- sinasc23 %>% filter(CODMUNRES == 261160)

sinasc22_rec <- sinasc22 %>% filter(CODMUNRES == 261160)

sinasc21_rec <- sinasc21 %>% filter(CODMUNRES == 261160)

sinasc20_rec <- sinasc20 %>% filter(CODMUNRES == 261160)

sinasc19_rec <- sinasc19 %>% filter(CODMUNRES == 261160)

sinasc18_rec <- sinasc18 %>% filter(CODMUNRES == 261160)

sinasc17_rec <- sinasc17 %>% filter(CODMUNRES == 261160)


# Juntanto as bases

sinasc_rec <- bind_rows(sinasc17_rec, sinasc18_rec, sinasc19_rec, sinasc20_rec, sinasc21_rec, sinasc22_rec, sinasc23_rec)

write.csv(sinasc_rec, "C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/SINASC/sinasc_17a23_total.csv")


# Filtrar idade da mãe

class(sinasc_rec$IDADEMAE)

sinasc_rec <- sinasc_rec %>% filter(IDADEMAE %in% c(10,11,12,13,14,15,16,17,18,19))

# criar coluna ano
sinasc_rec$ANO <- substr(sinasc_rec$DTNASC, 5, 8)

sinasc_rec %>% group_by(ANO) %>% summarise(n = n())


# Salvando em um arquivo único

write.csv(sinasc_rec, "C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/SINASC/sinasc_17a23.csv")

# Definindo o diretório
setwd("C:/Users/OlíviaGuaranha/Documents/tcc")

# Instalando e recuperando pacotes
#install.packages("devtools")
library(read.dbc)
library(devtools)
library(tidyverse)

Sys.setenv(GITHUB_PAT = )

devtools::install_github("rfsaldanha/microdatasus")

library(microdatasus)

# Baixando dados do SIH

sih_rec <- fetch_datasus(year_start = 2017, 
                         month_start = 1, 
                         year_end = 2023, 
                         month_end = 12, 
                         uf = "PE", 
                         information_system = "SIH-RD")

# Selecionando o município do Recife

sih_rec <- sih_rec %>% filter(MUNIC_RES == 261160)

# Selecionando pop de interesse
# Sexo feminino == 2 e 3
# Idades entre 10 e 19 anos

sih_rec <- sih_rec %>% filter(SEXO %in% c(2,3) & 
                                IDADE %in% c(10,11,12,13,14,15,16,17,18,19))



# Salvando a base

write.csv(sih_rec, "C:/Users/OlíviaGuaranha/Documents/tcc/sih_rec_17a23.csv")

# Fonte: SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pré-processamento de microdados do Departamento de Informática do SUS (DATASUS). Cad. Saúde Pública, Rio de Janeiro , v. 35, n. 9, e00032419, 2019. Disponível em:  https://doi.org/10.1590/0102-311x00032419, acesso em 18 maio 2025.

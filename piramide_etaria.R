# Pirâmide etária construída com apoio do tutorial da Beatriz Milz
# https://beamilz.com/posts/2024-04-21-piramide-ggplot2/pt/

#Pacotes
library(tidyverse)
library(janitor)
library(ggplot2)

# Dados
# Obtidos em: https://censo2022.ibge.gov.br/panorama/

dados <- read.csv("C:/Users/OlíviaGuaranha/Documents/tcc/dados_piramide_etaria_recife.csv", sep=";")

# Ajustando os nomes das variáveis
dados <- dados %>% clean_names()

# Alterando o formato da base
dados_longos <- dados %>%
  pivot_longer(cols = c("populacao_feminina_pessoas", "populacao_masculina_pessoas")) %>%
  rename(populacao = value, sexo = name)

dados_longos2 <- dados_longos %>%
  mutate(pop = case_when(
      sexo == "populacao_masculina_pessoas" ~ populacao * -1,
      sexo == "populacao_feminina_pessoas" ~ populacao))

dados_longos3 <- dados_longos2 %>% 
  mutate(
    grupo_de_idade_fct = factor(
      grupo_de_idade,
      levels = c(
        "0 a 4 anos",
        "5 a 9 anos",
        "10 a 14 anos",
        "15 a 19 anos",
        "20 a 24 anos",
        "25 a 29 anos",
        "30 a 34 anos",
        "35 a 39 anos",
        "40 a 44 anos",
        "45 a 49 anos",
        "50 a 54 anos",
        "55 a 59 anos",
        "60 a 64 anos",
        "65 a 69 anos",
        "70 a 74 anos",
        "75 a 79 anos",
        "80 a 84 anos",
        "85 a 89 anos",
        "90 a 94 anos",
        "95 a 99 anos",
        "100 anos ou mais"
      )),
      sexo_renomeado = case_when(sexo == "populacao_masculina_pessoas" ~ "Masculino",
                                 sexo == "populacao_feminina_pessoas" ~ "Feminino"))

# Criando a pirâmide etária

dados_longos3 %>%
  ggplot() +
  aes(y = grupo_de_idade_fct, x = pop, fill=sexo_renomeado) +
  geom_col() +
  scale_fill_manual(values = c("#FF7F50", "#A52A2A")) +
  scale_x_continuous(labels = abs) +
  labs(
    x = "População", 
    y = "Faixa etária",
    fill = "Sexo"
  ) +
  theme_minimal()

# Instalando pacotes
library("tidyverse")
library("ggplot2")
library("gt")
library("viridis")

# Abre o arquivo
sinasc <- read.csv("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/SINASC/sinasc_17a23.csv")


--------------------------------------------------------------------------------
# Total de nascidos vivos por ano - mães 10 a 19 anos

sinasc %>% 
  group_by(ANO) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = ANO, y = n)) +
  geom_line() +
  geom_point() +  # Adiciona pontos para destacar os valores
  geom_text(aes(label = n), vjust = -0.5, size = 5) +  # Adiciona os valores acima dos pontos
  labs(
    x = "Ano",
    y = "Quantidade de registros"
  ) +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),   # Aumenta a fonte do eixo X
        axis.text.y = element_blank(),         # Remove as marcas do eixo Y
        axis.title.x = element_text(size = 14),  # Aumenta o nome do eixo X
        axis.title.y = element_text(size = 14),  # Aumenta o nome do eixo Y
        plot.title = element_text(hjust = 0.5))

--------------------------------------------------------------------------------
# Registros por idade e taxas

# Taxa de nascidos vivos por 1000 meninas entre 10 e 14 anos
  
n_nascidos_vivos22_g1 <- sinasc %>% 
  filter(IDADEMAE %in% c(10,11,12,13,14) &
           ANO == 2022) %>%
  group_by(IDADEMAE) %>% 
  summarise(n = n()) %>%
  summarise(sum = sum(n)) %>%
  as.numeric()

# Calculando a taxa de nascidos vivos entre meninas 10-14 anos em 2022

taxa_g1 <- ((n_nascidos_vivos22_g1/43624) * 1000)


# Taxa de nascidos vivos por 1000 meninas entre 15 e 19 anos

n_nascidos_vivos22_g2 <- sinasc %>% 
  filter(IDADEMAE %in% c(15,16,17,18,19) &
           ANO == 2022) %>%
  group_by(IDADEMAE) %>% 
  summarise(n = n()) %>%
  summarise(sum = sum(n)) %>%
  as.numeric()

# Calculando a taxa de nascidos vivos a cada 1000 meninas 15-19 anos em 2022
taxa_g2 <- ((n_nascidos_vivos22_g2/47219) * 1000)

# Taxa para a população entre 10 e 19 anos

n_nascidos_vivos22_total <- sinasc %>% 
  filter(IDADEMAE %in% c(10,11,12,13,14,15,16,17,18,19) &
           ANO == 2022) %>%
  group_by(IDADEMAE) %>% 
  summarise(n = n()) %>%
  summarise(sum = sum(n)) %>%
  as.numeric()

# Calculando a taxa de nascidos vivos entre meninas 10-14 anos em 2022

taxa_total <- ((n_nascidos_vivos22_total/90843) * 1000)

-------------------------------------------------------------------------------
# Distribuição de nascidos vivos por idade 2017 a 2023

sinasc %>% 
  group_by(IDADEMAE) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = as.factor(IDADEMAE), y = n)) +
  geom_col(fill = "#A52A2A") +
  geom_text(aes(label = n), vjust = -0.5, size = 5) +
  labs(x = "Idade da mãe",
       y = "Nº de nascidos vivos no período") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(hjust = 1, size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

--------------------------------------------------------------------------------
# Nascidos vivos por grupo etário (10 a 14, 15 a 19)
  
sinasc %>% 
  mutate(faixa_etaria = ifelse(IDADEMAE %in% c(10:14), "10-14 anos", "15-19 anos")) %>%
  group_by(ANO, faixa_etaria) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = ANO, y = n, color = faixa_etaria, group = faixa_etaria)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_color_manual(values = c("#E07B39", "#A52A2A"), name = "Faixa etária") +
  labs(x = "Ano",
    y = "Quantidade de registros",
    fill = "Faixa etária") +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "bottom",
    legend.direction = "horizontal")

-------------------------------------------------------------------------------
# Consultas pré-natal

table(sinasc$IDADEMAE,sinasc$CONSPRENAT)

sinasc %>% mutate(pre_natal = case_when(CONSULTAS == 1 ~ "Nenhuma",
                                        CONSULTAS == 2 ~ "de 1 a 3",
                                        CONSULTAS == 3 ~ "de 4 a 6",
                                        CONSULTAS == 4 ~ "7 a mais",
                                        CONSULTAS == 9 ~ "Ignorado"),
                  faixa_etaria = ifelse(IDADEMAE %in% c(10:14), "10-14 anos", "15-19 anos")) %>%
  group_by(faixa_etaria, pre_natal) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(faixa_etaria) %>% 
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(x = faixa_etaria, y = proportion, fill = pre_natal)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = ifelse(proportion >= 0.001, scales::percent(proportion, accuracy = 1), "")), 
    position = position_stack(vjust = 0.5), 
    size = 4, 
    color = "black") +
  scale_fill_manual(values = c("#D95F02", "#A52A2A", "#E69F00", "#8B0000", "#CC7722",
                               "#E07B39", "#6B2E2E", "#B5651D", "#D2B48C", "#5C4033")) +
  labs(
    x = "Idade da Mãe",
    y = "Porcentagem",
    fill = "Consultas pré-natal"
  ) +
  scale_y_continuous(labels = NULL) +
  theme_minimal() +
  theme(theme(axis.text.x = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              legend.position = "bottom"))


-------------------------------------------------------------------------------
# Tipo de parto

sinasc_summary <- sinasc %>%
  mutate(via_parto = case_when(
    PARTO == 1 ~ "Vaginal",
    PARTO == 2 ~ "Cesárea",
    PARTO == 9 ~ "Ignorado"
  )) %>%
  group_by(IDADEMAE, via_parto) %>% 
  summarise(n = n(), .groups = 'drop')

# Criando o gráfico de barras empilhadas 

ggplot(sinasc_summary, aes(x = IDADEMAE, y = n, fill = via_parto)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribuição dos Partos por Idade da Mãe e Tipo de Parto",
    x = "Idade da Mãe",
    y = "Número de Partos",
    fill = "Tipo de Parto"
  ) +
  scale_x_continuous(breaks = seq(12, 17, 1))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Criando gráfico de barras 100%

# Sumarizando e calculando proporções
sinasc_summary1 <- sinasc %>%
  mutate(via_parto = case_when(
    PARTO == 1 ~ "Vaginal",
    PARTO == 2 ~ "Cesárea",
    PARTO == 9 ~ "Ignorado"
  )) %>%
  group_by(IDADEMAE, via_parto) %>% 
  summarise(n = n(), .groups = 'drop') %>%
  group_by(IDADEMAE) %>% 
  mutate(proportion = n / sum(n)) # Calcula proporção dentro de cada IDADEMAE

# Criando o gráfico de barras 100%
ggplot(sinasc_summary1, aes(x = IDADEMAE, y = proportion, fill = via_parto)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = scales::percent(proportion, accuracy = 1)), 
    position = position_stack(vjust = 0.5), # Posiciona no meio da barra
    color = "white",
    size = 3.5
  ) +
  scale_y_continuous(labels = NULL) + # Oculta o eixo Y
  labs(
    title = "Distribuição Percentual dos Partos por Idade da Mãe e Tipo de Parto",
    x = "Idade da Mãe",
    y = NULL, # Remove o rótulo do eixo Y
    fill = "Tipo de Parto"
  ) +
  scale_x_continuous(breaks = seq(12, 17, 1)) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(), # Remove os valores do eixo Y
    axis.ticks.y = element_blank(), # Remove as marcas do eixo Y
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Criando visualização por grupo etário

sinasc_grupos <- sinasc %>%
  mutate(faixa_etaria = case_when(
    IDADEMAE >= 10 & IDADEMAE <= 14 ~ "10-14 anos",
    IDADEMAE >= 15 & IDADEMAE <= 19 ~ "15-19 anos",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(faixa_etaria))

sinasc_grupos <- sinasc_grupos %>% mutate(via_parto = case_when(
  PARTO == 1 ~ "Vaginal",
  PARTO == 2 ~ "Cesárea",
  PARTO == 9 ~ "Ignorado"
))


# Calcular proporções por grupo
dados_resumo <- sinasc_grupos %>%
  group_by(faixa_etaria, via_parto) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(faixa_etaria) %>%
  mutate(prop = n / sum(n),
         label = scales::percent(prop)) %>%
  filter(prop > 0, !is.na(via_parto))


ggplot(dados_resumo, aes(x = "", y = prop, fill = via_parto)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  facet_wrap(~faixa_etaria) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Vaginal" = "#E07B39", "Cesárea" = "#A52A2A")) +
  labs(title = NULL, x = NULL, y = NULL, fill = "Tipo de parto") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),       # Remove textos dos eixos
    axis.ticks = element_blank(),      # Remove marcas dos eixos
    panel.grid = element_blank(),      # Remove linhas de grade
    strip.text = element_text(size = 14),
    legend.position = "bottom",
    legend.direction = "horizontal")

--------------------------------------------------------------------------------

# Anomalia identificada
sinasc_grupos %>% mutate(anomalia = case_when(IDANOMAL == 1 ~ "Sim",
                  IDANOMAL == 2 ~ "Não",
                  IDANOMAL == 9 ~ "Ignorado",
                  is.na(IDANOMAL) ~ "Ignorado")) %>%
  group_by(faixa_etaria, anomalia) %>% 
  summarise(n = n()) %>%
  group_by(faixa_etaria) %>%
  mutate(proportion = (n / sum(n)) *100)


# Peso ao nascer
sinasc_grupos %>% mutate(peso_nascer = case_when(PESO < 2500 ~ "Baixo peso",
                                                 PESO < 1500 ~ "Muito baixo peso",
                                                 PESO >= 2500 ~ "Regular")) %>%
  group_by(faixa_etaria, peso_nascer) %>% 
  summarise(n = n()) %>%
  group_by(faixa_etaria)

sinasc_grupos %>% group_by(faixa_etaria) %>% summarise(n = n())

--------------------------------------------------------------------------------
# Situação conjugal da mãe
sinasc %>% 
  group_by(ESTCIVMAE) %>% 
  summarise(n = n())

table(sinasc$IDADEMAE, sinasc$ESTCIVMAE)


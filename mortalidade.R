#Pacotes

library('tidyverse')
library('gt')
library('ggplot2')
library('tidyr')
library('janitor')
library("dplyr")
library("forcats")  # Para reordenar fatores

# Dados de mortalidade - Recife 2017 a 2023
# Obtidos no TabNet
# Extração entre os anos 2017-2023 por Capítulo da CID-10

# Abrindo e arrumando os dados
df <- read.csv("sim_cnv_obt_rec.csv", header = TRUE, fileEncoding = "latin1", sep=";")

clean_names(df)

df <- df %>% rename(`2017` = X2017,
                    `2018` = X2018,
                    `2019` = X2019,
                    `2020` = X2020,
                    `2021` = X2021,
                    `2022` = X2022,
                    `2023` = X2023,
                    cid_10 = Capítulo.CID.10)

colnames(df)

# Mudando o tipo das variáveis
df <- df %>% mutate(across(`2017`:`2023`, as.numeric))

sapply(df, class)

# Criando o gráfico

df_long <- df %>%
  pivot_longer(cols = `2017`:`2023`, names_to = "ano", values_to = "valor")

# Para calcular % por ano
# Passo 1: Extrair totais de óbitos por ano
totais_ano <- df_long %>%
  filter(cid_10 == "Total") %>%
  select(ano, valor_total_ano = valor)


# Filtrando valores < 10

df_filtrado <- df_long %>%
  filter(cid_10 != "Total") %>%       # Remove a linha Total
  group_by(cid_10) %>%
  filter(any(valor >= 10)) %>%      # Mantém capítulos com valor ≥10 em algum ano
  ungroup()

df_filtrado <- df_filtrado %>%
  group_by(cid_10) %>%
  mutate(valor_total = sum(valor, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cid_10 = fct_reorder(cid_10, valor_total)) %>% # Ordena pelo total
  filter(ano %in% c(2019,2020,2021),
         cid_10 %in% c("I.   Algumas doenças infecciosas e parasitárias",
                       "II.  Neoplasias (tumores)",
                       "IV.  Doenças endócrinas nutricionais e metabólicas",
                       "V.   Transtornos mentais e comportamentais",
                       "VI.  Doenças do sistema nervoso",
                       "IX.  Doenças do aparelho circulatório",
                       "X.   Doenças do aparelho respiratório",
                       "XX.  Causas externas de morbidade e mortalidade"))

# Passo 2: Unir totais com o dataframe filtrado
df_pct <- df_filtrado %>%
  left_join(totais_ano, by = "ano") %>%
  mutate(porcentagem = (valor / valor_total_ano) * 100)


# Gráfico
cores_quentes <- c("#FF7F50", "#d73027","#8B0000", "#CC7722")

ggplot(df_pct, aes(x = cid_10, y = porcentagem, fill = ano)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = paste0(round(porcentagem), "%")), 
            position = position_dodge(width = 0.8), 
            hjust = -0.1, size = 4) +  # Ajusta a posição e tamanho do texto
  scale_fill_manual(values = cores_quentes) +
  labs(x = NULL,
       y = NULL,
       fill = "Ano") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.y = element_text(size = 13),
        axis.text.x = element_blank(),         # Remove o eixo x
        axis.ticks.x = element_blank(),        # Remove os ticks do eixo x
        legend.title = element_text(size = 13),
        legend.text = element_text(size=13),
        legend.position = "bottom",
        legend.text.position = "bottom",
        legend.location = "plot")


# Dados por sexo
obt_sexo <- read.csv("principais_obt_sexo.csv", header = TRUE, fileEncoding = "latin1", sep=";")

obt_sexo_long <- obt_sexo %>% 
  pivot_longer(cols = c("Masc", "Fem"), names_to = "sexo", values_to = "proporcao") %>%
  mutate(proporcao = as.numeric(proporcao),
         proporcao_pct = proporcao * 100)  # Convertendo para %

obt_sexo_long <- obt_sexo_long %>%
  group_by(cid_10) %>%
  mutate(media_prop = mean(proporcao_pct, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cid_10 = fct_reorder(cid_10, media_prop))

# Paleta de cores quentes
cores_quentes_2 <- c("Masc" = "#d73027", "Fem" = "#FF7F50")

# Gráfico
ggplot(obt_sexo_long, aes(x = proporcao_pct, y = cid_10, fill = sexo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = paste0(round(proporcao_pct, 1), "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.1, size = 4) +
  scale_fill_manual(values = cores_quentes_2) +
  labs(x = "Proporção",
       y = NULL,
       fill = "Sexo") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 13),
        axis.text.x = element_blank(),  # Remove os valores do eixo x
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 13),       # Eixo y aumentado
        legend.title = element_text(size = 13),
        legend.text = element_text(size=13),
        legend.position = "bottom",
        legend.text.position = "bottom",
        legend.location = "plot")

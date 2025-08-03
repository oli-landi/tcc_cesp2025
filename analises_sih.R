# Pacotes
library('tidyverse')
library('gt')
library('ggplot2')
library('tidyr')
library('devtools')

# Abrir os dados

sih <- read.csv("C:/Users/OlíviaGuaranha/Documents/tcc/sih_rec_17a23.csv")

# Pacote CID-10

#devtools::install_git(url = "http://github.com/msrodrigues/cid10.git", force = TRUE)
library('cid10')

cid_subcat

-----------------------------------------------------

# Procedimentos de CURETAGEM POS-ABORTAMENTO / PUERPERAL (04.11.02.001-3) 
# e ESVAZIAMENTO DE UTERO POS-ABORTO POR ASPIRACAO MANUAL INTRA-UTERINA (AMIU) (04.09.06.007-0)

proc_aborto <- sih %>% filter(PROC_REA %in% c(411020013, 0409060070))



# Gráfico: registros de curetagem pós-abortamento por ano entre meninas 10 a 19
proc_aborto %>%
  group_by(ANO_CMPT) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = ANO_CMPT, y = n)) +
  geom_line(color = "#8B0000", size = 0.5) +
  geom_point(color = "#8B0000", size = 1) +
  geom_text(aes(label = n), vjust = -0.5, color = "#8B0000") +
  labs(
    x = "Ano",
    y = "Quantidade de registros de curetagem \n pós-abortamento") +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Criando grupos etários

proc_aborto_combined <- proc_aborto %>%
  filter(IDADE %in% c(10:19)) %>%
  mutate(Grupo = ifelse(IDADE %in% c(10:14), "10-14 anos", "15-19 anos")) %>%
  group_by(ANO_CMPT, Grupo) %>%
  summarise(n = n(), .groups = "drop")

# Criar o gráfico combinado
ggplot(proc_aborto_combined, aes(x = ANO_CMPT, y = n, color = Grupo, group = Grupo)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(
    x = "Ano",
    y = "Quantidade de registros de \n procedimentos de abortamento",
    color = "Faixa etária") +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +
  scale_color_manual(values = c("#D95F02", "#6B2E2E")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))

--------------------------------------------------------------

# Procedimentos de parto

# Filtrando registros de parto normal e parto cesáreo
sih_parto <- sih %>% filter(PROC_REA %in% c(0310010039,0411010034))

# Registro de parto por ano para meninas de 10 a 19 anos
sih_parto %>%
  group_by(ANO_CMPT) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = ANO_CMPT, y = n)) +
  geom_line(color = "#8B0000", size = 0.5) +
  geom_point(color = "#8B0000", size = 1) +
  geom_text(aes(label = n), vjust = -0.5, color = "#8B0000") +
  labs(
    x = "Ano",
    y = "Quantidade de registros de parto") +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# Grupos etários
parto_combined <- sih_parto %>%
  filter(IDADE %in% c(10:19)) %>%
  mutate(Grupo = ifelse(IDADE %in% c(10:14), "10-14 anos", "15-19 anos")) %>%
  group_by(ANO_CMPT, Grupo) %>%
  summarise(n = n(), .groups = "drop")

# Criar o gráfico combinado
ggplot(parto_combined, aes(x = ANO_CMPT, y = n, color = Grupo, group = Grupo)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(
    x = "Ano",
    y = "Quantidade de registros de procedimentos de parto",
    color = "Faixa etária") +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +
  scale_color_manual(values = c("#D95F02", "#6B2E2E")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))

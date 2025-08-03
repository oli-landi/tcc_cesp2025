# Install packages

library('tidyverse')
library('gt')
library('ggplot2')
library('tidyr')
library('viridis')

# Abrir os dados

sinan <- read.csv("C:/Users/OlíviaGuaranha/Documents/CESP2024/TCC/sinan/sinan_17a23_rec.csv")

--------------------------------------------------------------------------------
# Selecionando população do estudo e alterando algumas variáveis
# Anos: 2017-2023
# Sexo feminino
# Idade entre 10 e 19
# Apenas casos de violência sexual (VIOL_SEXU == 1)
  
sinan_mulheres <- sinan %>% filter(CS_SEXO == "F",
                                   NU_IDADE_N >= 4010 & NU_IDADE_N <= 4019,
                                   VIOL_SEXU == 1)

# Criando a variável idade
sinan_mulheres <- sinan_mulheres %>%
  mutate(idade = case_when(NU_IDADE_N == 4010 ~ 10,
                           NU_IDADE_N == 4011 ~ 11,
                           NU_IDADE_N == 4012 ~ 12,
                           NU_IDADE_N == 4013 ~ 13,
                           NU_IDADE_N == 4014 ~ 14,
                           NU_IDADE_N == 4015 ~ 15,
                           NU_IDADE_N == 4016 ~ 16,
                           NU_IDADE_N == 4017 ~ 17,
                           NU_IDADE_N == 4018 ~ 18,
                           NU_IDADE_N == 4019 ~ 19))

# Raça/cor das vítimas
sinan_mulheres <- sinan_mulheres %>% mutate(raca = case_when(CS_RACA == 1 ~ "Branca",
                                                             CS_RACA == 2 ~ "Preta",
                                                             CS_RACA == 3 ~ "Amarela",
                                                             CS_RACA == 4 ~ "Parda",
                                                             CS_RACA == 5 ~ "Indígena",
                                                             CS_RACA == 9 ~ "Ignorado"))

# Faixas etárias

sinan_mulheres <-  sinan_mulheres %>% mutate(faixaetaria = case_when(idade < 15 ~ "10 a 14 anos",
                                                                     idade >= 15 ~ "15 a 19 anos"))

--------------------------------------------------------------------------------
# Número de registros de violência sexual \n com vítimas entre 10 e 19 anos, por ano (2017-2023)

sinan_mulheres %>%
  group_by(NU_ANO) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = NU_ANO, y = n)) +
  geom_line(color = "#8B0000", size = 0.5) +  # Linha bordô
  geom_point(color = "#8B0000", size = 1) +  # Pontos bordô
  geom_text(aes(label = n), vjust = -0.5, color = "#8B0000") +  # Texto acima dos pontos
  labs(
    x = "Ano",
    y = "Quantidade de registros"
  ) +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(hjust = 0.5))

# Por faixa etária

sinan_mulheres %>%
  group_by(NU_ANO, faixaetaria) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = NU_ANO, y = n, color = faixaetaria, group = faixaetaria)) +
  geom_line(size = 0.8) +
  geom_point(size = 2) +
  geom_text(aes(label = n), vjust = -0.5, size = 5) +
  labs(
    x = "Ano",
    y = "Quantidade de registros",
    color = "Faixa etária"
  ) +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +
  scale_color_manual(values = c("#E07B39", "#A52A2A"), name = "Faixa etária") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "horizontal")
  )


nrow(sinan_mulheres)

sinan_mulheres %>% filter(faixaetaria == "10 a 14 anos") %>% nrow()

sinan_mulheres %>% filter(faixaetaria == "15 a 19 anos") %>% nrow()

sinan_mulheres %>% filter(idade == 13) %>% nrow()

--------------------------------------------------------------------------------
# Número de registros de violência sexual contra meninas entre 10 e 19 anos, por idade e ano

sinan_mulheres %>%
  mutate(idade = as.factor(idade)) %>%  # Garantir que 'idade' seja um fator
  group_by(NU_ANO, idade) %>%
  summarise(n = n(), .groups = 'drop') %>%
  ggplot(aes(x = NU_ANO, y = n, fill = idade, group = idade)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.9),  # mesmo width das barras
            vjust = -0.5, size = 3.5)  +  # Adiciona os valores acima das barras
  labs(
    x = "Ano",
    y = "Quantidade de registros",
    fill = "Idade"
  ) +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +  # Ajusta o eixo X
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
    legend.position = "bottom")+  
  scale_fill_manual(values = c("#D95F02", "#A52A2A", "#E69F00", "#8B0000", "#CC7722",
                               "#E07B39", "#6B2E2E", "#B5651D", "#D2B48C", "#5C4033"))

# Por grupo etário

sinan_mulheres %>%
  mutate(faixa_etaria = ifelse(idade %in% c(10:14), "10-14 anos", "15-19 anos")) %>%  # Garantir que 'idade' seja um fator
  group_by(NU_ANO, faixa_etaria) %>%
  summarise(n = n(), .groups = 'drop') %>%
  ggplot(aes(x = NU_ANO, y = n, fill = faixa_etaria)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barras agrupadas
  geom_text(aes(label = n), position = position_dodge(width = 0.8), vjust = -0.5) +  # Adiciona os valores acima das barras
  labs(
    x = "Ano",
    y = "Quantidade de registros"
  ) +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +  # Ajusta o eixo X
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "bottom"  
  )+  
  scale_fill_manual(values = c("#D95F02", "#A52A2A", "#E69F00", "#8B0000", "#CC7722",
                               "#E07B39", "#6B2E2E", "#B5651D", "#D2B48C", "#5C4033"))

--------------------------------------------------------------------------------

# Número de registros de violência sexual contra meninas entre 10 e 19 anos, por ano e grupo racial


#Gráfico
sinan_mulheres %>%
  group_by(NU_ANO, raca) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = NU_ANO, y = n, color = raca, group = raca)) +
  geom_line() +
  geom_point() +  # Adiciona pontos para destacar os valores
  geom_text(aes(label = n), vjust = -0.5) +  # Adiciona os valores acima dos pontos
  labs(
    x = "Ano",
    y = "Quantidade de registros"
  ) +
  scale_x_continuous(breaks = seq(2017, 2023, 1)) +  # Ajusta o eixo X
  theme_minimal() +
  scale_color_manual(values = c(
    "#5C4033", "#A52A2A", "#E69F00","#6B2E2E" , "#CC7722",
    "#E07B39", "#8B0000", "#B5651D", "#D2B48C", "#D95F02"
  ))


# Tabela: prop de notificações por raça/cor das vítimas
sinan_mulheres %>% 
  group_by(raca) %>%
  summarise(n = n()) %>%
  mutate(prop = round((n/sum(n)*100),2)) %>%
  arrange(desc(prop)) %>%
  gt() %>%
  tab_header(title = "Frequência de registros por raça/cor") %>%
  cols_label(raca = "Raça/cor",
             prop = "Porcentagem")
------------------------------------------------------------------------------

# Qtd de agressores

sinan_mulheres %>% 
  group_by(NUM_ENVOLV) %>%
  summarise(n = n())

# Gestações

sinan_mulheres %>% group_by(CS_GESTANT) %>%
  summarise(n = n())


----------------------------------------------------------------------------
# Data da violência vs data da notificação

#Transformando as variáveis de chr em Date

sinan_mulheres <- sinan_mulheres %>% 
  mutate (DT_NOTIFIC = as.Date(DT_NOTIFIC, format = "%Y-%m-%d"),
          DT_OCOR = as.Date(DT_OCOR, format = "%Y-%m-%d"))

# Calculando a diferença de tempo

sinan_mulheres <- sinan_mulheres %>% 
  mutate(dif_date = as.numeric(DT_NOTIFIC - DT_OCOR))

# Um caso não tem data de ocorrência
sinan_mulheres %>% filter(is.na(dif_date)) %>% nrow()


# Meninas que chegaram em até 72 horas (3 dias): 1306
sinan_mulheres %>% select(NU_ANO,
                          idade,
                          raca,
                          PROC_DST,
                          PROC_HIV,
                          PROC_HEPB,
                          PROC_SANG,
                          PROC_SEMEN,
                          PROC_VAGIN,
                          PROC_CONTR,
                          PROC_ABORT,
                          dif_date) %>%
  filter(dif_date <=3) %>%
  nrow()

# % das meninas que chegaram em até 72h 
men_72 <- round(((1306/2422) * 100),2)


# Meninas que chegaram depois de 72h: 1115 
sinan_mulheres %>% select(NU_ANO,
                          idade,
                          raca,
                          PROC_DST,
                          PROC_HIV,
                          PROC_HEPB,
                          PROC_SANG,
                          PROC_SEMEN,
                          PROC_VAGIN,
                          PROC_CONTR,
                          PROC_ABORT,
                          dif_date) %>%
  filter(dif_date > 3) %>%
  nrow()

# % de meninas que chegaram depois das 72h
men_not_72 <- round(((1115/2422) *100),2)

------------------------------------------------------------------------------

# Procedimentos

# Verificar quantos campos de procedimento não foram preenchidos

# Coleta de sangue
sinan_mulheres %>% 
  group_by(PROC_SANG) %>%
  summarise(n = n()) %>%
  mutate(prop = round((n / nrow(sinan_mulheres)*100), 2))

# Coleta de sêmen
sinan_mulheres %>% 
  group_by(PROC_SEMEN) %>%
  summarise(n = n()) %>%
  mutate(prop = round((n / nrow(sinan_mulheres)*100), 2))

# Profilaxia DSTs
sinan_mulheres %>% 
  group_by(PROC_DST) %>%
  summarise(n = n()) %>%
  mutate(prop = round((n / nrow(sinan_mulheres)*100), 2))

# Profilaxia Hepatite B
sinan_mulheres %>% 
  group_by(PROC_HEPB) %>%
  summarise(n = n()) %>%
  mutate(prop = round((n / nrow(sinan_mulheres)*100), 2))

# Coleta de secreção vaginal
sinan_mulheres %>% 
  group_by(PROC_VAGIN) %>%
  summarise(n = n()) %>%
  mutate(prop = round((n / nrow(sinan_mulheres)*100), 2))

# Contracepção de emergência
sinan_mulheres %>% 
  group_by(PROC_CONTR) %>%
  summarise(n = n()) %>%
  mutate(prop = round((n / nrow(sinan_mulheres)*100), 2))

# Aborto legal
sinan_mulheres %>% 
  group_by(PROC_ABORT) %>%
  summarise(n = n()) %>%
  mutate(prop = round((n / nrow(sinan_mulheres)*100), 2))


# Criando uma tabela única com as % de procedimentos

tabela_frequencia <- bind_rows(
  
  sinan_mulheres %>%
    summarise(n = sum(PROC_SANG == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_SANG", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    summarise(n = sum(PROC_SEMEN == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_SEMEN", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    summarise(n = sum(PROC_DST == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_DST", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    summarise(n = sum(PROC_HEPB == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_HEPB", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    summarise(n = sum(PROC_VAGIN == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_VAGIN", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    summarise(n = sum(PROC_CONTR == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_CONTR", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    summarise(n = sum(PROC_ABORT == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_ABORT", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    summarise(n = sum(PROC_HIV == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_HIV", prop = round((n / nrow(sinan_mulheres) * 100), 2))
  
) %>%
  arrange(var) %>%
  select(var, n, prop)

# Exibir a tabela
print(tabela_frequencia)

-------------------------------------------------------------------------------

# Verificando casos sem nenhum procedimento
# a variável sem_procedimento será 1 (caso sem procedimento) e 0 caso contrário.

sinan_mulheres <- sinan_mulheres %>%
  mutate(sem_procedimento = if_else(
    rowSums(select(., starts_with("PROC_")) == 1, na.rm = TRUE) == 0,
    1, 0))  # Se nenhuma das colunas PROC_ for 1, marca como 1 (sem procedimento)

# Agora, calcule a porcentagem de casos sem procedimento
porcentagem_sem_procedimento <- sinan_mulheres %>%
  summarise(porcentagem = mean(sem_procedimento == 1) * 100)

print(porcentagem_sem_procedimento)

------------------------------------------------------------------------------

# Procedimentos por raça

table(sinan_mulheres$raca,sinan_mulheres$sem_procedimento)


tabela_frequencia_raca <- bind_rows(
    
    sinan_mulheres %>%
      group_by(raca) %>%
      summarise(n = sum(PROC_SANG == 1, na.rm = TRUE)) %>%
      mutate(var = "PROC_SANG", prop = round((n / nrow(sinan_mulheres)), 2)),
    
    sinan_mulheres %>%
      group_by(raca) %>%
      summarise(n = sum(PROC_SEMEN == 1, na.rm = TRUE)) %>%
      mutate(var = "PROC_SEMEN", prop = round((n / nrow(sinan_mulheres)), 2)),
    
    sinan_mulheres %>%
      group_by(raca) %>%
      summarise(n = sum(PROC_DST == 1, na.rm = TRUE)) %>%
      mutate(var = "PROC_DST", prop = round((n / nrow(sinan_mulheres)), 2)),
    
    sinan_mulheres %>%
      group_by(raca) %>%
      summarise(n = sum(PROC_HEPB == 1, na.rm = TRUE)) %>%
      mutate(var = "PROC_HEPB", prop = round((n / nrow(sinan_mulheres)), 2)),
    
    sinan_mulheres %>%
      group_by(raca) %>%
      summarise(n = sum(PROC_VAGIN == 1, na.rm = TRUE)) %>%
      mutate(var = "PROC_VAGIN", prop = round((n / nrow(sinan_mulheres)), 2)),
    
    sinan_mulheres %>%
      group_by(raca) %>%
      summarise(n = sum(PROC_CONTR == 1, na.rm = TRUE)) %>%
      mutate(var = "PROC_CONTR", prop = round((n / nrow(sinan_mulheres)), 2)),
    
    sinan_mulheres %>%
      group_by(raca) %>%
      summarise(n = sum(PROC_ABORT == 1, na.rm = TRUE)) %>%
      mutate(var = "PROC_ABORT", prop = round((n / nrow(sinan_mulheres)), 2)),
    
    sinan_mulheres %>%
      group_by(raca) %>%
      summarise(n = sum(PROC_HIV == 1, na.rm = TRUE)) %>%
      mutate(var = "PROC_HIV", prop = round((n / nrow(sinan_mulheres) * 100), 2))
    
  ) %>%
  arrange(raca, var) %>%
  select(raca, var, n, prop) 

# complete(raca, var, fill = list(n = 0, prop = NA))  # Completa com NA para valores ausentes


# Exibir a tabela
print(tabela_frequencia_raca)

# Tirando valores iguais a 0
#tabela_frequencia_raca <- tabela_frequencia_raca[tabela_frequencia_raca$prop > 0, ]


# Plotando o gráfico

tabela_frequencia_raca <- tabela_frequencia_raca %>% mutate(procedimento = case_when(var == "PROC_ABORT" ~ "Aborto",
                                                                                       var == "PROC_CONTR" ~ "Contracepção de emergência",
                                                                                       var == "PROC_DST" ~ "Profilaxia DST",
                                                                                       var == "PROC_HEPB" ~ "Profilaxia Hepatite B",
                                                                                       var == "PROC_SANG" ~ "Coleta de sangue",
                                                                                       var == "PROC_SEMEN" ~ "Coleta de sêmen",
                                                                                       var == "PROC_VAGIN" ~ "Coleta de secreção vaginal",
                                                                                       var == "PROC_HIV" ~ "Profilaxia HIV"))

ggplot(tabela_frequencia_raca, aes(x = raca, y = prop, fill = procedimento)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Porcentagem de Procedimentos por idade",
    x = "Raça",
    y = "Porcentagem (%)",
    fill = "Procedimento"
  ) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Ajusta o ângulo do texto no eixo X para melhorar a legibilidade
  scale_fill_viridis(discrete = TRUE)  # Alterar a paleta de cores para viridis


------------------------------------------------------------------------------
# Procedimentos por idade  
  

tabela_frequencia_idade <- bind_rows(
  
  sinan_mulheres %>%
    group_by(idade) %>%
    summarise(n = sum(PROC_SANG == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_SANG", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    group_by(idade) %>%
    summarise(n = sum(PROC_SEMEN == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_SEMEN", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    group_by(idade) %>%
    summarise(n = sum(PROC_DST == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_DST", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    group_by(idade) %>%
    summarise(n = sum(PROC_HEPB == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_HEPB", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    group_by(idade) %>%
    summarise(n = sum(PROC_VAGIN == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_VAGIN", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    group_by(idade) %>%
    summarise(n = sum(PROC_CONTR == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_CONTR", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    group_by(idade) %>%
    summarise(n = sum(PROC_ABORT == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_ABORT", prop = round((n / nrow(sinan_mulheres) * 100), 2)),
  
  sinan_mulheres %>%
    group_by(idade) %>%
    summarise(n = sum(PROC_HIV == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_HIV", prop = round((n / nrow(sinan_mulheres) * 100), 2))
  
) %>%
  arrange(idade, var) %>%
  select(idade, var, n, prop)

print(tabela_frequencia_idade)


# Gráfico procedimentos por idade

tabela_frequencia_idade <- tabela_frequencia_idade %>% mutate(procedimento = case_when(var == "PROC_ABORT" ~ "Aborto",
                                                            var == "PROC_CONTR" ~ "Contracepção de emergência",
                                                            var == "PROC_DST" ~ "Profilaxia DST",
                                                            var == "PROC_HEPB" ~ "Profilaxia Hepatite B",
                                                            var == "PROC_SANG" ~ "Coleta de sangue",
                                                            var == "PROC_SEMEN" ~ "Coleta de sêmen",
                                                            var == "PROC_VAGIN" ~ "Coleta de secreção vaginal",
                                                            var == "PROC_HIV" ~ "Profilaxia HIV"))

ggplot(tabela_frequencia_idade, aes(x = idade, y = prop, fill = procedimento)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Porcentagem de Procedimentos por idade",
    x = "Raça",
    y = "Porcentagem (%)",
    fill = "Procedimento"
  ) +
  scale_y_continuous(labels = scales::percent) + # Para exibir as porcentagens no eixo Y
  scale_x_continuous(breaks = seq(10, 19, 1)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Ajusta o ângulo do texto no eixo X para melhorar a legibilidade
  scale_fill_viridis(discrete = TRUE)  # Alterar a paleta de cores para viridis


---------------------
# Filtrando por aquelas que chegaram em 72h

proc_72 <- sinan_mulheres %>% select(NU_ANO,
                            idade,
                            raca,
                            PROC_DST,
                            PROC_HIV,
                            PROC_HEPB,
                            PROC_SANG,
                            PROC_SEMEN,
                            PROC_VAGIN,
                            PROC_CONTR,
                            PROC_ABORT,
                            sem_procedimento,
                            dif_date) %>%
  filter(dif_date <=3)


table(proc_72$sem_procedimento)


tabela_frequencia_proc <- bind_rows(
  
  proc_72 %>%
    group_by(sem_procedimento) %>%
    summarise(n = sum(PROC_SANG == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_SANG", prop = round((n / nrow(proc_72)), 2)),
  
  proc_72 %>%
    group_by(sem_procedimento) %>%
    summarise(n = sum(PROC_SEMEN == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_SEMEN", prop = round((n / nrow(proc_72)), 2)),
  
  proc_72 %>%
    group_by(sem_procedimento) %>%
    summarise(n = sum(PROC_DST == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_DST", prop = round((n / nrow(proc_72)), 2)),
  
  proc_72 %>%
    group_by(sem_procedimento) %>%
    summarise(n = sum(PROC_HEPB == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_HEPB", prop = round((n / nrow(proc_72)), 2)),
  
  proc_72 %>%
    group_by(sem_procedimento) %>%
    summarise(n = sum(PROC_VAGIN == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_VAGIN", prop = round((n / nrow(proc_72)), 2)),
  
  proc_72 %>%
    group_by(sem_procedimento) %>%
    summarise(n = sum(PROC_CONTR == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_CONTR", prop = round((n / nrow(proc_72)), 2)),
  
  proc_72 %>%
    group_by(sem_procedimento) %>%
    summarise(n = sum(PROC_ABORT == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_ABORT", prop = round((n / nrow(proc_72)), 2)),
  
  proc_72 %>%
    group_by(sem_procedimento) %>%
    summarise(n = sum(PROC_HIV == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_HIV", prop = round((n / nrow(proc_72) * 100), 2))
  
) %>%
  arrange(sem_procedimento, var) %>%
  select(sem_procedimento, var, n, prop) 

print(tabela_frequencia_proc)


table(proc_72$raca,proc_72$sem_procedimento)



tabela_frequencia <- bind_rows(
  
  proc_72 %>%
    summarise(n = sum(PROC_SANG == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_SANG", prop = round((n / nrow(proc_72) * 100), 2)),
  
  proc_72 %>%
    summarise(n = sum(PROC_SEMEN == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_SEMEN", prop = round((n / nrow(proc_72) * 100), 2)),
  
  proc_72 %>%
    summarise(n = sum(PROC_DST == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_DST", prop = round((n / nrow(proc_72) * 100), 2)),
  
  proc_72 %>%
    summarise(n = sum(PROC_HEPB == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_HEPB", prop = round((n / nrow(proc_72) * 100), 2)),
  
  proc_72 %>%
    summarise(n = sum(PROC_VAGIN == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_VAGIN", prop = round((n / nrow(proc_72) * 100), 2)),
  
  proc_72 %>%
    summarise(n = sum(PROC_CONTR == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_CONTR", prop = round((n / nrow(proc_72) * 100), 2)),
  
  proc_72 %>%
    summarise(n = sum(PROC_ABORT == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_ABORT", prop = round((n / nrow(proc_72) * 100), 2)),
  
  proc_72 %>%
    summarise(n = sum(PROC_HIV == 1, na.rm = TRUE)) %>%
    mutate(var = "PROC_HIV", prop = round((n / nrow(proc_72) * 100), 2))
  
) %>%
  arrange(var) %>%
  select(var, n, prop)

# Exibir a tabela
print(tabela_frequencia)

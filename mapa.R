# Instalar pacotes
#install.packages(c("sf", "ggplot2", "geobr"))

library(sf)
library(ggplot2)
library(geobr)  # pacote com dados geográficos do Brasil

municipios_pe <- read_municipality(code_muni = "PE", year = 2020)

# Remove Fernando de Noronha para evitar distorções na imagem
municipios_pe <- subset(municipios_pe, name_muni != "Fernando De Noronha")

recife <- subset(municipios_pe, name_muni == "Recife")

# Cria o mapa do estado de Pernambuco
ggplot() +
  geom_sf(data = municipios_pe, fill = "white", color = "black", size = 0.2) +
  geom_sf(data = recife, fill = "#A52A2A", color = "black", size = 0.5) +
  theme_void() 

# Filtra Recife e municípios de fronteira
recife_metro <- subset(municipios_pe, name_muni %in% c("Recife",
                                                "Jaboatão Dos Guararapes",
                                                "Olinda",
                                                "Camaragibe",
                                                "Paulista",
                                                "São Lourenço Da Mata"
                                                ))

# Criar coluna para destacar Recife
recife_metro <- recife_metro %>% 
  mutate(cor_muni = ifelse(name_muni == "Recife", "Recife", "Outros"))
                 
# Calcular centroides para posicionar os nomes dos municípios
centroids <- st_centroid(recife_metro)
              
                 
# Mapa do Recife
ggplot() +
  geom_sf(data = recife_metro, aes(fill = cor_muni), color = "black", size = 0.3) +
  scale_fill_manual(values = c("Recife" = "#A52A2A", "Outros" = "#E07B39")) +
  geom_sf_text(data = centroids, aes(label = name_muni), size = 4) +
  annotate("text", x = -34.82, y = -8.2, label = "Oceano Atlântico", angle = 90, size = 4, hjust = 0) +
  theme_void() +
  theme(legend.position = "none")

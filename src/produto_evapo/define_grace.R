
#%% --------------------- subir as bibliotecas 

library(sf)
library(ggplot2)
library(cowplot)
library(purrr)
library(tidyr)
library(terra)
library(forcats)
library(ncdf4)
library(parallel)
library(patchwork)
library(whitebox)
library(lubridate)
library(dplyr)
library(zoo)
#source("funcoes_balanco.R")


#%% --------------------- Importar arquivos espaciais
load("/mnt/externo/mestrado_note/evapotranspiration_basin/data/grace/2026_recortado.bin")
#transformar em sf
sf_grid_coords <- st_as_sf(df_grid_coords, coords = c("lon", "lat"), crs = 4326)

# %% # definindo a bacia

##args <- list("Alto_Iguacu","estacao_26025035_tele_flu" ) 
##args <- list("Ivai", "estacao_64693000_conv_flu")
##args <- list("Piquiri", "estacao_64890000_conv_flu")
##args <- list("Tibagi", "estacao_64506000_conv_flu")
##args <- list("Medio_Iguacu", "estacao_65883051_conv_flu") 
args <- list("Baixo_Iguacu", "estacao_25345435_tele_flu") 
cod_bacia<-args[[1]]

#%%  Ajeitando df_plu
bacia <- st_read(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/", cod_bacia,"/whitebox_output/",args[2],"/delimitada.shp"))
bacia<- st_make_valid(bacia)
pasta_saida <- file.path(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/", cod_bacia))

#%% 3. CONSTRUÇÃO DOS POLÍGONOS DAS CÉLULAS E RECORTE NA BACIA
coords_geo <- st_coordinates(sf_grid_coords)
res_lon <- min(diff(sort(unique(coords_geo[, "X"]))))
res_lat <- min(diff(sort(unique(coords_geo[, "Y"]))))

# Gera as células regulares retangulares em graus
poligonos_lista <- lapply(1:nrow(coords_geo), function(i) {
  x <- coords_geo[i, "X"]
  y <- coords_geo[i, "Y"]
  st_polygon(list(rbind(
    c(x - res_lon / 2, y - res_lat / 2),
    c(x - res_lon / 2, y + res_lat / 2),
    c(x + res_lon / 2, y + res_lat / 2),
    c(x + res_lon / 2, y - res_lat / 2),
    c(x - res_lon / 2, y - res_lat / 2)
  )))
})

grid_poligonos_geo <- st_sf(
  grid_id  = sf_grid_coords$grid_id,
  geometry = st_sfc(poligonos_lista, crs = 4326)
)

# Projeta para UTM 31983 para cálculo métrico real de áreas
epsg_utm <- 31983
bacia_utm <- st_transform(bacia, epsg_utm)
grid_poligonos_utm <- st_transform(grid_poligonos_geo, epsg_utm)

area_total_bacia_m2 <- sum(as.numeric(st_area(bacia_utm)), na.rm = TRUE)

# Interseção das células com a bacia (corta bordas parciais)
intersecao_grade <- suppressWarnings(st_intersection(grid_poligonos_utm, bacia_utm)) %>%
  st_make_valid() %>%
  filter(!st_is_empty(.)) %>%
  mutate(area_parcial_m2 = as.numeric(st_area(geometry))) %>%
  filter(area_parcial_m2 > 1) # Descarta artefatos numéricos insignificantes

# Calibração geodésica para garantir conservação estrita de volume
fator_calib <- area_total_bacia_m2 / sum(intersecao_grade$area_parcial_m2, na.rm = TRUE)
intersecao_grade <- intersecao_grade %>%
  mutate(area_ajustada_m2 = area_parcial_m2 * fator_calib)

#%%  4. PONDERAÇÃO TEMPORAL ESPACIAL (PRODUTO MATRICIAL)
celulas_alvo <- intersecao_grade$grid_id
pesos_area   <- intersecao_grade$area_ajustada_m2 / sum(intersecao_grade$area_ajustada_m2)

# Filtra colunas do histórico e calcula lâmina ponderada
# Multiplicação por 10: conversão de cm (EWH) para mm
matriz_grace <- as.matrix(df_grid_historico[, celulas_alvo])
# Aplica a média ponderada mês a mês ignorando NAs pontuais
twsa_bacia_cm <- apply(matriz_grace, 1, function(valores_mes) {
  validos <- !is.na(valores_mes)
  
  if (!any(validos)) {
    return(NA_real_)
  }
  
  # Média ponderada apenas sobre células com dados válidos
  weighted.mean(valores_mes[validos], w = pesos_area[validos])
})

# Converte de cm (EWH) para mm (* 10)
df_grace_bacia <- data.frame(
  data          = df_grid_historico$data,
  grace = twsa_bacia_cm * 10
)

# Visualiza as primeiras linhas
head(df_grace_bacia)

#%%5. DIAGNÓSTICO VISUAL: MAPA DE CÉLULAS E SÉRIE TEMPORAL

# 5.1 Mapa de verificação espacial
f_mapa <- file.path(pasta_saida, paste0(cod_bacia, "_mapa_cobertura_grace.png"))
png(f_mapa, width = 1800, height = 1400, res = 200)

bbox <- st_bbox(bacia)
xlim <- c(bbox["xmin"] - 0.4, bbox["xmax"] + 0.4)
ylim <- c(bbox["ymin"] - 0.4, bbox["ymax"] + 0.4)

plot(st_geometry(bacia), col = "gray95", border = "black", lwd = 2,
     xlim = xlim, ylim = ylim, axes = TRUE,
     main = paste0("Cobertura GRACE - Bacia ", cod_bacia))
plot(st_geometry(st_transform(grid_poligonos_utm, 4326)), border = "gray75", lty = "dotted", add = TRUE)
plot(st_geometry(st_transform(intersecao_grade, 4326)), col = rgb(0.2, 0.4, 0.8, 0.35), border = "blue", add = TRUE)
plot(st_geometry(bacia), col = NA, border = "black", lwd = 2, add = TRUE)

legend("topright",
       legend = c("Limite da Bacia", "Grade Completa", paste0("Células na Bacia (n = ", nrow(intersecao_grade), ")")),
       col = c("black", "gray75", "blue"),
       pch = c(NA, NA, 15),
       lty = c(1, 3, NA),
       lwd = c(2, 1, NA),
       bty = "n")
dev.off()

# 5.2 Gráfico da série temporal
p_serie <- ggplot(df_grace_bacia, aes(x = data, y = twsa_grace_mm)) +
  geom_line(color = "#2b5c8f", linewidth = 0.8) +
  geom_point(color = "#2b5c8f", size = 1.2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = paste0("Anomalia de Armazenamento Terrestre (TWSA) - ", cod_bacia),
    subtitle = paste("Exutória:", estacao_id, "| Média ponderada pela área real"),
    x = "Data",
    y = "TWSA (mm)"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  filename = file.path(pasta_saida, paste0(cod_bacia, "_", estacao_id, "_serie_twsa.png")),
  plot     = p_serie,
  width    = 9,
  height   = 4.5,
  dpi      = 300
)




#%% salvar

dir_dados <- file.path(pasta_saida, "df_data")
if (!dir.exists(dir_dados)) dir.create(dir_dados, recursive = TRUE)

# Salva o arquivo no destino
save(df_grace_bacia, file = file.path(dir_dados, "df_grace_bacia.bin"))
#%% 
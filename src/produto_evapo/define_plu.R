
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
source("funcoes_balanco.R")


#%% --------------------- Importar arquivos espaciais

load("/mnt/externo/mestrado_note/evapotranspiration_basin/data/precipitacao_vazao/df_chuva_espacializada_v4.bin")
load("/mnt/externo/mestrado_note/evapotranspiration_basin/data/precipitacao_vazao/sf_grid_interpolado.bin")

#%% definindo a bacia
#commandArgs(trailingOnly = TRUE)
#é a estação flu exutoria

#args <- list("Alto_Iguacu","estacao_26025035_tele_flu" ) 
#args <- list("Ivai", "estacao_64693000_conv_flu")
#args <- list("Piquiri", "estacao_64890000_conv_flu")
#args <- list("Tibagi", "estacao_64506000_conv_flu")
#args <- list("Medio_Iguacu", "estacao_65883051_conv_flu") 
#args <- list("Baixo_Iguacu", "estacao_25345435_tele_flu") 
cod_bacia<-args[[1]]

#%% --------------------- Ajeitando df_plu
bacia <- st_read(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/", cod_bacia,"/whitebox_output/",args[2],"/delimitada.shp"))
bacia<- st_make_valid(bacia)
pontos_na_bacia <- sf_grid_interpolado %>%  st_filter(bacia, .predicate = st_intersects)
pasta_saida <- file.path(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/", cod_bacia))

#plot
{ png(file.path(pasta_saida, paste0(cod_bacia, "_plu_grid_bacia.png")), width = 2000, height = 1500, res = 200)
# 1. Bounding box da bacia com margem de 0.5°
bbox <- st_bbox(bacia)
xlim <- c(bbox["xmin"], bbox["xmax"])
ylim <- c(bbox["ymin"] - 0.1, bbox["ymax"] + 0.1)

# 2. Plota primeiro o polígono da bacia para inicializar os eixos
plot( st_geometry(bacia), 
  col    = "gray95", 
  border = "black", 
  lwd    = 2,
  xlim   = xlim, 
  ylim   = ylim,
  axes   = TRUE,
  main   = paste("Pontos de precipitação -", cod_bacia)
)

plot(st_geometry(sf_grid_interpolado), col = "grey65", pch = 20, cex = 0.6, add = TRUE)
plot(st_geometry(bacia), col = NA, border = "black", lwd = 2, add = TRUE)
plot(st_geometry(pontos_na_bacia), col = "red", pch = 19, cex = 0.8, add = TRUE)

legend( "topright",
  legend = c("Limite da Bacia", "Grid Geral", paste0("Grid na Bacia (n = ", nrow(pontos_na_bacia), ")")),
  col    = c("black", "grey65", "red"),
  pch    = c(NA, 20, 19),
  lty    = c(1, NA, NA),
  lwd    = c(2, NA, NA),
  bty    = "n",
  bg     = "white",
  xpd    = TRUE
)
dev.off()
}

df_plu_bacia <- df_grid_interpolado[, c("Data", pontos_na_bacia$grid_id)]

#%% Calculando a media da bacia
media_bacia_plu <- data.frame(Data= df_plu_bacia$Data, plu_med=(rowMeans(df_plu_bacia[ , -1], na.rm = TRUE)))
df_plu_bacia<- media_bacia_plu
#%% salvando
dir_dados <- file.path(pasta_saida, "df_data")
if (!dir.exists(dir_dados)) dir.create(dir_dados, recursive = TRUE)

# Salva o arquivo no destino
save(df_plu_bacia, file = paste0(dir_dados,"/df_plu_bacia.bin"))






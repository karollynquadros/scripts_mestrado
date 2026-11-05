#' Extrai serie temporal media de um SpatRaster para um vetor de bacia
#' 
#' @param raster_obj SpatRaster carregado ou caminho do arquivo .nc
#' @param vetor sf ou SpatVector delimitando a bacia
#' @param nome_coluna Nome desejado para a coluna de saida de valores
#' @return Um tibble com colunas `date` e o nome definido em `nome_coluna`
#' 

#%% --------------------- subir as bibliotecas 
library(terra)
library(dplyr)
library(tibble)
#%%

extrair_serie_bacia <- function(raster_obj, vetor, nome_coluna = "valor") {
  
  # Se for passado o caminho em texto, carrega o raster
  if (is.character(raster_obj)) {
    raster_obj <- rast(raster_obj)
  }
  
  # Garante que o vetor esteja como SpatVector
  vetor_spat <- if (inherits(vetor, "sf")) vect(vetor) else vetor
  
  # 1. Recorte pela extensao da bacia para economizar memoria e processamento
  raster_crop <- crop(raster_obj, vetor_spat)
  
  # 2. Extracao com media ponderada pela fracao de cobertura do pixel
  valores <- extract(
    raster_crop, 
    vetor_spat, 
    fun = mean, 
    na.rm = TRUE, 
    weights = TRUE
  )
  
  # 3. Extrai as datas associadas as camadas
  datas <- as.Date(time(raster_obj))
  
  # 4. Monta a tibble final (exclui a coluna 1 que e o ID do poligono)
  res <- tibble(
    date = datas,
    !!nome_coluna := as.numeric(valores[1, -1])
  )
  
  return(res)
}


#%% --------------------- Importar arquivos espaciais
pasta_saida <- file.path("/mnt/externo/mestrado_note/evapotranspiration_basin/data/xavier")

serie_et <- rast(paste0(pasta_saida,"ETo_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc"))
serie_tmin<-rast(paste0(pasta_saida,"Tmin_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc"))
serie_tmax<- rast(paste0(pasta_saida,"Tmax_20010101_20251231_BR-DWGD_UFES_UTEXAS_v_3.2.4.nc"))

# %% # definindo a bacia

args <- list("Alto_Iguacu","estacao_26025035_tele_flu" ) 
##args <- list("Ivai", "estacao_64693000_conv_flu")
##args <- list("Piquiri", "estacao_64890000_conv_flu")
##args <- list("Tibagi", "estacao_64506000_conv_flu")
##args <- list("Medio_Iguacu", "estacao_65883051_conv_flu") 
#args <- list("Baixo_Iguacu", "estacao_25345435_tele_flu") 
cod_bacia<-args[[1]]

bacia <- st_read(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/", cod_bacia,"/whitebox_output/",args[2],"/delimitada.shp"))
bacia<- st_make_valid(bacia)
#add o nome da bacia no arquivo

#%%
# 1. Extrair cada uma das séries usando a função geral
df_tmin <- extrair_serie_bacia(serie_tmin, bacia, nome_coluna = "tmin")
df_tmax <- extrair_serie_bacia(serie_tmax, bacia, nome_coluna = "tmax")
df_eto  <- extrair_serie_bacia(serie_et,   bacia, nome_coluna = "eto")

# 2. Juntar tudo pelo campo de data
df_xavier <- df_tmax %>%
  left_join(df_tmin, by = "date") %>%
  left_join(df_eto,  by = "date") %>%
  rename(data = date) # renomeia para 'data' conforme você pediu


#%% salvar
dir_dados <- file.path(pasta_saida, "df_data")
if (!dir.exists(dir_dados)) dir.create(dir_dados, recursive = TRUE)

save( df_xavier, file = file.path(dir_dados, paste0("df_xavier_", cod_bacia, ".bin")))

#%%

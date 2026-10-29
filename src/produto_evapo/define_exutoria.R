#commandArgs(trailingOnly = TRUE)

#args <- list("Alto_Iguacu", c("Alto_Iguacu", "Medio_Iguacu")) nome_bacia_atual <- args[[1]]
args <- list("Ivai", "Ivai")

cod_bacia<-args[[1]]
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


#%% --------------------- Importar arquivos para definir e consistir a exutoria da bacia

load("/mnt/externo/mestrado_note/evapotranspiration_basin/data/precipitacao_vazao/inventario.bin")
load("/mnt/externo/mestrado_note/evapotranspiration_basin/data/precipitacao_vazao/df_flu.bin")
### verificar na fonte do dado
names(df_flu) <- make.unique(names(df_flu))
inventario_sf <- st_as_sf(inventario,coords = c("Longitude", "Latitude"),crs = 4326)
bacias <- st_read("/mnt/externo/mestrado_note/evapotranspiration_basin/data/shapes/bacias_parana/bacias_agregadas/bacias_agregadas.shp") %>% st_transform(4326)
r <- rast("/mnt/externo/mestrado_note/evapotranspiration_basin/data/shapes/mesclado.tiff")
pasta_saida <- file.path(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/", cod_bacia))
if (!dir.exists(pasta_saida)) {dir.create(pasta_saida, recursive = TRUE);  message("Pasta criada: ", pasta_saida)}



#%% --------------------- Ajeitando df_flu
df_codigos_flu <- st_transform(inventario_sf[inventario_sf$Tipo_Estacao == "Fluviométrica", ], st_crs(bacia))


#%% --------------------- REPROCESSAR pra cada bacia separando as estações flu na bacia  #pontos_na_bacia

#define a bacia
bacia <- bacias[bacias$bacia_nome %in% args[[2]], ]
cod_bacia <- args[1]
r <- rast("/mnt/externo/mestrado_note/evapotranspiration_basin/data/shapes/mesclado.tiff")
r_recorte <- mask(crop(r, vect(bacia)), vect(bacia))

#Atribui cota ao pontos_na_bacia 
pontos_na_bacia <- df_codigos_flu %>%  st_join(bacia, join = st_within) %>% filter(!is.na(bacia_nome))
pontos_na_bacia$Cota <- (terra::extract(r_bacia, vect(pontos_na_bacia)))[[2]]

#Atribuir disponibilidade ao pontos_na_bacia 
df_flu_bacia <- df_flu[, c("Data", pontos_na_bacia$Codigo)]
df_flu_long <- df_flu_bacia %>%  pivot_longer(-Data, names_to = "estacao", values_to = "valor") %>% mutate(tem_dado = !is.na(valor))
disp_estacoes <- df_flu_long %>%  group_by(estacao) %>%  summarise(n_dias = sum(tem_dado, na.rm = TRUE)) %>% ungroup()
pontos_na_bacia <- pontos_na_bacia %>%  left_join(disp_estacoes, by = c("Codigo" = "estacao"))


#%% --------------------- REPROCESSAR escolher a estação exutoria po disponibilidade e cota
df_codigos_flu_estacao <- pontos_na_bacia %>%
  filter(!is.na(Cota), !is.na(n_dias)) %>%
  arrange(Cota, desc(n_dias)) 
#%% --------------------- REPROCESSAR para cada bacia 
if (cod_bacia == "Alto_Iguacu") {

# Estações que serão a exutoria da bacia:
df_estacao_35 <- df_codigos_flu_estacao %>% slice(35)
df_estacao_41 <- df_codigos_flu_estacao %>% slice(41)

estacao_escolhida <- df_estacao_35$Codigo

serie_alvo<- df_flu[,c("Data",df_estacao_35$Codigo ,df_estacao_41$Codigo) ]
serie_alvo <- serie_alvo %>%   mutate(`26025035_tele_flu` = coalesce(`26025035_tele_flu`, `65220001_conv_flu`), `65220001_conv_flu` = NULL)

###Como tem NA faremos o preenchimento com as afluentes
# --- Vertente 1 ---
{
  est_51 <- (df_codigos_flu_estacao %>% slice(51))$Codigo
df_51 <- df_flu[, c("Data", est_51)]

est_75 <- (df_codigos_flu_estacao %>% slice(75))$Codigo
df_75 <- df_flu[, c("Data", est_75)]

est_bacia1 <- est_75
df_bacia1 <- full_join(df_75, df_51, by = "Data")
val_b1 <- rowMeans(df_bacia1[, c(est_75, est_51)], na.rm = TRUE)
val_b1[is.nan(val_b1)] <- NA_real_

df_bacia1 <- data.frame(Data = df_bacia1$Data, check.names = FALSE)
df_bacia1[[est_bacia1]] <- val_b1
}
# --- Vertente 2 ---
{
est_50 <- (df_codigos_flu_estacao %>% slice(50))$Codigo

est_bacia2 <- est_50
val_b2 <- df_flu[[est_50]]
val_b2 <- if_else(val_b2 < 0, NA_real_, val_b2)

df_bacia2 <- data.frame(Data = df_flu$Data, check.names = FALSE)
df_bacia2[[est_bacia2]] <- val_b2
}
# --- Vertente 3 ---
{
est_42 <- (df_codigos_flu_estacao %>% slice(42))$Codigo
df_42 <- df_flu[, c("Data", est_42)]

est_46 <- (df_codigos_flu_estacao %>% slice(46))$Codigo
df_46 <- df_flu[, c("Data", est_46)]

est_bacia3 <- est_42
df_bacia3 <- full_join(df_42, df_46, by = "Data")
val_b3 <- rowMeans(df_bacia3[, c(est_42, est_46)], na.rm = TRUE)
val_b3[is.nan(val_b3)] <- NA_real_

df_bacia3 <- data.frame(Data = df_bacia3$Data, check.names = FALSE)
df_bacia3[[est_bacia3]] <- val_b3
}
# 4. Definição das estações de interesse
estacao_exutoria  <- estacao_escolhida
estacoes_vertente <- c(est_bacia1, est_bacia2, est_bacia3)  

# 5. Montar as séries auxiliares/vertentes (nomes das colunas iguais aos códigos de df_areas)
series_aux <- df_bacia1 %>%
  left_join(df_bacia2, by = "Data") %>%
  left_join(df_bacia3, by = "Data")

necessario_consistir <- TRUE
} 

if (cod_bacia == "Ivai") { ## vai até dia 2024-04-30

# Estações que serão a exutoria da bacia:
df_estacao_2 <- df_codigos_flu_estacao %>% slice(2)
estacao_escolhida <- df_estacao_2$Codigo
serie_alvo<- df_flu[, c('Data',df_estacao_2$Codigo)]

necessario_consistir <- FALSE
estacao_exutoria  <- estacao_escolhida
estacoes_vertente <- NULL 
series_aux <- NULL


}



{ #plots teste
#para teste:
library(ggplot2)
library(dplyr)

# Pega o nome da coluna de vazão automaticamente
col_vazao <- setdiff(names(serie_alvo), "Data")[1]

ggplot(serie_alvo, aes(x = Data)) +
  # Linha da série observada
  geom_line(aes(y = .data[[col_vazao]]), color = "#2c3e50", na.rm = TRUE) +
  # Linhas verticais vermelhas marcando cada dia com NA
  geom_linerange(
    data = serie_alvo %>% filter(is.na(.data[[col_vazao]])),
    aes(ymin = 0, ymax = max(serie_alvo[[col_vazao]], na.rm = TRUE)),
    color = "red", alpha = 0.5, linewidth = 0.5
  ) +
  labs(
    title = paste("Série Temporal com Destaque de Falhas (NAs):", col_vazao),
    subtitle = paste("Total de falhas:", sum(is.na(serie_alvo[[col_vazao]])), "dias"),
    x = "Ano",
    y = expression(Vazão~(m^3/s))
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal(base_size = 12)
}



# %% --------------------------------  VOlta generico

# 1. Definir a pasta do Whitebox dentro da pasta de saída da bacia atual
{
dir_wbt <- file.path(pasta_saida, "whitebox_output")
if (!dir.exists(dir_wbt)) dir.create(dir_wbt, recursive = TRUE)

# 2. Recorte e salvamento do MDE dentro da pasta da bacia
f_dem_recorte <- file.path(dir_wbt, "dem_recorte.tif")

writeRaster(r_recorte, f_dem_recorte, overwrite = TRUE)
rm(r, r_recorte); gc()
}
# 3. Base hidrológica regional dentro da pasta da bacia
preparar_base_hidro( r_mde    = f_dem_recorte, dir_base = dir_wbt, threshold = 5000)

# 4. Definição das estações de interesse
todas_estacoes    <- c(estacao_exutoria, estacoes_vertente)

# 5. Delimitação e cálculo de área no mesmo diretório
df_areas <- obter_areas_lote(codigos = todas_estacoes,df_estacoes = df_codigos_flu_estacao, dir_base = dir_wbt)

# 6. Executar o preenchimento de falhas genérico
if (necessario_consistir) {
  message("[Consistência] Executando preenchimento de falhas por regressão...")
  dados_finais <- preencher_falhas_regressao_generica(
    df_alvo       = serie_alvo,
    df_auxiliares = series_aux,
    df_areas      = df_areas,
    cod_alvo      = estacao_exutoria
  )
} else {
  message("[Consistência] Bacia sem necessidade de consistência. Mantendo série original.")
  
  # Garante que a coluna de vazão se chame 'exult' caso ainda esteja com o código da estação
  col_val <- setdiff(names(serie_alvo), "Data")[1]
  val_original <- serie_alvo[[col_val]]
  
  # Cria dados_finais com as mesmas colunas esperadas pelas funções seguintes
  dados_finais <- data.frame(
    Data       = serie_alvo$Data,
    original   = val_original,
    preenchido = val_original,
    completo   = val_original
  )
}
## preenchido é com a regressão linear multipla e completo é com a regressao linear simples POR CIMA de tudo

#%% --------------------- Executa a geração dos 2 mapas, o gráfico temporal e a exportação CSV

dados_mm <- gerar_produtos_hidro_e_plots(
  nome_bacia             = cod_bacia,
  estacao_exutoria       = estacao_exutoria,
  todas_estacoes         = todas_estacoes,
  dados_finais           = dados_finais,
  df_areas               = df_areas,
  df_codigos_flu_estacao = df_codigos_flu_estacao,
  pontos_na_bacia        = pontos_na_bacia,
  bacia_limite           = bacia,
  dir_wbt                = dir_wbt,
  pasta_saida            = pasta_saida,
  bacias_shape           = bacias,
  ufs_shape              = UFs
)

#%% --------------------- salva em df_dados

df_flu_bacia <- dados_finais %>% select(Data, exult = completo_mm)

# Define o caminho da subpasta e cria recursivamente se não existir
dir_dados <- file.path(pasta_saida, "df_data")
if (!dir.exists(dir_dados)) dir.create(dir_dados, recursive = TRUE)

# Salva o arquivo no destino
save(df_flu_bacia, file = file.path(dir_dados, "df_flu_bacia.bin"))


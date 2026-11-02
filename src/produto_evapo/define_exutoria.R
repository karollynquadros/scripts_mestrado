#commandArgs(trailingOnly = TRUE)

#args <- list("Alto_Iguacu", c("Alto_Iguacu", "Medio_Iguacu")) 
#args <- list("Ivai", "Ivai")
args <- list("Piquiri", "Piquiri")
#args <- list("Tibagi", "Tibagi")
#args <- list("Medio_Iguacu", c("Medio_Iguacu", "Baixo_Iguacu")) 
#args <- list("Baixo_Iguacu", c("Baixo_Iguacu")) 
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
UFs<- st_read("/mnt/externo/mestrado_note/mestrado_2026/shapes/BR_UF_2022/BR_UF_2022.shp")

### verificar na fonte do dado
names(df_flu) <- make.unique(names(df_flu))

inventario_sf <- st_as_sf(inventario,coords = c("Longitude", "Latitude"),crs = 4326)
# corrigir coordenada #Piquiri
idx <- which(inventario_sf$Codigo == "64830000_conv_flu")
# Lembrete: X = Longitude (-53.7458), Y = Latitude (-24.1871)
inventario_sf$geometry[idx] <- st_sfc(st_point(c(-53.7458, -24.1871)), crs = st_crs(inventario_sf))

# corrigir coordenada #Tibagi
idx <- which(inventario_sf$Codigo == "64506000_conv_flu")
# Lembrete: X = Longitude (-50.9950), 
inventario_sf$geometry[idx] <- st_sfc(st_point(c(-50.9950, st_coordinates(inventario_sf$geometry[idx])[, "Y"])), crs = st_crs(inventario_sf))


# talvez precise disso: coords[1, 1] <-  
# Estações que serão a exutoria da bacia:
bacias <- st_read("/mnt/externo/mestrado_note/evapotranspiration_basin/data/shapes/bacias_parana/bacias_agregadas/bacias_agregadas.shp") %>% st_transform(4326)
r <- rast("/mnt/externo/mestrado_note/evapotranspiration_basin/data/shapes/mesclado.tiff")



#%% --------------------- Ajeitando df_flu
bacia <- bacias[bacias$bacia_nome %in% args[[2]], ]
df_codigos_flu <- st_transform(inventario_sf[inventario_sf$Tipo_Estacao == "Fluviométrica", ], st_crs(bacia))

#%% --------------------- REPROCESSAR pra cada bacia separando as estações flu na bacia  #pontos_na_bacia
#define a bacia

pasta_saida <- file.path(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/", cod_bacia))
if (!dir.exists(pasta_saida)) {dir.create(pasta_saida, recursive = TRUE);  message("Pasta criada: ", pasta_saida)}
r_recorte <- mask(crop(r, vect(bacia)), vect(bacia))

#Atribui cota ao pontos_na_bacia 
pontos_na_bacia <- df_codigos_flu %>%  st_join(bacia, join = st_within) %>% filter(!is.na(bacia_nome))
pontos_na_bacia$Cota <- (terra::extract(r_recorte, vect(pontos_na_bacia)))[[2]]

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
series_aux <- NULL}

if (cod_bacia == "Piquiri") { 

# Estações que serão a exutoria da bacia:
df_estacao_1 <- df_codigos_flu_estacao %>% slice(1)
estacao_escolhida <- df_estacao_1$Codigo
serie_alvo<- df_flu[, c('Data',df_estacao_1$Codigo)]

necessario_consistir <- FALSE
estacao_exutoria  <- estacao_escolhida
estacoes_vertente <- NULL 
series_aux <- NULL
}

if (cod_bacia == "Tibagi") { 

df_estacao_4 <- df_codigos_flu_estacao %>% slice(4)
estacao_escolhida <- df_estacao_4$Codigo
serie_alvo<- df_flu[, c('Data',estacao_escolhida)]

necessario_consistir <- FALSE
estacao_exutoria  <- estacao_escolhida
estacoes_vertente <- NULL 
series_aux <- NULL
}

if (cod_bacia == "Medio_Iguacu") { 
#complementares
df_estacao_77 <- df_codigos_flu_estacao %>% slice(77)
serie_alvo_77<- df_flu[, c('Data',df_estacao_77$Codigo)]
estacao_escolhida<- df_estacao_77$Codigo

df_estacao_78 <- df_codigos_flu_estacao %>% slice(78)
cod_78  <- df_estacao_78$Codigo
serie_alvo_78<- df_flu[, c('Data',df_estacao_78$Codigo)]

serie_alvo <- tibble(Data = df_flu$Data,"65883051_conv_flu" = coalesce(df_flu[[cod_78]], df_flu[[estacao_escolhida]]))

###Como tem NA faremos o preenchimento com as afluentes
# --- Vertente 1 ---
{
est_51 <- (df_codigos_flu_estacao %>% slice(51))$Codigo
df_51 <- df_flu[, c("Data", est_51)]

est_52 <- (df_codigos_flu_estacao %>% slice(52))$Codigo
df_52 <- df_flu[, c("Data", est_52)]

est_bacia1 <- est_51
df_bacia1 <- full_join(df_51, df_52, by = "Data")
val_b1 <- rowMeans(df_bacia1[, c(est_51, est_52)], na.rm = TRUE)
val_b1[is.nan(val_b1)] <- NA_real_

df_bacia1 <- data.frame(Data = df_bacia1$Data, check.names = FALSE)
df_bacia1[[est_bacia1]] <- val_b1
}
# --- Vertente 2 ---
{
est_97 <- (df_codigos_flu_estacao %>% slice(97))$Codigo
est_bacia2 <- est_97
val_b2 <- df_flu[[est_97]]
val_b2 <- if_else(val_b2 < 0, NA_real_, val_b2)

df_bacia2 <- data.frame(Data = df_flu$Data, check.names = FALSE)
df_bacia2[[est_bacia2]] <- val_b2
}

# 4. Definição das estações de interesse
estacao_exutoria  <- estacao_escolhida
estacoes_vertente <- c(est_bacia1, est_bacia2)  

# 5. Montar as séries auxiliares/vertentes (nomes das colunas iguais aos códigos de df_areas)
series_aux <- df_bacia1 %>%
  left_join(df_bacia2, by = "Data")

necessario_consistir <- TRUE
} 

if (cod_bacia == "Baixo_Iguacu") {

# Estações que serão a exutoria da bacia:
df_estacao_10 <- df_codigos_flu_estacao %>% slice(10)
estacao_escolhida <- df_estacao_10$Codigo
serie_alvo<- df_flu[, c('Data',df_estacao_10$Codigo)]

###Como tem NA faremos o preenchimento com as afluentes
# --- Vertente 1 ---
{
est_14 <- (df_codigos_flu_estacao %>% slice(14))$Codigo
df_14 <- df_flu[, c("Data", est_14)]

est_15 <- (df_codigos_flu_estacao %>% slice(15))$Codigo
df_15 <- df_flu[, c("Data", est_15)]

est_bacia1 <- est_14
df_bacia1 <- full_join(df_14, df_15, by = "Data")
val_b1 <- rowMeans(df_bacia1[, c(est_14, est_15)], na.rm = TRUE)
val_b1[is.nan(val_b1)] <- NA_real_

df_bacia1 <- data.frame(Data = df_bacia1$Data, check.names = FALSE)
df_bacia1[[est_bacia1]] <- val_b1
}

# 4. Definição das estações de interesse
estacao_exutoria  <- estacao_escolhida
estacoes_vertente <-est_bacia1  

# 5. Montar as séries auxiliares/vertentes (nomes das colunas iguais aos códigos de df_areas)
series_aux <- df_bacia1

necessario_consistir <- TRUE
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
#rm(r, r_recorte); gc()
}
# 3. Base hidrológica regional dentro da pasta da bacia
preparar_base_hidro( r_mde    = f_dem_recorte, dir_base = dir_wbt, threshold = 5000)

# 4. Definição das estações de interesse
todas_estacoes    <- c(estacao_exutoria, estacoes_vertente)

# 5. Delimitação e cálculo de área no mesmo diretório
df_areas <- obter_areas_lote(codigos = todas_estacoes,df_estacoes = df_codigos_flu_estacao, dir_base = dir_wbt)

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
  
  # Garante que acha a coluna de vazão mesmo se tiver o código numérico da estação
  col_val <- setdiff(names(serie_alvo), "Data")[1]
  val_original <- serie_alvo[[col_val]]
  
  # Cria a mesma estrutura de colunas que a função de regressão retorna
  dados_finais <- data.frame(
    Data       = serie_alvo$Data,
    original   = val_original,
    preenchido = val_original,
    completo   = val_original
  )
}

area_exut_km2 <- df_areas$area_km2[df_areas$Codigo == as.character(estacao_exutoria)]
if (length(area_exut_km2) == 0 || is.na(area_exut_km2)) {
  stop("Área da exutória ", estacao_exutoria, " não encontrada em df_areas!")
}

area_exut_m2 <- area_exut_km2 * 1e6
fator_mm_dia <- (86400 / area_exut_m2) * 1000

# Agora 'dados_finais' terá SEMPRE as colunas em m³/s e em mm/dia
dados_finais <- dados_finais %>%
  mutate(
    original_mm   = original * fator_mm_dia,
    preenchido_mm = preenchido * fator_mm_dia,
    completo_mm   = completo * fator_mm_dia
  )

df_flu_bacia <- dados_finais %>%
  select(Data, exult = completo_mm)
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

#%%
df_flu_bacia <- dados_finais %>% select(Data, exult = completo_mm)

# Define o caminho da subpasta e cria recursivamente se não existir
dir_dados <- file.path(pasta_saida, "df_data")
if (!dir.exists(dir_dados)) dir.create(dir_dados, recursive = TRUE)

if(cod_bacia == "Medio_Iguacu") { 
df_flu_bacia_medio_iguacu<-df_flu_bacia
load("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/Alto_Iguacu/df_data/df_flu_bacia.bin")
df_flu_bacia_alto_iguacu <-df_flu_bacia

df_flu_bacia_medio_iguacu$exult <- df_flu_bacia_medio_iguacu$exult - df_flu_bacia_alto_iguacu$exult
df_flu_bacia<-df_flu_bacia_medio_iguacu
}

if(cod_bacia == "Baixo_Iguacu") { 
df_flu_bacia_baixo_iguacu<-df_flu_bacia
load("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/Medio_Iguacu/df_data/df_flu_bacia.bin")
df_flu_bacia_medio_iguacu <-df_flu_bacia

df_flu_bacia_baixo_iguacu$exult <- df_flu_bacia_baixo_iguacu$exult - df_flu_bacia_medio_iguacu$exult
df_flu_bacia<-df_flu_bacia_baixo_iguacu
}
# Salva o arquivo no destino
save(df_flu_bacia, file = file.path(dir_dados, "df_flu_bacia.bin"))




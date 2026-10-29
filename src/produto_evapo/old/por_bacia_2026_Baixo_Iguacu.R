library(dplyr)
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
######### DEIXAR TODAS VARIAVEIS DIVIDIDAS PELA AREA DA BACIA
load("../../precipitacao_vazao/espacializado_FINAL/df_chuva_espacializada.bin")
load("../../precipitacao_vazao/espacializado_FINAL/grade_chuva_espacializada.bin")
load("../../precipitacao_vazao/espacializado_FINAL/inventario.bin")
load("../../precipitacao_vazao/espacializado_FINAL/df_flu.bin")

contorno_grade <- st_read("/mnt/externo/mestrado_note/mestrado_2026/shapes/contorno_grade/contorno_grade_parana.shp")
UFs            <- st_read("/mnt/externo/mestrado_note/mestrado_2026/shapes/BR_UF_2022/BR_UF_2022.shp")

inventario_sf <- st_as_sf(
  inventario,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

bacias <- st_read("/mnt/externo/mestrado_note/mestrado_2026/shapes/bacias_parana/bacias_agregadas/bacias_agregadas.shp") %>% 
  st_transform(4326)

################# Região: escolhe a bacia
cod_bacia <- "Baixo_Iguacu"
bacia <- bacias[bacias$bacia_nome =="Baixo_Iguacu", ]


pasta_saida <- file.path("./figuras")

if (!dir.exists(pasta_saida)) {
  dir.create(pasta_saida, recursive = TRUE)
  message("📁 Pasta criada: ", pasta_saida)
}  

r <- rast("/mnt/externo/mestrado_note/mestrado_shapes/mesclado.tiff")
#r_bacia <- mask(crop(r, vect(bacia)), vect(bacia))
r_bacia <- mask(crop(r, vect(bacia)), vect(bacia), 
  filename = file.path(pasta_saida, "r_bacia_recortado.tif"), 
  overwrite = TRUE
)
##################################
# 1. FLU # Seleção de pontos na bacia e cota
##################################
df_codigos_flu <- st_transform(inventario_sf[inventario_sf$Tipo_Estacao == "Fluviométrica", ], st_crs(bacia))

# Join espacial
pontos_na_bacia <- df_codigos_flu %>%
  st_join(bacia, join = st_within) %>%
  filter(!is.na(bacia_nome))

# Dados flu da bacia e extração de cota
df_flu_bacia <- df_flu[, c("Data", pontos_na_bacia$Codigo)]
cotas_extraidas <- terra::extract(r_bacia, vect(pontos_na_bacia))
pontos_na_bacia$Cota <- cotas_extraidas[[2]]

#########################################
# Cálculo da Disponibilidade e Estação Alvo
#########################################
bbox_exp <- st_bbox(bacia)
xlim_exp <- c(bbox_exp["xmin"] - 0.5, bbox_exp["xmax"] + 0.5)
ylim_exp <- c(bbox_exp["ymin"] - 0.5, bbox_exp["ymax"] + 0.5)

df_flu_long <- df_flu_bacia %>%
  pivot_longer(-Data, names_to = "estacao", values_to = "valor") %>%
  mutate(tem_dado = !is.na(valor))

# Contar dias com dados por estação
disp_estacoes <- df_flu_long %>%
  group_by(estacao) %>%
  summarise(n_dias = sum(tem_dado, na.rm = TRUE)) %>%
  ungroup()

# Juntar contagem de dias aos pontos da bacia
pontos_na_bacia <- pontos_na_bacia %>%
  left_join(disp_estacoes, by = c("Codigo" = "estacao"))

# DEFINIÇÃO DA ESTAÇÃO SELECIONADA (df_codigos_flu_estacao)
# Critério: Menor cota e maior número de dias com dados
df_codigos_flu_estacao <- pontos_na_bacia %>%
filter(!is.na(Cota), !is.na(n_dias)) %>%
arrange(Cota, desc(n_dias)) %>%
slice(11)#11

estacao_escolhida_conv <- df_codigos_flu_estacao$Codigo
estacao_escolhida<- df_codigos_flu_estacao$Codigo

df_codigos_flu_estacao <- pontos_na_bacia %>%
  filter(!is.na(Cota), !is.na(n_dias)) %>%
  arrange(Cota, desc(n_dias)) %>%
  slice(12)#12 é a tele

estacao_escolhida_tele <- df_codigos_flu_estacao$Codigo


# Escala visual dos pontos no mapa
cex_min <- 0.5
cex_max <- 3
n_dias_scaled <- scales::rescale(pontos_na_bacia$n_dias, to = c(cex_min, cex_max))

#########################################
# GERAÇÃO DO MAPA
#########################################
{
png(paste0(pasta_saida, "/", estacao_escolhida, "_mapa_flu_disponibilidade.png"), width = 2000, height = 1800, res = 300)

# Plot Principal
par(fig = c(0, 1, 0, 1), mar = c(4, 4, 4, 8))

plot(
  r_bacia,
  xlim = xlim_exp, ylim = ylim_exp,
  col = terrain.colors(20),
  main = "",
  legend.args = list(text = "Elevação (m)", side = 4, line = 3.5, cex = 0.8),
  legend.width = 0.5
)

plot(st_geometry(bacias), add = TRUE, col = NA, border = "gray", lwd = 1)
plot(st_geometry(UFs), add = TRUE, col = NA, border = "black", lwd = 1)

# Todos os pontos de medição
plot(
  st_geometry(pontos_na_bacia),
  add = TRUE,
  pch = 21,
  bg = adjustcolor("blue", 0.5),
  col = "white",
  lwd = 0.6,
  cex = n_dias_scaled
)

# Ponto em destaque (Estação Escolhida)
plot(
  st_geometry(df_codigos_flu_estacao),
  add = TRUE,
  pch = 21,
  bg = "red",
  col = "yellow",
  lwd = 1.2,
  cex = 1.5
)

# Adiciona o Rótulo (Label) ao lado da Estação Escolhida
coords_estacao <- st_coordinates(df_codigos_flu_estacao)
text(
  x = coords_estacao[1],
  y = coords_estacao[2],
  labels = paste0(df_codigos_flu_estacao$Nome, " (", df_codigos_flu_estacao$Codigo, ")"),
  pos = 4,
  cex = 0.7,
  font = 2
)

# Legenda dos tamanhos de bolinhas
valores_legenda <- c(0, 1000, 6000)
tamanhos_legenda <- scales::rescale(valores_legenda, to = c(cex_min, cex_max))

legend(
  x = -50.15, y = -23.2,
  legend = paste(valores_legenda, "dias"),
  pt.cex = tamanhos_legenda,
  pch = 21,
  pt.bg = adjustcolor("blue", 0.6),
  col = "white",
  title = "Disponibilidade",
  bty = "n",
  cex = 0.6
)

# Mini-mapa no Canto Inferior Esquerdo
par(
  fig = c(0.08, 0.28, 0.10, 0.32),
  mar = c(0, 0, 0, 0),
  new = TRUE
)

PR <- UFs %>% filter(SIGLA_UF == "PR")
bacia_8642 <- bacias %>% filter(bacia_nome == cod_bacia)

plot(st_geometry(PR), col = "grey85", border = "black", lwd = 0.8)
plot(st_geometry(bacia_8642), col = adjustcolor("blue", 0.35), border = "blue", lwd = 1.2, add = TRUE)

dev.off()
}

df_codigos_flu_estacao=df_codigos_flu[df_codigos_flu$Codigo==estacao_escolhida_conv  ,]
bacia_flu_conv<- df_flu[,c("Data",df_codigos_flu_estacao$Codigo)]

#df_codigos_flu=df_codigos_flu[df_codigos_flu$Codigo==estacao_escolhida  ,]
#bacia_flu<- df_flu[,c("Data",df_codigos_flu_estacao$Codigo)]

df_codigos_flu_estacao=df_codigos_flu[df_codigos_flu$Codigo==estacao_escolhida_tele  ,]
bacia_flu_tele<- df_flu[,c("Data",df_codigos_flu_estacao$Codigo)]


names(bacia_flu_tele)[2] <- "exult"
names(bacia_flu_conv)[2] <- "exult"

bacia_flu <- bacia_flu_conv %>%
  inner_join(bacia_flu_tele, by = "Data", suffix = c("_conv", "_tele")) %>%
  mutate(exult = coalesce(exult_conv, exult_tele)) %>%
  select(Data, exult)

historico_flu<-bacia_flu
### completar com a média dias sem dado -- verificar por bacia
{
  # 1. Preenchimento por interpolação linear dos NAs internos
  bacia_flu_preenchido <- bacia_flu %>%
    # Garante a ordenação temporal correta
    arrange(Data) %>%
    
    # Interpola apenas as falhas que possuem valores antes e depois
    mutate(
      exult_preenchido = na.approx(exult, na.rm = FALSE)
    )
  
  # Verificar quantas falhas restaram (devem ser apenas as do final após 2024)
  summary(bacia_flu_preenchido$exult_preenchido)
}
bacia_flu$exult <- bacia_flu_preenchido$exult_preenchido 
#### não necessário
{
library(dplyr)
library(imputeTS)
#### isso a mais é só pq ele inventou todo o final que não tinha dado então primeiro tirei o final pra dai interpolar e depois add de volta
#bacia_flu_preenchido <- bacia_flu %>%
#  arrange(Data) %>%
  # Filtra o data frame removendo as linhas finais que são NA em 'exult'
#  filter(!is.na(exult) | cumsum(!is.na(exult)) > 0) %>% 
  # Ou de forma mais simples para cortar o rastro final de NAs:
  # (Mantém até a última linha que possui dado real)
#  slice(1:max(which(!is.na(exult)))) %>%
#  mutate(
#    exult_kalman = na_kalman(exult, model = "StructTS"),
#    exult_stine = na_interpolation(exult, option = "stine")
# )
#bacia_flu$exult <- c(bacia_flu_preenchido$exult_kalman, 
#  rep(NA, nrow(bacia_flu) - nrow(bacia_flu_preenchido))
#)
}



#--------- só tem dados até: 2020
##################################
# 2. delimitar bacia #(mm) bacia é só a bacia e bacia_completa é todas 
##################################
{
# Criar a pasta de saída se ela ainda não existir
dir_out <- "whitebox_output"
if (!dir.exists(dir_out)) dir.create(dir_out)

# Caminho do arquivo final para checagem de existência
f_delimitada <- file.path(dir_out, "delimitada.shp")

# CHECAGEM: Só executa se o shapefile da bacia ainda não existir na pasta
if (!file.exists(f_delimitada)) {
  
  message("Delimitação não encontrada. Executando WhiteboxTools na pasta '", dir_out, "'...")
  
  # --- Criar hidrografia ---
  wbt_breach_depressions_least_cost(r_bacia, file.path(dir_out, "output.tif"), dist = 5, fill = TRUE)
  wbt_fill_depressions_wang_and_liu(file.path(dir_out, "output.tif"), file.path(dir_out, "filled_output.tif"))
  
  wbt_d8_pointer(file.path(dir_out, "filled_output.tif"), file.path(dir_out, "flow_output.tif"))
  wbt_d8_flow_accumulation(file.path(dir_out, "filled_output.tif"), file.path(dir_out, "flow_acumulation_output.tif"))
  
  wbt_extract_streams(file.path(dir_out, "flow_acumulation_output.tif"), file.path(dir_out, "streams.tif"), threshold = 5000)
  wbt_raster_streams_to_vector(file.path(dir_out, "streams.tif"), file.path(dir_out, "flow_output.tif"), file.path(dir_out, "streams_lines.shp"))
  wbt_smooth_vectors(file.path(dir_out, "streams_lines.shp"), file.path(dir_out, "hidrografia_bacia.shp"), filter = 7)
  
  ######### --- Extrair a geometria da estação --- MUDA PRA CADA BACIA
  coords <- st_coordinates(df_codigos_flu_estacao$geom)
  #coords[1, ] <- c(-53.7458, -24.1871) # Ajuste da posição para ""
  
  df <- st_sf(Codigo = df_codigos_flu_estacao$Codigo, geometry = st_sfc(st_point(coords[1, ]), crs = 4326))
  df$value <- 1
  
  st_write(df, file.path(dir_out, "station_point.shp"), delete_layer = TRUE, quiet = TRUE)
  
  wbt_vector_points_to_raster(
    i      = file.path(dir_out, "station_point.shp"),
    output = file.path(dir_out, "station_point.tif"),
    field  = "value",
    assign = "last",
    nodata = 0,
    base   = file.path(dir_out, "filled_output.tif")
  )
  
  wbt_jenson_snap_pour_points(
    pour_pts  = file.path(dir_out, "station_point.shp"),
    streams   = file.path(dir_out, "streams.tif"),
    output    = file.path(dir_out, "ponto_no_rio.shp"),
    snap_dist = 20
  )
  
  wbt_watershed(
    d8_pntr  = file.path(dir_out, "flow_output.tif"),
    pour_pts = file.path(dir_out, "ponto_no_rio.shp"),
    output   = file.path(dir_out, "delimitada.tif")
  )
  
  wbt_raster_to_vector_polygons(file.path(dir_out, "delimitada.tif"), f_delimitada)
  
  message("Delimitação concluída com sucesso!")
  
} else {
  message("Arquivo 'delimitada.shp' já existe na pasta '", dir_out, "'. Processamento pulado!")
}
f_delimitada <- file.path(dir_out, "delimitada.shp")
# --- CARREGAR CAMADAS DA PASTA E GERAR PLOT ---
dem      <- terra::rast(file.path(dir_out, "filled_output.tif"))
streams  <- terra::rast(file.path(dir_out, "streams.tif"))
basin    <- terra::rast(file.path(dir_out, "delimitada.tif"))

# Carrega e corrige polígonos inválidos
basin_sf <- st_read(f_delimitada, quiet = TRUE) %>% 
  st_make_valid()

rivers   <- st_read(file.path(dir_out, "hidrografia_bacia.shp"), quiet = TRUE) %>% 
  st_set_crs(4326)

exut<- st_read(file.path(dir_out, "ponto_no_rio.shp"), quiet = TRUE)
}
# Salvar Imagem PNG
{
png(paste0(pasta_saida, "/", estacao_escolhida, "_delimitada.png"))
plot(dem, col = "lightgray", legend = FALSE, main = "Bacia Delimitada")
plot(st_geometry(basin_sf), border = "darkgreen", lwd = 2, add = TRUE)
plot(st_geometry(rivers), col = "steelblue", lwd = 1, add = TRUE)
plot(st_geometry(exut), col = "red", pch = 19, cex = 1.5, add = TRUE)
dev.off()

# --- TRATAMENTO GEOMÉTRICO E CÁLCULO DE ÁREA ---
basin_sf <- basin_sf %>%
  select(geometry) %>%      
  st_cast("MULTIPOLYGON")

bacia_completa <- bacia
bacia          <- basin_sf

# Reprojetar para UTM 22S (EPSG:32722) e calcular área em m²
basin_utm <- st_transform(bacia, 32722)
area_m2   <- st_area(basin_utm)
}
# --- CONVERSÃO DA VAZÃO PARA LÂMINA D'ÁGUA (mm/dia) ---
{
  bacia_flu$exult<- ((bacia_flu$exult*86400)/ as.numeric(area_m2))*1000 #mm/dia para cada m3 da bacia

}
##### historico
historico_flu$exult<-((historico_flu$exult*86400)/ as.numeric(area_m2))*1000
historico_flu$exult_preenchida<-bacia_flu$exult
hist <- ggplot(historico_flu) +
  # Camada inferior: Série preenchida (destaque para as falhas)
  geom_col(aes(x = Data, y = exult_preenchida), fill = "#e74c3c") + 
  # Camada superior: Dado original (sobrepõe o preenchido onde existir)
  geom_col(aes(x = Data, y = exult), fill = "#27ae60") + 
  labs(y = "Q (mm/dia)", x = "Ano", title = cod_bacia) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal()

# Salvar a imagem em alta resolução
ggsave(
  filename = paste0(pasta_saida, "/historico_flu.png"),
  plot = hist,
  width = 15,
  height = 6,
  dpi = 300,
  bg = "white"
)
##################################
# 3. PLU #media_bacia_plu  $data $plu_med
##################################
{
grid <- st_as_sf(
  grade_chuva_espacializada,
  coords = c("lon", "lat"),
  crs = 4326
)
# buffer: ids_grid
{
# 1. transformar em metros para poder criar o buffer
  # Se não tiver definido, atribui WGS84 (graus)
  st_crs(bacia) <- 4326
  # Transforma para UTM (vou usar zona 22S como exemplo, comum no PR/SC)
  bacia_utm <- st_transform(bacia, 31982)  # SIRGAS 2000 / UTM zone 22S
  # Cria buffer de 10 km
  bacia_buffer_utm <- st_buffer(bacia_utm, dist = 5000) #5km
  # Volta para lat/lon, se quiser
  bacia_buffer <- st_transform(bacia_buffer_utm, 4326)
  
  # seleciona os pontos do grid que caem dentro da bacia + buffer
  grid_sel <- grid %>%
    filter(st_intersects(geometry, bacia_buffer, sparse = FALSE))
  
  # ids dos pontos selecionados
  ids_grid <- grid_sel$grid_id
  
  #plot
  {
    png(paste0(pasta_saida, "/", estacao_escolhida, "_buffer.png"),
        width = 2000, height = 1500, res = 200)
    # bounding box da bacia
    bbox <- st_bbox(bacia)
    # aumenta 1° em cada direção
    xlim <- c(bbox["xmin"] - 1, bbox["xmax"] + 1)
    ylim <- c(bbox["ymin"] - 1, bbox["ymax"] + 1)
    plot(dem, col = "lightgray", legend = FALSE,
         main = "Bacia, Buffer e Pontos Selecionados",
         xlim = xlim, ylim = ylim)
    plot(st_geometry(bacia_buffer), col = NA, border = "blue", lwd = 2, add = TRUE)
    plot(st_geometry(bacia), col = NA, border = "black", lwd = 2, add = TRUE)
    plot(st_geometry(grid), col = "grey70", pch = 20, cex = 0.6, add = TRUE)
    plot(st_geometry(grid_sel), col = "red", pch = 19, cex = 0.8, add = TRUE)
    
    legend("topright",
           legend = c("Buffer", "Bacia", "Grid", "Grid selecionado"),
           col = c("blue", "black", "grey70", "red"),
           pch = c(NA, NA, 20, 19),
           lty = c(1, 1, NA, NA),
           bty = "n",
           xpd = TRUE)
    
    dev.off()
  }
  
}
}
df_bacia_plu <- df_grid_interpolado[, c("Data", ids_grid)]
## x area da bacia pra transformar em lâmina de agua #PARA BACIA TODA
media_bacia_plu <- data.frame(Data= df_bacia_plu$Data, plu_med=(rowMeans(df_bacia_plu[ , -1], na.rm = TRUE)))

##################################
# 5. Satélite - h_bacia_total #h_bacia_total $data $h_bacia
#               h_bacia_dia $data $h_bacia_diario
##################################
{
#subir dados de grade e historico

load("../../Grace/2026/2026_recortado.bin")
# Criando o objeto espacial
sf_grid_coords <- st_as_sf(
  df_grid_coords, 
  coords = c("lon", "lat"), # Nome das colunas de longitude e latitude (nessa ordem!)
  crs = 4326                # Define o sistema de coordenadas (4326 é o WGS84, padrão para lat/lon)
)

# --- 1. Criar polígonos reais para os pontos da grade e calcular as áreas ---
{
  # 1.0 Definir a área real de referência que você precisa cravar no final
  area_bacia_alvo <- area_m2  # em m²
  
  # 1.1 Extrair as coordenadas dos pontos em GRAUS (onde a grade é 100% alinhada)
  coords_geo <- st_coordinates(sf_grid_coords)
  
  # Descobre o espaçamento exato original em graus de lon e lat
  res_lon <- min(diff(sort(unique(coords_geo[, "X"]))))
  res_lat <- min(diff(sort(unique(coords_geo[, "Y"]))))
  
  # 1.2 Construir os quadrados perfeitos em GRAUS (garante alinhamento sem degraus)
  poligonos_lista <- lapply(1:nrow(coords_geo), function(i) {
    x <- coords_geo[i, "X"]
    y <- coords_geo[i, "Y"]
    
    xmin <- x - res_lon / 2
    xmax <- x + res_lon / 2
    ymin <- y - res_lat / 2
    ymax <- y + res_lat / 2
    
    st_polygon(list(rbind(
      c(xmin, ymin),
      c(xmin, ymax),
      c(xmax, ymax),
      c(xmax, ymin),
      c(xmin, ymin)
    )))
  })
  
  # Criar o objeto sf de polígonos original em graus (4326)
  grade_pol_sat_geo <- st_sf(
    grid_id = sf_grid_coords$grid_id,
    geometry = st_sfc(poligonos_lista, crs = 4326)
  )
  
  # 1.3 Projetar a grade já montada e a bacia para UTM (31983)
  bacia_utm <- st_transform(bacia, 31983) %>% st_make_valid()
  grade_pol_sat_utm <- st_transform(grade_pol_sat_geo, 31983)
  
  # 1.4 Cortar os polígonos no limite exato da bacia (tudo em UTM)
  intersecao_sat <- st_intersection(grade_pol_sat_utm, bacia_utm)
  
  # 1.5 Limpar geometrias e calcular a área UTM provisória
  intersecao_sat <- intersecao_sat %>%
    st_make_valid() %>%
    filter(!st_is_empty(.)) %>%
    mutate(area_utm_provisoria = as.numeric(st_area(geometry))) %>%
    filter(area_utm_provisoria > 1)
  
  # --- CALIBRAÇÃO GEODÉSICA ---
  area_bacia_utm_total <- sum(intersecao_sat$area_utm_provisoria, na.rm = TRUE)
  fator_correcao <- area_bacia_alvo / area_bacia_utm_total
  
  intersecao_sat <- intersecao_sat %>%
    mutate(area_m2 = area_utm_provisoria * fator_correcao) %>% 
    st_transform(4326) # Mantém em 4326 para compatibilidade
  
  # Captura as variáveis fixas calibradas
  grid_ids_selecionados <- intersecao_sat$grid_id
  areas_sat <- intersecao_sat$area_m2
}

# --- 2. Função otimizada para o cálculo mensal ---
calcula_h_sat_bacia_mes <- function(m) {
  linha_mes <- df_grid_historico[m, ]
  data_atual <- linha_mes$data
  
  # Seleciona os valores das colunas (Grids) correspondentes às áreas da interseção
  h_sel <- unlist(linha_mes[, grid_ids_selecionados])
  
  # Média ponderada clássica (Soma de cada Valor * sua Área / Área Total)
  h_total <- as.numeric(sum(h_sel * areas_sat, na.rm = TRUE)) #-------> em cm
  area_total <- as.numeric(sum(areas_sat, na.rm = TRUE))
  
  h_bacia <- ifelse(area_total > 0, (h_total*10) / area_total, NA_real_) #mm
  
  return(data.frame(data = data_atual, h_bacia= h_bacia))
}
# --- 3. Rodar a função para toda a série histórica ---
n_meses <- nrow(df_grid_historico)
h_bacia_total <- do.call(rbind, lapply(1:n_meses, calcula_h_sat_bacia_mes))
# --- 4. Plot de Validação Geográfica (Satélite) ---
# NÃO projetamos para UTM na hora de plotar, mantemos tudo em 4326 (Graus)
{
  bacia_em_pe <- st_transform(bacia, 4326)
  grids_em_pe <- st_transform(intersecao_sat, 4326)
  
  grace_plot<- ggplot() +
    # 1. Bacia por baixo
    geom_sf(data = bacia_em_pe, fill = "gray95", color = "black", size = 0.7) +
    
    # 2. Grids por cima (vão aparecer 100% retos, ortogonais e sem degraus)
    geom_sf(data = grids_em_pe, fill = alpha("skyblue", 0.3), color = "blue", size = 0.3) +
    
    # Força o fundo a mostrar o xadrez perfeito em graus (Lat/Lon)
    coord_sf(datum = 4326) + 
    
    theme_minimal() +
    labs(
      title = "Mosaico de Grids Alinhado",
      subtitle = "Grade contínua e verticalizada no sistema geográfico",
      x = "Longitude",
      y = "Latitude"
    )
  ggsave(
    filename = paste0(pasta_saida,"/grace_bacia_grid.png"),
    plot = grace_plot,
    width = 9,          # Largura ligeiramente maior que a altura
    height = 12,          # Altura ideal para a distribuição 2x2
    dpi = 300,
    bg = "white"
  )
}

}
#definir grace dia
####
{
h_bacia_dia <- h_bacia_total %>%
  # 1. Converter para Date para evitar problemas de fuso horário
  mutate(Data = as.Date(data)) %>% 
  
  # 2. Ordenar por data (garantia extra para o lead() funcionar certo)
  arrange(data) %>% 
  
  # 3. Trazer a data e o valor da próxima linha
  mutate(
    proxima_data = lead(data),
    proxima_h    = lead(h_bacia)
  ) %>% 
  
  # 4. REMOVER a última linha (ela terá NA no proxima_data, pois não há próximo mês)
  filter(!is.na(proxima_data)) %>% 
  
  # 5. Agora sim: criar as sequências e calcular sem perigo de erro
  mutate(
    # Gera os dias do intervalo (ex: de 17/04 até 17/05)
    dias = map2(data, proxima_data - 1, ~ seq(.x, .y, by = "1 day")),
    
    # Quantos dias tem esse intervalo específico
    n_dias = map_int(dias, length),
    
    # Divide a variação do GRACE igualmente pelos dias do intervalo
    h_bacia_diario = (proxima_h - h_bacia) / n_dias
  ) %>%
  
  # 6. Expandir o data frame e limpar as colunas auxiliares
  select(dias, h_bacia_diario) %>%
  unnest(cols = c(dias)) %>%
  rename(Data = dias)
}
###########################################################################################################################
##################################
#calculo  #df_balanco_diario
##################################
{
# 1. Unir os três data frames pelas datas em comum
df_balanco_diario <- media_bacia_plu %>%
  # Une com os dados de vazão
  inner_join(bacia_flu, by = "Data") %>%
  # Une com os dados desagregados do GRACE
  inner_join(h_bacia_dia, by = "Data") %>%
  
  # 2. Calcular a evapotranspiração (E = P - Q - S)
  mutate(
    evapo = plu_med - exult - h_bacia_diario
  )
}
### plot
{
  # Plot 1: Precipitação Diária (P)
  p_plu <- ggplot(df_balanco_diario, aes(x = Data, y = plu_med)) +
    geom_col(fill = "#2980b9", width = 1) + # width = 1 evita espaços verticais finos em barras diárias
    labs(y = "P (mm/dia)", x = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Plot 2: Vazão Diária (Q)
  p_flu <- ggplot(df_balanco_diario, aes(x = Data, y = exult)) +
    geom_col(fill = "#27ae60", width = 1) + 
    labs(y = "Q (mm/dia)", x = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Plot 3: Variação de Armazenamento GRACE Diária (S)
  # Nota: Se o GRACE for mensal e interpolado para diário, linhas ou barras finas funcionam bem
  p_grace <- ggplot(df_balanco_diario, aes(x = Data, y = h_bacia_diario)) +
    geom_col(fill = "#8e44ad", width = 1) + 
    labs(y = "ΔS (mm/dia)", x = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Plot 4: Evapotranspiração Diária por Balanço (E)
  p_evapo <- ggplot(df_balanco_diario, aes(x = Data, y = evapo)) +
    geom_col(fill = "#e67e22", width = 1) + 
    labs(y = "E (mm/dia)", x = "Data") +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  # Juntando os 4 gráficos na vertical usando patchwork
  grafico_final_diario <- p_plu / p_flu / p_grace / p_evapo + 
    plot_annotation(
      title = "Balanço Hídrico Diário da Bacia",
      subtitle = "Componentes Diários: E = P - Q - ΔS",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )
  
  # Exibir o gráfico no RStudio
  print(grafico_final_diario)
  
  # Salvar a imagem em alta resolução (com mais largura para acomodar os dias)
  ggsave(
    filename = paste0(pasta_saida, "/balanco_hidrico_diario_SemCorrecao.png"),
    plot = grafico_final_diario,
    width = 14,
    height = 10,
    dpi = 300,
    bg = "white"
  )
}
##################################
################################## PROODUTOS ##################################
##################################


##################################
# 5. MENSAL #df_balanco_mensal
##################################

df_balanco_diario <- df_balanco_diario %>%
  # Remove todos os dias em que a vazão é NA 
  filter(!is.na(exult))

df_balanco_mensal <- df_balanco_diario %>%
  # 1. Agrupa pela primeira data de cada mês (ex: 2002-04-01)
  mutate(Data_Mes = floor_date(Data, unit = "month")) %>%
  
  # 2. Acumula (soma) os valores diários de cada mês
  group_by(Data_Mes) %>%
  summarise(
    plu_med        = sum(plu_med, na.rm = TRUE),
    exult          = sum(exult, na.rm = TRUE),
    h_bacia_mensal = sum(h_bacia_diario, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 3. Calcula a evapotranspiração mensal sobre os totais do mês
  mutate(
    evapo = plu_med - exult - h_bacia_mensal
  )
## plot sem correção
{
  # Plot 1: Precipitação Mensal (P)
  p_plu <- ggplot(df_balanco_mensal, aes(x = Data_Mes, y = plu_med)) +
    geom_col(fill = "#2980b9") + # Azul para chuva
    labs(y = "P (mm/mês)", x = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Plot 2: Vazão Mensal (Q)
  p_flu <- ggplot(df_balanco_mensal, aes(x = Data_Mes, y = exult)) +
    geom_col(fill = "#27ae60") + # Verde para vazão fluviométrica
    labs(y = "Q (mm/mês)", x = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Plot 3: Variação de Armazenamento GRACE Mensal (S)
  p_grace <- ggplot(df_balanco_mensal, aes(x = Data_Mes, y = h_bacia_mensal)) +
    geom_col(fill = "#8e44ad") + # Roxo para o GRACE
    labs(y = "ΔS (mm/mês)", x = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Plot 4: Evapotranspiração Mensal por Balanço (E)
  p_evapo <- ggplot(df_balanco_mensal, aes(x = Data_Mes, y = evapo)) +
    geom_col(fill = "#e67e22") + # Laranja para evapotranspiração
    labs(y = "E (mm/mês)", x = "Data") +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "1 year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Juntando os 4 gráficos na vertical usando patchwork
  grafico_final_mensal <- p_plu / p_flu / p_grace / p_evapo + 
    plot_annotation(
      title = "Balanço Hídrico Mensal da Bacia",
      subtitle = "Componentes Acumulados Mensalmente: E = P - Q - ΔS",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )
  
  # Exibir o gráfico no RStudio
  print(grafico_final_mensal)
  
  # Salvar a imagem em alta resolução
  ggsave(
    filename = paste0(pasta_saida, "/balanco_hidrico_mensal_SemCorrecao.png"),
    plot = grafico_final_mensal,
    width = 12,
    height = 10,
    dpi = 300,
    bg = "white"
  )
  }

#################### Correção dos negativos
{
df_balanco_mensal <- df_balanco_mensal %>%
  mutate(
    # Soma total dos valores negativos (em valor absoluto)
    total_deficit = sum(abs(evapo[evapo < 0]), na.rm = TRUE),
    # Quantidade de meses não-negativos
    n_positivos = sum(evapo >= 0, na.rm = TRUE),
    # Desconto a ser aplicado em cada mês positivo
    desconto = total_deficit / n_positivos,
    # Se for negativo vira 0; se for positivo recebe o desconto
    evapo = if_else(evapo <= 0, 0, evapo - desconto)
  ) %>%
  select(-total_deficit, -n_positivos, -desconto)
}
## plot com correção
{
# Plot 1: Precipitação Mensal (P)
p_plu <- ggplot(df_balanco_mensal, aes(x = Data_Mes, y = plu_med)) +
  geom_col(fill = "#2980b9") + # Azul para chuva
  labs(y = "P (mm/mês)", x = NULL) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Plot 2: Vazão Mensal (Q)
p_flu <- ggplot(df_balanco_mensal, aes(x = Data_Mes, y = exult)) +
  geom_col(fill = "#27ae60") + # Verde para vazão fluviométrica
  labs(y = "Q (mm/mês)", x = NULL) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Plot 3: Variação de Armazenamento GRACE Mensal (S)
p_grace <- ggplot(df_balanco_mensal, aes(x = Data_Mes, y = h_bacia_mensal)) +
  geom_col(fill = "#8e44ad") + # Roxo para o GRACE
  labs(y = "ΔS (mm/mês)", x = NULL) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Plot 4: Evapotranspiração Mensal por Balanço (E)
p_evapo <- ggplot(df_balanco_mensal, aes(x = Data_Mes, y = evapo)) +
  geom_col(fill = "#e67e22") + # Laranja para evapotranspiração
  labs(y = "E (mm/mês)", x = "Data") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Juntando os 4 gráficos na vertical usando patchwork
grafico_final_mensal <- p_plu / p_flu / p_grace / p_evapo + 
  plot_annotation(
    title = "Balanço Hídrico Mensal da Bacia",
    subtitle = "Componentes Acumulados Mensalmente: E = P - Q - ΔS",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

# Exibir o gráfico no RStudio
print(grafico_final_mensal)

# Salvar a imagem em alta resolução
ggsave(
  filename = paste0(pasta_saida, "/balanco_hidrico_mensal_corrigido3.png"),
  plot = grafico_final_mensal,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)
}
###### plot climatologia mensal dado entrada mensal
{
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(lubridate)
  
  # 1. Preparar os dados para climatologia
  df_climatologia <- df_balanco_mensal %>%
    mutate(
      mes = month(Data_Mes, label = TRUE, abbr = TRUE)
    )
  
  # 2. Criar os 4 Boxplots (Sem rótulo do eixo X nos 3 primeiros)
  
  # Boxplot: Precipitação (P)
  b_plu <- ggplot(df_climatologia, aes(x = mes, y = plu_med)) +
    geom_boxplot(fill = "#2980b9", alpha = 0.7, outlier.size = 0.5) +
    labs(y = "P (mm/mês)", x = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Boxplot: Vazão (Q)
  b_flu <- ggplot(df_climatologia, aes(x = mes, y = exult)) +
    geom_boxplot(fill = "#27ae60", alpha = 0.7, outlier.size = 0.5) +
    labs(y = "Q (mm/mês)", x = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Boxplot: Variação de Armazenamento GRACE (S)
  b_grace <- ggplot(df_climatologia, aes(x = mes, y = h_bacia_mensal)) +
    geom_boxplot(fill = "#8e44ad", alpha = 0.7, outlier.size = 0.5) +
    labs(y = "ΔS (mm/mês)", x = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Boxplot: Evapotranspiração (E)
  b_evapo <- ggplot(df_climatologia, aes(x = mes, y = evapo)) +
    geom_boxplot(fill = "#e67e22", alpha = 0.7, outlier.size = 0.5) +
    labs(y = "E (mm/mês)", x = "Mês") +
    theme_minimal()
  
  # 3. Unir e compartilhar o eixo X usando patchwork
  climatologia <- (b_plu / b_flu / b_grace / b_evapo) +
    plot_layout(axes = "collect") + # Coleta e centraliza o eixo X na base
    plot_annotation(
      title = "Climatologia Mensal dos Componentes do Balanço Hídrico",
      subtitle = "Distribuição acumulada mensal (E = P - Q - ΔS)",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )
  
  # Exibir o gráfico no RStudio
  print(climatologia)
  
  # 4. Salvar com proporção mais comprida na horizontal (ex: 14x8 ou 15x8)
  ggsave(
    filename = paste0(pasta_saida, "/climatologia_mensal_boxplots.png"),
    plot = climatologia,
    width = 14,          # Aumentado para dar mais largura (comprido)
    height = 8,          # Altura reduzida em relação à largura
    dpi = 300,
    bg = "white"
  )
}


################################## 
# 6. media anual --- #df_balanco_anual
##################################
{
df_balanco_anual <- df_balanco_diario %>%
  # 1. Filtra dias válidos (onde exult não é NA)
  filter(!is.na(exult)) %>%
  
  # 2. Cria a coluna do ano
  mutate(Data_Ano = floor_date(Data, unit = "year")) %>%
  
  # 3. Agrupa por ano e verifica a contagem de dias
  group_by(Data_Ano) %>%
  filter(n() >= 365) %>% # Mantém apenas anos com 365 ou 366 dias (anos completos)
  
  # 4. Acumula os totais anuais
  summarise(
    plu_med        = sum(plu_med, na.rm = TRUE),
    exult          = sum(exult, na.rm = TRUE),
    h_bacia_anual = sum(h_bacia_diario, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # 5. Calcula a evapotranspiração anual
  mutate(
    evapo = plu_med - exult - h_bacia_anual
  )
}
################ plot
{

# Plot 1: Precipitação Mensal (P)
p_plu <- ggplot(df_balanco_anual, aes(x = Data_Ano, y = plu_med)) +
  geom_col(fill = "#2980b9") + # Azul para chuva
  labs(y = "P (mm/ano)", x = NULL) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Plot 2: Vazão Mensal (Q)
p_flu <- ggplot(df_balanco_anual, aes(x = Data_Ano, y = exult)) +
  geom_col(fill = "#27ae60") + # Verde para vazão fluviométrica
  labs(y = "Q (mm/ano)", x = NULL) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Plot 3: Variação de Armazenamento GRACE Mensal (S)
p_grace <- ggplot(df_balanco_anual, aes(x = Data_Ano, y = h_bacia_anual)) +
  geom_col(fill = "#8e44ad") + # Roxo para o GRACE
  labs(y = "ΔS (mm/ano)", x = NULL) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Plot 4: Evapotranspiração Mensal por Balanço (E)
p_evapo <- ggplot(df_balanco_anual, aes(x = Data_Ano, y = evapo)) +
  geom_col(fill = "#e67e22") + # Laranja para evapotranspiração
  labs(y = "E (mm/ano)", x = "Data") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Juntando os 4 gráficos na vertical usando patchwork
grafico_final_anual <- p_plu / p_flu / p_grace / p_evapo + 
  plot_annotation(
    title = "Balanço Hídrico Anual da Bacia",
    subtitle = "Componentes Acumulados Anualmente: E = P - Q - ΔS",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

# Exibir o gráfico no RStudio
print(grafico_final_anual)

# Salvar a imagem em alta resolução
ggsave(
  filename = paste0(pasta_saida, "/balanco_hidrico_anual.png"),
  plot = grafico_final_anual,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)
}
################################## 
# 6. media intervalo grace #grace_interv
##################################
{
#deixar os intervalos obrigatoriamentem em 30 dias

  # 1. Calcula a variação REAL (grace_dif) e a quantidade de meses no intervalo original
  h_bacia_interp <- h_bacia_total %>%
    arrange(data) %>%
    mutate(
      data_prox = lead(data),
      h_bacia_prox = lead(h_bacia),
      dias_totais = as.numeric(data_prox - data),
      # Número de parcelas de ~30 dias
      n_intervalos = pmax(1, round(dias_totais / 30)),
      # A variação total dividida igualmente pelo número de meses
      grace_dif = (h_bacia_prox - h_bacia) / n_intervalos
    ) %>%
    filter(!is.na(data_prox)) %>%
    # Duplica as linhas para os meses intermediários
    uncount(n_intervalos, .id = "id_parcela", .remove = FALSE) %>%
    mutate(
      # Recalcula as datas de início (t1) e fim (t2) de cada parcela de 30 dias
      t1 = data + days(round((dias_totais / n_intervalos) * (id_parcela - 1))),
      t2 = data + days(round((dias_totais / n_intervalos) * id_parcela)),
      dias = as.numeric(t2 - t1)
    )
  
  # 2. Agora calcula o balanço de chuva (plu) e vazão (flu) para cada novo intervalo t1 -> t2
  grace_interv <- h_bacia_interp %>%
    rowwise() %>%
    mutate(
      plu_sum = if (all(is.na(media_bacia_plu$plu_med[media_bacia_plu$Data >= t1 & media_bacia_plu$Data < t2]))) NA_real_ 
      else sum(media_bacia_plu$plu_med[media_bacia_plu$Data >= t1 & media_bacia_plu$Data < t2], na.rm = TRUE),
      
      flu_sum = if (all(is.na(bacia_flu$exult[bacia_flu$Data >= t1 & bacia_flu$Data < t2]))) NA_real_ 
      else sum(bacia_flu$exult[bacia_flu$Data >= t1 & bacia_flu$Data < t2], na.rm = TRUE),
      
      evapo_sum = plu_sum - flu_sum - grace_dif
    ) %>%
    ungroup() %>%
    select(t1, t2, dias, grace_dif, plu_sum, flu_sum, evapo_sum)
  
  
  grace_interv  <- grace_interv  %>%
    mutate(
      # Soma total dos valores negativos (em valor absoluto)
      total_deficit = sum(abs(evapo_sum[evapo_sum < 0]), na.rm = TRUE),
      # Quantidade de meses não-negativos
      n_positivos = sum(evapo_sum >= 0, na.rm = TRUE),
      # Desconto a ser aplicado em cada mês positivo
      desconto = total_deficit / n_positivos,
      # Se for negativo vira 0; se for positivo recebe o desconto
      evapo_sum = if_else(evapo_sum < 0, 0, evapo_sum - desconto)
    ) %>%
    select(-total_deficit, -n_positivos, -desconto)

# AJEITAR DATAS 
  data_min <- min(grace_interv$t1[!is.na(grace_interv$evapo_sum)])
  data_max <- max(grace_interv$t1[!is.na(grace_interv$evapo_sum)])
  
  # 2. Filtra o dataframe para manter todo o intervalo de anos entre elas
  grace_interv<- grace_interv %>%
    filter(t1 >= data_min & t1 <= data_max)  

# Remove o intervalo longo de transição das missões GRACE/GRACE-FO
#grace_interv <- grace_interv %>%
# filter(t1 != as.Date("2017-06-17"))
}
################## plot
{
# Plot 1: Precipitação Acumulada no Intervalo (P)
p_plu <- ggplot(grace_interv, aes(x = t1, y = plu_sum)) +
  geom_col(fill = "#2980b9") + # Azul para chuva
  labs(y = "P (mm/intervalo)", x = NULL) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Plot 2: Vazão Acumulada no Intervalo (Q)
p_flu <- ggplot(grace_interv, aes(x = t1, y = flu_sum)) +
  geom_col(fill = "#27ae60") + # Verde para vazão fluviométrica
  labs(y = "Q (mm/intervalo)", x = NULL) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Plot 3: Variação de Armazenamento GRACE (ΔS)
p_grace <- ggplot(grace_interv, aes(x = t1, y = grace_dif)) +
  geom_col(fill = "#8e44ad") + # Roxo para o GRACE
  labs(y = "ΔS (mm/intervalo)", x = NULL) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Plot 4: Evapotranspiração por Balanço no Intervalo (E)
p_evapo <- ggplot(grace_interv, aes(x = t1, y = evapo_sum)) +
  geom_col(fill = "#e67e22") + # Laranja para evapotranspiração
  labs(y = "E (mm/intervalo)", x = "Data de Início do Intervalo (t1)") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Juntando os 4 gráficos na vertical usando patchwork
grafico_final_interv <- p_plu / p_flu / p_grace / p_evapo + 
  plot_annotation(
    title = "Balanço Hídrico por Intervalo de Amostragem do GRACE",
    subtitle = "Componentes Acumulados por Intervalo (E = P - Q - ΔS)",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

# Exibir o gráfico no RStudio
print(grafico_final_interv)

# Salvar a imagem em alta resolução
ggsave(
  filename = paste0(pasta_saida, "/balanco_hidrico_grace_intervalos_semCorrecao.png"),
  plot = grafico_final_interv,
  width = 12,
  height = 10,
  dpi = 300,
  bg = "white"
)

}
################## Correcao
{
  grace_interv <- grace_interv %>%
    mutate(
      # Soma total dos valores negativos (em valor absoluto)
      total_deficit = sum(abs(evapo_sum[evapo_sum < 0]), na.rm = TRUE),
      # Quantidade de meses não-negativos
      n_positivos = sum(evapo_sum >= 0, na.rm = TRUE),
      # Desconto a ser aplicado em cada mês positivo
      desconto = total_deficit / n_positivos,
      # Se for negativo vira 0; se for positivo recebe o desconto
      evapo_sum = if_else(evapo_sum <= 0, 0, evapo_sum - desconto)
    ) %>%
    select(-total_deficit, -n_positivos, -desconto)
}
################## plot
{
  # Plot 1: Precipitação Acumulada no Intervalo (P)
  p_plu <- ggplot(grace_interv, aes(x = t1, y = plu_sum)) +
    geom_col(fill = "#2980b9") + # Azul para chuva
    labs(y = "P (mm/intervalo)", x = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Plot 2: Vazão Acumulada no Intervalo (Q)
  p_flu <- ggplot(grace_interv, aes(x = t1, y = flu_sum)) +
    geom_col(fill = "#27ae60") + # Verde para vazão fluviométrica
    labs(y = "Q (mm/intervalo)", x = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Plot 3: Variação de Armazenamento GRACE (ΔS)
  p_grace <- ggplot(grace_interv, aes(x = t1, y = grace_dif)) +
    geom_col(fill = "#8e44ad") + # Roxo para o GRACE
    labs(y = "ΔS (mm/intervalo)", x = NULL) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Plot 4: Evapotranspiração por Balanço no Intervalo (E)
  p_evapo <- ggplot(grace_interv, aes(x = t1, y = evapo_sum)) +
    geom_col(fill = "#e67e22") + # Laranja para evapotranspiração
    labs(y = "E (mm/intervalo)", x = "Data de Início do Intervalo (t1)") +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "1 year") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Juntando os 4 gráficos na vertical usando patchwork
  grafico_final_interv <- p_plu / p_flu / p_grace / p_evapo + 
    plot_annotation(
      title = "Balanço Hídrico por Intervalo de Amostragem do GRACE",
      subtitle = "Componentes Acumulados por Intervalo (E = P - Q - ΔS)",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )
  
  # Exibir o gráfico no RStudio
  print(grafico_final_interv)
  
  # Salvar a imagem em alta resolução
  ggsave(
    filename = paste0(pasta_saida, "/balanco_hidrico_grace_intervalos_corrigido.png"),
    plot = grafico_final_interv,
    width = 12,
    height = 10,
    dpi = 300,
    bg = "white"
  )
  
}
################################## 
#Ajeitando pra salvar
##################################

if (!dir.exists("produtos_finais")) {
  dir.create("produtos_finais", recursive = TRUE)
  message("📁 Pasta criada: ", "produtos_finais")
}  

df_balanco_anual_Baixo_Iguacu<- df_balanco_anual
df_balanco_mensal_Baixo_Iguacu<- df_balanco_mensal
grace_interv_Medio_Baixo<- grace_interv

save(df_balanco_anual_Baixo_Iguacu, file="./produtos_finais/df_balanco_anual_Baixo_Iguacu.bin")
save(df_balanco_mensal_Baixo_Iguacu, file="./produtos_finais/df_balanco_mensal_Baixo_Iguacu.bin")
save(grace_interv_Medio_Baixo, file="./produtos_finais/grace_interv_Baixo_Iguacu.bin")




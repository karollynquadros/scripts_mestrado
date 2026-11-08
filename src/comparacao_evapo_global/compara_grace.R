library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggspatial)

# =============================================================
# 1. Carregamento dos dados
# =============================================================
grace_global <- rast("/mnt/externo/mestrado_note/evapotranspiration_basin/data/grace/grace_lwe_2021_05_parana/grace_lwe_2021_05_parana.lwe_thickness.tif")
load("/mnt/externo/mestrado_note/evapotranspiration_basin/data/grace/2026_recortado.bin")
bacias <- st_read("/mnt/externo/mestrado_note/evapotranspiration_basin/data/shapes/bacias_parana/bacias_agregadas")
UF     <- st_read("/mnt/externo/mestrado_note/evapotranspiration_basin/data/shapes/BR_UF_2022/BR_UF_2022.shp")

library(dplyr)
library(tidyr)
library(purrr)

# ==============================================================================
# 1. Carregar os dados originais
# ==============================================================================
caminho_bin <- "/mnt/externo/mestrado_note/evapotranspiration_basin/data/grace/2026_recortado.bin"
load(caminho_bin)

# ==============================================================================
# 2. Construir a grade espacial estendida (deslocada + bordas originais)
# ==============================================================================
# Grade original
coords_orig <- df_grid_coords %>%
  mutate(
    lon_r = round(lon, 4),
    lat_r = round(lat, 4),
    grid_orig = grid_id
  )

# Grade deslocada (+0.5° para leste e norte)
coords_desloc <- df_grid_coords %>%
  mutate(
    lon_r = round(lon + 0.5, 4),
    lat_r = round(lat + 0.5, 4),
    grid_fonte = grid_id  # de qual grid_id antigo vem o dado
  )

# Mapeamento espacial:
# - Células da grade deslocada recebem dados de 'grid_fonte'
# - Bordas que sobraram sem deslocamento recebem os dados do ponto original 'grid_orig'
coords_unificadas <- full_join(
  coords_desloc %>% select(lon_r, lat_r, grid_fonte),
  coords_orig %>% select(lon_r, lat_r, grid_orig),
  by = c("lon_r", "lat_r")
) %>%
  mutate(
    # Prioriza o dado deslocado; se for borda sem deslocamento, usa o original
    grid_atribuido = coalesce(grid_fonte, grid_orig)
  ) %>%
  arrange(desc(lat_r), lon_r) %>%
  mutate(
    grid_id_novo = sprintf("Grid_%04d", row_number()),
    lon = lon_r,
    lat = lat_r
  )

# Novo df_grid_coords completo e ordenado
df_grid_coords_corrigido <- coords_unificadas %>%
  select(grid_id = grid_id_novo, lon, lat)

# ==============================================================================
# 3. Reconstruir df_grid_historico para TODAS as datas
# ==============================================================================
# Extrai a coluna de datas
datas_serie <- df_grid_historico$data

# Cria uma matriz com as novas colunas referenciando os dados originais
# Se houver algum grid_atribuido sem correspondência exata, preenche com NA
vetor_correspondencia <- coords_unificadas$grid_atribuido

# Constrói o novo data.frame histórico
novas_colunas <- as.data.frame(
  lapply(vetor_correspondencia, function(id_antigo) {
    if (id_antigo %in% colnames(df_grid_historico)) {
      df_grid_historico[[id_antigo]]
    } else {
      rep(NA_real_, nrow(df_grid_historico))
    }
  })
)

colnames(novas_colunas) <- coords_unificadas$grid_id_novo

df_grid_historico_corrigido <- bind_cols(
  tibble(data = datas_serie),
  novas_colunas
)

# ==============================================================================
# 4. Substituição das variáveis em memória
# ==============================================================================
df_grid_coords    <- df_grid_coords_corrigido
df_grid_historico <- df_grid_historico_corrigido

# Conferência da estrutura
str(df_grid_coords)
dim(df_grid_historico)

# =============================================================
# 2. Preparação dos dados espaciais e tabulares
# =============================================================
# 2.1 Raster GRACE Global para data.frame
df_grace_global <- as.data.frame(grace_global, xy = TRUE, na.rm = TRUE)
colnames(df_grace_global) <- c("lon", "lat", "valor")

# 2.2 Filtrar Downscaling para Maio de 2021
linha_maio_2021 <- df_grid_historico %>%
  filter(year(data) == 2021 & month(data) == 5)

if (nrow(linha_maio_2021) == 0) {
  linha_maio_2021 <- df_grid_historico %>%
    slice(which.min(abs(data - as.Date("2021-05-15"))))
}

df_grace_downscaling <- linha_maio_2021 %>%
  select(-data) %>%
  pivot_longer(cols = everything(), names_to = "grid_id", values_to = "valor") %>%
  inner_join(df_grid_coords, by = "grid_id")

# =============================================================
# 3. Configurações de escala, extensão e paleta
# =============================================================
lim_min <- min(c(df_grace_global$valor, df_grace_downscaling$valor), na.rm = TRUE)
lim_max <- max(c(df_grace_global$valor, df_grace_downscaling$valor), na.rm = TRUE)

# Extensão do recorte usando terra::ext()
ext_g <- ext(grace_global)

paleta_cores <- c("#d73027", "#f46d43", "#fdae61", "#fee08b", 
                  "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850")

# =============================================================
# 4. Construção dos Mapas
# =============================================================

# Plot (a): Grace TWSA
p1 <- ggplot() +
  geom_raster(data = df_grace_global, aes(x = lon, y = lat, fill = valor)) +
  geom_sf(data = UF, fill = NA, color = "gray30", linewidth = 0.4) +
  geom_sf(data = bacias, fill = NA, color = "black", linewidth = 0.6) +
  coord_sf(xlim = c(ext_g$xmin, ext_g$xmax), 
           ylim = c(ext_g$ymin, ext_g$ymax), 
           expand = FALSE) +
  scale_fill_gradientn(
    colors = paleta_cores, 
    limits = c(lim_min, lim_max), 
    name = "(cm)"
  ) +
  labs(title = "(a) Grace TWSA", x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed", linewidth = 0.2)
  )

# Plot (b): Downscaled TWSA (com escala gráfica e seta de norte discreta)
p2 <- ggplot() +
  geom_tile(data = df_grace_downscaling, aes(x = lon, y = lat, fill = valor)) +
  geom_sf(data = UF, fill = NA, color = "gray30", linewidth = 0.4) +
  geom_sf(data = bacias, fill = NA, color = "black", linewidth = 0.6) +
  coord_sf(xlim = c(ext_g$xmin, ext_g$xmax), 
           ylim = c(ext_g$ymin, ext_g$ymax), 
           expand = FALSE) +
  scale_fill_gradientn(
    colors = paleta_cores, 
    limits = c(lim_min, lim_max), 
    name = "(cm)"
  ) +
  labs(title = "(b) Downscaled TWSA", x = "Longitude", y = "") +
  # Escala gráfica no canto inferior direito
  annotation_scale(
    location = "br", 
    width_hint = 0.25, 
    style = "ticks", 
    text_cex = 0.75,
    line_width = 0.6,
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.3, "cm")
  ) +
    # Seta preenchida toda preta e sem o "N"
  annotation_north_arrow(
    location = "br", 
    which_north = "true",
    height = unit(0.7, "cm"), 
    width = unit(0.5, "cm"),
    pad_x = unit(0.9, "cm"),
    pad_y = unit(1.1, "cm"),
    style = north_arrow_orienteering(
      fill = c("black", "black"),
      line_col = "black",
      text_size = 0
    )  )+
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.y = element_blank(),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed", linewidth = 0.2)
  )

# =============================================================
# 5. Arranjo final: Barra de cores horizontal abaixo
# =============================================================
p_final <- (p1 | p2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(vjust = 0.85, face = "bold", size = 10),
    legend.key.width = unit(2.2, "cm"),
    legend.key.height = unit(0.35, "cm")
  )

#print(p_final)

# Para salvar em alta resolução:
 ggsave("/mnt/externo/mestrado_note/evapotranspiration_basin/results/comparacao_evapo_global/mapa_comparativo_twsa_2021_05.png", plot = p_final, width = 11, height = 6, dpi = 300)

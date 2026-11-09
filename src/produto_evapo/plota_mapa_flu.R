


library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(purrr)
bacias <- st_read("/mnt/externo/mestrado_note/evapotranspiration_basin/data/shapes/bacias_parana/bacias_agregadas")
UF     <- st_read("/mnt/externo/mestrado_note/evapotranspiration_basin/data/shapes/BR_UF_2022/BR_UF_2022.shp")

# ==============================================================================
# 1. Configuração e Lista das 6 Bacias
# ==============================================================================
bacias_info <- list(
  list(cod = "Alto_Iguacu",   est = "estacao_26025035_tele_flu", nome_exibicao = "Alto Iguaçu"),
  list(cod = "Ivai",          est = "estacao_64693000_conv_flu", nome_exibicao = "Ivaí"),
  list(cod = "Piquiri",       est = "estacao_64830000_conv_flu", nome_exibicao = "Piquiri"),
  list(cod = "Tibagi",        est = "estacao_64506000_conv_flu", nome_exibicao = "Tibagi"),
  list(cod = "Medio_Iguacu",  est = "estacao_65883051_conv_flu", nome_exibicao = "Médio Iguaçu"),
  list(cod = "Baixo_Iguacu",  est = "estacao_25345435_tele_flu", nome_exibicao = "Baixo Iguaçu")
)

caminho_base <- "/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/"

# ==============================================================================
# 2. Carregar Polígonos e Dados Fluviométricos de Cada Bacia
# ==============================================================================
dados_bacias <- lapply(bacias_info, function(b) {
  
  # Caminhos específicos da bacia
  path_shp <- file.path(caminho_base, b$cod, "whitebox_output", b$est, "delimitada.shp")
  path_bin <- file.path(caminho_base, b$cod, "df_data", "df_flu_bacia.bin")
  
  # Validação do shapefile
  if (!file.exists(path_shp)) {
    warning(paste("Shapefile ausente para:", b$cod, "em:", path_shp))
    return(NULL)
  }
  
  # Validação do arquivo binário
  if (!file.exists(path_bin)) {
    warning(paste("Arquivo .bin ausente para:", b$cod, "em:", path_bin))
    return(NULL)
  }
  
  # Leitura e reparo do shapefile
  shp <- st_read(path_shp, quiet = TRUE) %>% 
    st_make_valid() %>% 
    st_transform(st_crs(UF))
  
  cent <- st_centroid(st_union(shp)) %>% st_coordinates()
  
  # Leitura isolada do .bin
  env_temp <- new.env()
  load(path_bin, envir = env_temp)
  
  # Identifica se o objeto salvo foi nomeado como 'df_flu_bacia' ou 'df_bacia_flu'
  if (exists("df_flu_bacia", envir = env_temp)) {
    df_flu <- env_temp$df_flu_bacia
  } else if (exists("df_bacia_flu", envir = env_temp)) {
    df_flu <- env_temp$df_bacia_flu
  } else {
    warning(paste("Nenhum dataframe fluviométrico reconhecido dentro do .bin de:", b$cod))
    return(NULL)
  }
  
  list(
    cod    = b$cod,
    nome   = b$nome_exibicao,
    shp    = shp,
    cx     = cent[1, "X"],
    cy     = cent[1, "Y"],
    df_flu = df_flu
  )
})

# Remove eventuais bacias cujo shapefile não foi encontrado
dados_bacias <- purrr::compact(dados_bacias)

# Verificação se o objeto foi criado corretamente:
length(dados_bacias)

# ==============================================================================
# 3. Preparação Espacial da Base do Paraná e Moldura (+2 graus)
# ==============================================================================
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(purrr)

# ==============================================================================
# 1. Preparação Espacial e Moldura Expandida
# ==============================================================================
uf_pr       <- UF %>% filter(SIGLA_UF == "PR")
bacias_proj <- st_transform(bacias, st_crs(uf_pr))

bbox_pr <- st_bbox(uf_pr)

# Margem horizontal um pouco maior (+2.8 graus) para acomodar os plots alongados
x_min_plot <- bbox_pr["xmin"] - 2.8
x_max_plot <- bbox_pr["xmax"] + 2.8
y_min_plot <- bbox_pr["ymin"] - 2.0
y_max_plot <- bbox_pr["ymax"] + 2.0

# ==============================================================================
# 2. Dimensões dos Insets (Alongados na Horizontal)
# ==============================================================================
largura_box <- 3.4  # Bem mais largo para esticar a série no tempo
altura_box  <- 1.15 # Altura controlada para aspecto retangular

# Distribuição estratégica nas bordas livres
caixas_pos <- list(
  "Ivai"         = list(xmin = x_min_plot + 0.15, xmax = x_min_plot + 0.15 + largura_box, ymin = -23.3, ymax = -23.3 + altura_box, curv = -0.2),
  "Piquiri"      = list(xmin = x_min_plot + 0.15, xmax = x_min_plot + 0.15 + largura_box, ymin = -25.1, ymax = -25.1 + altura_box, curv = -0.15),
  "Baixo_Iguacu" = list(xmin = x_min_plot + 0.3,  xmax = x_min_plot + 0.3 + largura_box,  ymin = y_min_plot + 0.2, ymax = y_min_plot + 0.2 + altura_box, curv = 0.2),
  "Medio_Iguacu" = list(xmin = -52.6,             xmax = -52.6 + largura_box,             ymin = y_min_plot + 0.2, ymax = y_min_plot + 0.2 + altura_box, curv = 0.1),
  "Alto_Iguacu"  = list(xmin = x_max_plot - 0.15 - largura_box, xmax = x_max_plot - 0.15, ymin = -26.0, ymax = -26.0 + altura_box, curv = 0.2),
  "Tibagi"       = list(xmin = x_max_plot - 0.15 - largura_box, xmax = x_max_plot - 0.15, ymin = -23.3, ymax = -23.3 + altura_box, curv = -0.2)
)

# ==============================================================================
# 3. Função do Subgráfico com Y fixo [0, 50] e layout horizontal
# ==============================================================================
criar_grob_serie <- function(df, nome_bacia) {
  p <- ggplot(df, aes(x = Data, y = exult)) +
    geom_line(color = "#084081", linewidth = 0.28, alpha = 0.9) +
    # Eixo X com quebras anuais espaçadas
    scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
    # Eixo Y rigorosamente fixado de 0 a 50 mm
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(title = nome_bacia, x = NULL, y = "mm") +
    theme_minimal(base_size = 7.5) +
    theme(
      plot.title       = element_text(face = "bold", size = 8, color = "#084081", hjust = 0.5, margin = margin(b = 2)),
      axis.title.y     = element_text(size = 6.5, face = "bold", angle = 90, vjust = 0.5),
      axis.text        = element_text(size = 6),
      axis.ticks       = element_line(linewidth = 0.2, color = "gray60"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.2),
      plot.background  = element_rect(fill = "white", color = "gray70", linewidth = 0.4),
      plot.margin      = margin(2, 4, 2, 2)
    )
  
  ggplotGrob(p)
}

# ==============================================================================
# 4. Construção do Mapa Integrado
# ==============================================================================
p_mapa <- ggplot() +
  # Contorno do Paraná
  geom_sf(data = uf_pr, fill = "#fcfcfc", color = "gray50", linewidth = 0.6) +
  # Grade de bacias de fundo
  geom_sf(data = bacias_proj, fill = "gray96", color = "gray85", linewidth = 0.3, linetype = "dashed")

# Adiciona os polígonos das bacias
for (b in dados_bacias) {
  p_mapa <- p_mapa +
    geom_sf(data = b$shp, fill = "#2b83ba", color = "#084081", linewidth = 0.6, alpha = 0.65)
}

# Adiciona as séries alongadas e suas respectivas setas
for (b in dados_bacias) {
  pos <- caixas_pos[[b$cod]]
  g_sub <- criar_grob_serie(b$df_flu, b$nome)
  
  # Âncora no meio da caixa do inset
  x_box_mid <- (pos$xmin + pos$xmax) / 2
  y_box_mid <- (pos$ymin + pos$ymax) / 2
  
  p_mapa <- p_mapa +
    # Seta curva
    annotate(
      "curve",
      x = b$cx, y = b$cy,
      xend = x_box_mid, yend = y_box_mid,
      curvature = pos$curv,
      arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
      color = "#084081",
      linewidth = 0.6
    ) +
    # Gráfico embutido horizontal
    annotation_custom(
      grob = g_sub,
      xmin = pos$xmin, xmax = pos$xmax,
      ymin = pos$ymin, ymax = pos$ymax
    )
}

# Ajuste final de enquadramento e elementos cartográficos
p_mapa_final <- p_mapa +
  annotation_scale(
    location = "bl", 
    width_hint = 0.16, 
    style = "ticks", 
    text_cex = 0.6, 
    line_width = 0.4,
    pad_x = unit(0.4, "cm"),
    pad_y = unit(0.4, "cm")
  ) +
  annotation_north_arrow(
    location = "tl", 
    which_north = "true",
    height = unit(0.6, "cm"), 
    width = unit(0.4, "cm"),
    pad_x = unit(0.4, "cm"),
    pad_y = unit(0.4, "cm"),
    style = north_arrow_orienteering(fill = c("black", "black"), text_size = 0)
  ) +
  coord_sf(
    xlim = c(x_min_plot, x_max_plot),
    ylim = c(y_min_plot, y_max_plot),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = "gray80", linewidth = 0.5),
    plot.margin      = margin(8, 8, 8, 8)
  )

print(p_mapa_final)

# Para salvar em proporção ideal para pôster / artigo:
ggsave("/mnt/externo/mestrado_note/evapotranspiration_basin/results/comparacao_evapo_global/mapa_6_bacias_integradas.png", plot = p_mapa_final, width = 14, height = 9, dpi = 300)























#%% 

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

bacias_info <- list(
  list(cod = "Alto_Iguacu",   est = "estacao_26025035_tele_flu", nome_exibicao = "Alto Iguaçu"),
  list(cod = "Ivai",          est = "estacao_64693000_conv_flu", nome_exibicao = "Ivaí"),
  list(cod = "Piquiri",       est = "estacao_64830000_conv_flu", nome_exibicao = "Piquiri"),
  list(cod = "Tibagi",        est = "estacao_64506000_conv_flu", nome_exibicao = "Tibagi"),
  list(cod = "Medio_Iguacu",  est = "estacao_65883051_conv_flu", nome_exibicao = "Médio Iguaçu"),
  list(cod = "Baixo_Iguacu",  est = "estacao_25345435_tele_flu", nome_exibicao = "Baixo Iguaçu")
)

pasta_saida <- "/mnt/externo/mestrado_note/evapotranspiration_basin/results/comparacao_evapo_global/"

# 1. Função de processamento por item da lista
processar_bacia <- function(info) {
  cod_bacia  <- info$cod
  nome_label <- info$nome_exibicao
  
  load(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/", cod_bacia, "/df_produtos/df_balanco_mensal.bin"))
  df_MODIS_raw <- read.csv(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/data/modis_ET/ET_8dias_", cod_bacia, ".csv")) %>%
    mutate(date = as.Date(date))
  load(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/data/xavier/df_data/df_xavier_", cod_bacia, ".bin"))
  
  df_modis_diario <- df_MODIS_raw %>%
    arrange(date) %>%
    mutate(
      et_modis = mean / 8,
      date_end = lead(date, default = max(date) + days(8)) - days(1)
    ) %>%
    rowwise() %>%
    mutate(data = list(seq(date, date_end, by = "day"))) %>%
    unnest(data) %>%
    ungroup() %>%
    select(data, et_modis)
  
  df_xavier_mensal <- df_xavier %>%
    mutate(Data_Mes = floor_date(data, unit = "month")) %>%
    group_by(Data_Mes) %>%
    summarise(
      eto_xavier = sum(eto, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_modis_mensal <- df_modis_diario %>%
    mutate(Data_Mes = floor_date(data, unit = "month")) %>%
    group_by(Data_Mes) %>%
    summarise(
      et_modis_tot = sum(et_modis, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_balanco_mensal %>%
    left_join(df_xavier_mensal, by = "Data_Mes") %>%
    left_join(df_modis_mensal,  by = "Data_Mes") %>%
    mutate(
      cod_bacia = cod_bacia,
      nome_exibicao = nome_label
    )
}

# 2. Empilhar dados mantendo a ordem exata da lista
ordem_niveis <- sapply(bacias_info, function(x) x$cod)

df_todas_bacias <- bind_rows(lapply(bacias_info, processar_bacia)) %>%
  mutate(cod_bacia = factor(cod_bacia, levels = ordem_niveis))

# 3. Separar camadas de dados
df_balanco_todas <- df_todas_bacias %>%
  select(cod_bacia, nome_exibicao, Data_Mes, evapo) %>%
  filter(!is.na(evapo))

df_linhas_todas <- df_todas_bacias %>%
  select(cod_bacia, nome_exibicao, Data_Mes, et_modis_tot, eto_xavier) %>%
  pivot_longer(
    cols = c(et_modis_tot, eto_xavier),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  mutate(
    variavel = factor(variavel, levels = c("et_modis_tot", "eto_xavier"))
  )

# 4. Coordenadas internas para o rótulo formatado no canto superior esquerdo
df_rotulos <- df_todas_bacias %>%
  group_by(cod_bacia, nome_exibicao) %>%
  summarise(
    x_pos = min(Data_Mes, na.rm = TRUE) + days(45),
    y_pos = max(c(evapo, et_modis_tot, eto_xavier), na.rm = TRUE) * 0.92,
    .groups = "drop"
  )

# 5. Construção gráfica compacta sem cabeçalhos externos
p_painel_6_bacias <- ggplot() +
  
  # CAMADA 1: Área ET Balanço
  geom_area(
    data = df_balanco_todas,
    aes(x = Data_Mes, y = evapo),
    fill  = "#7570B3",
    alpha = 0.35
  ) +
  
  # CAMADA 2: Linha ET Balanço
  geom_line(
    data = df_balanco_todas,
    aes(x = Data_Mes, y = evapo, color = "evapo"),
    linewidth = 0.7
  ) +
  
  # CAMADA 3: Linhas MODIS e Xavier
  geom_line(
    data = df_linhas_todas,
    aes(x = Data_Mes, y = valor, color = variavel),
    linewidth = 0.8
  ) +
  
  # CAMADA 4: Nome de exibição formatado dentro da área útil do plot
  geom_text(
    data = df_rotulos,
    aes(x = x_pos, y = y_pos, label = nome_exibicao),
    hjust = 0,
    vjust = 1,
    fontface = "bold",
    size = 3.3,
    color = "#222222"
  ) +
  
  # Facetamento 1 coluna x 6 linhas
  facet_wrap(~ cod_bacia, ncol = 1, scales = "free_y") +
  
  scale_y_continuous(
    name = "Evapotranspiração (mm/mês)",
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_color_manual(
    values = c(
      "et_modis_tot" = "#d95f02",
      "eto_xavier"   = "#009E73",
      "evapo"        = "#7570B3"
    ),
    labels = c(
      "et_modis_tot" = "ET MODIS",
      "eto_xavier"   = "ETo Xavier",
      "evapo"        = "ET Balanço"
    ),
    breaks = c("et_modis_tot", "eto_xavier", "evapo")
  ) +
  labs(
    x = "Data",
    color = "Variável"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    # Oculta completamente as faixas de cabeçalho externas dos facets
    strip.text = element_blank(),
    strip.background = element_blank(),
    # Junta os gráficos minimizando o espaço entre linhas
    panel.spacing.y = unit(0.2, "lines"),
    axis.title.y = element_text(color = "#333333", face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_painel_6_bacias)

# 6. Salvar imagem
ggsave(
  filename = file.path(pasta_saida, "series_todas_bacias_custom_label.png"),
  plot     = p_painel_6_bacias,
  width    = 12,
  height   = 16,
  dpi      = 300
)

#%% temp
# 1. Função para carregar e agregar dados de balanço e temperatura Xavier
processar_correlacao <- function(info) {
  cod_bacia  <- info$cod
  nome_label <- info$nome_exibicao
  
  load(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/", cod_bacia, "/df_produtos/df_balanco_mensal.bin"))
  load(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/data/xavier/df_data/df_xavier_", cod_bacia, ".bin"))
  
  df_xavier_mensal <- df_xavier %>%
    mutate(Data_Mes = floor_date(data, unit = "month")) %>%
    group_by(Data_Mes) %>%
    summarise(
      tmin = mean(tmin, na.rm = TRUE),
      tmax = mean(tmax, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_balanco_mensal %>%
    select(Data_Mes, evapo) %>%
    left_join(df_xavier_mensal, by = "Data_Mes") %>%
    filter(!is.na(evapo), !is.na(tmin), !is.na(tmax)) %>%
    mutate(
      cod_bacia     = cod_bacia,
      nome_exibicao = nome_label
    )
}

# 2. Empilhar dados mantendo a ordem das bacias
ordem_niveis <- sapply(bacias_info, function(x) x$cod)

df_corr_bruto <- bind_rows(lapply(bacias_info, processar_correlacao)) %>%
  mutate(cod_bacia = factor(cod_bacia, levels = ordem_niveis))

# 3. Pivotar para formato longo (duas colunas: Tmin e Tmax)
df_corr_longo <- df_corr_bruto %>%
  pivot_longer(
    cols = c(tmin, tmax),
    names_to = "tipo_temp",
    values_to = "temp_valor"
  ) %>%
  mutate(
    tipo_temp = factor(
      tipo_temp,
      levels = c("tmin", "tmax"),
      labels = c("Temperatura Mínima (°C)", "Temperatura Máxima (°C)")
    )
  )

# 4. Cálculo dos coeficientes de correlação (R e R²) para os rótulos internos
df_stats_rotulo <- df_corr_longo %>%
  group_by(cod_bacia, nome_exibicao, tipo_temp) %>%
  summarise(
    r_val  = cor(temp_valor, evapo, use = "complete.obs"),
    r2_val = r_val^2,
    x_pos  = min(temp_valor, na.rm = TRUE),
    y_pos  = max(evapo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Rótulo interno com nome da bacia apenas na 1ª coluna (Tmin) para não poluir
    texto_label = ifelse(
      grepl("Mínima", tipo_temp),
      sprintf("%s\nr = %.2f (R² = %.2f)", nome_exibicao, r_val, r2_val),
      sprintf("r = %.2f (R² = %.2f)", r_val, r2_val)
    )
  )

# 5. Gráfico com 6 linhas (bacias) e 2 colunas (Tmin e Tmax)
p_correlacao_6x2 <- ggplot(df_corr_longo, aes(x = temp_valor, y = evapo)) +
  
  # Pontos de dispersão
  geom_point(color = "#7570B3", alpha = 0.5, size = 1.3) +
  
  # Reta de tendência linear
  geom_smooth(method = "lm", se = FALSE, color = "#d95f02", linewidth = 0.8) +
  
  # Rótulo de texto dentro de cada painel
  geom_text(
    data = df_stats_rotulo,
    aes(x = x_pos, y = y_pos, label = texto_label),
    hjust = 0,
    vjust = 1,
    size = 3.2,
    fontface = "bold",
    color = "#222222",
    lineheight = 0.9
  ) +
  
  # Matriz: linhas = bacias, colunas = tipo_temp
  facet_grid(
    rows = vars(cod_bacia),
    cols = vars(tipo_temp),
    scales = "free"
  ) +
  
  scale_y_continuous(
    name = "ET Balanço (mm/mês)",
    expand = expansion(mult = c(0.05, 0.18))
  ) +
  scale_x_continuous(
    name = "Temperatura Média Mensal (°C)",
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    # Mantém cabeçalho apenas no topo das 2 colunas (Tmin e Tmax)
    strip.text.x = element_text(face = "bold", size = 11, margin = margin(b = 6)),
    strip.text.y = element_blank(), # Remove a faixa lateral das bacias
    strip.background = element_blank(),
    panel.spacing.y = unit(0.3, "lines"),
    panel.spacing.x = unit(0.8, "lines"),
    axis.title = element_text(color = "#333333", face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_correlacao_6x2)

# 6. Salvar imagem com proporção vertical para acomodar as 6 linhas
ggsave(
  filename = file.path(pasta_saida, "correlacao_et_temp_6x2.png"),
  plot     = p_correlacao_6x2,
  width    = 9,
  height   = 16,
  dpi      = 300
)

#%% correlacao e rmse
# 1. Função para carregar e alinhar os dados mensais
# 1. Função para carregar e alinhar os dados mensais
carregar_dados_metricas <- function(info) {
  cod_bacia  <- info$cod
  nome_label <- info$nome_exibicao
  
  load(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/", cod_bacia, "/df_produtos/df_balanco_mensal.bin"))
  df_MODIS_raw <- read.csv(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/data/modis_ET/ET_8dias_", cod_bacia, ".csv")) %>%
    mutate(date = as.Date(date))
  load(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/data/xavier/df_data/df_xavier_", cod_bacia, ".bin"))
  
  df_modis_diario <- df_MODIS_raw %>%
    arrange(date) %>%
    mutate(
      et_modis = mean / 8,
      date_end = lead(date, default = max(date) + days(8)) - days(1)
    ) %>%
    rowwise() %>%
    mutate(data = list(seq(date, date_end, by = "day"))) %>%
    unnest(data) %>%
    ungroup() %>%
    select(data, et_modis)
  
  df_xavier_mensal <- df_xavier %>%
    mutate(Data_Mes = floor_date(data, unit = "month")) %>%
    group_by(Data_Mes) %>%
    summarise(
      eto_xavier = sum(eto, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_modis_mensal <- df_modis_diario %>%
    mutate(Data_Mes = floor_date(data, unit = "month")) %>%
    group_by(Data_Mes) %>%
    summarise(
      et_modis_tot = sum(et_modis, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_balanco_mensal %>%
    select(Data_Mes, evapo) %>%
    left_join(df_xavier_mensal, by = "Data_Mes") %>%
    left_join(df_modis_mensal,  by = "Data_Mes") %>%
    mutate(
      cod_bacia = cod_bacia,
      nome_exibicao = nome_label
    )
}

# 2. Consolidação de todas as observações
df_metricas_bruto <- bind_rows(lapply(bacias_info, carregar_dados_metricas))

# 3. Função de cálculo sem as colunas N
calcular_estatisticas <- function(df, rotulo) {
  dados_modis  <- df %>% filter(!is.na(evapo) & !is.na(et_modis_tot))
  dados_xavier <- df %>% filter(!is.na(evapo) & !is.na(eto_xavier))
  
  tibble(
    Bacia = rotulo,
    `r (Evapo x MODIS)` = cor(dados_modis$evapo, dados_modis$et_modis_tot, method = "pearson"),
    `RMSE (Evapo x MODIS) [mm]` = sqrt(mean((dados_modis$evapo - dados_modis$et_modis_tot)^2)),
    `r (Evapo x Xavier)` = cor(dados_xavier$evapo, dados_xavier$eto_xavier, method = "pearson"),
    `RMSE (Evapo x Xavier) [mm]` = sqrt(mean((dados_xavier$evapo - dados_xavier$eto_xavier)^2))
  )
}

# 4. Cálculo por bacia individual
tabela_por_bacia <- df_metricas_bruto %>%
  group_split(cod_bacia) %>%
  lapply(function(sub_df) {
    nome <- unique(sub_df$nome_exibicao)
    calcular_estatisticas(sub_df, nome)
  }) %>%
  bind_rows()

# Garantir a mesma sequência definida na lista
ordem_bacias <- sapply(bacias_info, function(x) x$nome_exibicao)
tabela_por_bacia <- tabela_por_bacia %>%
  arrange(match(Bacia, ordem_bacias))

# 5. Cálculo para a Série Geral (todas as bacias)
tabela_serie_geral <- calcular_estatisticas(df_metricas_bruto, "Série Geral (Todas as Bacias)")

# 6. Unificação e arredondamento
tabela_final <- bind_rows(tabela_por_bacia, tabela_serie_geral) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# 7. Exibição e salvamento
print(kable(tabela_final, align = "lcccc"))

write.csv(
  tabela_final,
  file = file.path(pasta_saida, "tabela_rmse_correlacao_et_modis_xavier.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

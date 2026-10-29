#' Normaliza a vazão pela área da bacia (mm/dia ou l/s/km²)
normalizar_vazao <- function(vazao_m3s, area_km2) {
  # Retorna vazão específica em mm/dia
  # (m³/s * 86400 s/dia) / (area_km² * 1.000.000 m²) * 1000 mm/m = (m³/s * 86.4) / area_km²
  vazao_mm_dia <- (vazao_m3s * 86.4) / area_km2
  return(vazao_mm_dia)
}



# %%
library(sf)
library(dplyr)
library(terra)
library(whitebox)
library(zoo)
library(ggplot2)
library(scales)



# ==============================================================================
# 1. PROCESSAMENTO DA BASE HIDROLÓGICA REGIONAL (EXECUTA 1 VEZ POR MDE)
# ==============================================================================
preparar_base_hidro <- function(r_mde, dir_base = "whitebox_base", threshold = 5000) {
  if (!dir.exists(dir_base)) dir.create(dir_base, recursive = TRUE)
  
  f_streams <- file.path(dir_base, "streams.tif")
  f_pointer <- file.path(dir_base, "flow_output.tif")
  
  if (file.exists(f_streams) && file.exists(f_pointer)) {
    message("[Base] Hidrografia regional existente em: ", dir_base)
    return(invisible(dir_base))
  }
  
  message("[Base] Gerando hidrografia e acumulacao D8...")
  
  # Depressões
  wbt_breach_depressions_least_cost(
    dem    = r_mde, 
    output = file.path(dir_base, "output_breached.tif"), 
    dist   = 5, 
    fill   = TRUE
  )
  wbt_fill_depressions_wang_and_liu(
    dem    = file.path(dir_base, "output_breached.tif"), 
    output = file.path(dir_base, "filled_output.tif")
  )
  
  # Direção e Acumulação D8
  wbt_d8_pointer(
    dem    = file.path(dir_base, "filled_output.tif"), 
    output = f_pointer
  )
  wbt_d8_flow_accumulation(
    input = file.path(dir_base, "filled_output.tif"),
    output = file.path(dir_base, "flow_acumulation_output.tif"))
  
  # Drenagem
  wbt_extract_streams(
    flow_accum = file.path(dir_base, "flow_acumulation_output.tif"), 
    output     = f_streams, 
    threshold  = threshold
  )
  
  wbt_raster_streams_to_vector(
    streams = f_streams, 
    d8_pntr = f_pointer, 
    output  = file.path(dir_base, "streams_lines.shp")
  )
  
  wbt_smooth_vectors(
    input  = file.path(dir_base, "streams_lines.shp"), 
    output = file.path(dir_base, "hidrografia_bacia.shp"), 
    filter = 7
  )
  
  message("[Base] Finalizada com sucesso.")
  return(invisible(dir_base))
}

# ==============================================================================
# 2. DELIMITAÇÃO E CÁLCULO DE ÁREA (INDIVIDUAL)
# ==============================================================================
processar_bacia_estacao <- function(cod_estacao, df_estacoes, dir_base = "whitebox_base", snap_dist = 20) {
  dir_estacao <- file.path(dir_base, paste0("estacao_", cod_estacao))
  if (!dir.exists(dir_estacao)) dir.create(dir_estacao, recursive = TRUE)
  
  f_bacia_shp <- file.path(dir_estacao, "delimitada.shp")
  f_exut_shp  <- file.path(dir_estacao, "ponto_no_rio.shp")
  f_bacia_tif <- file.path(dir_estacao, "delimitada.tif")
  
  est_sf <- df_estacoes %>% filter(Codigo == cod_estacao)
  if (nrow(est_sf) == 0) {
    stop(paste("Estação", cod_estacao, "não encontrada no conjunto de pontos!"))
  }
  
  if (!file.exists(f_bacia_shp)) {
    message("[Delimitação] Processando estação: ", cod_estacao)
    
    ponto_shp <- file.path(dir_estacao, "station_point.shp")
    st_write(est_sf[, "Codigo"], ponto_shp, delete_layer = TRUE, quiet = TRUE)
    
    wbt_jenson_snap_pour_points(
      pour_pts  = ponto_shp,
      streams   = file.path(dir_base, "streams.tif"),
      output    = f_exut_shp,
      snap_dist = snap_dist
    )
    
    wbt_watershed(
      d8_pntr  = file.path(dir_base, "flow_output.tif"),
      pour_pts = f_exut_shp,
      output   = f_bacia_tif
    )
    
    wbt_raster_to_vector_polygons(
      input  = f_bacia_tif, 
      output = f_bacia_shp
    )
  }
  
  bacia_sf <- st_read(f_bacia_shp, quiet = TRUE) %>% 
    st_set_crs(st_crs(est_sf)) %>% 
    st_make_valid()
  
  area_km2 <- as.numeric(sum(st_area(bacia_sf))) / 1e6
  
  return(data.frame(
    Codigo      = as.character(cod_estacao),
    area_km2    = area_km2,
    caminho_shp = f_bacia_shp,
    stringsAsFactors = FALSE
  ))
}

# ==============================================================================
# 3. DELIMITAÇÃO EM LOTE PARA VETOR DE ESTAÇÕES
# ==============================================================================
obter_areas_lote <- function(codigos, df_estacoes, dir_base = "whitebox_base", snap_dist = 20) {
  codigos <- unique(as.character(codigos))
  res <- lapply(codigos, function(cod) {
    processar_bacia_estacao(
      cod_estacao = cod,
      df_estacoes = df_estacoes,
      dir_base    = dir_base,
      snap_dist   = snap_dist
    )
  })
  do.call(rbind, res)
}

# ==============================================================================
# 4. PREENCHIMENTO GENÉRICO DE FALHAS (1 ALVO + N AUXILIARES/VERTENTES)
# ==============================================================================
preencher_falhas_regressao_generica <- function(df_alvo, df_auxiliares, df_areas, cod_alvo) {
  # df_alvo: data.frame com colunas c("Data", "exult")
  # df_auxiliares: data.frame com c("Data", "var1", "var2", ...) onde os nomes batem com df_areas$Codigo
  # df_areas: data.frame c("Codigo", "area_km2")
  
  cod_alvo <- as.character(cod_alvo)
  area_alvo <- df_areas$area_km2[df_areas$Codigo == cod_alvo]
  
  if (length(area_alvo) == 0) {
    stop("Área do alvo ", cod_alvo, " não encontrada em df_areas!")
  }
  
  # Junta séries temporais
  df_completo <- inner_join(df_alvo, df_auxiliares, by = "Data")
  
  # Normalização por vazão específica (Q / Área)
  df_norm <- df_completo
  df_norm$exult <- df_norm$exult / area_alvo
  
  colunas_aux <- setdiff(names(df_auxiliares), "Data")
  for (col in colunas_aux) {
    area_aux <- df_areas$area_km2[df_areas$Codigo == col]
    if (length(area_aux) > 0) {
      df_norm[[col]] <- df_norm[[col]] / area_aux
    }
  }
  
  # --- ETAPA 1: Regressão Linear Múltipla ---
  formula_mult <- as.formula(paste("exult ~", paste(colunas_aux, collapse = " + ")))
  mod_mult <- lm(formula_mult, data = df_norm, na.action = na.exclude)
  pred_mult <- predict(mod_mult, newdata = df_norm)
  
  df_norm$exult_reg <- if_else(is.na(df_norm$exult), pred_mult, df_norm$exult)
  
  # --- ETAPA 2: Fallback com Regressões Lineares Simples ---
  # Roda modelos univariados para cada auxiliar ordenada por correlação
  if (any(is.na(df_norm$exult_reg))) {
    correlacoes <- sapply(colunas_aux, function(col) {
      cor(df_norm$exult, df_norm[[col]], use = "complete.obs")
    })
    colunas_ordenadas <- names(sort(correlacoes, decreasing = TRUE, na.last = TRUE))
    
    for (col in colunas_ordenadas) {
      if (!any(is.na(df_norm$exult_reg))) break
      f_simples <- as.formula(paste("exult ~", col))
      mod_s <- lm(f_simples, data = df_norm, na.action = na.exclude)
      pred_s <- predict(mod_s, newdata = df_norm)
      df_norm$exult_reg <- if_else(is.na(df_norm$exult_reg), pred_s, df_norm$exult_reg)
    }
  }
  
  # --- ETAPA 3: Interpolação Linear Temporal ---
  df_norm$exult_comp <- df_norm$exult_reg
  if (any(is.na(df_norm$exult_comp))) {
    df_norm$exult_comp <- na.approx(df_norm$exult_comp, na.rm = FALSE)
    df_norm$exult_comp <- na.locf(df_norm$exult_comp, na.rm = FALSE)
    df_norm$exult_comp <- na.locf(df_norm$exult_comp, fromLast = TRUE, na.rm = FALSE)
  }
  
  # Desnormaliza para m³/s
  return(data.frame(
    Data       = df_completo$Data,
    original   = df_completo$exult,
    preenchido = df_norm$exult_reg * area_alvo,
    completo   = df_norm$exult_comp * area_alvo
  ))
}

# ==============================================================================
# 5. PLOTS FLU 
# ==============================================================================

gerar_produtos_hidro_e_plots_old <- function(nome_bacia,
                                        estacao_exutoria,
                                        todas_estacoes,
                                        dados_finais,
                                        df_areas,
                                        df_codigos_flu_estacao,
                                        pontos_na_bacia,
                                        bacia_limite,
                                        dir_wbt,
                                        pasta_saida,
                                        bacias_shape = NULL,
                                        ufs_shape = NULL) {
  
  if (!dir.exists(pasta_saida)) dir.create(pasta_saida, recursive = TRUE)
  
  # --- 1. CARREGAR CAMADAS DO WHITEBOX ---
  f_filled <- file.path(dir_wbt, "filled_output.tif")
  f_recorte <- file.path(dir_wbt, "dem_recorte.tif")
  dem_base <- if (file.exists(f_filled)) rast(f_filled) else rast(f_recorte)
  
  f_hidro <- file.path(dir_wbt, "hidrografia_bacia.shp")
  rivers_sf <- if (file.exists(f_hidro)) st_read(f_hidro, quiet = TRUE) else NULL
  
  lista_bacias <- lapply(todas_estacoes, function(cod) {
    f_shp <- file.path(dir_wbt, paste0("estacao_", cod), "delimitada.shp")
    if (file.exists(f_shp)) {
      shp <- st_read(f_shp, quiet = TRUE) %>% st_make_valid()
      shp$Codigo <- cod
      return(shp[, c("Codigo", "geometry")])
    }
    return(NULL)
  })
  bacias_todas_sf <- do.call(rbind, lista_bacias[!sapply(lista_bacias, is.null)])
  
  bacia_principal_sf <- bacias_todas_sf %>% filter(Codigo == estacao_exutoria)
  f_exut_shp <- file.path(dir_wbt, paste0("estacao_", estacao_exutoria), "ponto_no_rio.shp")
  exut_sf <- if (file.exists(f_exut_shp)) st_read(f_exut_shp, quiet = TRUE) else NULL
  
  area_exut_km2 <- df_areas$area_km2[df_areas$Codigo == estacao_exutoria]
  area_exut_m2  <- area_exut_km2 * 1e6
  
  
  # --- 2. PLOT 1: DISPONIBILIDADE TEMPORAL DAS ESTAÇÕES ---
  message("[Plots] Gerando mapa de disponibilidade...")
  cex_min <- 0.6
  cex_max <- 3.0
  n_dias_scaled <- scales::rescale(pontos_na_bacia$n_dias, to = c(cex_min, cex_max))
  
  ponto_destaque <- df_codigos_flu_estacao %>% filter(Codigo == estacao_exutoria)
  coords_destaque <- st_coordinates(ponto_destaque)
  
  bb <- st_bbox(bacia_limite)
  xlim_exp <- c(bb["xmin"] - 0.1, bb["xmax"] + 0.1)
  ylim_exp <- c(bb["ymin"] - 0.1, bb["ymax"] + 0.1)
  
  f_mapa_disp <- file.path(pasta_saida, paste0(estacao_exutoria, "_mapa_flu_disponibilidade.png"))
  png(f_mapa_disp, width = 2200, height = 1900, res = 300)
  
  par(fig = c(0, 1, 0, 1), mar = c(4, 4, 4, 8))
  
  plot(
    dem_base,
    xlim = xlim_exp, ylim = ylim_exp,
    col = terrain.colors(25),
    main = paste0("Disponibilidade Fluviométrica - ", nome_bacia),
    legend.args = list(text = "Elevação (m)", side = 4, line = 3.5, cex = 0.8),
    legend.width = 0.5
  )
  
  if (!is.null(bacias_shape)) plot(st_geometry(bacias_shape), add = TRUE, col = NA, border = "gray40", lwd = 1)
  if (!is.null(ufs_shape))    plot(st_geometry(ufs_shape), add = TRUE, col = NA, border = "black", lwd = 1.2)
  
  plot(
    st_geometry(pontos_na_bacia),
    add = TRUE,
    pch = 21,
    bg = adjustcolor("#1f78b4", 0.6),
    col = "white",
    lwd = 0.6,
    cex = n_dias_scaled
  )
  
  plot(
    st_geometry(ponto_destaque),
    add = TRUE,
    pch = 21,
    bg = "red",
    col = "yellow",
    lwd = 1.5,
    cex = 1.8
  )
  
  text(
    x = coords_destaque[1],
    y = coords_destaque[2],
    labels = paste0(ponto_destaque$Nome, " (", ponto_destaque$Codigo, ")"),
    pos = 4,
    cex = 0.75,
    font = 2
  )
  
  val_leg <- c(min(pontos_na_bacia$n_dias, na.rm = TRUE),
               round(mean(pontos_na_bacia$n_dias, na.rm = TRUE)),
               max(pontos_na_bacia$n_dias, na.rm = TRUE))
  tam_leg <- scales::rescale(val_leg, to = c(cex_min, cex_max))
  
  legend(
    "bottomleft",
    legend = paste(val_leg, "dias"),
    pt.cex = tam_leg,
    pch = 21,
    pt.bg = adjustcolor("#1f78b4", 0.6),
    col = "white",
    title = "Disponibilidade",
    bty = "n",
    cex = 0.7
  )
  
  # Mini mapa contextual
  if (!is.null(ufs_shape)) {
    par(fig = c(0.05, 0.28, 0.10, 0.35), mar = c(0, 0, 0, 0), new = TRUE)
    PR <- ufs_shape %>% filter(SIGLA_UF == "PR")
    plot(st_geometry(PR), col = "grey90", border = "black", lwd = 0.8)
    plot(st_geometry(bacia_limite), col = adjustcolor("blue", 0.4), border = "blue", lwd = 1.2, add = TRUE)
  }
  
  dev.off()
  
  
  # --- 3. PLOT 2: BACIAS DELIMITADAS ---
  message("[Plots] Gerando mapa das bacias delimitadas...")
  f_mapa_delimitada <- file.path(pasta_saida, paste0(estacao_exutoria, "_delimitada.png"))
  png(f_mapa_delimitada, width = 2000, height = 1800, res = 300)
  
  plot(dem_base, col = gray.colors(25, start = 0.85, end = 0.3), legend = FALSE, 
       main = paste0("Bacias Delimitadas - ", nome_bacia))
  
  if (!is.null(bacias_todas_sf)) {
    plot(st_geometry(bacias_todas_sf), border = "orange", lwd = 1.2, col = adjustcolor("orange", 0.15), add = TRUE)
  }
  if (nrow(bacia_principal_sf) > 0) {
    plot(st_geometry(bacia_principal_sf), border = "darkgreen", lwd = 2.2, col = NA, add = TRUE)
  }
  if (!is.null(rivers_sf)) {
    plot(st_geometry(rivers_sf), col = "steelblue", lwd = 0.8, add = TRUE)
  }
  if (!is.null(exut_sf)) {
    plot(st_geometry(exut_sf), col = "red", pch = 19, cex = 1.6, add = TRUE)
  }
  
  legend("topleft", 
         legend = c("Bacia Principal", "Sub-bacias / Afluentes", "Exutório"),
         col = c("darkgreen", "orange", "red"),
         lwd = c(2.2, 1.2, NA),
         pch = c(NA, NA, 19),
         bg = "white", cex = 0.75, bty = "o")
  
  dev.off()
  
  
  # --- 4. CONVERSÃO PARA mm/dia E GRÁFICO DA SÉRIE TEMPORAL ---
  message("[Plots] Gerando gráfico e tabela da vazão em mm/dia...")
  fator_mm_dia <- (86400 / area_exut_m2) * 1000
  
  dados_convertidos <- dados_finais %>%
    mutate(
      original_mm   = original * fator_mm_dia,
      preenchido_mm = preenchido * fator_mm_dia,
      completo_mm   = completo * fator_mm_dia
    )
  
  grafico_flu <- ggplot(dados_convertidos, aes(x = Data)) +
    geom_line(aes(y = completo_mm, color = "Preenchido / Interpolado"), linewidth = 0.6) +
    geom_line(aes(y = original_mm, color = "Observado"), linewidth = 0.7) +
    scale_color_manual(
      name = "Série Fluviométrica",
      values = c("Observado" = "#1e824c", "Preenchido / Interpolado" = "#e67e22")
    ) +
    labs(
      title = paste0("Série de Vazão Específica - ", nome_bacia, " (Estação ", estacao_exutoria, ")"),
      subtitle = paste0("Área de Drenagem: ", round(area_exut_km2, 2), " km²"),
      x = "Ano",
      y = "Escoamento (mm/dia)"
    ) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14)
    )
  
  ggsave(
    filename = file.path(pasta_saida, paste0(estacao_exutoria, "_historico_flu.png")),
    plot = grafico_flu,
    width = 14,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  
  write.csv(
    dados_convertidos, 
    file = file.path(pasta_saida, paste0(estacao_exutoria, "_serie_flu_preenchida.csv")), 
    row.names = FALSE
  )
  
  message("[Produtos] Concluído com sucesso na pasta: ", pasta_saida)
  
  return(invisible(dados_convertidos))
}

library(ggplot2)
library(scales)
library(terra)
library(sf)
library(dplyr)

gerar_produtos_hidro_e_plots <- function(nome_bacia,
                                        estacao_exutoria,
                                        todas_estacoes,
                                        dados_finais,
                                        df_areas,
                                        df_codigos_flu_estacao,
                                        pontos_na_bacia,
                                        bacia_limite,
                                        dir_wbt,
                                        pasta_saida,
                                        bacias_shape = NULL,
                                        ufs_shape = NULL) {
  
  if (!dir.exists(pasta_saida)) dir.create(pasta_saida, recursive = TRUE)
  
  # --- 1. CARREGAR CAMADAS DO WHITEBOX ---
  f_filled  <- file.path(dir_wbt, "filled_output.tif")
  f_recorte <- file.path(dir_wbt, "dem_recorte.tif")
  dem_base  <- if (file.exists(f_filled)) rast(f_filled) else rast(f_recorte)
  
  f_hidro   <- file.path(dir_wbt, "hidrografia_bacia.shp")
  rivers_sf <- if (file.exists(f_hidro)) st_read(f_hidro, quiet = TRUE) else NULL
  
  # Carrega todas as delimitações existentes
  lista_bacias <- lapply(todas_estacoes, function(cod) {
    f_shp <- file.path(dir_wbt, paste0("estacao_", cod), "delimitada.shp")
    if (file.exists(f_shp)) {
      shp <- st_read(f_shp, quiet = TRUE) %>% st_make_valid()
      shp$Codigo <- as.character(cod)
      return(shp[, c("Codigo", "geometry")])
    }
    return(NULL)
  })
  bacias_todas_sf <- do.call(rbind, lista_bacias[!sapply(lista_bacias, is.null)])
  
  # AJUSTE 1: Separação estrita entre bacia principal e sub-bacias auxiliares
  bacia_principal_sf <- bacias_todas_sf %>% filter(Codigo == as.character(estacao_exutoria))
  sub_bacias_sf      <- bacias_todas_sf %>% filter(Codigo != as.character(estacao_exutoria))
  
  f_exut_shp <- file.path(dir_wbt, paste0("estacao_", estacao_exutoria), "ponto_no_rio.shp")
  exut_sf    <- if (file.exists(f_exut_shp)) st_read(f_exut_shp, quiet = TRUE) else NULL
  
  area_exut_km2 <- df_areas$area_km2[df_areas$Codigo == as.character(estacao_exutoria)]
  area_exut_m2  <- area_exut_km2 * 1e6
  
  
  # --- 2. PLOT 1: DISPONIBILIDADE TEMPORAL DAS ESTAÇÕES ---
  message("[Plots] Gerando mapa de disponibilidade...")
  cex_min <- 0.6
  cex_max <- 3.0
  n_dias_scaled <- scales::rescale(pontos_na_bacia$n_dias, to = c(cex_min, cex_max))
  
  ponto_destaque  <- df_codigos_flu_estacao %>% filter(Codigo == as.character(estacao_exutoria))
  coords_destaque <- st_coordinates(ponto_destaque)
  
  bb <- st_bbox(bacia_limite)
  xlim_exp <- c(bb["xmin"] - 0.1, bb["xmax"] + 0.1)
  ylim_exp <- c(bb["ymin"] - 0.1, bb["ymax"] + 0.1)
  
  f_mapa_disp <- file.path(pasta_saida, paste0(estacao_exutoria, "_mapa_flu_disponibilidade.png"))
  png(f_mapa_disp, width = 2200, height = 1900, res = 300)
  
  par(fig = c(0, 1, 0, 1), mar = c(4, 4, 4, 8))
  
  plot(
    dem_base,
    xlim = xlim_exp, ylim = ylim_exp,
    col = terrain.colors(25),
    main = paste0("Disponibilidade Fluviométrica - ", nome_bacia),
    legend.args = list(text = "Elevação (m)", side = 4, line = 3.5, cex = 0.8),
    legend.width = 0.5
  )
  
  if (!is.null(bacias_shape)) plot(st_geometry(bacias_shape), add = TRUE, col = NA, border = "gray40", lwd = 1)
  if (!is.null(ufs_shape))    plot(st_geometry(ufs_shape), add = TRUE, col = NA, border = "black", lwd = 1.2)
  
  plot(
    st_geometry(pontos_na_bacia),
    add = TRUE,
    pch = 21,
    bg = adjustcolor("#1f78b4", 0.6),
    col = "white",
    lwd = 0.6,
    cex = n_dias_scaled
  )
  
  plot(
    st_geometry(ponto_destaque),
    add = TRUE,
    pch = 21,
    bg = "red",
    col = "yellow",
    lwd = 1.5,
    cex = 1.8
  )
  
  text(
    x = coords_destaque[1],
    y = coords_destaque[2],
    labels = paste0(ponto_destaque$Nome, " (", ponto_destaque$Codigo, ")"),
    pos = 4,
    cex = 0.75,
    font = 2
  )
  
  val_leg <- c(min(pontos_na_bacia$n_dias, na.rm = TRUE),
               round(mean(pontos_na_bacia$n_dias, na.rm = TRUE)),
               max(pontos_na_bacia$n_dias, na.rm = TRUE))
  tam_leg <- scales::rescale(val_leg, to = c(cex_min, cex_max))
  
  legend(
    "bottomleft",
    legend = paste(val_leg, "dias"),
    pt.cex = tam_leg,
    pch = 21,
    pt.bg = adjustcolor("#1f78b4", 0.6),
    col = "white",
    title = "Disponibilidade",
    bty = "n",
    cex = 0.7
  )
  
  if (!is.null(ufs_shape)) {
    par(fig = c(0.05, 0.28, 0.10, 0.35), mar = c(0, 0, 0, 0), new = TRUE)
    PR <- ufs_shape %>% filter(SIGLA_UF == "PR")
    plot(st_geometry(PR), col = "grey90", border = "black", lwd = 0.8)
    plot(st_geometry(bacia_limite), col = adjustcolor("blue", 0.4), border = "blue", lwd = 1.2, add = TRUE)
  }
  
  dev.off()
  
  
  # --- 3. PLOT 2: BACIAS DELIMITADAS ---
  message("[Plots] Gerando mapa das bacias delimitadas...")
  f_mapa_delimitada <- file.path(pasta_saida, paste0(estacao_exutoria, "_delimitada.png"))
  png(f_mapa_delimitada, width = 2000, height = 1800, res = 300)
  
  plot(dem_base, col = gray.colors(25, start = 0.85, end = 0.3), legend = FALSE, 
       main = paste0("Bacias Delimitadas - ", nome_bacia))
  
  # Plota sub-bacias auxiliares somente se existirem
  tem_subbacias <- !is.null(sub_bacias_sf) && nrow(sub_bacias_sf) > 0
  if (tem_subbacias) {
    plot(st_geometry(sub_bacias_sf), border = "orange", lwd = 1.2, col = adjustcolor("orange", 0.15), add = TRUE)
  }
  
  # Bacia principal
  if (nrow(bacia_principal_sf) > 0) {
    plot(st_geometry(bacia_principal_sf), border = "darkgreen", lwd = 2.2, col = NA, add = TRUE)
  }
  
  if (!is.null(rivers_sf)) {
    plot(st_geometry(rivers_sf), col = "steelblue", lwd = 0.8, add = TRUE)
  }
  if (!is.null(exut_sf)) {
    plot(st_geometry(exut_sf), col = "red", pch = 19, cex = 1.6, add = TRUE)
  }
  
# AJUSTE 2: Plota legenda APENAS se houver sub-bacias para diferenciar
  if (tem_subbacias) {
    leg_text <- c("Bacia Principal", "Sub-bacias / Afluentes")
    leg_col  <- c("darkgreen", "orange")
    leg_lwd  <- c(2.2, 1.2)
    leg_pch  <- c(NA, NA)
    
    if (!is.null(exut_sf)) {
      leg_text <- c(leg_text, "Exutório")
      leg_col  <- c(leg_col, "red")
      leg_lwd  <- c(leg_lwd, NA)
      leg_pch  <- c(leg_pch, 19)
    }
    
    legend("topleft", 
           legend = leg_text,
           col    = leg_col,
           lwd    = leg_lwd,
           pch    = leg_pch,
           bg     = "white", 
           cex    = 0.75, 
           bty    = "o")
  }
  
  dev.off()
  
  
  # --- 4. CONVERSÃO PARA mm/dia E GRÁFICO DA SÉRIE TEMPORAL ---
  message("[Plots] Gerando gráfico e tabela da vazão em mm/dia...")
  fator_mm_dia <- (86400 / area_exut_m2) * 1000
  
  dados_convertidos <- dados_finais %>%
    mutate(
      original_mm   = original * fator_mm_dia,
      preenchido_mm = preenchido * fator_mm_dia,
      completo_mm   = completo * fator_mm_dia
    )
  
  # AJUSTE 3: Adapta o gráfico se houve preenchimento real ou se é puramente original
  houve_consistencia <- !identical(dados_convertidos$original_mm, dados_convertidos$completo_mm) &&
                        any(!is.na(dados_convertidos$completo_mm) & is.na(dados_convertidos$original_mm))
  
  grafico_flu <- ggplot(dados_convertidos, aes(x = Data))
  
  if (houve_consistencia) {
    grafico_flu <- grafico_flu +
      geom_line(aes(y = completo_mm, color = "Preenchido / Interpolado"), linewidth = 0.6) +
      geom_line(aes(y = original_mm, color = "Observado"), linewidth = 0.7) +
      scale_color_manual(
        name = "Série Fluviométrica",
        values = c("Observado" = "#1e824c", "Preenchido / Interpolado" = "#e67e22")
      )
  } else {
    grafico_flu <- grafico_flu +
      geom_line(aes(y = original_mm), color = "#1e824c", linewidth = 0.7, na.rm = TRUE)
  }
  
  grafico_flu <- grafico_flu +
    labs(
      title = paste0("Série de Vazão Específica - ", nome_bacia, " (Estação ", estacao_exutoria, ")"),
      subtitle = paste0("Área de Drenagem: ", round(area_exut_km2, 2), " km²"),
      x = "Ano",
      y = "Escoamento (mm/dia)"
    ) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14)
    )
  
  ggsave(
    filename = file.path(pasta_saida, paste0(estacao_exutoria, "_historico_flu.png")),
    plot = grafico_flu,
    width = 14,
    height = 5,
    dpi = 300,
    bg = "white"
  )
  
  write.csv(
    dados_convertidos, 
    file = file.path(pasta_saida, paste0(estacao_exutoria, "_serie_flu_preenchida.csv")), 
    row.names = FALSE
  )
  
  message("[Produtos] Concluído com sucesso na pasta: ", pasta_saida)
  
  return(invisible(dados_convertidos))
}
library(data.table)
library(tidyr)
library(dplyr)
library(sf)
library(lubridate)

load("df_plu.bin") #2428 estaçoes
load("df_flu.bin")
load("inventario.bin")
load("2026_recortado.bin")

brasil=st_read("../../data/shapes/BR_UF_2022/BR_UF_2022.shp")
brasil_sf <- st_transform(brasil, crs = 4674)

contorno_grade=st_read("../../data/shapes/contorno_grade/contorno_grade_parana.shp")
#df_plu$Data.1<-NULL

bacias=st_read("../../data/shapes/bacias_parana/bacias_agregadas/bacias_agregadas.shp")
bacias_sf <- st_transform(bacias, crs = 4674)
#############################################
# Concistencia de chuva
#############################################
{
## 1. Identificar quais colunas NÃO são 100% cheias de NA
# Mantemos a primeira coluna (Data) obrigatoriamente ------ de 3568 -> 2428
colunas_validas <- c(TRUE, sapply(df_plu[-1], function(coluna) !all(is.na(coluna))))
# 2. Filtrar o data frame mantendo apenas as colunas válidas
df_plu_filtrado <- df_plu[, colunas_validas]
# 3. Atualizar o seu df original
df_plu <- df_plu_filtrado
df_plu$Data.1<-NULL

##1. Aplica a consistência: substitui valores < 0 ou > 200 por NA
df_plu_range <- df_plu %>%
  mutate(across(
    .cols = -Data, # Aplica em todas as colunas, exceto na coluna "Data"
    .fns = ~ ifelse(.x < 0 | .x > 1000, NA, .x)))
df_plu <- df_plu_range

##2. Aplica a consistência: de  o dia anterior E o seguinte são NAs então recebe NA
df_plu <- df_plu %>%
# Garante que os dados estão na ordem cronológica correta antes de olhar o vizinho
arrange(Data) %>% 
# Aplica a transformação em todas as colunas, exceto na coluna 'Data'
mutate(across(-Data, ~ ifelse(is.na(lag(.)) & is.na(lead(.)), NA, .)))

## remover_travados_e_zerados 
remover_travados_e_zerados <- function(vetor, n_dias = 3) {
  # Se a série toda for apenas 0 e NAs (sem nenhum valor > 0), invalida a coluna inteira
  if (all(is.na(vetor) | vetor == 0)) {
    return(rep(NA_real_, length(vetor)))
  }
  
  # Aplica a regra de valores repetidos travados (> 0 por >= n_dias)
  r <- rle(vetor)
  travados <- rep(r$lengths >= n_dias & !is.na(r$values) & r$values > 0, r$lengths)
  vetor[travados] <- NA
  
  return(vetor)
}

# Aplica a consistência nas colunas
df_plu <- df_plu %>%
  arrange(Data) %>%
  mutate(across(-Data, ~ remover_travados_e_zerados(.x, n_dias = 3)))

# Em seguida, remove as colunas que ficaram 100% NA
df_plu <- df_plu[, c(TRUE, sapply(df_plu[-1], function(col) !all(is.na(col))))]

## 4. Remover estações com menos de 30 dias de dados válidos no total
min_dias <- 30

# Mantém 'Data' (TRUE) + colunas onde a soma de dias com dados (!is.na) é >= 30
colunas_com_historico_minimo <- c(
  TRUE, 
  colSums(!is.na(df_plu[-1])) >= min_dias
)

# Filtra o df_plu mantendo apenas as estações com histórico suficiente
df_plu <- df_plu[, colunas_com_historico_minimo]

}

# -------------------------------------------------------------------------
# inventario só pras df_plu
# -------------------------------------------------------------------------
{
library(dplyr)

# 1. Obter os códigos de estações presentes nas colunas de df_plu (excluindo a coluna 'Data')
estacoes_df_plu <- setdiff(colnames(df_plu), "Data")

# 2. Filtrar o inventário para manter apenas essas estações
inventario <- inventario %>%
  filter(Codigo %in% estacoes_df_plu)

# 1. Remover duplicatas por 'Codigo'
inventario <- inventario %>%
  distinct(Codigo, .keep_all = TRUE)
}
#############################################
# Verificar as estações para consistir / excluir
#############################################
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(ggplot2)

# -------------------------------------------------------------------------
# 1. Passo 1: Matriz/Lista de Proximidade (Buffer de 5 km) --- talvez aumentar pra 10
# -------------------------------------------------------------------------

{
library(dplyr)

inventario_sf <- inventario %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4674)
  
matriz_dist <- st_distance(inventario_sf)
dimnames(matriz_dist) <- list(inventario_sf$Codigo, inventario_sf$Codigo)

  
# Parâmetros
n_vizinhos <- 5       # Número máximo de vizinhas desejadas
raio_max_m <- 50000   # Raio máximo de busca em metros (20 km)

vizinhos <- apply(matriz_dist, 1, function(linha) {
  # 1. Filtra distâncias maiores que zero e dentro do raio máximo de 20 km
  validas <- linha[linha > 0 & linha <= raio_max_m]
  
  if (length(validas) == 0) {
    return(character(0)) # Nenhuma vizinha dentro dos 20 km
  }
  
  # 2. Ordena as distâncias da menor para a maior
  validas_ordenadas <- sort(validas)
  
  # 3. Pega os nomes das N primeiras mais próximas (até o limite de 5)
  top_vizinhos <- names(head(validas_ordenadas, n_vizinhos))
  
  return(top_vizinhos)
})
}

#### plot estaçoes sem vizinhos
{
library(ggplot2)
library(sf)
library(dplyr)

# 1. Garantir o alinhamento das estações sem vizinhas
estacoes_sem_vizinhos <- names(vizinhos)[lengths(vizinhos) == 0]

# Filtrar a geometria apenas das estações isoladas
estacoes_isoladas_sf <- inventario_sf %>%
  filter(Codigo %in% estacoes_sem_vizinhos)


# 3. Plot do Mapa com ggplot2
ggplot() +
  # Camada do contorno do Paraná / Área de Estudo
  geom_sf(data = contorno, fill = "gray95", color = "black", linewidth = 0.6) +
  
  # Camada das Bacias Hidrográficas
  geom_sf(data = bacias_sf, fill = NA, color = "dodgerblue4", linewidth = 0.4, alpha = 0.6) +
  
  # PONTOS: Estações com vizinhas (Cinza ao fundo)
  geom_sf(
    data = inventario_sf %>% filter(!Codigo %in% estacoes_sem_vizinhos), 
    color = "gray60", size = 1, alpha = 0.4
  ) +
  
  # PONTOS: As estações sem vizinhas em DESTAQUE (Vermelho)
  geom_sf(data = estacoes_isoladas_sf, color = "red", size = 2.5, alpha = 0.9) +
  
  # Rótulos e Tema
  theme_bw() +
  labs(
    title = paste("Espacialização das", nrow(estacoes_isoladas_sf), "Estações Pluviométricas Isoladas"),
    subtitle = "Pontos em vermelho não possuem vizinhas no raio/critério estabelecido",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted")
  )
}
# -------------------------------------------------------------------------
# 2. Passo 2: Data Frame com a Série da Estação e a Médiana das Vizinhas
# -------------------------------------------------------------------------
{
library(dplyr)
library(purrr)

estacoes_validas <- intersect(names(vizinhos), colnames(df_plu))

library(matrixStats)

series_estacoes <- map(estacoes_validas, function(est_foco) {
  vizinhas <- vizinhos[[est_foco]]
  vizinhas_presentes <- intersect(vizinhas, colnames(df_plu))
  
  # Garante que a estação foco não esteja por engano entre as vizinhas
  vizinhas_presentes <- setdiff(vizinhas_presentes, est_foco)
  
  if (length(vizinhas_presentes) == 0) {
    return(NULL) # Sem vizinhas válidas com dados no df_plu
  }
  
  # Extrai matriz das vizinhas
  matriz_viz <- as.matrix(df_plu[, vizinhas_presentes, drop = FALSE])
  
  # === ALTERAÇÃO AQUI: Alta performance em C ===
  mediana_viz <- rowMedians(matriz_viz, na.rm = TRUE)
  mediana_viz[is.nan(mediana_viz)] <- NA
  
  # Constrói o data frame da estação atual
  data.frame(
    Data = df_plu$Data,
    Codigo_Estacao = est_foco,
    Estacao_Foco = df_plu[[est_foco]],
    Mediana_Vizinhas = mediana_viz, # Renomeado para clareza
    stringsAsFactors = FALSE
  )
})

# Empilha os resultados de todas as estações válidas
df_series_comparativas <- bind_rows(series_estacoes)

}
# -------------------------------------------------------------------------
# 3. Passo 3: Cálculo do Coeficiente pbias
# -------------------------------------------------------------------------
{
library(dplyr)

calcular_consistencia_completa <- function(estacao_foco_vetor, mediana_vizinhas_vetor) {
  validos <- !is.na(estacao_foco_vetor) & !is.na(mediana_vizinhas_vetor)
  n_dias <- sum(validos)
  
  # Se houver menos de 30 dias de dados simultâneos
  if (n_dias < 30) { 
    return(data.frame(
      b = NA_real_, r2_acum = NA_real_, 
      pbias = NA_real_, ratio_escala_media = NA_real_, ratio_escala_cv = NA_real_,
      kge = NA_real_, n_dias = n_dias
    ))
  }
  
  x <- mediana_vizinhas_vetor[validos] # Média vizinhas (Diária)
  y <- estacao_foco_vetor[validos]   # Estação foco (Diária)
  
  # 1. Dupla massa (Tendência acumulada)
  p_foco_acum <- cumsum(y)
  p_mediana_acum <- cumsum(x)
  
  # Caso todos os valores acumulados sejam 0
  if (max(p_mediana_acum) == 0) {
    return(data.frame(
      b = NA_real_, r2_acum = NA_real_, 
      pbias = NA_real_, ratio_escala_media = NA_real_, ratio_escala_cv = NA_real_,
      kge = NA_real_, n_dias = n_dias
    ))
  }
  
  fit <- lm(p_foco_acum ~ 0 + p_mediana_acum)
  b_val <- unname(coef(fit)[1])
  r2_val <- summary(fit)$r.squared
  
  # 2. Métricas Diárias de Escala
  sum_x <- sum(x)
  pbias_val <- if (!is.na(sum_x) && sum_x > 0) ((sum(y) - sum_x) / sum_x) * 100 else NA_real_
  
  mean_x <- mean(x, na.rm = TRUE)
  mean_y <- mean(y, na.rm = TRUE)
  ratio_media <- if (!is.na(mean_x) && mean_x > 0) mean_y / mean_x else NA_real_
  
  sd_x <- sd(x, na.rm = TRUE)
  sd_y <- sd(y, na.rm = TRUE)
  
  cv_x <- if (!is.na(mean_x) && mean_x > 0 && !is.na(sd_x)) sd_x / mean_x else NA_real_
  cv_y <- if (!is.na(mean_y) && mean_y > 0 && !is.na(sd_y)) sd_y / mean_y else NA_real_
  
  # Correção do erro: teste seguro de cv_x
  ratio_cv <- if (!is.na(cv_x) && cv_x > 0 && !is.na(cv_y)) cv_y / cv_x else NA_real_
  
  # Correlação diária (proteção contra variância zero)
  r_diario <- if (!is.na(sd_x) && !is.na(sd_y) && sd_x > 0 && sd_y > 0) {
    cor(x, y)
  } else {
    NA_real_
  }
  
  # 3. Kling-Gupta Efficiency (KGE)
  kge_val <- if (!is.na(r_diario) && !is.na(ratio_media) && !is.na(ratio_cv)) {
    1 - sqrt((r_diario - 1)^2 + (ratio_media - 1)^2 + (ratio_cv - 1)^2)
  } else {
    NA_real_
  }
  
  return(data.frame(
    b = b_val, 
    r2_acum = r2_val, 
    pbias = pbias_val,               # % de super/subestimação em volume
    kge = kge_val,                    # Métrica global (Forma + Escala + Variabilidade)
    n_dias = n_dias
  ))
}

# Aplicar ao dataset sem erros
resultado_consistencia <- df_series_comparativas %>%
  group_by(Codigo_Estacao) %>%
  reframe(calcular_consistencia_completa(Estacao_Foco, Mediana_Vizinhas))

# Visualizar o resultado final ordenado
head(resultado_consistencia)
}


#estaçoes escluidas a mao
{
library(dplyr)

# 1. Sua lista de estações alvo
estacoes_alvo <- c(
  "2548079_tele_plu", "65220001_tele_plu", "73699000_tele_plu",
  "73710000_tele_plu", "74200980_tele_plu", "81018900_tele_plu", "81420000_tele_plu",
  "64230350_tele_plu", "65136550_tele_plu", "73680800_tele_plu", "83570000_tele_plu",
  "65980200_tele_plu", "2549040_tele_plu",  "73290100_tele_plu", "2548094_tele_plu",
  "2549127_tele_plu",  "2453084_tele_plu",  "2352072_tele_plu",  "2350079_tele_plu",
  "2549042_tele_plu",  "2253024_tele_plu",  "2549123_tele_plu",  "2353055_tele_plu",
  "2549110_tele_plu",  "65006040_tele_plu", "2550077_tele_plu",  "2549111_tele_plu",
  "2549114_tele_plu",  "2351076_tele_plu",  "2653010_tele_plu",  "2350074_tele_plu",
  "2653067_tele_plu",  "2453083_tele_plu",  "63750000_tele_plu", "62378005_tele_plu",
  "65270000_tele_plu", "65954150_tele_plu", "25425350_tele_plu", "2551064_tele_plu",
  "74319000_tele_plu", "73331920_tele_plu", "73513000_tele_plu", "73511000_tele_plu", 
  "2548098_tele_plu",  "62379050_tele_plu", "62400600_tele_plu", "82234000_tele_plu", 
  "65775901_tele_plu", "64632200_tele_plu", "64632100_tele_plu", "65370001_tele_plu", 
  "65960001_tele_plu", "65970001_tele_plu", "65764001_tele_plu", "2351080_tele_plu",  
  "65987001_tele_plu", "65365801_tele_plu", "65260001_tele_plu", "65035001_tele_plu", 
  "65060001_tele_plu", "65824880_tele_plu", "81303001_tele_plu", "2548083_conv_plu", 
  "81299001_tele_plu", "62820080_tele_plu", "65855110_tele_plu", "73511300_tele_plu", 
  "82121002_tele_plu", "73680900_tele_plu", "64452190_tele_plu", "71386500_tele_plu", 
  "2750065_conv_plu",  "74359000_tele_plu", "64482000_tele_plu", "82336000_tele_plu", 
  "64450001_tele_plu", "2453067_conv_plu",  "62510100_tele_plu", "71351000_tele_plu", 
  "65819300_tele_plu", "65819300_conv_plu", "2554001_conv_plu",  "2554035_conv_plu",  
  "73693800_tele_plu", "25874857_tele_plu"
)

# 2. Filtrar e ordenar os resultados
resultado_lista_especifica <- resultado_consistencia %>%
  filter(Codigo_Estacao %in% estacoes_alvo) %>%
  arrange(desc(b))

# 3. Exibir todas as linhas no console do R
print(resultado_lista_especifica, n = Inf)
}
#descobrir pq deu NA no pbias:

estacoes_pbias_na <- resultado_consistencia %>%
  filter(is.na(pbias)) %>%
  pull(Codigo_Estacao)
x<-resultado_consistencia %>% filter(Codigo_Estacao %in% estacoes_pbias_na) %>% select(Codigo_Estacao, n_dias)
df_sub <- df_series_comparativas[df_series_comparativas$Codigo_Estacao %in% x$Codigo_Estacao, ]
##localização:
{
library(ggplot2)
library(sf)

# 1. Obter o sf apenas das 3 estações
estacoes_3_na_sf <- inventario_sf %>%
  filter(Codigo %in% estacoes_pbias_na)

# 2. Plot com o contorno e bacias
ggplot() +
  geom_sf(data = contorno, fill = "gray98", color = "black") +
  geom_sf(data = bacias_sf, fill = NA, color = "dodgerblue4", alpha = 0.5) +
  geom_sf(data = estacoes_3_na_sf, color = "red", size = 4) +
  geom_sf_text(data = estacoes_3_na_sf, aes(label = Codigo), vjust = -1, size = 3) +
  theme_bw() +
  labs(
    title = "Localização das 3 Estações com PBIAS NA",
    x = "Longitude", y = "Latitude"
  )
  }
  
#A Série
{
# 1. Filtrar as 16 estações e reorganizar para formato longo (facilitar a legenda)
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Reorganizar os dados para formato longo (apenas valores não NAs)
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Formatar dados para formato longo
df_plot_16 <- df_sub %>%
  pivot_longer(
    cols = c(Estacao_Foco, Mediana_Vizinhas),
    names_to = "Serie",
    values_to = "Precipitacao"
  ) %>%
  filter(!is.na(Precipitacao))

# 2. Calcular a média por Estação e Série + Ponto Médio no tempo para posicionar o texto
df_medias <- df_plot_16 %>%
  group_by(Codigo_Estacao, Serie) %>%
  summarise(
    Media = mean(Precipitacao, na.rm = TRUE),
    Data_Mid = min(Data) + (max(Data) - min(Data)) / 2, # Ponto central no eixo X
    .groups = "drop"
  ) %>%
  mutate(
    Rotulo = paste0(round(Media, 2), " mm")
  )

# 3. Gerar o gráfico com rótulos EXATAMENTE EM CIMA das linhas
ggplot(df_plot_16, aes(x = Data, y = Precipitacao, color = Serie)) +
  # Séries Temporais (Linhas e Pontos)
  geom_line(alpha = 0.4, linewidth = 0.35) +
  geom_point(size = 0.5, alpha = 0.3) +
  
  # Linhas Horizontais das Médias
  geom_hline(
    data = df_medias,
    aes(yintercept = Media, color = Serie),
    linetype = "dashed",
    linewidth = 0.6,
    alpha = 0.9
  ) +
  
  # Texto posicionado DIRETAMENTE SOBRE a linha de cada média (y = Media)
  geom_text(
    data = df_medias,
    aes(
      x = Data_Mid, 
      y = Media, 
      label = Rotulo, 
    ),
    color = "black",
    vjust = -0.4,       # Descola o texto ligeiramente para CIMA da linha
    hjust = 0.5,        # Centraliza o texto no meio do gráfico
    size = 5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  # Painel de 16 subplots
  facet_wrap(~ Codigo_Estacao, scales = "free_y", ncol = 4) +
  
  # Cores e Legenda
  scale_color_manual(
    values = c("Estacao_Foco" = "firebrick", "Mediana_Vizinhas" = "dodgerblue3"),
    labels = c("Estação Foco", "Mediana das Vizinhas")
  ) +
  
  theme_bw() +
  labs(
    title = "Comparativo Temporal de Precipitação com Média nas Linhas",
    subtitle = "Valores de média marcados diretamente sobre as linhas tracejadas de referência",
    x = "Data",
    y = "Precipitação (mm)",
    color = "Série"
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray92", color = "gray70"),
    strip.text = element_text(face = "bold", size = 7.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5),
    axis.text.y = element_text(size = 6.5),
    panel.grid.minor = element_blank()
  )
}


# -------------------------------------------------------------------------
# 4. Dado que o limiar é +-50% e NAs com diferença na media em 2mm/d
# -------------------------------------------------------------------------
{
library(dplyr)

# 1. Calcular a média diária de cada estação e das vizinhas no df_series_comparativas
medias_diarias <- df_series_comparativas %>%
  group_by(Codigo_Estacao) %>%
  summarise(
    media_foco = mean(Estacao_Foco, na.rm = TRUE),
    media_viz = mean(Mediana_Vizinhas, na.rm = TRUE),
    diff_media = abs(mean(Estacao_Foco, na.rm = TRUE) - mean(Mediana_Vizinhas, na.rm = TRUE)),
    .groups = "drop"
  )

# 2. Unir as médias ao resultado_consistencia e filtrar aprovadas + resgatadas
estacoes_aprovadas <- resultado_consistencia %>%
  left_join(medias_diarias, by = "Codigo_Estacao") %>%
  filter(
    # Critério 1: Aprovação normal por PBIAS (para séries com sobreposição)
    (!is.na(pbias) & pbias >= -50 & pbias <= 50) |
    
    # Critério 2: Resgate de pbias == NA via diferença de média <= 2 mm
    (is.na(pbias) & !is.na(diff_media) & diff_media <= 2.0)
  ) %>%
  pull(Codigo_Estacao)

# 3. Filtrar o df_plu mantendo as colunas válidas
colunas_manter <- c("Data", intersect(estacoes_aprovadas, colnames(df_plu)))

df_plu_consistido <- df_plu %>%
  select(all_of(colunas_manter))

# 4. Verificação do resgate
cat("Total de estações no df_plu_consistido (incluindo Data):", length(colunas_manter), "\n")
cat("Estações com pbias = NA resgatadas:", 
    sum(is.na(resultado_consistencia$pbias[resultado_consistencia$Codigo_Estacao %in% estacoes_aprovadas])), "\n")

}
# -------------------------------------------------------------------------
# 4. Plot do Mapa Espacial
# -------------------------------------------------------------------------
{
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. Calcular a Precipitação Média Anual Considerante o Mínimo de 300 Dias Válidos
# -----------------------------------------------------------------------------
precip_anual_df <- df_plu_consistido %>%
  # Passa o data frame para formato longo (Data, Codigo, Chuva)
  pivot_longer(
    cols = -Data, 
    names_to = "Codigo", 
    values_to = "Chuva"
  ) %>%
  # Extrai o Ano da Data
  mutate(Ano = format(Data, "%Y")) %>%
  # Agrupa por Estação e Ano para calcular o acumulado de cada ano
  group_by(Codigo, Ano) %>%
  summarise(
    dias_validos = sum(!is.na(Chuva)),
    soma_anual = sum(Chuva, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # FILTRO CRÍTICO: Mantém apenas os anos com pelo menos 300 dias com dados
  filter(dias_validos >= 300) %>%
  # Agrupa por Estação para tirar a média dos anos válidos
  group_by(Codigo) %>%
  summarise(
    anos_completos = n(),
    media_anual_mm = mean(soma_anual, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 2. Unir com o Inventário Espacial (inventario_sf)
# -----------------------------------------------------------------------------
estacoes_precip_sf <- inventario_sf %>%
  inner_join(precip_anual_df, by = "Codigo")

# -----------------------------------------------------------------------------
# 3. Gerar o Mapa Geográfico
# -----------------------------------------------------------------------------
mapa_precip_anual <- ggplot() +
  geom_sf(data = brasil_sf, fill = "gray98", color = "gray80", linewidth = 0.3) +
  geom_sf(data = bacias_sf, fill = NA, color = "dodgerblue4", linewidth = 0.3, alpha = 0.5) +
  geom_sf(data = contorno, fill = NA, color = "black", linewidth = 0.5) +
   
  geom_sf(
    data = estacoes_precip_sf,  
    aes(color = media_anual_mm),  
    size = 2,  
    alpha = 0.85
  ) +
  
  scale_color_gradientn(
    colors = c("#d73027", "#fc8d59", "#fee090", "#e0f3f8", "#91bfdb", "#4575b4", "#7b3294"),
    limits = c(500, 3500),
    oob = scales::squish,
    name = "Média Anual\n(mm/ano)"
  ) +
   
  coord_sf(
    xlim = c(st_bbox(estacoes_precip_sf)["xmin"] - 0.5, st_bbox(estacoes_precip_sf)["xmax"] + 0.5),
    ylim = c(st_bbox(estacoes_precip_sf)["ymin"] - 0.5, st_bbox(estacoes_precip_sf)["ymax"] + 0.5),
    expand = FALSE
  ) +
   
  labs(
    title = "Média Anual de Precipitação (Anos com ≥ 300 dias válidos)",
    subtitle = paste("Mapeamento espacial para", nrow(estacoes_precip_sf), "estações consistidas"),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed", linewidth = 0.2),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13)
  )

print(mapa_precip_anual)
}

########################################### espacialização

df_plu <- df_plu_consistido



#############################################
# grade 0.1 pra espacializar a chuva
#############################################
sf_use_s2(FALSE)
grade_01_sf <- st_make_grid(contorno_grade, cellsize = c(0.1, 0.1), what = "polygons") %>%  st_sf()
grade_01_recortada <- st_intersection(grade_grade_01_sf, contorno) %>%   st_make_valid()

# 3. Extrair os centroides temporários para fazer a ordenação geográfica
centroides_temp <- st_centroid(grade_01_recortada) %>%   st_coordinates() %>%   as.data.frame() %>% 
  rename(lon = X, lat = Y)
# 4. Acoplar as coordenadas ao objeto sf e ORDENAR:
# Primeiro por Latitude DECRESCENTE (começa no Norte/cima e vai descendo)
# Depois por Longitude CRESCENTE (da esquerda/Oeste para a direita/Leste)
grade_01_recortada <- grade_01_recortada %>% mutate(lon_temp = centroides_temp$lon, lat_temp = centroides_temp$lat) %>% 
  arrange(desc(lat_temp), lon_temp) %>% 
  # Agora que está na ordem certa de cima para baixo, criamos os IDs sequenciais
  mutate(grid_id = paste0("Grid_01_", sprintf("%04d", 1:n()))) %>% 
  select(-lon_temp, -lat_temp) # Remove as colunas temporárias
# 5. Criar a sua tabela final de coordenadas já na ordem correta (de cima para baixo)
coords_nova_grade <- st_centroid(grade_01_recortada) %>%   st_coordinates() %>% 
  as.data.frame() %>%   rename(lon = X, lat = Y) %>%  mutate(grid_id = grade_01_recortada$grid_id)
sf_use_s2(TRUE)

#############################################
# grade 0.1 pra espacializar a chuva
#############################################
library(sf)
library(gstat)
library(dplyr)
library(tidyr)
library(pbapply) # Para a barra de progresso (fundamental para 8.766 dias)

# ---- 1. Prepara os Alvos (Nova Grade de 0.1°) ----
grid_target_sf <- st_as_sf(
  coords_nova_grade, 
  coords = c("lon", "lat"), 
  crs = 4326
)

# ---- 2. Prepara as Estações ----
# Garante que as coordenadas estão alinhadas com as colunas do df_plu
inventario_validos <- inventario %>%
  filter(Codigo %in% names(df_plu)) %>%
  distinct(Codigo, .keep_all = TRUE)

estacoes_sf <- st_as_sf(
  inventario_validos, 
  coords = c("Longitude", "Latitude"), 
  crs = 4326
)

# ---- 3. Cria a Matriz Base do df_plu (Muito mais rápido que data.frame) ----
datas_serie <- df_plu$Data
matriz_chuva <- as.matrix(df_plu[, -1])
nomes_estacoes <- names(df_plu)[-1]

# ---- 4. A Função Otimizada para 1 Dia ----
# Como o seu mínimo é de 883 estações, removemos os testes condicionais para poupar tempo.
idw_diario_rapido <- function(i) {
  
  # 1. Extrai a linha do dia 'i'
  chuva_hoje <- matriz_chuva[i, ]
  
  # 2. Identifica quais estações têm dado hoje (ignora NAs)
  tem_dado <- !is.na(chuva_hoje)
  
  # 3. Filtra as estações geográficas e os valores correspondentes
  # Usa o match para garantir que a ordem dos pontos sf e da chuva seja exata
  idx_match <- match(nomes_estacoes[tem_dado], inventario_validos$Codigo)
  
  pontos_hoje <- estacoes_sf[idx_match, ]
  pontos_hoje$chuva <- chuva_hoje[tem_dado]
  
  # 4. Roda o IDW (idp = 2)
  modelo <- gstat(formula = chuva ~ 1, data = pontos_hoje, set = list(idp = 2))
  pred <- predict(modelo, newdata = grid_target_sf, debug.level = 0)
  
  # 5. Retorna só o vetor numérico (mais leve que retornar data frames)
  return(pred$var1.pred)
}

# ---- 5. O Loop de Processamento (Vai demorar um pouco!) ----
cat("Iniciando o IDW para", length(datas_serie), "dias. Pegue um café!\n")
{
# O pblapply mostra a barra de progresso no console
lista_resultados <- pblapply(seq_along(datas_serie), idw_diario_rapido)

# ---- 6. Montagem da Tabela Final (No formato exato do df_grid_historico) ----
# Transforma a lista de vetores em uma grande matriz e depois em data.frame
df_grid_interpolado <- as.data.frame(do.call(rbind, lista_resultados))
names(df_grid_interpolado) <- grid_target_sf$grid_id

# Adiciona a coluna Data como a primeira
df_grid_interpolado <- cbind(Data = datas_serie, df_grid_interpolado)

# Salva imediatamente para você não perder o trabalho de processamento
saveRDS(df_grid_interpolado, "Grade_IDW_Diaria.rds")
save(df_grid_interpolado, file="df_chuva_espacializada_v4.bin")
}
#################################
## PLOT IDW
## 
{
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)

# 1. Calcular o acumulado ANUAL e depois a MEDIANA por grid
df_acumulado_medio <- df_grid_interpolado %>%
  mutate(Ano = year(Data)) %>%
  group_by(Ano) %>%
  summarise(across(starts_with("Grid_"), sum, na.rm = TRUE)) %>%
  select(-Ano) %>%
  summarise(across(everything(), median, na.rm = TRUE)) %>%
  pivot_longer(
    cols = everything(), 
    names_to = "grid_id", 
    values_to = "chuva_media_anual"
  )

# 2. Unir com os polígonos da grade de 0,1°
mapa_climatologia <- grade_01_recortada %>%
  left_join(df_acumulado_medio, by = "grid_id")

# 3. Gerar o plot da Climatologia Anual
idw_plot <- ggplot() +
  # 1. BASE: Preenchimento colorido da grade
  geom_sf(data = mapa_climatologia, aes(fill = chuva_media_anual), color = NA) +
  
  # 2. FUNDO VETORIAL: Divisas políticas do Brasil (desenhado PRIMEIRO)
  geom_sf(data = brasil_sf, fill = NA, color = "gray60", linewidth = 0.2) +
  
  # 3. INTERMEDIÁRIO: Sub-bacias (desenhado POR CIMA do Brasil para cobrir divisas coincidentes)
  geom_sf(data = bacias, fill = NA, color = "gray20", linewidth = 0.45, linetype = "solid") +
  
  # 4. TOPO ABSOLUTO: Contorno preto da área de estudo (cobre tudo nos limites)
  geom_sf(data = contorno_grade, fill = NA, color = "black", linewidth = 0.9) +
  
# Estilização das Cores
  scale_fill_viridis_c(
    option = "mako", 
    direction = -1, 
    limits = c(NA, 2700), # <--- ADICIONE ESTA LINHA (fixa o limite máximo em 2500)
    name = "Precipitação\n(mm/ano)",
    labels = scales::number_format(suffix = " mm")
  ) +
  
  labs(x = "Longitude", y = "Latitude") +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.2, linetype = "dashed"),
    legend.box = "horizontal",
    legend.title = element_text(face = "bold", size = 10, vjust = 0.8),
    legend.text = element_text(size = 9),
    legend.position = "right",
    axis.title = element_text(face = "bold")
  ) +
  coord_sf(
    xlim = c(st_bbox(contorno_grade)["xmin"], st_bbox(contorno_grade)["xmax"]),
    ylim = c(st_bbox(contorno_grade)["ymin"], st_bbox(contorno_grade)["ymax"])
  )

# 4. Salvar o mapa
ggsave(
  filename = "../../results/precipitacao_vazao/media_acumulada_grid.png",
  plot = idw_plot,
  width = 8.5,      
  height = 7,     
  dpi = 300,        
  bg = "white"
)
}




######################################## old
# -------------------------------------------------------------------------
# 5. encontrar as de maior media
# -------------------------------------------------------------------------
library(dplyr)

# Ordena do maior para o menor valor de precipitação média anual
media_anual_estacoes <- media_anual_estacoes %>%
  arrange(desc(media_anual_mm))

# Visualiza as 20 estações com maiores médias (para identificar os outliers/erros)
head(media_anual_estacoes, 20)

####-------------------------------------------------------------------------
# FUNÇÃO PARA INVESTIGAR ESTAÇÃO x VIZINHAS (MAPA + SÉRIE TEMPORAL)
# -------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(patchwork)

investigar_estacao <- function(codigo_alvo, 
                               num_vizinhos = 4, 
                               df_dados = df_plu, 
                               sf_estacoes = estacoes_plu_sf, 
                               sf_mapa = brasil_sf,
                               sf_grade = contorno_grade,
                               sf_bacias = bacias_sf) {
  
  # -------------------------------------------------------------------------
  # 1. SELEÇÃO DA ESTAÇÃO ALVO E DAS VIZINHAS PRÓXIMAS
  # -------------------------------------------------------------------------
  {
  alvo_sf <- sf_estacoes %>% filter(Codigo == codigo_alvo)
  
  if (nrow(alvo_sf) == 0) {
    stop("Código da estação não encontrado na camada espacial 'sf_estacoes'!")
  }
  
  # Calcula distância e obtém as N estações mais próximas
  distancias <- st_distance(sf_estacoes, alvo_sf)
  
  sf_estacoes_dist <- sf_estacoes %>%
    mutate(dist_km = as.numeric(distancias) / 1000) %>%
    filter(Codigo != codigo_alvo) %>%
    arrange(dist_km) %>%
    slice(1:num_vizinhos)
  
  codigos_grupo <- c(codigo_alvo, sf_estacoes_dist$Codigo)
  
  grupo_sf <- sf_estacoes %>%
    filter(Codigo %in% codigos_grupo) %>%
    mutate(Tipo = ifelse(Codigo == codigo_alvo, "Investigada (Alvo)", "Vizinha"))
  
  # -------------------------------------------------------------------------
  # 2. MAPA PRINCIPAL (ZOOM DETALHADO DO GRUPO COM BACIAS)
  # -------------------------------------------------------------------------
  bbox_grupo <- st_bbox(grupo_sf)
  margem <- 0.4
  
  p_mapa_detalhado <- ggplot() +
    # Fundo 1: Estados
    geom_sf(data = sf_mapa, fill = "gray98", color = "gray80", linewidth = 0.3) +
    
    # Fundo 2: Bacias Hidrográficas
    geom_sf(data = sf_bacias, fill = "#e0f2fe", color = "#0284c7", linewidth = 0.4, alpha = 0.4) +
    
    # Pontos e Textos das Estações
    geom_sf(data = grupo_sf, aes(color = Tipo, shape = Tipo), size = 3.5) +
    geom_sf_text(
      data = grupo_sf, 
      aes(label = Codigo, color = Tipo), 
      vjust = -0.8, 
      size = 3.2, 
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_color_manual(values = c("Investigada (Alvo)" = "red", "Vizinha" = "#1f78b4")) +
    scale_shape_manual(values = c("Investigada (Alvo)" = 17, "Vizinha" = 16)) +
    coord_sf(
      xlim = c(bbox_grupo["xmin"] - margem, bbox_grupo["xmax"] + margem),
      ylim = c(bbox_grupo["ymin"] - margem, bbox_grupo["ymax"] + margem),
      expand = FALSE
    ) +
    labs(
      title = paste("Localização Espacial - Estação", codigo_alvo),
      subtitle = paste("Investigada +", num_vizinhos, "vizinhas mais próximas"),
      x = "Longitude", y = "Latitude",
      color = "Tipo de Estação", shape = "Tipo de Estação"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 11)
    )

  # -------------------------------------------------------------------------
  # 3. MAPA DE CONTEXTO (INSET) LIMITADO POR CONTORNO_GRADE
  # -------------------------------------------------------------------------
  bbox_rect <- st_as_sfc(bbox_grupo)
  bbox_grade <- st_bbox(sf_grade)
  
  p_inset <- ggplot() +
    # Fundo regional (estados)
    geom_sf(data = sf_mapa, fill = "gray92", color = "white", linewidth = 0.2) +
    # Bacias na visão geral
    geom_sf(data = sf_bacias, fill = "gray85", color = "gray70", linewidth = 0.2) +
    # Limites da grade de estudo
    geom_sf(data = sf_grade, fill = NA, color = "black", linewidth = 0.5) +
    # Destaque do zoom atual
    geom_sf(data = bbox_rect, fill = NA, color = "red", linewidth = 0.8) +
    coord_sf(
      xlim = c(bbox_grade["xmin"], bbox_grade["xmax"]),
      ylim = c(bbox_grade["ymin"], bbox_grade["ymax"]),
      expand = TRUE
    ) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
      plot.margin = margin(0, 0, 0, 0)
    )

  # Inserção do mini-mapa no canto do mapa detalhado
  p_mapa <- p_mapa_detalhado + 
    inset_element(
      p_inset, 
      left = 0.02, 
      bottom = 0.65, 
      right = 0.35, 
      top = 0.98
    )
  }
  # -------------------------------------------------------------------------
  # 4. SÉRIE TEMPORAL COMPARATIVA
  # -------------------------------------------------------------------------
  cols_disponiveis <- intersect(codigos_grupo, colnames(df_dados))
  
  df_serie <- df_dados %>% # df_dados[5500:8000, ] %>% #  #8766
    select(Data, all_of(cols_disponiveis)) %>%
    pivot_longer(
      cols = -Data,
      names_to = "Codigo",
      values_to = "precip_mm"
    ) %>%
    mutate(
      Tipo = ifelse(Codigo == codigo_alvo, "Investigada (Alvo)", "Vizinha")
    ) %>%
    arrange(desc(Tipo == "Investigada (Alvo)"))

  p_serie <- ggplot(
    df_serie, 
    aes(
      x = Data, 
      y = precip_mm, 
      color = Codigo, 
      linetype = Tipo, 
      linewidth = Tipo, 
      alpha = Tipo
    )
  ) +
    geom_line() +
    scale_linetype_manual(values = c("Investigada (Alvo)" = "dotted", "Vizinha" = "solid")) +
    scale_linewidth_manual(values = c("Investigada (Alvo)" = 0.6, "Vizinha" = 0.5)) +
    scale_alpha_manual(values = c("Investigada (Alvo)" = 0.6, "Vizinha" = 0.8)) +
    labs(
      title = "Série Temporal de Precipitação Diária (mm)",
      subtitle = "Comparativo para consistência de dados",
      x = "Data", 
      y = "Precipitação (mm/dia)",
      color = "Estação",
      linetype = "Tipo de Estação",
      linewidth = "Tipo de Estação",
      alpha = "Tipo de Estação"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 11)
    )

  # -------------------------------------------------------------------------
  # 5. JUNÇÃO DOS PAINÉIS
  # -------------------------------------------------------------------------
  figura_final <- p_mapa + p_serie + plot_layout(widths = c(1, 1.4))
  return(figura_final)
}


# Testar para as est. com maior média anual 
{
#excluir:
investigar_estacao(codigo_alvo = "73331920_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "73513000_tele_plu", num_vizinhos = 8)
investigar_estacao(codigo_alvo = "73511000_tele_plu", num_vizinhos = 8)
investigar_estacao(codigo_alvo = "2548098_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "62379050_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "62400600_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "82234000_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65775901_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "64632200_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "64632100_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65370001_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65960001_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65970001_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65764001_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "2351080_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65987001_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65365801_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65310001_tele_plu", num_vizinhos = 4) a partir de 2015
investigar_estacao(codigo_alvo = "65260001_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65259000_tele_plu", num_vizinhos = 4) só maio/2025
investigar_estacao(codigo_alvo = "65035001_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65060001_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65824880_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "81303001_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "2548083_conv_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65100001_tele_plu", num_vizinhos = 4) a partir de 2015
investigar_estacao(codigo_alvo = "81299001_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "62820080_tele_plu", num_vizinhos = 4) 
investigar_estacao(codigo_alvo = "65855080_tele_plu", num_vizinhos = 4) até 2021
investigar_estacao(codigo_alvo = "65855110_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "73511300_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "73511000_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "82121002_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "73680900_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "64452190_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "71386500_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "2750065_conv_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "74359000_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "64482000_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "82336000_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "64450001_tele_plu", num_vizinhos = 8)
investigar_estacao(codigo_alvo = "2453067_conv_plu", num_vizinhos = 8)
investigar_estacao(codigo_alvo = "62510100_tele_plu", num_vizinhos = 8)
investigar_estacao(codigo_alvo = "71351000_tele_plu", num_vizinhos = 8)
investigar_estacao(codigo_alvo = "65819300_tele_plu", num_vizinhos = 8)
investigar_estacao(codigo_alvo = "65819300_tele_plu", num_vizinhos = 8)
investigar_estacao(codigo_alvo = "65819300_conv_plu", num_vizinhos = 8)

# Testar para a 1ª estação com maior média anual
#excluir:
investigar_estacao(codigo_alvo = "2548079_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "65220001_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "82188000_tele_plu", num_vizinhos = 4)#excluir 2019 e 2020
investigar_estacao(codigo_alvo = "73699000_tele_plu", num_vizinhos = 4)
investigar_estacao(codigo_alvo = "73710000_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "74200980_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "81018900_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "81420000_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "64230350_tele_plu", num_vizinhos =1)
investigar_estacao(codigo_alvo = "65136550_tele_plu", num_vizinhos =1)
investigar_estacao(codigo_alvo = "73680800_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "83570000_tele_plu", num_vizinhos =1)
investigar_estacao(codigo_alvo = "65980200_tele_plu", num_vizinhos =1)
investigar_estacao(codigo_alvo = "2549040_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "73290100_tele_plu", num_vizinhos =1)
investigar_estacao(codigo_alvo = "2548094_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2549127_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2453084_tele_plu", num_vizinhos =1)
investigar_estacao(codigo_alvo = "2352072_tele_plu", num_vizinhos =3)
investigar_estacao(codigo_alvo = "2350079_tele_plu", num_vizinhos =1)
investigar_estacao(codigo_alvo = "2549042_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2253024_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2549123_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2353055_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2549110_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "65006040_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2550077_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2549111_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2549114_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2351076_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2653010_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2350074_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2653067_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2453083_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "63750000_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "62378005_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "65270000_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "65954150_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "25425350_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "2551064_tele_plu", num_vizinhos =2)
investigar_estacao(codigo_alvo = "74319000_tele_plu", num_vizinhos =1)
}

library(dplyr)
library(lubridate)

# 1. Lista de estações a serem removidas COMPLETAMENTE
estacoes_excluir_total <- unique(c(
  # Novas estações adicionadas
  "2548079_tele_plu", "65220001_tele_plu", "73699000_tele_plu",
  "73710000_tele_plu", "74200980_tele_plu", "81018900_tele_plu", "81420000_tele_plu",
  "64230350_tele_plu", "65136550_tele_plu", "73680800_tele_plu", "83570000_tele_plu",
  "65980200_tele_plu", "2549040_tele_plu",  "73290100_tele_plu", "2548094_tele_plu",
  "2549127_tele_plu",  "2453084_tele_plu",  "2352072_tele_plu",  "2350079_tele_plu",
  "2549042_tele_plu",  "2253024_tele_plu",  "2549123_tele_plu",  "2353055_tele_plu",
  "2549110_tele_plu",  "65006040_tele_plu", "2550077_tele_plu",  "2549111_tele_plu",
  "2549114_tele_plu",  "2351076_tele_plu",  "2653010_tele_plu",  "2350074_tele_plu",
  "2653067_tele_plu",  "2453083_tele_plu",  "63750000_tele_plu", "62378005_tele_plu",
  "65270000_tele_plu", "65954150_tele_plu", "25425350_tele_plu", "2551064_tele_plu",
  "74319000_tele_plu", "73331920_tele_plu", "73513000_tele_plu","73511000_tele_plu", "2548098_tele_plu",
  "62379050_tele_plu", "62400600_tele_plu", "82234000_tele_plu", "65775901_tele_plu",
  "64632200_tele_plu", "64632100_tele_plu", "65370001_tele_plu", "65960001_tele_plu",
  "65970001_tele_plu", "65764001_tele_plu", "2351080_tele_plu",  "65987001_tele_plu",
  "65365801_tele_plu", "65260001_tele_plu", "65035001_tele_plu", "65060001_tele_plu",
  "65824880_tele_plu", "81303001_tele_plu", "2548083_conv_plu", "81299001_tele_plu",
  "62820080_tele_plu", "65855110_tele_plu", "73511300_tele_plu", "82121002_tele_plu",
  "73680900_tele_plu", "64452190_tele_plu", "71386500_tele_plu", "2750065_conv_plu",
  "74359000_tele_plu", "64482000_tele_plu", "82336000_tele_plu", "64450001_tele_plu",
  "2453067_conv_plu",  "62510100_tele_plu", "71351000_tele_plu", "65819300_tele_plu",
  "65819300_conv_plu", "2554001_conv_plu",  "2554035_conv_plu",  "73693800_tele_plu", 
  "25874857_tele_plu"
))

# Garante a remoção apenas de colunas que realmente existem no df_plu
cols_remover <- intersect(estacoes_excluir_total, colnames(df_plu))

# 2. Remoção das colunas inteiras e aplicação dos filtros temporais específicos
df_plu_consistido <- df_plu %>%
  # Exclui as colunas das estações problemáticas
  select(-all_of(cols_remover)) %>%
  
  # Aplica 'NA' nos períodos específicos solicitados
  mutate(
    # 65310001_tele_plu: excluir a partir de 2015
    `65310001_tele_plu` = if_else(
      !is.na(Data) & Data >= as.Date("2015-01-01"), 
      NA_real_, 
      `65310001_tele_plu`
    ),
    
    # 65259000_tele_plu: excluir só maio/2025
    `65259000_tele_plu` = if_else(
      !is.na(Data) & Data >= as.Date("2025-05-01") & Data <= as.Date("2025-05-31"), 
      NA_real_, 
      `65259000_tele_plu`
    ),
    
    # 65100001_tele_plu: excluir a partir de 2015
    `65100001_tele_plu` = if_else(
      !is.na(Data) & Data >= as.Date("2015-01-01"), 
      NA_real_, 
      `65100001_tele_plu`
    ),
    
    # 65855080_tele_plu: excluir até 2021
    `65855080_tele_plu` = if_else(
      !is.na(Data) & Data <= as.Date("2021-12-31"), 
      NA_real_, 
      `65855080_tele_plu`
    ),
    
    # 82188000_tele_plu: excluir apenas 2019 e 2020
    `82188000_tele_plu` = if_else(
      !is.na(Data) & year(Data) %in% c(2019, 2020),
      NA_real_,
      `82188000_tele_plu`
    )
  )

# Sobrescreve o dataframe original com a versão tratada
df_plu <- df_plu_consistido

########################################################################
#plot disponibilidade dados
#############################
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(patchwork)

# ==========================================
# PASSO 1: PROCESSAMENTO DOS DADOS PARA O GRÁFICO (B)
# ==========================================

# Contagem das estações ativas por ano diretamente da matriz única df_plu
dados_evolucao_temporal <- df_plu %>%
  mutate(Ano = year(Data)) %>%
  pivot_longer(cols = -c(Data, Ano), names_to = "Codigo", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  distinct(Ano, Codigo) %>%
  # Cruza com o inventário para resgatar o Tipo de Rede
  inner_join(inventario, by = "Codigo") %>%
  count(Ano, Tipo_Rede, name = "Quantidade")

# ==========================================
# PASSO 2: CONSTRUÇÃO DO MAPA (SUPERIOR) - FIGURA A
# ==========================================

inventario_sf <- st_as_sf(
  subset(inventario, Tipo_Estacao == "Pluviométrica"), 
  coords = c("Longitude", "Latitude"), 
  crs = 4326, 
  remove = FALSE
)

inventario_sf <- st_transform(inventario_sf, crs = st_crs(brasil))
contorno      <- st_transform(contorno, crs = st_crs(brasil))
bacias        <- st_transform(bacias, crs = st_crs(brasil))

limites <- st_bbox(contorno)

mapa <- ggplot() +
  geom_sf(data = brasil, fill = NA, color = "#C0C0C0", size = 0.4) +
  geom_sf(data = bacias, fill = NA, color = "#000000", size = 0.3) +
  geom_sf(data = contorno, fill = NA, color = "#222222", size = 1.0, linetype = "solid") +
  geom_sf(
    data = inventario_sf, 
    aes(color = Tipo_Rede, shape = Fonte), 
    size = 2.0, 
    alpha = 0.55
  ) +
  scale_color_manual(values = c("Convencional" = "#4169E1", "Telemétrica" = "#FFA500")) +
  scale_shape_manual(values = c("Hidroweb" = 15, "Simepar" = 17)) +
  coord_sf(
    xlim = c(limites["xmin"], limites["xmax"]), 
    ylim = c(limites["ymin"], limites["ymax"]), 
    expand = TRUE
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    # Moldura preta forte em volta do mapa idêntica à imagem
    panel.border = element_rect(color = "black", fill = NA, size = 1.2), 
    panel.grid.major = element_line(color = "#F0F0F0", size = 0.2),
    legend.position = "bottom",       
    legend.box = "horizontal", 
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.margin = margin(t = 5, b = 5),
    plot.title = element_text(face = "bold", size = 14),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
  ) +
  labs(
    x = "Longitude", 
    y = "Latitude", 
    color = "Tipo de Rede:", 
    shape = "Fonte dos Dados:",
    title = "a)"
  )

# ==========================================
# PASSO 3: GRÁFICO DE BARRAS (INFERIOR) - FIGURA B
# ==========================================

grafico_temporal <- ggplot(dados_evolucao_temporal, aes(x = Ano, y = Quantidade, fill = Tipo_Rede)) +
  geom_bar(stat = "identity", position = "stack", width = 0.75, alpha = 0.9) +
  scale_fill_manual(values = c("Convencional" = "#4169E1", "Telemétrica" = "#FFA500")) +
  scale_x_continuous(breaks = seq(min(dados_evolucao_temporal$Ano), max(dados_evolucao_temporal$Ano), by = 2)) +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "#F8F8F8", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "#E5E5E5", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",       
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = "black"),
    plot.title = element_text(face = "bold", size = 14),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
  ) +
  labs(
    x = "Ano",
    y = "Nº de Estações Ativas",
    fill = "Tipo de Rede:",
    title = "b)"
  )

# ==========================================
# PASSO 4: COMPOSIÇÃO FINAL
# ==========================================

painel_final <- mapa / grafico_temporal + 
  plot_layout(heights = c(2.5, 1))

ggsave(
  filename = "painel_mapa_barras_plu.jpg",
  plot = painel_final,
  width = 8.5,      
  height = 10.5,     
  dpi = 300,        
  bg = "white"
)

cat("Painel idêntico à imagem de referência gerado com sucesso!\n")


#############################################
# grade 0.1 pra espacializar a chuva
#############################################
sf_use_s2(FALSE)
grade_01_sf <- st_make_grid(contorno_grade, cellsize = c(0.1, 0.1), what = "polygons") %>%  st_sf()
grade_01_recortada <- st_intersection(grade_01_sf, contorno_grade) %>%   st_make_valid()

# 3. Extrair os centroides temporários para fazer a ordenação geográfica
centroides_temp <- st_centroid(grade_01_recortada) %>%   st_coordinates() %>%   as.data.frame() %>% 
  rename(lon = X, lat = Y)
# 4. Acoplar as coordenadas ao objeto sf e ORDENAR:
# Primeiro por Latitude DECRESCENTE (começa no Norte/cima e vai descendo)
# Depois por Longitude CRESCENTE (da esquerda/Oeste para a direita/Leste)
grade_01_recortada <- grade_01_recortada %>% mutate(lon_temp = centroides_temp$lon, lat_temp = centroides_temp$lat) %>% 
  arrange(desc(lat_temp), lon_temp) %>% 
  # Agora que está na ordem certa de cima para baixo, criamos os IDs sequenciais
  mutate(grid_id = paste0("Grid_01_", sprintf("%04d", 1:n()))) %>% 
  select(-lon_temp, -lat_temp) # Remove as colunas temporárias
# 5. Criar a sua tabela final de coordenadas já na ordem correta (de cima para baixo)
coords_nova_grade <- st_centroid(grade_01_recortada) %>%   st_coordinates() %>% 
  as.data.frame() %>%   rename(lon = X, lat = Y) %>%  mutate(grid_id = grade_01_recortada$grid_id)
sf_use_s2(TRUE)

#############################################
# grade 0.1 pra espacializar a chuva
#############################################
library(sf)
library(gstat)
library(dplyr)
library(tidyr)
library(pbapply) # Para a barra de progresso (fundamental para 8.766 dias)

# ---- 1. Prepara os Alvos (Nova Grade de 0.1°) ----
grid_target_sf <- st_as_sf(
  coords_nova_grade, 
  coords = c("lon", "lat"), 
  crs = 4326
)

# ---- 2. Prepara as Estações ----
# Garante que as coordenadas estão alinhadas com as colunas do df_plu
inventario_validos <- inventario %>%
  filter(Codigo %in% names(df_plu)) %>%
  distinct(Codigo, .keep_all = TRUE)

estacoes_sf <- st_as_sf(
  inventario_validos, 
  coords = c("Longitude", "Latitude"), 
  crs = 4326
)

# ---- 3. Cria a Matriz Base do df_plu (Muito mais rápido que data.frame) ----
datas_serie <- df_plu$Data
matriz_chuva <- as.matrix(df_plu[, -1])
nomes_estacoes <- names(df_plu)[-1]

# ---- 4. A Função Otimizada para 1 Dia ----
# Como o seu mínimo é de 883 estações, removemos os testes condicionais para poupar tempo.
idw_diario_rapido <- function(i) {
  
  # 1. Extrai a linha do dia 'i'
  chuva_hoje <- matriz_chuva[i, ]
  
  # 2. Identifica quais estações têm dado hoje (ignora NAs)
  tem_dado <- !is.na(chuva_hoje)
  
  # 3. Filtra as estações geográficas e os valores correspondentes
  # Usa o match para garantir que a ordem dos pontos sf e da chuva seja exata
  idx_match <- match(nomes_estacoes[tem_dado], inventario_validos$Codigo)
  
  pontos_hoje <- estacoes_sf[idx_match, ]
  pontos_hoje$chuva <- chuva_hoje[tem_dado]
  
  # 4. Roda o IDW (idp = 2)
  modelo <- gstat(formula = chuva ~ 1, data = pontos_hoje, set = list(idp = 2))
  pred <- predict(modelo, newdata = grid_target_sf, debug.level = 0)
  
  # 5. Retorna só o vetor numérico (mais leve que retornar data frames)
  return(pred$var1.pred)
}

# ---- 5. O Loop de Processamento (Vai demorar um pouco!) ----
cat("Iniciando o IDW para", length(datas_serie), "dias. Pegue um café!\n")
{
# O pblapply mostra a barra de progresso no console
lista_resultados <- pblapply(seq_along(datas_serie), idw_diario_rapido)

# ---- 6. Montagem da Tabela Final (No formato exato do df_grid_historico) ----
# Transforma a lista de vetores em uma grande matriz e depois em data.frame
df_grid_interpolado <- as.data.frame(do.call(rbind, lista_resultados))
names(df_grid_interpolado) <- grid_target_sf$grid_id

# Adiciona a coluna Data como a primeira
df_grid_interpolado <- cbind(Data = datas_serie, df_grid_interpolado)

# Salva imediatamente para você não perder o trabalho de processamento
saveRDS(df_grid_interpolado, "Grade_IDW_Diaria.rds")
save(df_grid_interpolado, file="df_chuva_espacializada_v3.bin")
}
#################################
## PLOT IDW
## 
{
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)

# 1. Calcular o acumulado ANUAL e depois a MEDIANA por grid
df_acumulado_medio <- df_grid_interpolado %>%
  mutate(Ano = year(Data)) %>%
  group_by(Ano) %>%
  summarise(across(starts_with("Grid_"), sum, na.rm = TRUE)) %>%
  select(-Ano) %>%
  summarise(across(everything(), median, na.rm = TRUE)) %>%
  pivot_longer(
    cols = everything(), 
    names_to = "grid_id", 
    values_to = "chuva_media_anual"
  )

# 2. Unir com os polígonos da grade de 0,1°
mapa_climatologia <- grade_01_recortada %>%
  left_join(df_acumulado_medio, by = "grid_id")

# 3. Gerar o plot da Climatologia Anual
idw_plot <- ggplot() +
  # 1. BASE: Preenchimento colorido da grade
  geom_sf(data = mapa_climatologia, aes(fill = chuva_media_anual), color = NA) +
  
  # 2. FUNDO VETORIAL: Divisas políticas do Brasil (desenhado PRIMEIRO)
  geom_sf(data = brasil_sf, fill = NA, color = "gray60", linewidth = 0.2) +
  
  # 3. INTERMEDIÁRIO: Sub-bacias (desenhado POR CIMA do Brasil para cobrir divisas coincidentes)
  geom_sf(data = bacias, fill = NA, color = "gray20", linewidth = 0.45, linetype = "solid") +
  
  # 4. TOPO ABSOLUTO: Contorno preto da área de estudo (cobre tudo nos limites)
  geom_sf(data = contorno_grade, fill = NA, color = "black", linewidth = 0.9) +
  
  # Estilização das Cores
  scale_fill_viridis_c(
    option = "mako", 
    direction = -1, 
    name = "Precipitação\n(mm/ano)",
    labels = scales::number_format(suffix = " mm")
  ) +
  
  labs(x = "Longitude", y = "Latitude") +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.2, linetype = "dashed"),
    legend.box = "horizontal",
    legend.title = element_text(face = "bold", size = 10, vjust = 0.8),
    legend.text = element_text(size = 9),
    legend.position = "right",
    axis.title = element_text(face = "bold")
  ) +
  coord_sf(
    xlim = c(st_bbox(contorno_grade)["xmin"], st_bbox(contorno_grade)["xmax"]),
    ylim = c(st_bbox(contorno_grade)["ymin"], st_bbox(contorno_grade)["ymax"])
  )

# 4. Salvar o mapa
ggsave(
  filename = "media_acumulada_grid.png",
  plot = idw_plot,
  width = 8.5,      
  height = 7,     
  dpi = 300,        
  bg = "white"
)
##############################################################
#### Ajeitando os gids max -olho de boi
##############################################################
{
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)

# 1. Prepara a camada extraindo apenas os números do grid_id
grade_plot <- grade_01_recortada %>%
  mutate(
    # Extrai apenas os dígitos finais (ex: "Grid_01_0001" vira "1")
    numero_grid = as.numeric(str_extract(grid_id, "\\d+$"))
  )

# 2. Plot do Mapa com os rótulos (labels)
ggplot(grade_plot) +
  # Desenha a malha dos polígonos
  geom_sf(fill = "gray95", color = "gray60", linewidth = 0.2) +
  
  # Adiciona o número no centro de cada polígono
  geom_sf_text(
    aes(label = numero_grid), 
    size = 2,           # Tamanho do texto (ajuste se ficar muito poluído)
    color = "darkblue", 
    check_overlap = TRUE # Evita que os números fiquem uns por cima dos outros
  ) +
  
  labs(
    title = "Identificação dos Grids",
    x = "Longitude", 
    y = "Latitude"
  ) +
  theme_minimal()
  
#### ordenar os valores plotados 
 
# Ordena da maior para a menor chuva média anual (decrescente)
df_acumulado_medio <- df_acumulado_medio %>%
  arrange(desc(chuva_media_anual))
  
head(df_acumulado_medio)  
# A tibble: 6 × 2
  grid_id      chuva_media_anual
  <chr>                    <dbl>
1 Grid_01_4406             2683.
2 Grid_01_4052             2671.
3 Grid_01_3390             2468.
4 Grid_01_3474             2343.
5 Grid_01_3638             2289.
6 Grid_01_4051             2287.
> 



#### destacar o grid de interesse 

# Subconjunto apenas do grid procurado (ex: número 250)
grid_alvo <- grade_plot %>% filter(numero_grid == 4406)

ggplot() +
  # Toda a grade de fundo
  geom_sf(data = grade_plot, fill = "gray95", color = "gray75", linewidth = 0.2) +
  
  # O grid procurado em destaque
  geom_sf(data = grid_alvo, fill = "red", color = "black", alpha = 0.6) +
  geom_sf_text(data = grid_alvo, aes(label = numero_grid), color = "white", fontface = "bold", size = 4) +
  
  theme_minimal()  

#### encontrar a estação mais proxima a esse grid

encontrar_estacao_proxima <- function(id_grid_alvo, 
                                       sf_grade = grade_01_recortada, 
                                       sf_estacoes = estacoes_plu_sf) {
  
  # 1. Filtra o grid selecionado (aceita formato "Grid_01_0001" ou apenas o número ex: 1)
  grid_sel <- sf_grade %>%
    filter(grid_id == id_grid_alvo | as.numeric(str_extract(grid_id, "\\d+$")) == as.numeric(id_grid_alvo))
  
  if (nrow(grid_sel) == 0) {
    stop("Grid informado não encontrado na camada 'sf_grade'!")
  }
  
  # 2. Calcula o centroide do polígono do grid escolhido
  centroide_grid <- st_centroid(grid_sel)
  
  # 3. Garante que os CRS (Sistemas de Coordenadas) sejam idênticos
  if (st_crs(sf_estacoes) != st_crs(centroide_grid)) {
    centroide_grid <- st_transform(centroide_grid, st_crs(sf_estacoes))
  }
  
  # 4. Calcula as distâncias do centroide do grid para todas as estações
  distancias <- st_distance(sf_estacoes, centroide_grid)
  
  # 5. Encontra a estação mais próxima (menor distância)
  estacao_mais_proxima <- sf_estacoes %>%
    mutate(distancia_km = as.numeric(distancias) / 1000) %>%
    arrange(distancia_km) %>%
    slice(1)
  
  # Exibe os resultados no console
  cat("====================================================\n")
  cat(" Grid Analisado:         ", grid_sel$grid_id, "\n")
  cat(" Estação Mais Próxima:   ", estacao_mais_proxima$Codigo, "\n")
  cat(" Distância aproximada:   ", round(estacao_mais_proxima$distancia_km, 2), "km\n")
  cat("====================================================\n")
  
  return(estacao_mais_proxima)
}

# Buscando pelo número direto do grid (ex: Grid número 1)
resultado <- encontrar_estacao_proxima(id_grid_alvo = 4406)


#### plotar essa e as proximas 5 estações
investigar_estacao(codigo_alvo = "2649060_conv_plu", num_vizinhos = 1)
}

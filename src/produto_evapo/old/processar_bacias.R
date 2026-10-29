# ==============================================================================
# processar_bacias.R - Script de Execução
# ==============================================================================

# %%
# Carrega as funções customizadas
source("funcoes_balanco.R")

# --- PARÂMETROS E ENTRADAS ---
dir_base <- "whitebox_output"

# Lista de estações a processar
estacoes_alvo <- c(est_bacia1, est_bacia2, est_bacia3)

# 1. Executa o MDE Base apenas uma vez para a região inteira
preparar_mde_base(r_bacia = r_bacia, dir_out = dir_base, threshold = 5000)

# 2. Executa a delimitação e cálculo de área para todas as estações desejadas
resultados_list <- lapply(estacoes_alvo, function(cod) {
  delimitar_e_calcular_area(
    cod_estacao = cod, 
    df_estacoes = df_codigos_flu_estacao, 
    dir_base    = dir_base, 
    snap_dist   = 20
  )
})

# Agrupa todos os data.frames em um único
df_areas <- do.call(rbind, resultados_list)

# Exibe a tabela final de áreas
print(df_areas)

# 3. Exemplo de uso para normalizar suas vazões futuramente:
# df_dados <- df_dados %>% 
#   left_join(df_areas, by = "Codigo") %>% 
#   mutate(vazao_mm = normalizar_vazao(vazao_m3s, area_km2))




#%%
#' Processa a média das estações de uma vertente em mm/dia (ou m³/s se sem áreas)
#'
#' @param df_flu Data frame com a série temporal completa de vazões
#' @param codigos_estacoes Vetor com os códigos das estações da vertente
#' @param df_areas Data frame contendo 'Codigo' e 'area_km2'
#' @param datas_filtro Opcional: vetor de datas para filtrar (ex: datas_na$Data)
processar_vertente <- function(df_flu, codigos_estacoes, df_areas = NULL, datas_filtro = NULL) {
  
  # Seleciona Data e as colunas das estações solicitadas
  df_sub <- df_flu %>% select(Data, all_of(codigos_estacoes))
  
  # Aplica o filtro de datas se informado
  if (!is.null(datas_filtro)) {
    df_sub <- df_sub %>% filter(Data %in% datas_filtro)
  }
  
  # Se informou a tabela de áreas, converte para mm/dia ANTES de tirar a média
  if (!is.null(df_areas)) {
    for (cod in codigos_estacoes) {
      area <- df_areas %>% filter(Codigo == cod) %>% pull(area_km2)
      if (length(area) > 0 && !is.na(area)) {
        # Converte m³/s para mm/dia: (m³/s * 86.4) / area_km²
        df_sub[[cod]] <- (df_sub[[cod]] * 86.4) / area
      }
    }
  }
  
  # Calcula a média linha a linha desconsiderando os NAs
  df_res <- df_sub %>% 
    rowwise() %>% 
    mutate(vazao_estimada = mean(c_across(-Data), na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(Data, vazao_estimada)
  
  return(df_res)
}
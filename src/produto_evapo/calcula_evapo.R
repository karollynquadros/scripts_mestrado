
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


# %% # definindo a bacia

#args <- list("Alto_Iguacu","estacao_26025035_tele_flu" ) 
#args <- list("Ivai", "estacao_64693000_conv_flu")
#args <- list("Piquiri", "estacao_64890000_conv_flu")
#args <- list("Tibagi", "estacao_64506000_conv_flu")
#args <- list("Medio_Iguacu", "estacao_65883051_conv_flu") 
args <- list("Baixo_Iguacu", "estacao_25345435_tele_flu") 
cod_bacia<-args[[1]]


diretorio_bacia <- file.path("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo",cod_bacia,"df_data")
arquivos_bin <- list.files(path = diretorio_bacia,pattern = "\\.bin$",full.names = TRUE)
lapply(arquivos_bin, load, envir = .GlobalEnv)
pasta_saida <- file.path(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/", cod_bacia))


#%% ################################## definir grace diario

df_grace_diario <- df_grace_bacia %>%
  # 1. Converter para Date para garantir operações de data e fuso horário corretas
  mutate(Data = as.Date(data)) %>% 
  
  # 2. Ordenar por Data para garantir a ordem cronológica do lead()
  arrange(Data) %>% 
  mutate(
    proxima_data = lead(Data),
    proxima_twsa = lead(grace)
  ) %>% 
  
  # 3. Remover a última linha (terá NA pois não há próximo registro para fechar o intervalo)
  filter(!is.na(proxima_data)) %>% 
  
  # 4. Criar as sequências diárias e calcular a taxa diária
  mutate(
    # Gera a sequência de dias do intervalo: [Data_atual, proxima_Data - 1]
    dias = map2(Data, proxima_data - 1, ~ seq(.x, .y, by = "1 day")),
    
    # Quantidade de dias no intervalo
    n_dias = map_int(dias, length),
    
    # Divide a variação de TWSA (mm) igualmente pelos dias do intervalo
    grace = (proxima_twsa - grace) / n_dias
  ) %>%
  
  # 5. Expandir o data frame para formato diário e renomear
  select(dias, grace) %>%
  unnest(cols = c(dias)) %>%
  rename(Data = dias)

#%% ################################## df_balanco_diario

# 1. Unir os três data frames pelas datas em comum
df_balanco_diario <- df_flu_bacia %>%
  inner_join(df_plu_bacia, by = "Data") %>%
  inner_join(df_grace_diario, by = "Data") %>%
  mutate(evapo = plu_med - exult - grace)

# Remove todos os dias em que a vazão é NA 
df_balanco_diario <- df_balanco_diario %>%
  filter(!is.na(exult))

# 1. Diário (apenas passa a coluna de data diferente)
g_diario <- gerar_grafico_balanco(
  dados = df_balanco_diario,col_data= "Data", escala = "diario",
  pasta_saida  = pasta_saida,nome_arquivo = "balanco_hidrico_diario.png")

#%% ################################## PRODUTO MENSAL (df_balanco_mensal)

df_balanco_mensal <- df_balanco_diario %>%
#mutate(Data_Mes = Data)
  # 1. Agrupa pela primeira data de cada mês (ex: 2002-04-01)
  mutate(Data_Mes = floor_date(Data, unit = "month")) %>%
  # 2. Acumula (soma) os valores diários de cada mês
  group_by(Data_Mes) %>%
  summarise(
    plu_med        = sum(plu_med, na.rm = TRUE),
    exult          = sum(exult, na.rm = TRUE),
    grace = sum(grace, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 3. Calcula a evapotranspiração mensal sobre os totais do mês
  mutate(
    evapo = plu_med - exult - grace
  )

g_mensal <- gerar_grafico_balanco(
  dados = df_balanco_mensal, col_data = "Data_Mes", col_ds = "grace",
  escala = "mensal", pasta_saida = pasta_saida, nome_arquivo = "balanco_mensal_sem_correcao.png"
)

#%% ################################## PRODUTO MENSAL Correção dos negativos
df_balanco_mensal <- df_balanco_mensal %>%
  mutate(
    # Soma total dos valores negativos (em valor absoluto)
    total_deficit = sum(abs(evapo[evapo < 0]), na.rm = TRUE),
    # Quantidade de meses não-negativos
    n_positivos = sum(evapo >= 0, na.rm = TRUE),
    # Desconto a ser aplicado em cada mês positivo
    desconto = total_deficit / n_positivos,
    # Se for negativo vira 0; se for positivo recebe o desconto
    evapo = if_else(evapo < 0, 0, evapo - desconto)
  ) %>%
  select(-total_deficit, -n_positivos, -desconto)

g_mensal_corrigido <- gerar_grafico_balanco(
  dados = df_balanco_mensal, col_data = "Data_Mes", col_ds = "grace",
  escala = "mensal", pasta_saida = pasta_saida, nome_arquivo = "balanco_mensal.png"
)

#%% ################################## PRODUTO boxplot climatologia mensal
  
# 1. Preparar os dados para climatologia
df_climatologia <- df_balanco_mensal %>%
    mutate( mes = month(Data_Mes, label = TRUE, abbr = TRUE))
  
# 3. Climatologia (Boxplot)
g_clima <- gerar_boxplot_climatologia(
  dados = df_balanco_mensal, col_data = "Data_Mes", col_ds = "grace",
  pasta_saida = pasta_saida, nome_arquivo = "climatologia_mensal_boxplots.png"
)


#%% ################################## PRODUTO ANUAL --- #df_balanco_anual

df_balanco_anual <- df_balanco_diario %>%
  # 1. Filtra dias válidos (onde exult não é NA)
  filter(!is.na(exult)) %>%
  
  # 2. Cria a coluna do ano
  mutate(Data_Ano = year(Data)) %>%
  
  # 3. Agrupa por ano e verifica a contagem de dias
  group_by(Data_Ano) %>%
  filter(n() >= 365) %>% # Mantém apenas anos com 365 ou 366 dias (anos completos)
  
  # 4. Acumula os totais anuais
  summarise(
    plu_med        = sum(plu_med, na.rm = TRUE),
    exult          = sum(exult, na.rm = TRUE),
    grace = sum(grace, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 5. Calcula a evapotranspiração anual
  mutate(
    evapo = plu_med - exult - grace
  )

# 2. Série Temporal Anual
g_anual <- gerar_grafico_balanco(
  dados = df_balanco_anual, col_data = "Data_Ano", col_ds = "grace",
  escala = "anual", pasta_saida = pasta_saida, nome_arquivo = "balanco_anual.png"
)
#%% ################################# PRODUTO Media intervalo grace #grace_interv

  # 1. Calcula a variação REAL (grace_dif) e a quantidade de meses no intervalo original
  df_grace_interv <- df_grace_bacia %>%
    arrange(data) %>%
    mutate(
      data_prox = lead(data),
      grace_prox = lead(grace),
      dias_totais = as.numeric(data_prox - data),
      # Número de parcelas de ~30 dias
      n_intervalos = pmax(1, round(dias_totais / 30)),
      # A variação total dividida igualmente pelo número de meses
      grace_dif = (grace_prox - grace) / n_intervalos
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
  df_grace_interv <- df_grace_interv %>%
    rowwise() %>%
    mutate(
      plu_sum = if (all(is.na(df_plu_bacia$plu_med[df_plu_bacia$Data >= t1 & df_plu_bacia$Data < t2]))) NA_real_ 
      else sum(df_plu_bacia$plu_med[df_plu_bacia$Data >= t1 & df_plu_bacia$Data < t2], na.rm = TRUE),
      
      flu_sum = if (all(is.na(df_flu_bacia$exult[df_flu_bacia$Data >= t1 & df_flu_bacia$Data < t2]))) NA_real_ 
      else sum(df_flu_bacia$exult[df_flu_bacia$Data >= t1 & df_flu_bacia$Data < t2], na.rm = TRUE),
      
      evapo_sum = plu_sum - flu_sum - grace_dif
    ) %>%
    ungroup() %>%
    select(t1, t2, dias, grace_dif, plu_sum, flu_sum, evapo_sum)
  
  
df_grace_interv <- df_grace_interv  %>%
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
  data_min <- min(df_grace_interv$t1[!is.na(df_grace_interv$evapo_sum)])
  data_max <- max(df_grace_interv$t1[!is.na(df_grace_interv$evapo_sum)])
  
  # 2. Filtra o dataframe para manter todo o intervalo de anos entre elas
  df_grace_interv<- df_grace_interv %>%
    filter(t1 >= data_min & t1 <= data_max)  


grafico_interv <- gerar_grafico_balanco(
  dados         = df_grace_interv,
  col_data      = "t1",
  col_p         = "plu_sum",
  col_q         = "flu_sum",
  col_ds        = "grace_dif",
  col_e         = "evapo_sum",
  escala        = "intervalo",
  rotulo_eixo_x = "Data de Início do Intervalo (t1)",
  pasta_saida   = pasta_saida,
  nome_arquivo  = "balanco_hidrico_grace_intervalos.png"
)


#%% ################################## Ajeitando pra salvar
dir_dados <- file.path(pasta_saida, "df_produtos")
if (!dir.exists(dir_dados)) dir.create(dir_dados, recursive = TRUE)

# Salva o arquivo no destino
save(df_balanco_mensal, file = file.path(dir_dados, "df_balanco_mensal.bin"))
save(df_balanco_anual, file = file.path(dir_dados, "df_balanco_anual.bin"))
save(df_grace_interv, file = file.path(dir_dados, "df_grace_interv.bin"))
#%% #### 



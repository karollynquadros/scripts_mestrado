library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(png)
library(grid)

# %% # definindo a bacia

args <- list("Alto_Iguacu","estacao_26025035_tele_flu" ) 
##args <- list("Ivai", "estacao_64693000_conv_flu")
##args <- list("Piquiri", "estacao_64890000_conv_flu")
##args <- list("Tibagi", "estacao_64506000_conv_flu")
#args <- list("Medio_Iguacu", "estacao_65883051_conv_flu") 
#args <- list("Baixo_Iguacu", "estacao_25345435_tele_flu") 
cod_bacia<-args[[1]]


load(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/results/produto_evapo/",cod_bacia,"/df_produtos/df_balanco_mensal.bin"))
df_MODIS <- read.csv(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/data/modis_ET/ET_8dias_",cod_bacia,".csv"))%>%mutate(date = as.Date(date))
load(paste0("/mnt/externo/mestrado_note/evapotranspiration_basin/data/xavier/df_data/df_xavier_", cod_bacia, ".bin"))

pasta_saida <- file.path("/mnt/externo/mestrado_note/evapotranspiration_basin/results/comparacao_evapo_global/")

#%% Modis diario
# 2. Expansão diária exata (do dia da observação até a véspera da próxima)
df_MODIS <- df_MODIS %>%
  arrange(date) %>%
  mutate( et_modis = mean / 8,
          date_end = lead(date, default = max(date) + days(8)) - days(1)) %>%
  rowwise() %>%
  # Gera a sequência de dias contínuos para cada intervalo
  mutate(data = list(seq(date, date_end, by = "day"))) %>%
  unnest(data) %>%
  ungroup() %>%
  # Seleciona as colunas finais
  select(data, et_modis)


#%% Mensal xavier e modis

df_xavier_mensal <- df_xavier %>%
  mutate(Data_Mes = floor_date(data, unit = "month")) %>%
  group_by(Data_Mes) %>%
  summarise(
    tmax_xavier = mean(tmax, na.rm = TRUE),
    tmin_xavier = mean(tmin, na.rm = TRUE),
    eto_xavier  = sum(eto, na.rm = TRUE), # soma diária -> mm/mês
    .groups  = "drop" )

# 2. Agregar df_MODIS para escala mensal
df_modis_mensal <- df_MODIS %>%
  mutate(Data_Mes = floor_date(data, unit = "month")) %>%
  group_by(Data_Mes) %>%
  summarise(
    et_modis_tot = sum(et_modis, na.rm = TRUE), # soma diária -> mm/mês
    .groups      = "drop" )

# 3. Unir tudo no df_balanco_mensal_global
df_balanco_mensal_global <- df_balanco_mensal %>%
  left_join(df_xavier_mensal, by = "Data_Mes") %>%
  left_join(df_modis_mensal,  by = "Data_Mes")



#%% ######################## plot boxplot

fator_escala <- 5
nome_titulo  <- cod_bacia

df_plot_mensal <- df_balanco_mensal_global %>%
  mutate(Mes = factor(month(Data_Mes, label = TRUE, abbr = TRUE), ordered = TRUE)) %>%
  pivot_longer(
    cols = c(et_modis_tot, eto_xavier, evapo, tmin_xavier, tmax_xavier),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  mutate(
    tipo = if_else(grepl("^t", variavel), "Temperatura", "Evapotranspiração"),
    valor_escalado = if_else(tipo == "Temperatura", valor * fator_escala, valor),
    variavel = factor(
      variavel,
      levels = c("et_modis_tot", "eto_xavier", "evapo", "tmin_xavier", "tmax_xavier")
    )
  )

p_box <- ggplot(df_plot_mensal, aes(x = Mes, y = valor_escalado, fill = variavel)) +
  geom_boxplot(outlier.size = 1, alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_y_continuous(
    name = "Evapotranspiração (mm)",
    sec.axis = sec_axis(~ . / fator_escala, name = "Temperatura (°C)")
  ) +
  scale_fill_manual(
    values = c(
      "et_modis_tot" = "#d95f02", # Laranja Queimado / Âmbar
      "eto_xavier"   = "#009E73", # Verde / Teal
      "evapo"        = "#7570B3", # Roxo
      "tmin_xavier"  = "#4575b4", # Azul (frio/mínima)
      "tmax_xavier"  = "#d73027"  # Vermelho (quente/máxima)
    ),
    labels = c(
      "et_modis_tot" = "ET MODIS",
      "eto_xavier"   = "ETo Xavier",
      "evapo"        = "ET Balanço",
      "tmin_xavier"  = "T. Mínima",
      "tmax_xavier"  = "T. Máxima"
    )
  ) +
  labs(
    title = paste("Distribuição Mensal de Evapotranspiração e Temperatura - Bacia", nome_titulo),
    x = "Mês",
    fill = "Variável"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.title.y.left = element_text(color = "#333333", face = "bold"),
    axis.title.y.right = element_text(color = "#d73027", face = "bold")
  )

print(p_box)

ggsave(file.path(pasta_saida, paste0("boxplot_mensal_", cod_bacia, ".png")), plot = p_box, width = 12, height = 7, dpi = 300)
#%% plot anual


df_anual <- df_balanco_mensal_global %>%
  mutate(Ano = year(Data_Mes)) %>%
  group_by(Ano) %>%
  # Filtra apenas anos que possuem os 12 meses completos
  filter(n() == 12) %>%
  summarise(
    et_modis_tot = sum(et_modis_tot, na.rm = TRUE),
    eto_xavier   = sum(eto_xavier,   na.rm = TRUE),
    evapo        = sum(evapo,        na.rm = TRUE),
    .groups      = "drop"
  )


df_anual_et_plot <- df_anual %>%
  select(Ano, et_modis_tot, eto_xavier, evapo) %>%
  pivot_longer(
    cols = c(et_modis_tot, eto_xavier, evapo),
    names_to = "variavel",
    values_to = "acumulo_mm"
  ) %>%
  mutate(
    Ano = factor(Ano),
    variavel = factor(
      variavel, 
      levels = c("et_modis_tot", "eto_xavier", "evapo")
    )
  )

p_anual_et <- ggplot(df_anual_et_plot, aes(x = Ano, y = acumulo_mm, fill = variavel)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = round(acumulo_mm, 0)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 2.8,
    na.rm = TRUE
  ) +
  scale_y_continuous(
    name = "Acúmulo Anual (mm)",
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_fill_manual(
    values = c(
      "et_modis_tot" = "#d95f02", # Laranja Queimado / Âmbar
      "eto_xavier"   = "#009E73", # Verde / Teal
      "evapo"        = "#7570B3"  # Roxo
    ),
    labels = c(
      "et_modis_tot" = "ET MODIS",
      "eto_xavier"   = "ETo Xavier",
      "evapo"        = "ET Balanço"
    )
  ) +
  labs(
    title = paste("Acúmulo Anual de Evapotranspiração - Bacia", nome_titulo),
    subtitle = "Apenas anos completos (12 meses)",
    x = "Ano",
    fill = "Variável"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

ggsave(
  filename = file.path(pasta_saida, paste0("barras_acumulo_anual_et_", cod_bacia, ".png")),
  plot = p_anual_et,
  width = 12,
  height = 6,
  dpi = 300
)

print(p_anual_et)
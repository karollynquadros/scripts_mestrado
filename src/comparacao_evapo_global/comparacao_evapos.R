library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(lubridate)
library(png)
library(grid)

#teste git
x<-"b"

load("df_et_bacias.bin")
load("df_et_xavier_bacias.bin")
load("df_Tmed_xavier_bacias.bin")

########################
## Ajeitar Modis (8 dias)
########################
{
df_et_diario <- df_et_bacias %>%
    # 1. Agrupa por bacia e garante a ordem cronológica
    group_by(bacia_nome) %>%
    arrange(Data, .by_group = TRUE) %>%
    
    # 2. Encontra o número de dias exato entre a data atual e a próxima
    mutate(n_dias = as.numeric(lead(Data) - Data),
           # Para o último registro de cada bacia (onde lead é NA), assume 8 dias
           n_dias = if_else(is.na(n_dias), 8, n_dias),
           
           et_modis = et_mm_8dias / n_dias) %>%
    ungroup() %>%
    uncount(n_dias, .id = "dia_offset") %>%
    mutate(Data = Data + (dia_offset - 1)) %>%
    select(bacia_nome, Data, et_modis)
}  
########################
## df_diario das ets 
########################
#ets diaria ---- df_diario
{
df_diario <- df_et_diario %>%
  # 1. Ajusta o dataframe MODIS
  mutate(Data = as.Date(Data)) %>%
  
  # 2. Junta com a Evapotranspiração Xavier
  full_join(
    df_et_xavier_bacias %>%
      mutate(Data = as.Date(Data)) %>%
      rename(eto_xavier = et_mm_diaria),
    by = c("bacia_nome", "Data")
  ) %>%
  
  # 3. Junta com as Temperaturas Xavier
  full_join(
    df_Tmed_xavier_bacias %>%
      mutate(Data = as.Date(Data)) %>%
      rename(
        Tmed_xavier = T_med_diaria,
        Tmin_xavier = T_min_diaria,
        Tmax_xavier = Tmax_diaria
      ),
    by = c("bacia_nome", "Data")
  )
}
#ets mensal ---- df_mensal
{
df_mensal <- df_diario %>%
  # Trunca a data para o primeiro dia de cada mês (ex: 2002-09-01)
  mutate(Data = floor_date(Data, unit = "month")) %>%
  group_by(bacia_nome, Data) %>%
  summarise(
    et_modis    = sum(et_modis, na.rm = TRUE),
    eto_xavier  = sum(eto_xavier, na.rm = TRUE),
    Tmin_xavier = mean(Tmin_xavier, na.rm = TRUE),
    Tmax_xavier = mean(Tmax_xavier, na.rm = TRUE),
    Tmed_xavier = mean(Tmed_xavier, na.rm = TRUE),
    .groups     = "drop"
  )
}
########################
## juntar os dados ---- df_bacia
########################

############################## loop produto comparação:
{
#1. Vetor com o nome de cada bacia
bacias <- c("Tibagi", "Piquiri", "Ivai", "Alto_Iguacu", "Medio_Iguacu", "Baixo_Iguacu")
# Backup do dataframe completo original
df_mensal_base <- df_mensal 
# 2. Loop principal
for (bacia in bacias) {
  message(paste("Processando bacia:", bacia))
  # Criar a pasta de saída para a bacia (se não existir)
  pasta_saida <- paste0("resultados_", bacia)
  if (!dir.exists(pasta_saida)) {
    dir.create(pasta_saida, recursive = TRUE)
  }
  
  # --- Carregar e juntar os dados do balanço mensal ---
  caminho_bin <- paste0("../produto_evapo/bacia_", bacia, "/produtos_finais/df_balanco_mensal_", bacia, ".bin")
  
  if (file.exists(caminho_bin)) {
    load(caminho_bin)
    df_balanco <- get(paste0("df_balanco_mensal_", bacia))
    
    df_bacia <- df_mensal_base %>%
      filter(bacia_nome == bacia) %>%
      left_join(
        df_balanco %>% 
          mutate(bacia_nome = bacia) %>%
          rename(et_balanco = evapo) %>%
          select(bacia_nome, Data_Mes, et_balanco),
        by = c("bacia_nome", "Data" = "Data_Mes")
      )
  } else {
    warning(paste("Arquivo não encontrado:", caminho_bin, "- Utilizando dados sem et_balanco."))
    df_bacia <- df_mensal_base %>% 
      filter(bacia_nome == bacia) %>% 
      mutate(et_balanco = NA_real_)
  }
  
  # Nome formatado para os títulos gráficos
  nome_titulo <- gsub("_", " ", bacia)
  
  # ==========================================
  # PLOT 1: Boxplot Mensal (com Duplo Eixo)
  # ==========================================
  fator_escala <- 5
  
  df_plot_mensal <- df_bacia %>%
    mutate(Mes = factor(month(Data, label = TRUE, abbr = TRUE), ordered = TRUE)) %>%
    pivot_longer(
      cols = c(et_modis, eto_xavier, et_balanco, Tmin_xavier, Tmax_xavier, Tmed_xavier),
      names_to = "variavel",
      values_to = "valor"
    ) %>%
    mutate(
      tipo = if_else(grepl("^T", variavel), "Temperatura", "Evapotranspiração"),
      valor_escalado = if_else(tipo == "Temperatura", valor * fator_escala, valor)
    )
  
  p_box <- ggplot(df_plot_mensal, aes(x = Mes, y = valor_escalado, fill = variavel)) +
    geom_boxplot(outlier.size = 1, alpha = 0.8, position = position_dodge(width = 0.8)) +
    scale_y_continuous(
      name = "Evapotranspiração (mm)",
      sec.axis = sec_axis(~ . / fator_escala, name = "Temperatura (°C)")
    ) +
    scale_fill_manual(
      values = c(
        "et_modis"    = "#E69F00", # Laranja
        "eto_xavier"  = "#009E73", # Verde / Teal
        "et_balanco"  = "#7570B3", # Roxo
        "Tmin_xavier" = "#fdae61", # Amarelo/Laranja claro
        "Tmed_xavier" = "#f46d43", # Laranja/Vermelho
        "Tmax_xavier" = "#d73027"  # Vermelho escuro
      ),
      labels = c(
        "et_modis"    = "ET MODIS",
        "eto_xavier"  = "ETo Xavier",
        "et_balanco"  = "ET Balanço",
        "Tmin_xavier" = "T. Mínima",
        "Tmed_xavier" = "T. Média",
        "Tmax_xavier" = "T. Máxima"
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
      axis.title.y.left = element_text(color = "#E69F00", face = "bold"),
      axis.title.y.right = element_text(color = "#d73027", face = "bold")
    )
  
  ggsave(file.path(pasta_saida, paste0("boxplot_mensal_", bacia, ".png")), plot = p_box, width = 12, height = 7, dpi = 300)
  
  # ==========================================
  # PLOT 2: Acúmulo Anual (com Temperatura)
  # ==========================================
  df_anual <- df_bacia %>%
    mutate(Ano = year(Data)) %>%
    group_by(Ano) %>%
    filter(n() == 12) %>%
    summarise(
      et_modis    = sum(et_modis, na.rm = TRUE),
      eto_xavier  = sum(eto_xavier, na.rm = TRUE),
      et_balanco  = if(all(is.na(et_balanco))) NA_real_ else sum(et_balanco, na.rm = TRUE),
      Tmin_xavier = mean(Tmin_xavier, na.rm = TRUE),
      Tmax_xavier = mean(Tmax_xavier, na.rm = TRUE),
      Tmed_xavier = mean(Tmed_xavier, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(df_anual) > 0) {
    fator_escala_anual <- 40
    
    df_anual_plot <- df_anual %>%
      pivot_longer(
        cols = -Ano,
        names_to = "variavel",
        values_to = "valor"
      ) %>%
      mutate(
        tipo = if_else(grepl("^T", variavel), "Temperatura", "Evapotranspiração"),
        valor_escalado = if_else(tipo == "Temperatura", valor * fator_escala_anual, valor),
        Ano = factor(Ano)
      )
    
    p_anual_t <- ggplot(df_anual_plot, aes(x = Ano, y = valor_escalado, fill = variavel)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      scale_y_continuous(
        name = "Acúmulo Anual de Evapotranspiração (mm)",
        sec.axis = sec_axis(~ . / fator_escala_anual, name = "Temperatura Média Anual (°C)")
      ) +
      scale_fill_manual(
        values = c(
          "et_modis"    = "#E69F00",
          "eto_xavier"  = "#009E73",
          "et_balanco"  = "#7570B3",
          "Tmin_xavier" = "#fdae61",
          "Tmed_xavier" = "#f46d43",
          "Tmax_xavier" = "#d73027"
        ),
        labels = c(
          "et_modis"    = "ET MODIS (Soma)",
          "eto_xavier"  = "ETo Xavier (Soma)",
          "et_balanco"  = "ET Balanço (Soma)",
          "Tmin_xavier" = "T. Mínima (Média)",
          "Tmed_xavier" = "T. Média (Média)",
          "Tmax_xavier" = "T. Máxima (Média)"
        )
      ) +
      labs(
        title = paste("Totais Anuais de ET e Médias Anuais de T - Bacia", nome_titulo),
        subtitle = "Apenas anos com 12 meses completos inclusos",
        x = "Ano",
        fill = "Variável"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y.left = element_text(color = "#E69F00", face = "bold"),
        axis.title.y.right = element_text(color = "#d73027", face = "bold")
      )
    
    ggsave(file.path(pasta_saida, paste0("barras_acumulo_anual_com_T_", bacia, ".png")), plot = p_anual_t, width = 14, height = 7, dpi = 300)
    
    # ==========================================
    # PLOT 3: Acúmulo Anual (SEM Temperatura)
    # ==========================================
    df_anual_et_plot <- df_anual %>%
      select(Ano, et_modis, eto_xavier, et_balanco) %>%
      pivot_longer(
        cols = c(et_modis, eto_xavier, et_balanco),
        names_to = "variavel",
        values_to = "acumulo_mm"
      ) %>%
      mutate(
        Ano = factor(Ano),
        variavel = factor(variavel, levels = c("et_modis", "eto_xavier", "et_balanco"))
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
          "et_modis"   = "#E69F00", # Laranja
          "eto_xavier" = "#009E73", # Verde / Teal
          "et_balanco" = "#7570B3"  # Roxo
        ),
        labels = c(
          "et_modis"   = "ET MODIS",
          "eto_xavier" = "ETo Xavier",
          "et_balanco" = "ET Balanço"
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
    
    ggsave(file.path(pasta_saida, paste0("barras_acumulo_anual_et_", bacia, ".png")), plot = p_anual_et, width = 12, height = 6, dpi = 300)
  }
}
}

############################## pdf produtos finais
{
  # Lista de bacias
  bacias <- c("Tibagi", "Piquiri", "Alto_Iguacu", "Baixo_Iguacu", "Medio_Iguacu", "Ivai")
  
  # Nome do arquivo PDF consolidado
  pdf_unico <- "Relatorio_Geral_Todas_Bacias.pdf"
  
  # 1. Abre o dispositivo PDF UMA ÚNICA VEZ antes de iniciar o loop
  pdf(pdf_unico, width = 11, height = 8.5) # Orientação paisagem
  
  for (bacia in bacias) {
    
    caminho_figuras <- paste0("../produto_evapo/bacia_", bacia, "/figuras")
    imagens_png <- list.files(path = caminho_figuras, pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE)
    
    if (length(imagens_png) == 0) {
      message(paste("Pulando: Nenhuma imagem PNG encontrada para a bacia:", bacia))
      next
    }
    
    message(paste("Adicionando bacia ao PDF:", bacia, "-", length(imagens_png), "imagens."))
    
    # --- CAPA/DIVISOR DA BACIA ---
    nome_titulo <- gsub("_", " ", bacia)
    
    p_capa <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = nome_titulo, size = 18, fontface = "bold", color = "#1a365d") +
      theme_void() +
      theme(panel.background = element_rect(fill = "#f7fafc", color = NA))
    
    print(p_capa)
    
    # --- ANEXAR IMAGENS DA BACIA ---
    for (img_path in imagens_png) {
      img <- readPNG(img_path)
      grid.newpage()
      grid.raster(img)
    }
  }
  
  # 2. Fecha e salva o PDF apenas no final de tudo
  dev.off()
  
  message(paste("PDF único gerado com sucesso:", pdf_unico))
}
############################## pdf comparação
{
  library(png)
  library(grid)
  library(ggplot2)
  
  # 1. Vetor com o nome de cada bacia
  bacias <- c("Tibagi", "Piquiri", "Ivai", "Alto_Iguacu", "Medio_Iguacu", "Baixo_Iguacu")
  
  # 2. Definição do arquivo PDF único de saída
  pdf_unico <- "Relatorio_Geral_Resultados_Bacias.pdf"
  
  # Abre o dispositivo PDF uma única vez antes do loop
  pdf(pdf_unico, width = 11, height = 8.5) # Orientação paisagem
  
  # 3. Loop principal navegando nas pastas locais resultados_<BACIA>
  for (bacia in bacias) {
    
    caminho_figuras <- paste0("resultados_", bacia)
    
    # Lista todas as imagens PNG geradas na pasta de resultados da bacia
    imagens_png <- list.files(
      path = caminho_figuras, 
      pattern = "\\.png$", 
      full.names = TRUE, 
      ignore.case = TRUE
    )
    
    if (length(imagens_png) == 0) {
      message(paste("Nenhuma imagem PNG encontrada em:", caminho_figuras, "- Pulando..."))
      next
    }
    
    message(paste("Processando bacia:", bacia, "| Imagens encontradas:", length(imagens_png)))
    
    # --- PÁGINA DE CAPA DA BACIA ---
    nome_titulo <- gsub("_", " ", bacia)
    
    p_capa <- ggplot() +
      annotate(
        "text", 
        x = 0.5, y = 0.5, 
        label = nome_titulo, 
        size = 18, 
        fontface = "bold", 
        color = "#1a365d"
      ) +
      theme_void() +
      theme(panel.background = element_rect(fill = "#f7fafc", color = NA))
    
    print(p_capa)
    
    # --- ANEXAR CADA FIGURA PNG SEQUENCIALMENTE ---
    for (img_path in imagens_png) {
      img <- readPNG(img_path)
      grid.newpage()
      grid.raster(img)
    }
  }
  
  # 4. Fecha e salva o arquivo PDF consolidado
  dev.off()
  
  message(paste("PDF único gerado com sucesso:", pdf_unico))
}

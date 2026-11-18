# relatorio_escorpioes
Relatório sobre incidência de escorpiões em Piracicaba a ser apresentado no RBras.

rm(list=ls())
graphics.off()

# ==============================================================================
# ANÁLISE DE REGISTROS MENSAIS DE ESCORPIÕES EM PIRACICABA
# ==============================================================================
# Estrutura:
#   0. Preparação do ambiente
#   1. Importação e organização da planilha
#   2. Verificações de consistência
#   3. Estatística descritiva
#   4. Visualizações
#   5. Estrutura temporal, tendência e sazonalidade
#   6. Comparação entre Poisson e binomial negativa
#   7. Diagnóstico do modelo binomial negativo sem dependência temporal
#   8. Modelos temporais de contagem com tscount
#   9. Seleção e diagnóstico do melhor modelo temporal
#  10. Validação fora da amostra
#  11. Previsão dos próximos 12 meses
#  12. Bloco opcional: GARMA-NBI com gamlss.util
#
# IMPORTANTE:
# - A variável é tratada como CONTAGEM mensal de registros.
# - Só use o termo "incidência" se houver um denominador, como população exposta.
# - Dezembro de 2025 é convertido para NA quando aparece como zero, pois o total
#   anual informado corresponde apenas a janeiro-novembro.
# ==============================================================================


# ==============================================================================
# ETAPA 0 — PREPARAÇÃO DO AMBIENTE
# Instala e carrega os pacotes necessários. Cria uma pasta para os resultados.
# ==============================================================================

pacotes <- c(
  "readxl",
  "tidyverse",
  "lubridate",
  "MASS",
  "tscount"
)

instalados <- rownames(installed.packages())
faltantes <- setdiff(pacotes, instalados)

if (length(faltantes) > 0) {
  install.packages(faltantes, dependencies = TRUE)
}

suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(lubridate)
  library(tscount)
})

options(scipen = 999)
set.seed(123)

dir_resultados <- file.path(getwd(), "resultados_escorpioes")
dir.create(dir_resultados, showWarnings = FALSE, recursive = TRUE)

# Ajuste estes parâmetros quando necessário.
aba_dados <- "Planilha1"
h_validacao <- 12
h_previsao <- 12
B_bootstrap <- 500  # Aumente para 1000 ou mais na análise final.


# ==============================================================================
# ETAPA 1 — IMPORTAÇÃO E ORGANIZAÇÃO
# Lê a aba Planilha1 e transforma os meses, originalmente em colunas, em linhas.
# ==============================================================================

arquivo_padrao <- "Dados_Escorpiao_Piracicaba.xlsx"

if (file.exists(arquivo_padrao)) {
  arquivo_xlsx <- arquivo_padrao
} else {
  message("Selecione a planilha .xlsx na janela que será aberta.")
  arquivo_xlsx <- file.choose()
}

meses_abrev <- c(
  "Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
  "Jul", "Ago", "Set", "Out", "Nov", "Dez"
)

meses_num <- setNames(1:12, meses_abrev)

dados_largos <- readxl::read_excel(
  path = arquivo_xlsx,
  sheet = aba_dados
)

colunas_obrigatorias <- c("Ano", meses_abrev)

if (!all(colunas_obrigatorias %in% names(dados_largos))) {
  stop(
    "A planilha não contém todas as colunas esperadas: ",
    paste(colunas_obrigatorias, collapse = ", ")
  )
}

dados_largos <- dados_largos |>
  mutate(
    Ano = as.integer(Ano),
    across(all_of(meses_abrev), as.numeric)
  )

# Confere se o total informado coincide com a soma dos meses.
if ("Total" %in% names(dados_largos)) {
  total_calculado <- rowSums(
    as.data.frame(dados_largos[, meses_abrev]),
    na.rm = TRUE
  )
  
  conferencia_totais <- tibble(
    Ano = dados_largos$Ano,
    Total_informado = as.numeric(dados_largos$Total),
    Total_calculado = total_calculado,
    Diferenca = Total_informado - Total_calculado
  )
  
  readr::write_csv(
    conferencia_totais,
    file.path(dir_resultados, "conferencia_totais_planilha.csv")
  )
}

dados <- dados_largos |>
  dplyr::select(Ano, all_of(meses_abrev)) |>
  pivot_longer(
    cols = all_of(meses_abrev),
    names_to = "mes",
    values_to = "registros"
  ) |>
  mutate(
    mes = factor(mes, levels = meses_abrev),
    mes_num = unname(meses_num[as.character(mes)]),
    data = lubridate::make_date(Ano, mes_num, 1),
    registros = as.numeric(registros),
    
    status_dado = case_when(
      Ano == 2025L &
        as.character(mes) == "Dez" &
        registros == 0 ~ "ausente_provisorio",
      is.na(registros) ~ "ausente",
      TRUE ~ "observado"
    ),
    
    registros = if_else(
      status_dado == "ausente_provisorio",
      NA_real_,
      registros
    )
  ) |>
  arrange(data)

# Base efetivamente utilizada na modelagem.
dados_modelo <- dados |>
  filter(!is.na(registros)) |>
  mutate(
    registros = as.integer(registros),
    indice_t = row_number(),
    tendencia_anos = indice_t / 12,
    
    # Harmônicos anuais.
    cos12 = cos(2 * pi * indice_t / 12),
    sin12 = sin(2 * pi * indice_t / 12),
    
    # Harmônicos semestrais.
    cos6 = cos(2 * pi * indice_t / 6),
    sin6 = sin(2 * pi * indice_t / 6),
    
    # Média móvel retrospectiva de 12 meses.
    media_movel_12 = as.numeric(
      stats::filter(registros, rep(1 / 12, 12), sides = 1)
    )
  )

readr::write_csv(
  dados,
  file.path(dir_resultados, "dados_organizados_com_ausencias.csv")
)

readr::write_csv(
  dados_modelo,
  file.path(dir_resultados, "dados_utilizados_na_modelagem.csv")
)


# ==============================================================================
# ETAPA 3 — ESTATÍSTICA DESCRITIVA
# Produz resumos geral, anual e por mês do calendário.
# ==============================================================================

resumo_geral <- dados_modelo |>
  summarise(
    inicio = min(data),
    fim = max(data),
    n_meses = n(),
    media = mean(registros),
    mediana = median(registros),
    desvio_padrao = sd(registros),
    variancia = var(registros),
    q1 = as.numeric(quantile(registros, 0.25)),
    q3 = as.numeric(quantile(registros, 0.75)),
    minimo = min(registros),
    maximo = max(registros),
    cv_percentual = 100 * desvio_padrao / media,
    razao_variancia_media = variancia / media
  )

resumo_anual <- dados_modelo |>
  group_by(Ano) |>
  summarise(
    meses_observados = n(),
    total = sum(registros),
    media_mensal = mean(registros),
    mediana_mensal = median(registros),
    desvio_padrao = sd(registros),
    minimo = min(registros),
    maximo = max(registros),
    .groups = "drop"
  ) |>
  mutate(
    status_ano = if_else(
      meses_observados == 12,
      "completo",
      "parcial"
    )
  )

resumo_mensal <- dados_modelo |>
  group_by(mes, mes_num) |>
  summarise(
    n_anos = n(),
    media = mean(registros),
    mediana = median(registros),
    desvio_padrao = sd(registros),
    q1 = as.numeric(quantile(registros, 0.25)),
    q3 = as.numeric(quantile(registros, 0.75)),
    minimo = min(registros),
    maximo = max(registros),
    cv_percentual = 100 * desvio_padrao / media,
    .groups = "drop"
  ) |>
  arrange(mes_num)

readr::write_csv(
  resumo_geral,
  file.path(dir_resultados, "resumo_geral.csv")
)

readr::write_csv(
  resumo_anual,
  file.path(dir_resultados, "resumo_anual.csv")
)

readr::write_csv(
  resumo_mensal,
  file.path(dir_resultados, "resumo_por_mes.csv")
)

print(resumo_geral)
print(resumo_anual)
print(resumo_mensal)


# ==============================================================================
# ETAPA 4 — VISUALIZAÇÕES DESCRITIVAS
# Constrói série temporal, heatmap, boxplot mensal, totais anuais e sazonalidade.
# ==============================================================================

tema_graficos <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# 4.1 Série mensal e média móvel de 12 meses.
grafico_serie <- ggplot(dados_modelo, aes(x = data, y = registros)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.2) +
  geom_line(
    aes(y = media_movel_12),
    linewidth = 1,
    color = "red",
    na.rm = TRUE
  ) +
  labs(x = "Data",
    y = "Número de registros"
  ) +
  tema_graficos

ggsave(
  file.path(dir_resultados, "01_serie_temporal.png"),
  grafico_serie,
  width = 11,
  height = 5.5,
  dpi = 300
)

# 4.2 Heatmap ano × mês.
grafico_heatmap <- ggplot(
  dados,
  aes(x = mes, y = Ano, fill = registros)
) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    breaks = sort(unique(dados$Ano))
  ) +
  scale_fill_gradient2(
    low = "green",
    mid = "yellow",
    high = "red",
    midpoint = median(dados$registros, na.rm = TRUE),
    na.value = "grey90"
  ) +
  labs(x = "Mês",
    y = "Ano",
    fill = "Registros"
  ) +
  tema_graficos

ggsave(
  file.path(dir_resultados, "02_heatmap_ano_mes.png"),
  grafico_heatmap,
  width = 10,
  height = 6.5,
  dpi = 300
)

# 4.3 Boxplot por mês do calendário.
grafico_boxplot <- ggplot(
  dados_modelo,
  aes(x = mes, y = registros)
) +
  geom_boxplot(outlier.alpha = 0.7) +
  labs(x = "Mês",
    y = "Número de registros"
  ) +
  tema_graficos

ggsave(
  file.path(dir_resultados, "03_boxplot_por_mes.png"),
  grafico_boxplot,
  width = 10,
  height = 5.5,
  dpi = 300
)

# 4.4 Totais anuais. O ano parcial aparece identificado.
grafico_anual <- ggplot(
  resumo_anual,
  aes(x = factor(Ano), y = total, fill = status_ano)
) +
  geom_col() +
  geom_text(
    aes(label = total),
    vjust = -0.3,
    size = 3
  ) +
  labs(x = "Ano",
    y = "Total",
    fill = "Situação do ano"
  ) +
  tema_graficos +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(dir_resultados, "04_totais_anuais.png"),
  grafico_anual,
  width = 10,
  height = 5.5,
  dpi = 300
)

# 4.5 Média sazonal por mês.
grafico_sazonal <- ggplot(
  resumo_mensal,
  aes(x = mes, y = media, group = 1)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(
      ymin = pmax(0, media - desvio_padrao),
      ymax = media + desvio_padrao
    ),
    width = 0.15,
    alpha = 0.5
  ) +
  labs(x = "Mês",
    y = "Média de registros"
  ) +
  tema_graficos

ggsave(
  file.path(dir_resultados, "05_padrao_sazonal.png"),
  grafico_sazonal,
  width = 10,
  height = 5.5,
  dpi = 300
)


# ==============================================================================
# ETAPA 5 — ESTRUTURA TEMPORAL
# Examina ACF, PACF e decomposição STL antes da modelagem.
# ==============================================================================

inicio_serie <- c(
  lubridate::year(min(dados_modelo$data)),
  lubridate::month(min(dados_modelo$data))
)

serie_ts <- ts(
  dados_modelo$registros,
  start = inicio_serie,
  frequency = 12
)

png(
  file.path(dir_resultados, "06_acf_pacf_serie_bruta.png"),
  width = 1800,
  height = 800,
  res = 160
)

par(mfrow = c(1, 2))

acf(
  serie_ts,
  lag.max = 36,
  main = ""
)

pacf(
  serie_ts,
  lag.max = 36,
  main = ""
)

dev.off()

decomposicao_stl <- stl(
  serie_ts,
  s.window = "periodic",
  robust = TRUE
)

png(
  file.path(dir_resultados, "07_decomposicao_STL.png"),
  width = 1600,
  height = 1200,
  res = 160
)

plot(decomposicao_stl)
dev.off()


# ==============================================================================
# ETAPA 6 — POISSON VERSUS BINOMIAL NEGATIVA
# Ajusta modelos com tendência e harmônicos anuais/semestrais.
# A comparação verifica se a binomial negativa acomoda melhor a sobredispersão.
# ==============================================================================

formula_completa <- registros ~
  tendencia_anos +
  cos12 + sin12 +
  cos6 + sin6

modelo_poisson <- glm(
  formula = formula_completa,
  family = poisson(link = "log"),
  data = dados_modelo
)

modelo_nb_intercepto <- MASS::glm.nb(
  registros ~ 1,
  data = dados_modelo,
  link = log
)

modelo_nb_tendencia <- MASS::glm.nb(
  registros ~ tendencia_anos,
  data = dados_modelo,
  link = log
)

modelo_nb_completo <- MASS::glm.nb(
  formula = formula_completa,
  data = dados_modelo,
  link = log
)

dispersao_poisson <- sum(
  residuals(modelo_poisson, type = "pearson")^2
) / df.residual(modelo_poisson)

comparacao_distribuicoes <- tibble(
  modelo = c(
    "Poisson: tendência + harmônicos",
    "NB: somente intercepto",
    "NB: tendência",
    "NB: tendência + harmônicos"
  ),
  AIC = c(
    AIC(modelo_poisson),
    AIC(modelo_nb_intercepto),
    AIC(modelo_nb_tendencia),
    AIC(modelo_nb_completo)
  ),
  BIC = c(
    BIC(modelo_poisson),
    BIC(modelo_nb_intercepto),
    BIC(modelo_nb_tendencia),
    BIC(modelo_nb_completo)
  )
) |>
  arrange(AIC) |>
  mutate(
    delta_AIC = AIC - min(AIC)
  )

coeficientes_nb <- as.data.frame(
  summary(modelo_nb_completo)$coefficients
) |>
  tibble::rownames_to_column("termo") |>
  as_tibble()

names(coeficientes_nb) <- c(
  "termo",
  "estimativa",
  "erro_padrao",
  "estatistica_z",
  "p_valor"
)

teste_harmonicos <- anova(
  modelo_nb_tendencia,
  modelo_nb_completo,
  test = "Chisq"
)

readr::write_csv(
  comparacao_distribuicoes,
  file.path(dir_resultados, "comparacao_poisson_binomial_negativa.csv")
)

readr::write_csv(
  coeficientes_nb,
  file.path(dir_resultados, "coeficientes_modelo_NB_completo.csv")
)

capture.output(
  summary(modelo_poisson),
  file = file.path(dir_resultados, "resumo_modelo_poisson.txt")
)

capture.output(
  summary(modelo_nb_completo),
  file = file.path(dir_resultados, "resumo_modelo_NB_completo.txt")
)

capture.output(
  teste_harmonicos,
  file = file.path(dir_resultados, "teste_contribuicao_harmonicos.txt")
)

writeLines(
  paste0(
    "Dispersão de Pearson do modelo Poisson: ",
    round(dispersao_poisson, 4)
  ),
  file.path(dir_resultados, "dispersao_poisson.txt")
)

print(comparacao_distribuicoes)
message(
  "Dispersão de Pearson do modelo Poisson: ",
  round(dispersao_poisson, 3)
)


# ==============================================================================
# ETAPA 7 — DIAGNÓSTICO DO MODELO NB SEM DEPENDÊNCIA TEMPORAL
# Calcula resíduos quantílicos aleatorizados, ACF, PACF, QQ-plot e Ljung-Box.
# ==============================================================================

residuos_quantile_nbinom <- function(
    y,
    mu,
    size,
    semente = 123
) {
  if (length(y) != length(mu)) {
    stop("y e mu precisam ter o mesmo comprimento.")
  }
  
  set.seed(semente)
  
  limite_inferior <- pnbinom(
    q = y - 1,
    size = size,
    mu = mu
  )
  
  limite_superior <- pnbinom(
    q = y,
    size = size,
    mu = mu
  )
  
  u <- runif(
    n = length(y),
    min = limite_inferior,
    max = limite_superior
  )
  
  epsilon <- .Machine$double.eps
  u <- pmin(pmax(u, epsilon), 1 - epsilon)
  
  qnorm(u)
}

tabela_ljung_box <- function(
    residuos,
    lags = c(6, 10, 12, 18, 24),
    fitdf = 0
) {
  purrr::map_dfr(
    lags,
    function(h) {
      if (h <= fitdf) {
        return(
          tibble(
            lag = h,
            estatistica = NA_real_,
            p_valor = NA_real_,
            rejeita_H0_5pct = NA
          )
        )
      }
      
      teste <- Box.test(
        residuos,
        lag = h,
        type = "Ljung-Box",
        fitdf = fitdf
      )
      
      tibble(
        lag = h,
        estatistica = unname(teste$statistic),
        p_valor = teste$p.value,
        rejeita_H0_5pct = teste$p.value < 0.05
      )
    }
  )
}

salvar_diagnostico_residuos <- function(
    residuos,
    datas,
    observados,
    ajustados,
    titulo,
    arquivo_pdf
) {
  grDevices::pdf(
    arquivo_pdf,
    width = 11,
    height = 7.5
  )
  
  par(mfrow = c(2, 3))
  
  plot(
    datas,
    residuos,
    type = "p",
    pch = 16,
    cex = 0.7,
    main = "Resíduos ao longo do tempo",
    xlab = "Data",
    ylab = "Resíduo"
  )
  abline(h = 0, lty = 2)
  
  acf(
    residuos,
    lag.max = 24,
    main = "ACF dos resíduos"
  )
  
  pacf(
    residuos,
    lag.max = 24,
    main = "PACF dos resíduos"
  )
  
  qqnorm(
    residuos,
    pch = 16,
    cex = 0.7,
    main = "QQ-plot"
  )
  qqline(residuos, lty = 2)
  
  hist(
    residuos,
    breaks = "FD",
    main = "Histograma dos resíduos",
    xlab = "Resíduo"
  )
  
  plot(
    ajustados,
    observados,
    pch = 16,
    cex = 0.7,
    main = "Observado × ajustado",
    xlab = "Valor ajustado",
    ylab = "Valor observado"
  )
  abline(0, 1, lty = 2)
  
  mtext(
    titulo,
    outer = TRUE,
    line = -1.5,
    cex = 1.1,
    font = 2
  )
  
  dev.off()
}

residuos_q_nb <- residuos_quantile_nbinom(
  y = dados_modelo$registros,
  mu = fitted(modelo_nb_completo),
  size = modelo_nb_completo$theta
)

ljung_nb <- tabela_ljung_box(
  residuos = residuos_q_nb,
  lags = c(6, 10, 12, 18, 24),
  fitdf = 0
)

readr::write_csv(
  ljung_nb,
  file.path(dir_resultados, "ljung_box_modelo_NB_sem_dependencia.csv")
)

salvar_diagnostico_residuos(
  residuos = residuos_q_nb,
  datas = dados_modelo$data,
  observados = dados_modelo$registros,
  ajustados = fitted(modelo_nb_completo),
  titulo = "Modelo NB com tendência e sazonalidade, sem dependência temporal",
  arquivo_pdf = file.path(
    dir_resultados,
    "08_diagnostico_NB_sem_dependencia.pdf"
  )
)


# ==============================================================================
# ETAPA 8 — MODELOS TEMPORAIS DE CONTAGEM COM tscount
# Ajusta modelos log-lineares NB com:
#   past_obs  = dependência em observações anteriores;
#   past_mean = dependência em médias condicionais anteriores.
#
# Esses modelos são próximos, mas não idênticos, ao GARMA de gamlss.util.
# O bloco GARMA exato aparece ao final do script.
# ==============================================================================

xreg_completo <- dados_modelo |>
  dplyr::select(
    tendencia_anos,
    cos12,
    sin12,
    cos6,
    sin6
  ) |>
  as.matrix()

# Os modelos com lag 12 representam dependência sazonal específica.
# Eles não equivalem a incluir todos os lags 1:12.
candidatos_tsglm <- list(
  SEM_DEPENDENCIA = list(
    past_obs = NULL,
    past_mean = NULL
  ),
  
  OBS_1 = list(
    past_obs = 1,
    past_mean = NULL
  ),
  
  MEDIA_1 = list(
    past_obs = NULL,
    past_mean = 1
  ),
  
  OBS_1_MEDIA_1 = list(
    past_obs = 1,
    past_mean = 1
  ),
  
  OBS_1_2 = list(
    past_obs = 1:2,
    past_mean = NULL
  ),
  
  OBS_1_2_MEDIA_1 = list(
    past_obs = 1:2,
    past_mean = 1
  ),
  
  OBS_12 = list(
    past_obs = 12,
    past_mean = NULL
  ),
  
  OBS_1_12 = list(
    past_obs = c(1, 12),
    past_mean = NULL
  ),
  
  OBS_1_12_MEDIA_1 = list(
    past_obs = c(1, 12),
    past_mean = 1
  )
)

ajustar_tsglm <- function(
    serie,
    xreg,
    especificacao
) {
  tscount::tsglm(
    ts = serie,
    model = list(
      past_obs = especificacao$past_obs,
      past_mean = especificacao$past_mean,
      external = FALSE
    ),
    xreg = xreg,
    link = "log",
    distr = "nbinom",
    init.drop = FALSE
  )
}

ajustes_tsglm <- purrr::imap(
  candidatos_tsglm,
  function(especificacao, nome) {
    tryCatch(
      ajustar_tsglm(
        serie = serie_ts,
        xreg = xreg_completo,
        especificacao = especificacao
      ),
      error = function(e) {
        structure(
          list(
            nome = nome,
            mensagem = conditionMessage(e)
          ),
          class = "erro_tsglm"
        )
      }
    )
  }
)

erros_tsglm <- purrr::keep(
  ajustes_tsglm,
  ~ inherits(.x, "erro_tsglm")
)

if (length(erros_tsglm) > 0) {
  tabela_erros <- purrr::imap_dfr(
    erros_tsglm,
    function(objeto, nome) {
      tibble(
        modelo = nome,
        erro = objeto$mensagem
      )
    }
  )
  
  readr::write_csv(
    tabela_erros,
    file.path(dir_resultados, "erros_ajustes_tsglm.csv")
  )
}

ajustes_tsglm_ok <- purrr::keep(
  ajustes_tsglm,
  ~ inherits(.x, "tsglm")
)

if (length(ajustes_tsglm_ok) == 0) {
  stop(
    "Nenhum modelo do tscount convergiu. ",
    "Consulte o arquivo erros_ajustes_tsglm.csv."
  )
}

extrair_resultados_tsglm <- function(
    modelo,
    nome,
    especificacao
) {
  
  # Confirma que o ajuste realmente é um objeto tsglm completo.
  if (!inherits(modelo, "tsglm") || !is.list(modelo)) {
    stop(
      "O ajuste '", nome,
      "' não é um objeto tsglm válido. Classe encontrada: ",
      paste(class(modelo), collapse = ", ")
    )
  }
  
  campos_obrigatorios <- c(
    "coefficients",
    "logLik",
    "n_eff",
    "fitted.values",
    "response"
  )
  
  campos_ausentes <- setdiff(
    campos_obrigatorios,
    names(modelo)
  )
  
  if (length(campos_ausentes) > 0) {
    stop(
      "O ajuste '", nome,
      "' não contém os campos: ",
      paste(campos_ausentes, collapse = ", ")
    )
  }
  
  # Resíduos de Pearson do modelo.
  residuos_pearson <- stats::residuals(
    modelo,
    type = "pearson"
  )
  
  fitdf <- length(especificacao$past_obs) +
    length(especificacao$past_mean)
  
  ljung <- tabela_ljung_box(
    residuos = residuos_pearson,
    lags = c(6, 10, 12),
    fitdf = fitdf
  )
  
  obter_p <- function(h) {
    valor <- ljung$p_valor[ljung$lag == h]
    
    if (length(valor) == 0) {
      NA_real_
    } else {
      valor
    }
  }
  
  # Número de parâmetros da média + parâmetro de dispersão, quando existente.
  numero_parametros <- length(modelo$coefficients) +
    length(modelo$distrcoefs)
  
  log_verossimilhanca <- as.numeric(modelo$logLik)
  
  numero_observacoes_efetivas <- as.numeric(
    modelo$n_eff
  )
  
  # Cálculo direto para evitar conflito entre métodos genéricos.
  aic_modelo <- -2 * log_verossimilhanca +
    2 * numero_parametros
  
  bic_modelo <- -2 * log_verossimilhanca +
    log(numero_observacoes_efetivas) *
    numero_parametros
  
  qic_modelo <- tryCatch(
    as.numeric(tscount::QIC(modelo)),
    error = function(e) {
      warning(
        "Não foi possível calcular o QIC para '",
        nome,
        "': ",
        conditionMessage(e)
      )
      
      NA_real_
    }
  )
  
  tibble::tibble(
    modelo = nome,
    
    lags_observacoes = if (
      length(especificacao$past_obs) == 0
    ) {
      "nenhum"
    } else {
      paste(
        especificacao$past_obs,
        collapse = ","
      )
    },
    
    lags_medias = if (
      length(especificacao$past_mean) == 0
    ) {
      "nenhum"
    } else {
      paste(
        especificacao$past_mean,
        collapse = ","
      )
    },
    
    numero_parametros = numero_parametros,
    logLik = log_verossimilhanca,
    AIC = aic_modelo,
    BIC = bic_modelo,
    QIC = qic_modelo,
    
    p_Ljung_6 = obter_p(6),
    p_Ljung_10 = obter_p(10),
    p_Ljung_12 = obter_p(12)
  )
}

tabela_modelos_tsglm <- purrr::imap_dfr(
  ajustes_tsglm_ok,
  function(modelo, nome) {
    extrair_resultados_tsglm(
      modelo = modelo,
      nome = nome,
      especificacao = candidatos_tsglm[[nome]]
    )
  }
) |>
  arrange(AIC) |>
  mutate(
    delta_AIC = AIC - min(AIC),
    residuos_ok_5pct =
      p_Ljung_6 > 0.05 &
      p_Ljung_10 > 0.05 &
      p_Ljung_12 > 0.05
  )

readr::write_csv(
  tabela_modelos_tsglm,
  file.path(dir_resultados, "comparacao_modelos_tscount.csv")
)

print(tabela_modelos_tsglm)


# ==============================================================================
# ETAPA 9 — SELEÇÃO E DIAGNÓSTICO DO MELHOR MODELO TEMPORAL
# Entre modelos com delta AIC < 2, prioriza resíduos sem autocorrelação e,
# depois, o menor número de parâmetros. Se nenhum satisfizer, usa o menor AIC.
# ==============================================================================

modelos_elegiveis <- tabela_modelos_tsglm |>
  filter(
    delta_AIC < 2,
    residuos_ok_5pct
  ) |>
  arrange(
    numero_parametros,
    AIC
  )

if (nrow(modelos_elegiveis) > 0) {
  melhor_nome <- modelos_elegiveis$modelo[1]
  criterio_selecao <- paste(
    "delta AIC < 2, ausência de autocorrelação nos lags 6, 10 e 12",
    "e parcimônia"
  )
} else {
  melhor_nome <- tabela_modelos_tsglm$modelo[1]
  criterio_selecao <- paste(
    "menor AIC; nenhum candidato com delta AIC < 2",
    "passou simultaneamente nos três testes de Ljung-Box"
  )
  
  warning(
    "O modelo selecionado ainda pode apresentar autocorrelação residual. ",
    "Examine os diagnósticos antes de utilizá-lo para previsão."
  )
}

melhor_modelo <- ajustes_tsglm_ok[[melhor_nome]]
melhor_especificacao <- candidatos_tsglm[[melhor_nome]]

writeLines(
  c(
    paste("Modelo selecionado:", melhor_nome),
    paste("Critério:", criterio_selecao)
  ),
  file.path(dir_resultados, "modelo_selecionado.txt")
)

capture.output(
  summary(melhor_modelo),
  file = file.path(dir_resultados, "resumo_melhor_modelo_tscount.txt")
)

fitdf_melhor <- length(melhor_especificacao$past_obs) +
  length(melhor_especificacao$past_mean)

residuos_pearson_melhor <- residuals(
  melhor_modelo,
  type = "pearson"
)

size_melhor <- as.numeric(
  melhor_modelo$distrcoefs[1]
)

residuos_q_melhor <- residuos_quantile_nbinom(
  y = melhor_modelo$response,
  mu = as.numeric(fitted(melhor_modelo)),
  size = size_melhor
)

ljung_melhor_1_24 <- tabela_ljung_box(
  residuos = residuos_q_melhor,
  lags = 1:24,
  fitdf = fitdf_melhor
)

readr::write_csv(
  ljung_melhor_1_24,
  file.path(
    dir_resultados,
    "ljung_box_melhor_modelo_lags_1_24.csv"
  )
)

salvar_diagnostico_residuos(
  residuos = residuos_q_melhor,
  datas = dados_modelo$data,
  observados = dados_modelo$registros,
  ajustados = as.numeric(fitted(melhor_modelo)),
  titulo = paste("Melhor modelo temporal:", melhor_nome),
  arquivo_pdf = file.path(
    dir_resultados,
    "09_diagnostico_melhor_modelo_temporal.pdf"
  )
)

# Diagnósticos próprios do pacote tscount:
# ACF dos resíduos de Pearson, resíduos no tempo, periodograma,
# histograma PIT e calibração marginal.
pdf(
  file.path(
    dir_resultados,
    "10_diagnosticos_nativos_tscount.pdf"
  ),
  width = 8,
  height = 6
)
plot(melhor_modelo, ask = FALSE)
dev.off()

dados_ajuste <- dados_modelo |>
  transmute(
    data,
    observado = registros,
    ajustado = as.numeric(fitted(melhor_modelo))
  )

grafico_ajuste <- ggplot(
  dados_ajuste,
  aes(x = data)
) +
  geom_line(
    aes(y = observado, linetype = "Observado"),
    linewidth = 0.65
  ) +
  geom_line(
    aes(y = ajustado, linetype = "Ajustado"),
    linewidth = 0.8
  ) +
  labs(
    x = "Data",
    y = "Número de registros",
    linetype = NULL
  ) +
  tema_graficos

ggsave(
  file.path(dir_resultados, "11_observado_versus_ajustado.png"),
  grafico_ajuste,
  width = 11,
  height = 5.5,
  dpi = 300
)


# ==============================================================================
# ETAPA 10 — VALIDAÇÃO FORA DA AMOSTRA
# Reserva os últimos 12 meses. Faz previsões de um passo à frente, utilizando
# em cada mês os valores já observados nos meses anteriores do período de teste.
# ==============================================================================

n_total <- nrow(dados_modelo)

if (n_total <= h_validacao + 36) {
  warning(
    "A amostra de treinamento ficará curta para a validação escolhida."
  )
}

indices_treino <- seq_len(n_total - h_validacao)
indices_teste <- (n_total - h_validacao + 1):n_total

dados_treino <- dados_modelo[indices_treino, ]
dados_teste <- dados_modelo[indices_teste, ]

serie_treino <- ts(
  dados_treino$registros,
  start = c(
    year(min(dados_treino$data)),
    month(min(dados_treino$data))
  ),
  frequency = 12
)

xreg_treino <- xreg_completo[indices_treino, , drop = FALSE]
xreg_teste <- xreg_completo[indices_teste, , drop = FALSE]

modelo_treino <- ajustar_tsglm(
  serie = serie_treino,
  xreg = xreg_treino,
  especificacao = melhor_especificacao
)

previsao_validacao <- predict(
  modelo_treino,
  n.ahead = h_validacao,
  newobs = dados_teste$registros,
  newxreg = xreg_teste,
  level = 0.95,
  method = "conddistr"
)

tabela_validacao <- tibble(
  data = dados_teste$data,
  observado = dados_teste$registros,
  previsto = as.numeric(previsao_validacao$pred),
  limite_inferior = as.numeric(
    previsao_validacao$interval[, "lower"]
  ),
  limite_superior = as.numeric(
    previsao_validacao$interval[, "upper"]
  )
)

metricas_validacao <- tabela_validacao |>
  summarise(
    MAE = mean(abs(observado - previsto)),
    RMSE = sqrt(mean((observado - previsto)^2)),
    sMAPE_percentual = mean(
      200 * abs(observado - previsto) /
        (abs(observado) + abs(previsto))
    ),
    cobertura_intervalo_95 = mean(
      observado >= limite_inferior &
        observado <= limite_superior
    )
  )

readr::write_csv(
  tabela_validacao,
  file.path(dir_resultados, "validacao_previsoes.csv")
)

readr::write_csv(
  metricas_validacao,
  file.path(dir_resultados, "metricas_validacao.csv")
)

grafico_validacao <- ggplot(
  tabela_validacao,
  aes(x = data)
) +
  geom_ribbon(
    aes(
      ymin = limite_inferior,
      ymax = limite_superior
    ),
    alpha = 0.2
  ) +
  geom_line(
    aes(y = observado, linetype = "Observado"),
    linewidth = 0.75
  ) +
  geom_line(
    aes(y = previsto, linetype = "Previsto"),
    linewidth = 0.75
  ) +
  geom_point(
    aes(y = observado),
    size = 1.7
  ) +
  labs(x = "Data",
    y = "Número de registros",
    linetype = NULL
  ) +
  tema_graficos

ggsave(
  file.path(dir_resultados, "12_validacao_fora_da_amostra.png"),
  grafico_validacao,
  width = 10,
  height = 5.5,
  dpi = 300
)

print(metricas_validacao)


# ==============================================================================
# ETAPA 11 — PREVISÃO DOS PRÓXIMOS 12 MESES
# Reajusta-se o melhor modelo com todos os dados e fornecem-se os valores futuros
# da tendência e dos harmônicos. Intervalos são obtidos por bootstrap paramétrico.
# ==============================================================================

ultima_data <- max(dados_modelo$data)

datas_futuras <- seq(
  from = ultima_data %m+% months(1),
  by = "month",
  length.out = h_previsao
)

indices_futuros <- max(dados_modelo$indice_t) +
  seq_len(h_previsao)

dados_futuros_regressores <- tibble(
  data = datas_futuras,
  indice_t = indices_futuros,
  tendencia_anos = indice_t / 12,
  cos12 = cos(2 * pi * indice_t / 12),
  sin12 = sin(2 * pi * indice_t / 12),
  cos6 = cos(2 * pi * indice_t / 6),
  sin6 = sin(2 * pi * indice_t / 6)
)

xreg_futuro <- dados_futuros_regressores |>
  dplyr::select(
    tendencia_anos,
    cos12,
    sin12,
    cos6,
    sin6
  ) |>
  as.matrix()

previsao_futura <- tryCatch(
  predict(
    melhor_modelo,
    n.ahead = h_previsao,
    newxreg = xreg_futuro,
    level = 0.95,
    method = "bootstrap",
    B = B_bootstrap
  ),
  error = function(e) {
    stop(
      "A previsão futura falhou: ",
      conditionMessage(e),
      "\nTente aumentar a estabilidade do modelo ou reduzir B_bootstrap."
    )
  }
)

tabela_previsao_futura <- tibble(
  data = datas_futuras,
  previsao_media = as.numeric(previsao_futura$pred),
  limite_inferior_95 = as.numeric(
    previsao_futura$interval[, "lower"]
  ),
  limite_superior_95 = as.numeric(
    previsao_futura$interval[, "upper"]
  )
)

readr::write_csv(
  tabela_previsao_futura,
  file.path(dir_resultados, "previsao_proximos_12_meses.csv")
)

dados_historico_grafico <- dados_modelo |>
  filter(
    data >= max(data) %m-% years(3)
  ) |>
  transmute(
    data,
    registros
  )

grafico_previsao <- ggplot() +
  geom_line(
    data = dados_historico_grafico,
    aes(x = data, y = registros),
    linewidth = 0.7
  ) +
  geom_ribbon(
    data = tabela_previsao_futura,
    aes(
      x = data,
      ymin = limite_inferior_95,
      ymax = limite_superior_95
    ),
    alpha = 0.2
  ) +
  geom_line(
    data = tabela_previsao_futura,
    aes(x = data, y = previsao_media),
    linewidth = 0.9
  ) +
  geom_point(
    data = tabela_previsao_futura,
    aes(x = data, y = previsao_media),
    size = 1.7
  ) +
  labs(x = "Data",
    y = "Número de registros"
  ) +
  tema_graficos

ggsave(
  file.path(dir_resultados, "13_previsao_proximos_12_meses.png"),
  grafico_previsao,
  width = 11,
  height = 5.5,
  dpi = 300
)


# ==============================================================================
# ETAPA 12 — BLOCO OPCIONAL: GARMA-NBI COM gamlss.util
# Use apenas se for necessário reproduzir mais diretamente a estrutura da IC.
#
# ATENÇÃO:
# - gamlss.util é antigo e pode exigir instalação a partir do arquivo do CRAN.
# - garmaFit() não verifica automaticamente a estacionariedade.
# - O bloco fica desativado para não interromper o restante do script.
# ==============================================================================

executar_garma_gamlss_util <- TRUE

if (executar_garma_gamlss_util) {
  
  pacotes_garma <- c(
    "gamlss",
    "gamlss.dist",
    "gamlss.util"
  )
  
  faltantes_garma <- setdiff(
    pacotes_garma,
    rownames(installed.packages())
  )
  
  if (length(faltantes_garma) > 0) {
    stop(
      "Instale primeiro os pacotes: ",
      paste(faltantes_garma, collapse = ", "),
      "\nPara gamlss.util, uma alternativa é:\n",
      "install.packages('remotes')\n",
      "remotes::install_version(",
      "'gamlss.util', version = '4.3-4', ",
      "repos = 'https://cran.r-project.org')"
    )
  }
  
  ordens_garma <- list(
    GARMA_0_0 = c(0, 0),
    GARMA_1_0 = c(1, 0),
    GARMA_0_1 = c(0, 1),
    GARMA_1_1 = c(1, 1),
    GARMA_2_0 = c(2, 0),
    GARMA_2_1 = c(2, 1),
    GARMA_12_0 = c(12, 0),
    GARMA_12_1 = c(12, 1)
  )
  
  ajustes_garma <- purrr::imap(
    ordens_garma,
    function(ordem, nome) {
      tryCatch(
        gamlss.util::garmaFit(
          formula = registros ~
            tendencia_anos +
            cos12 + sin12 +
            cos6 + sin6,
          order = ordem,
          data = dados_modelo,
          family = gamlss.dist::NBI()
        ),
        error = function(e) {
          structure(
            list(
              nome = nome,
              mensagem = conditionMessage(e)
            ),
            class = "erro_garma"
          )
        }
      )
    }
  )
  
  ajustes_garma_ok <- purrr::keep(
    ajustes_garma,
    ~ !inherits(.x, "erro_garma")
  )
  
  if (length(ajustes_garma_ok) > 0) {
    comparacao_garma <- purrr::imap_dfr(
      ajustes_garma_ok,
      function(modelo, nome) {
        tibble(
          modelo = nome,
          AIC = AIC(modelo),
          BIC = tryCatch(
            BIC(modelo),
            error = function(e) NA_real_
          )
        )
      }
    ) |>
      arrange(AIC) |>
      mutate(
        delta_AIC = AIC - min(AIC)
      )
    
    readr::write_csv(
      comparacao_garma,
      file.path(
        dir_resultados,
        "comparacao_GARMA_gamlss_util.csv"
      )
    )
    
    print(comparacao_garma)
    
  } else {
    warning(
      "Nenhum ajuste GARMA com gamlss.util foi concluído."
    )
  }
}


# ==============================================================================
# ENCERRAMENTO
# Salva as versões dos pacotes e informa onde os resultados foram gravados.
# ==============================================================================

capture.output(
  sessionInfo(),
  file = file.path(dir_resultados, "sessionInfo.txt")
)

message("")
message("Análise concluída.")
message("Resultados salvos em:")
message(normalizePath(dir_resultados))

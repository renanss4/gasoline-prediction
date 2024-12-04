# Carregar pacotes necessários
library(readr)        # Leitura de arquivos
library(dplyr)        # Manipulação de dados
library(stringr)      # Manipulação de strings
library(ggcorrplot)     # Visualização de correlações
library(tidyverse)
library(forecast)
library(lubridate)

# 1. LEITURA E PREPARAÇÃO DOS DADOS

# Leitura da base de dados (formato TSV)
dataset <- read_tsv("2004-2021.tsv")

# Renomear colunas: remover acentos e substituir espaços por '_'
colnames(dataset) <- colnames(dataset) %>%
  str_replace_all(" ", "_") %>%
  stringi::stri_trans_general("Latin-ASCII")

# Seleção e agregação inicial dos dados
# nosso objetivo é interpretar como a média do preço de revenda foi mudando seu
# valor ao longo do tempo, para cada região e para determinados produtos. 
# É possível filtrar o produto, iremos verificar a GASOLINA COMUM
base <- dataset %>%
  select(DATA_INICIAL, REGIAO, PRODUTO, NUMERO_DE_POSTOS_PESQUISADOS, PRECO_MEDIO_REVENDA) %>%
  mutate(DATA_INICIAL = as.Date(DATA_INICIAL, format = "%Y-%m-%d"))

# Cálculo do preço médio ponderado
# isso pq a variavel PRECO_MEDIO_REVENDA já é uma média com base na variável de
# NUMERO_DE_POSTOS_PESQUISADOS, então buscamos a média ponderada do preço médio
# de revenda, utilizando como peso o número de postos pesquisados, e fazendo um
# agrupamento por data, região e produto
base_ponderada <- base %>%
  group_by(DATA_INICIAL, REGIAO, PRODUTO) %>%
  summarise(
    media_ponderada_preco = sum(PRECO_MEDIO_REVENDA * NUMERO_DE_POSTOS_PESQUISADOS, na.rm = TRUE) /
      sum(NUMERO_DE_POSTOS_PESQUISADOS, na.rm = TRUE),
    .groups = "drop"
  )

# 2. FUNÇÃO PARA FILTRAR DADOS POR REGIÃO E PRODUTO
filtrar_dados <- function(base, regiao = NULL, produto = NULL) {
  if (!is.null(regiao)) {
    base <- base %>%
      filter(REGIAO == regiao)
  }
  
  if (!is.null(produto)) {
    base <- base %>%
      filter(PRODUTO == produto)
  }
  
  return(base)
}


# Filtrar dados para região e gasolina comum
baseCO_GC <- filtrar_dados(base_ponderada, "CENTRO OESTE", "GASOLINA COMUM")
baseN_GC <- filtrar_dados(base_ponderada, "NORTE", "GASOLINA COMUM")
baseNE_GC <- filtrar_dados(base_ponderada, "NORDESTE", "GASOLINA COMUM")
baseS_GC <- filtrar_dados(base_ponderada, "SUL", "GASOLINA COMUM")
baseSE_GC <- filtrar_dados(base_ponderada, "SUDESTE", "GASOLINA COMUM")

# 3. FUNÇÃO PARA ORGANIZAR SÉRIES TEMPORAIS
organizar_series <- function(base, periodicidade) {
  if (periodicidade == "semanal") {
    base %>%
      mutate(DATA = floor_date(DATA_INICIAL, "week")) %>%
      group_by(DATA) %>%
      summarise(media_ponderada_preco = mean(media_ponderada_preco, na.rm = TRUE), .groups = "drop")
  } else if (periodicidade == "mensal") {
    base %>%
      mutate(DATA = floor_date(DATA_INICIAL, "month")) %>%
      group_by(DATA) %>%
      summarise(media_ponderada_preco = mean(media_ponderada_preco, na.rm = TRUE), .groups = "drop")
  } else if (periodicidade == "anual") {
    base %>%
      mutate(DATA = year(DATA_INICIAL)) %>%
      group_by(DATA) %>%
      summarise(media_ponderada_preco = mean(media_ponderada_preco, na.rm = TRUE), .groups = "drop")
  } else {
    stop("Periodicidade inválida. Use 'semanal', 'mensal' ou 'anual'.")
  }
}

# Organizar dados em séries temporais (exemplo mensal)
baseCO_GC_mensal <- organizar_series(baseCO_GC, "mensal")
baseN_GC_mensal <- organizar_series(baseN_GC, "mensal")
baseNE_GC_mensal <- organizar_series(baseNE_GC, "mensal")
baseS_GC_mensal <- organizar_series(baseS_GC, "mensal")
baseSE_GC_mensal <- organizar_series(baseSE_GC, "mensal")

# 4. AJUSTE DE MODELO E PREVISÕES
# Função para separar treino e teste
separar_treino_teste <- function(series, n_treino) {
  n_total <- nrow(series)
  n_teste <- n_total - n_treino
  treino <- series[1:n_treino, ]
  teste <- series[(n_treino + 1):n_total, ]
  list(treino = treino, teste = teste)
}

# Separando conjuntos para cada região
conjuntos_CO <- separar_treino_teste(baseCO_GC_mensal, 191)
treino_CO <- conjuntos_CO$treino
teste_CO <- conjuntos_CO$teste

conjuntos_N <- separar_treino_teste(baseN_GC_mensal, 191)
treino_N <- conjuntos_N$treino
teste_N <- conjuntos_N$teste

conjuntos_NE <- separar_treino_teste(baseNE_GC_mensal, 191)
treino_NE <- conjuntos_NE$treino
teste_NE <- conjuntos_NE$teste

conjuntos_S <- separar_treino_teste(baseS_GC_mensal, 191)
treino_S <- conjuntos_S$treino
teste_S <- conjuntos_S$teste

conjuntos_SE <- separar_treino_teste(baseSE_GC_mensal, 191)
treino_SE <- conjuntos_SE$treino
teste_SE <- conjuntos_SE$teste


# Criando a base temporal de cada região e visualizando
baseCO_ts = ts(treino_CO$media_ponderada_preco, frequency=12, start=c(2004,1))
plot(baseCO_ts, type="s")

baseN_ts = ts(treino_N$media_ponderada_preco, frequency=12, start=c(2004,1))
plot(baseN_ts, type="s")

baseNE_ts = ts(treino_NE$media_ponderada_preco, frequency=12, start=c(2004,1))
plot(baseNE_ts, type="s")

baseS_ts = ts(treino_S$media_ponderada_preco, frequency=12, start=c(2004,1))
plot(baseS_ts, type="s")

baseSE_ts = ts(treino_SE$media_ponderada_preco, frequency=12, start=c(2004,1))
plot(baseSE_ts, type="s")

# Suavizações Exponenciais para os modelos SIMPLES, HOLT E HW_ad,
# Utilizamos o modelo aditivo ao invés do multiplicativo por conta da amplitude
# da série temporal não varia significativamente com o tempo
# SIMPLES
SES_CO = ses(baseCO_ts, h = 12)
SES_N = ses(baseN_ts, h = 12)
SES_NE = ses(baseNE_ts, h = 12)
SES_S = ses(baseS_ts, h = 12)
SES_SE = ses(baseSE_ts, h = 12)

# HOLT
HOLT_CO = holt(baseCO_ts, h = 12)
HOLT_N = holt(baseN_ts, h = 12)
HOLT_NE = holt(baseNE_ts, h = 12)
HOLT_S = holt(baseS_ts, h = 12)
HOLT_SE = holt(baseSE_ts, h = 12)

# HOLT-WINTER ADITIVO
HW_ad_CO = hw(baseCO_ts, seasonal = "additive", h = 12)
HW_ad_N = hw(baseN_ts, seasonal = "additive", h = 12)
HW_ad_NE = hw(baseNE_ts, seasonal = "additive", h = 12)
HW_ad_S = hw(baseS_ts, seasonal = "additive", h = 12)
HW_ad_SE = hw(baseSE_ts, seasonal = "additive", h = 12)

# COMPARACAO GERAL
list(SES_CO, HOLT_CO, HW_ad_CO) %>% map(accuracy)
list(SES_N, HOLT_N, HW_ad_N) %>% map(accuracy)
list(SES_NE, HOLT_NE, HW_ad_NE) %>% map(accuracy)
list(SES_S, HOLT_S, HW_ad_S) %>% map(accuracy)
list(SES_SE, HOLT_SE, HW_ad_SE) %>% map(accuracy)

# Resumo Geral
# O RMSE (Root Mean Squared Error) é uma métrica que mede o erro entre os
# valores previstos e os observados. Para cada região, escolhemos o modelo com
# o menor RMSE, pois isso indica melhor desempenho preditivo.
# Região - Melhor Modelo - RMSE (métrica utilizada para escolha do modelo)
# CO - SES_CO - 0.0646
# N - SES_N - 0.0594
# NE - HOLT_NE - 0.0609
# S - SES_S - 0.0657
# SE - HOLT_SE - 0.0571

# Testando as pipelines
SES_CO.predito = summary(SES_CO, h=12)
# SES_CO.predito
previsao_CO = cbind(SES_CO.predito$mean, SES_CO.predito$lower, SES_CO.predito$upper)
previsao_CO = as.data.frame(previsao_CO[c(1:12),c(1,3,5)])
colnames(previsao_CO) = c("Previsao","LI","LS")
teste_CO = cbind(teste_CO, previsao_CO)
teste_CO$Periodo = seq(1:12)

SES_N.predito = summary(SES_N, h=12)
# SES_N.predito
previsao_N = cbind(SES_N.predito$mean, SES_N.predito$lower, SES_N.predito$upper)
previsao_N = as.data.frame(previsao_N[c(1:12),c(1,3,5)])
colnames(previsao_N) = c("Previsao","LI","LS")
teste_N = cbind(teste_N, previsao_N)
teste_N$Periodo = seq(1:12)

HOLT_NE.predito = summary(HOLT_NE, h=12)
# HOLT_NE.predito
previsao_NE = cbind(HOLT_NE.predito$mean, HOLT_NE.predito$lower, HOLT_NE.predito$upper)
previsao_NE = as.data.frame(previsao_NE[c(1:12),c(1,3,5)])
colnames(previsao_NE) = c("Previsao","LI","LS")
teste_NE = cbind(teste_NE, previsao_NE)
teste_NE$Periodo = seq(1:12)

SES_S.predito = summary(SES_S, h=12)
# SES_S.predito
previsao_S = cbind(SES_S.predito$mean, SES_S.predito$lower, SES_S.predito$upper)
previsao_S = as.data.frame(previsao_S[c(1:12),c(1,3,5)])
colnames(previsao_S) = c("Previsao","LI","LS")
teste_S = cbind(teste_S, previsao_S)
teste_S$Periodo = seq(1:12)

HOLT_SE.predito = summary(HOLT_SE, h=12)
# HOLT_SE.predito
previsao_SE = cbind(HOLT_SE.predito$mean, HOLT_SE.predito$lower, HOLT_SE.predito$upper)
previsao_SE = as.data.frame(previsao_SE[c(1:12),c(1,3,5)])
colnames(previsao_SE) = c("Previsao","LI","LS")
teste_SE = cbind(teste_SE, previsao_SE)
teste_SE$Periodo = seq(1:12)

# 5. VISUALIZAÇÃO
# CO
plot(teste_CO$Periodo, teste_CO$media_ponderada_preco, 
     xlab = "Período de Meses - Região CO", 
     ylab = "Média Ponderada do Preço de Revenda (R$)/L", 
     ylim = c(1.5, 7.0))

lines(teste_CO$Periodo, teste_CO$media_ponderada_preco, col = "black", lwd = 2)
lines(teste_CO$Periodo, teste_CO$Previsao, col = "red", lwd = 2)
lines(teste_CO$Periodo, teste_CO$LI, col = "red", lwd = 2, lty = 'dashed')
lines(teste_CO$Periodo, teste_CO$LS, col = "red", lwd = 2, lty = 'dashed')

# N
plot(teste_N$Periodo, teste_N$media_ponderada_preco, 
     xlab = "Período de Meses - Região N", 
     ylab = "Média Ponderada do Preço de Revenda (R$)/L", 
     ylim = c(1.5, 7.0))

lines(teste_N$Periodo, teste_N$media_ponderada_preco, col = "black", lwd = 2)
lines(teste_N$Periodo, teste_N$Previsao, col = "red", lwd = 2)
lines(teste_N$Periodo, teste_N$LI, col = "red", lwd = 2, lty = 'dashed')
lines(teste_N$Periodo, teste_N$LS, col = "red", lwd = 2, lty = 'dashed')

# NE
plot(teste_NE$Periodo, teste_NE$media_ponderada_preco, 
     xlab = "Período de Meses - Região NE", 
     ylab = "Média Ponderada do Preço de Revenda (R$)/L", 
     ylim = c(1.5, 7.0))

lines(teste_NE$Periodo, teste_NE$media_ponderada_preco, col = "black", lwd = 2)
lines(teste_NE$Periodo, teste_NE$Previsao, col = "red", lwd = 2)
lines(teste_NE$Periodo, teste_NE$LI, col = "red", lwd = 2, lty = 'dashed')
lines(teste_NE$Periodo, teste_NE$LS, col = "red", lwd = 2, lty = 'dashed')

# S
plot(teste_S$Periodo, teste_S$media_ponderada_preco, 
     xlab = "Período de Meses - Região S", 
     ylab = "Média Ponderada do Preço de Revenda (R$)/L", 
     ylim = c(1.5, 7.0))

lines(teste_S$Periodo, teste_S$media_ponderada_preco, col = "black", lwd = 2)
lines(teste_S$Periodo, teste_S$Previsao, col = "red", lwd = 2)
lines(teste_S$Periodo, teste_S$LI, col = "red", lwd = 2, lty = 'dashed')
lines(teste_S$Periodo, teste_S$LS, col = "red", lwd = 2, lty = 'dashed')

# SE
plot(teste_SE$Periodo, teste_SE$media_ponderada_preco, 
     xlab = "Período de Meses - Região SE", 
     ylab = "Média Ponderada do Preço de Revenda (R$)/L", 
     ylim = c(1.5, 7.0))

lines(teste_SE$Periodo, teste_SE$media_ponderada_preco, col = "black", lwd = 2)
lines(teste_SE$Periodo, teste_SE$Previsao, col = "red", lwd = 2)
lines(teste_SE$Periodo, teste_SE$LI, col = "red", lwd = 2, lty = 'dashed')
lines(teste_SE$Periodo, teste_SE$LS, col = "red", lwd = 2, lty = 'dashed')
  
# Carregar pacotes necessários
library(readr)        # Leitura de arquivos 
library(dplyr)        # Manipulação de dados
library(stringr)      # Manipulação de strings
library(ggcorrplot)   # Visualização de correlações
library(tidyverse)    # Conjunto de pacotes para ciência de dados
library(forecast)     # Modelagem de séries temporais 
library(lubridate)    # Manipulação de datas

# 0. OBJETIVO DA ANÁLISE
# O objetivo da análise das séries temporais de cada região é entender como a
# média ponderada do preço de revenda aumentou ao longo do tempo. A análise pode
# ser filtrada por região e por produto. Para esta primeira análise, decidimos
# considerar todas as regiões e utilizar a gasolina comum como produto..

# 1. LEITURA E PREPARAÇÃO DOS DADOS

# Leitura da base de dados (formato TSV)
dataset <- read_tsv("2004-2021.tsv")

# Renomear colunas: remover acentos e substituir espaços por '_'
colnames(dataset) <- colnames(dataset) %>%
  str_replace_all(" ", "_") %>%
  stringi::stri_trans_general("Latin-ASCII")

# Seleção e agregação inicial dos dados
base <- dataset %>%
  select(DATA_INICIAL, REGIAO, PRODUTO, NUMERO_DE_POSTOS_PESQUISADOS, PRECO_MEDIO_REVENDA) %>%
  mutate(DATA_INICIAL = as.Date(DATA_INICIAL, format = "%Y-%m-%d"))

# Cálculo do preço médio ponderado
# Criamos uma base contendo o cálculo da média ponderada do preço médio de
# revenda. A variável PRECO_MEDIO_REVENDA já representa uma média com base no
# NUMERO_DE_POSTOS_PESQUISADOS. Utilizamos esse valor como peso para calcular a
# média ponderada do preço médio de revenda, e também realizamos o agrupamento
# por data, região e produto.
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

# Função para separar treino e teste 
separar_treino_teste <- function(series, n_treino = NULL) {
  if (!is.null(n_treino)) {
    # Caso seja passado n_treino, usamos a lógica genérica
    n_total <- nrow(series)
    treino <- series[1:n_treino, ]
    teste <- series[(n_treino + 1):n_total, ]
  } else {
    # Caso não seja passado n_treino, usamos a separação fixa
    treino <- series[1:180, ]
    teste <- series[181:192, ] # Desconsidera os dados de 2020 e 2021
  }
  list(treino = treino, teste = teste)
}

# Separando conjuntos para cada região
# Estamos desconsiderando os anos de 2020 e 2021. O ano de 2021 foi excluído
# devido à insuficiência de dados, com apenas 4 meses registrados. Já o ano de
# 2020 foi desconsiderado devido ao impacto da pandemia, que aumentou a
# sazonalidade da série temporal.
conjuntos_CO <- separar_treino_teste(baseCO_GC_mensal)
treino_CO <- conjuntos_CO$treino
teste_CO <- conjuntos_CO$teste

conjuntos_N <- separar_treino_teste(baseN_GC_mensal)
treino_N <- conjuntos_N$treino
teste_N <- conjuntos_N$teste

conjuntos_NE <- separar_treino_teste(baseNE_GC_mensal)
treino_NE <- conjuntos_NE$treino
teste_NE <- conjuntos_NE$teste

conjuntos_S <- separar_treino_teste(baseS_GC_mensal)
treino_S <- conjuntos_S$treino
teste_S <- conjuntos_S$teste

conjuntos_SE <- separar_treino_teste(baseSE_GC_mensal)
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

# Suavizações Exponenciais para os modelos SIMPLES, HOLT, HW_ad e HW_mult
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

# HOLT-WINTER MULTIPLICATIVE
HW_mult_CO <- hw(baseCO_ts, seasonal = "multiplicative", h = 15)
HW_mult_N <- hw(baseN_ts, seasonal = "multiplicative", h = 12)
HW_mult_NE <- hw(baseNE_ts, seasonal = "multiplicative", h = 12)
HW_mult_S <- hw(baseS_ts, seasonal = "multiplicative", h = 12)
HW_mult_SE <- hw(baseSE_ts, seasonal = "multiplicative", h = 12)

# COMPARACAO GERAL
list(SES_CO, HOLT_CO, HW_ad_CO, HW_mult_CO) %>% map(accuracy)
list(SES_N, HOLT_N, HW_ad_N, HW_mult_N) %>% map(accuracy)
list(SES_NE, HOLT_NE, HW_ad_NE, HW_mult_NE) %>% map(accuracy)
list(SES_S, HOLT_S, HW_ad_S, HW_mult_S) %>% map(accuracy)
list(SES_SE, HOLT_SE, HW_ad_SE, HW_mult_SE) %>% map(accuracy)

# Resumo Geral
# O RMSE (Root Mean Squared Error) é uma métrica que avalia a diferença entre os
# valores previstos e observados. Para cada região, escolhemos o modelo com o
# menor RMSE, pois isso indica o melhor desempenho preditivo. No entanto, as
# séries que inicialmente não apresentavam muita sazonalidade começam a exibir
# esse padrão a partir de 2019.

# 1. Região CO: Melhor modelo = HOLT_CO, RMSE = 0.06248424
# 2. Região N: Melhor modelo = HOLT_N, RMSE = 0.05794965
# 3. Região NE: Melhor modelo = HOLT_NE, RMSE = 0.05860728
# 4. Região S: Melhor modelo = HOLT_S, RMSE = 0.06218726
# 5. Região SE: Melhor modelo = HOLT_SE, RMSE = 0.05440955

# O modelo que apresentou o melhor desempenho em todas as regiões foi o HOLT
# (Holt's Linear Trend Model), com o menor RMSE na Região SE (RMSE = 0.05440955).
# Portanto, o modelo HOLT é a melhor escolha para as previsões nessas regiões.

# Testando as pipelines
HOLT_CO.predito = summary(HOLT_CO, h=12)
HOLT_CO.predito
previsao_CO = cbind(HOLT_CO.predito$mean, HOLT_CO.predito$lower, HOLT_CO.predito$upper)
previsao_CO = as.data.frame(previsao_CO[c(1:12),c(1,3,5)])
colnames(previsao_CO) = c("Previsao","LI","LS")
teste_CO = cbind(teste_CO, previsao_CO)
teste_CO$Periodo = seq(1:12)

HOLT_N.predito = summary(HOLT_N, h=12)
HOLT_N.predito
previsao_N = cbind(HOLT_N.predito$mean, HOLT_N.predito$lower, HOLT_N.predito$upper)
previsao_N = as.data.frame(previsao_N[c(1:12),c(1,3,5)])
colnames(previsao_N) = c("Previsao","LI","LS")
teste_N = cbind(teste_N, previsao_N)
teste_N$Periodo = seq(1:12)

HOLT_NE.predito = summary(HOLT_NE, h=12)
HOLT_NE.predito
previsao_NE = cbind(HOLT_NE.predito$mean, HOLT_NE.predito$lower, HOLT_NE.predito$upper)
previsao_NE = as.data.frame(previsao_NE[c(1:12),c(1,3,5)])
colnames(previsao_NE) = c("Previsao","LI","LS")
teste_NE = cbind(teste_NE, previsao_NE)
teste_NE$Periodo = seq(1:12)

HOLT_S.predito = summary(HOLT_S, h=12)
HOLT_S.predito
previsao_S = cbind(HOLT_S.predito$mean, HOLT_S.predito$lower, HOLT_S.predito$upper)
previsao_S = as.data.frame(previsao_S[c(1:12),c(1,3,5)])
colnames(previsao_S) = c("Previsao","LI","LS")
teste_S = cbind(teste_S, previsao_S)
teste_S$Periodo = seq(1:12)

HOLT_SE.predito = summary(HOLT_SE, h=12)
HOLT_SE.predito
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

# 6. CONCLUSÃO
# Com base nos gráficos, observa-se que o modelo ajustado escolhido está
# conseguindo capturar de forma precisa a tendência dos dados reais durante o
# período de previsão (maio de 2019 a abril de 2020). Os intervalos de
# confiança, representados pelas linhas vermelhas tracejadas, demonstram
# consistência e largura adequada, indicando boa confiabilidade nas previsões.
# No entanto, é importante notar que, à medida que o período de previsão se
# estende, o intervalo de confiança se amplia, sugerindo que a confiabilidade
# diminui para períodos mais distantes.

# O modelo parece capturar bem a tendência crescente observada nos dados reais.
# Em alguns pontos, há pequenas discrepâncias entre os valores reais e a
# previsão, mas nada muito significativo.

# O intervalo de confiança acompanha a previsão sem ser excessivamente amplo,
# o que reforça a confiabilidade do modelo. Quase todos os dados reais estão
# dentro desse intervalo, o que é um bom indicativo de adequação do modelo.
# Reforço que os anos de 2020 e 2021 foram parcialmente desconsiderados. 

# 7. EXTRA - GRÁFICO UTILIZANDO O MODELO HOLT PARA CADA REGIÃO
# Apenas para visualizar as futuras tendencias

autoplot(HOLT_CO) + 
  autolayer(baseCO_ts, series = "Dados Reais") +
  ggtitle("Holt-Winters Aditivo (Região CO)")

autoplot(HOLT_N) + 
  autolayer(baseN_ts, series = "Dados Reais") +
  ggtitle("Holt-Winters Aditivo (Região N)")

autoplot(HOLT_NE) + 
  autolayer(baseNE_ts, series = "Dados Reais") +
  ggtitle("Holt-Winters Aditivo (Região NE)")

autoplot(HOLT_S) + 
  autolayer(baseS_ts, series = "Dados Reais") +
  ggtitle("Holt-Winters Aditivo (Região S)")

autoplot(HOLT_SE) + 
  autolayer(baseSE_ts, series = "Dados Reais") +
  ggtitle("Holt-Winters Aditivo (Região SE)")


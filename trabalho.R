# Carregar pacotes necessários
library(readr)        # Leitura de arquivos
library(dplyr)        # Manipulação de dados
library(lubridate)    # Manipulação de datas
library(forecast)     # Modelos de séries temporais

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


# Filtrar dados para Centro-Oeste e Etanol Hidratado (exemplo inicial)
baseCO_EH <- filtrar_dados(base_ponderada, "CENTRO OESTE", "ETANOL HIDRATADO")
# BASE <- filtrar_dados(base_ponderada, produto = "GNV")

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
baseCO_EH_mensal <- organizar_series(baseCO_EH, "mensal")

# 4. AJUSTE DE MODELO E PREVISÕES
# Função para separar treino e teste
separar_treino_teste <- function(series, n_treino) {
  n_total <- nrow(series)
  n_teste <- n_total - n_treino
  treino <- series[1:n_treino, ]
  teste <- series[(n_treino + 1):n_total, ]
  list(treino = treino, teste = teste)
}

conjuntos <- separar_treino_teste(baseCO_EH_mensal, 191)
treino <- conjuntos$treino
teste <- conjuntos$teste


base_ts = ts(treino$media_ponderada_preco, frequency=12, start=c(2004,1))
plot(base_ts, type="s")

# SUAVIZACAO EXPONENCIAL SIMPLES
SES = ses(base_ts)

# SUAVIZACAO EXPONENCIAL DE HOLT
HOLT = holt(base_ts)

# SUAVIZACAO EXPONENCIAL DE HOLT-WINTER
HW_ad = hw(base_ts, seasonal = "additive")

# COMPARACAO GERAL
list(SES, HOLT, HW_ad) %>% map(accuracy)

# Testando o pipeline
HWa.predito = summary(HW_ad,h=12)
HWa.predito
previsao = cbind(HWa.predito$mean,HWa.predito$lower,HWa.predito$upper)
previsao = as.data.frame(previsao[c(1:12),c(1,3,5)])
colnames(previsao) = c("Previsao","LI","LS")
teste = cbind(teste, previsao)

teste$Periodo = seq(1:12)

# 5. VISUALIZAÇÃO
plot(teste$Periodo,teste$media_ponderada_preco,xlab="Periodo de Tempo em Meses", ylab="PREÇO MEDIO DE REVENDA",ylim=c(0.0,7.0))
lines(teste$Periodo,teste$media_ponderada_preco, col = "black",lwd=2)
lines(teste$Periodo,teste$Previsao, col = "red",lwd=2)
lines(teste$Periodo,teste$LI, col = "red",lwd=2, lty='dashed')
lines(teste$Periodo,teste$LS, col = "red",lwd=2, lty='dashed')
  
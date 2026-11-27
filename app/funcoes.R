# ------------------------------------------------------------------------------
# funcoes.R
# Pacotes, dados e funções auxiliares usadas pelo app.R
# ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(maps)     # contornos dos países (carregado antes do purrr para não esconder purrr::map)
library(stringr)
library(purrr)
library(scales)

# ---- Dados --------------------------------------------------------------------
# cópias dos arquivos gerados por R/01_importacao_limpeza.R e R/02_analise.R
# (o app precisa ter os dados dentro da própria pasta para funcionar no shinyapps.io)
reparos    <- readRDS("dados/reparos.rds")
resultados <- readRDS("dados/resultados.rds")

anos_completos <- 2017:2025
lista_paises     <- sort(unique(reparos$pais))
lista_categorias <- sort(unique(reparos$categoria))
lista_motivos    <- sort(unique(na.omit(reparos$motivo_falha)))
lista_fontes     <- sort(unique(na.omit(reparos$fonte_info)))

# ---- Números no padrão brasileiro ---------------------------------------------
num <- function(x, dig = 0) formatC(round(x, dig), format = "f", digits = dig,
                                    big.mark = ".", decimal.mark = ",")
pct <- function(x, dig = 1) paste0(num(100 * x, dig), "%")
rotulo_pct <- label_percent(decimal.mark = ",", accuracy = 1)
rotulo_num <- label_number(big.mark = ".", decimal.mark = ",")

# ---- Taxa de sucesso com IC 95% (Wilson) --------------------------------------
# recebe uma tabela agrupada e devolve n, consertados, taxa e IC.
# O IC de Wilson é calculado pela fórmula direto (sem prop.test) porque
# algumas abas calculam milhares de grupos e o prop.test ficava lento.
resumir_taxa <- function(dados) {
  z <- qnorm(0.975)
  dados %>%
    summarise(n = n(), consertados = sum(consertado), .groups = "drop") %>%
    mutate(
      taxa   = consertados / n,
      centro = (taxa + z^2 / (2 * n)) / (1 + z^2 / n),
      margem = z * sqrt(taxa * (1 - taxa) / n + z^2 / (4 * n^2)) / (1 + z^2 / n),
      ic_inf = centro - margem,
      ic_sup = centro + margem
    ) %>%
    select(-centro, -margem)
}

# ---- Tabelas DT em português --------------------------------------------------
tabela <- function(df, altura = "300px") {
  datatable(
    df,
    rownames = FALSE,
    class = "compact hover",
    options = list(
      paging = FALSE,          # mostra tudo com rolagem, sem páginas
      scrollY = altura,
      scrollCollapse = TRUE,
      scrollX = TRUE,
      dom = "t",
      language = list(zeroRecords = "Nenhum registro encontrado")
    )
  )
}

# formata colunas de taxa e IC para exibir nas tabelas
formatar_taxas <- function(df) {
  df %>%
    mutate(
      `Taxa de conserto` = pct(taxa),
      `IC 95%` = paste0(num(100 * ic_inf, 1), "–", pct(ic_sup)),
      n = num(n)
    ) %>%
    select(-any_of(c("taxa", "ic_inf", "ic_sup", "consertados")))
}

# aplica o filtro de países só quando algum foi escolhido
filtrar_paises <- function(dados, paises) {
  if (length(paises) == 0) dados else filter(dados, pais %in% paises)
}
filtrar_categoria <- function(dados, cat) {
  if (is.null(cat) || cat == "Todas") dados else filter(dados, categoria == cat)
}

# descrições curtas dos filtros, usadas nos subtítulos dos gráficos
desc_paises <- function(paises) {
  if (length(paises) == 0) "todos os países"
  else if (length(paises) <= 3) paste(paises, collapse = ", ")
  else paste(length(paises), "países")
}
desc_categoria <- function(cat) if (is.null(cat) || cat == "Todas") "todas as categorias" else cat
desc_anos <- function(anos) paste0(anos[1], "–", anos[2])

# ---- Mapa ---------------------------------------------------------------------
# contornos dos países (pacote maps) e correspondência com as siglas do banco
mapa_mundo <- map_data("world") %>% filter(region != "Antarctica")

nome_no_mapa <- c(
  AT = "Austria", AU = "Australia", BE = "Belgium", CA = "Canada",
  CH = "Switzerland", DE = "Germany", ES = "Spain", FI = "Finland",
  FR = "France", GB = "UK", GF = "French Guiana", IE = "Ireland",
  IL = "Israel", IT = "Italy", KR = "South Korea", LI = "Liechtenstein",
  LU = "Luxembourg", NL = "Netherlands", NZ = "New Zealand", PT = "Portugal",
  SE = "Sweden", TW = "Taiwan", US = "USA", ZA = "South Africa"
)  # Hong Kong fica de fora: é pequeno demais para aparecer no mapa

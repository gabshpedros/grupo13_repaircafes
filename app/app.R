# ------------------------------------------------------------------------------
# app.R - Repair Cafés pelo mundo (Grupo 13, CE302)
# ------------------------------------------------------------------------------

# PACOTES, DADOS E FUNÇÕES AUXILIARES

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


# moldura e textos
cor_moldura    <- "#2a1a12"   # espresso bem escuro: barra de navegação
cor_detalhe    <- "#d9953f"   # caramelo: linha sob a barra e destaques
cor_fundo      <- "#f6efe6"   # leite: fundo da página
cor_texto      <- "#2a211c"   # texto principal
cinza_texto    <- "#7a6a5e"   # textos secundários (eixos, legendas, notas)
cinza_grade    <- "#ece2d6"   # linhas de grade e bordas
cinza_sem_dado <- "#e8dfd3"   # países sem dados no mapa

# dados
cor_escura    <- "#4a2c1d"    # espresso: cor de todos os gráficos de "taxa de conserto"
cor_principal <- "#a65f2c"    # grão de café: controles (sliders, botões) e realces
cor_media     <- "#c99a6b"    # café com leite: segundo ponto em comparações
cor_clara     <- "#ecdcc8"    # espuma: faixas de IC e ligações

cores_resultado <- c("Não consertado" = "#d2a679",   # latte
                     "Parcial"        = "#a0673d",   # cortado
                     "Consertado"     = "#4a2c1d")   # espresso

# cardápio para categorias sem ordem
paleta_series <- c("#a65f2c", "#3d73b5", "#d9953f", "#b34a62", "#7a9a48", "#7a5fa6")
cores_fontes  <- c("Internet (sites e buscas)" = "#3d73b5",   # mirtilo
                   "YouTube"                   = "#b34a62",   # frutas vermelhas
                   "Manual ou fabricante"      = "#d9953f",   # caramelo
                   "iFixit"                    = "#7a9a48",   # pistache
                   "IA generativa"             = "#7a5fa6",   # lavanda
                   "Outra"                     = "#a65f2c")   # grão de café

# mapa: do creme ao espresso (uma cor só, do claro ao escuro)
rampa_mapa <- c("#f1e3cf", "#d9b48c", "#b0805a", "#7a4f33", "#4a2c1d")

# fonte: a mesma do ggplot ("sans"), para texto da página e dos gráficos combinarem
fonte_css <- '"Helvetica Neue", Helvetica, Arial, sans-serif'

# estilo dos gráficos
tema_app <- function(base = 12) {
  theme_minimal(base_size = base, base_family = "sans") +
    theme(
      text             = element_text(color = cor_texto),
      axis.text        = element_text(color = cinza_texto, size = base - 1),
      axis.title       = element_text(color = cinza_texto, size = base - 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = cinza_grade, linewidth = 0.4),
      plot.background  = element_rect(fill = "white", color = NA),
      strip.text       = element_text(face = "bold", color = cor_escura, size = base),
      legend.position  = "top",
      legend.justification = "left",
      legend.title     = element_text(color = cinza_texto, size = base - 1),
      legend.text      = element_text(color = cor_texto, size = base - 1),
      plot.margin      = margin(8, 14, 8, 8)
    )
}
tema_mapa <- function(base = 12) {
  theme_void(base_size = base, base_family = "sans") +
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      legend.position   = "right",
      legend.title      = element_text(color = cinza_texto, size = base - 1),
      legend.text       = element_text(color = cor_texto, size = base - 2),
      legend.key.height = unit(1.3, "cm")
    )
}
update_geom_defaults("text", list(colour = cor_texto, family = "sans", size = 3.6))


tema <- bs_theme(
  version = 5,
  bg = cor_fundo,
  fg = cor_texto,
  primary = cor_principal,
  secondary = cinza_texto,
  base_font = fonte_css,
  heading_font = fonte_css,
  "border-radius" = "0.5rem",
  "card-border-color" = cinza_grade
)

# ajustes de estilo 
estilo <- tags$style(HTML(sprintf("
  body { font-size: 0.93rem; }
  .navbar { border-bottom: 3px solid %6$s; }
  .navbar-brand { font-weight: 700; letter-spacing: 0.3px; }
  .navbar .nav-link { font-size: 0.9rem; color: rgba(255,255,255,0.72) !important; }
  .navbar .nav-link.active, .navbar .nav-link:hover { color: #ffffff !important; }
  .navbar .nav-link.active { font-weight: 600; }

  .bslib-sidebar-layout { border: none; }
  .bslib-sidebar-layout > .sidebar { background: #ffffff; border-right: 1px solid %1$s; }
  .sidebar-title { color: %2$s; font-weight: 700; font-size: 1rem; }
  .form-label, .control-label { color: %3$s; font-weight: 600; font-size: 0.8rem;
                                text-transform: uppercase; letter-spacing: 0.4px; }
  .irs--shiny .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to,
  .irs--shiny .irs-single { background: %4$s; border-color: %4$s; }

  .card { background: #fffdfa; border: 1px solid %1$s; border-radius: 0.5rem;
          box-shadow: 0 1px 3px rgba(31,41,51,0.05); }
  .card-header { background: #ffffff; border-bottom: 1px solid %1$s; padding: 0.75rem 1rem; }
  .titulo-card    { font-weight: 700; font-size: 0.98rem; color: %2$s; }
  .subtitulo-card { font-size: 0.8rem; color: %3$s; margin-top: 2px; }
  .nota-aba       { font-size: 0.82rem; color: %3$s; margin: 0 0 0.75rem 0; }

  .bslib-value-box { border: 1px solid %1$s !important; box-shadow: none; }
  .bslib-value-box .value-box-title { font-size: 0.78rem; text-transform: uppercase;
                                      letter-spacing: 0.5px; opacity: 0.8; }
  .bslib-value-box .value-box-value { font-weight: 700; }

  table.dataTable thead th { color: %2$s; font-weight: 700; font-size: 0.8rem;
                             text-transform: uppercase; letter-spacing: 0.3px;
                             border-bottom: 2px solid %4$s !important; background: #ffffff; }
  table.dataTable tbody td { font-size: 0.86rem; border-top: 1px solid %1$s; }
  table.dataTable thead th, table.dataTable tbody td:not(:first-child) { white-space: nowrap; }
  table.dataTable tbody tr:hover > * { background-color: %5$s !important; }
", cinza_grade, cor_escura, cinza_texto, cor_principal, cor_clara, cor_detalhe)))

# cabeçalho de cartão com título e subtítulo que mudam com os filtros
cabecalho <- function(id) {
  card_header(
    div(class = "titulo-card", textOutput(paste0("tit_", id), inline = TRUE)),
    div(class = "subtitulo-card", textOutput(paste0("sub_", id), inline = TRUE))
  )
}

# layout padrão de cada aba: filtros à esquerda, gráfico e tabela à direita
aba <- function(titulo, id, filtros, nota = NULL) {
  nav_panel(
    titulo,
    layout_sidebar(
      sidebar = do.call(sidebar, c(list(width = 270, title = "Filtros", bg = "#ffffff"), filtros)),
      if (!is.null(nota)) div(class = "nota-aba", nota),
      card(full_screen = TRUE, cabecalho(paste0("g", id)),
           card_body(plotOutput(paste0("graf_", id), height = "440px"))),
      card(full_screen = TRUE, cabecalho(paste0("t", id)),
           card_body(DTOutput(paste0("tab_", id))))
    )
  )
}

sel_paises <- function(id, max = NULL, rotulo = "Países") {
  selectizeInput(id, rotulo, choices = lista_paises, multiple = TRUE,
                 options = list(placeholder = "Todos os países",
                                maxItems = max))
}
sel_categoria <- function(id) {
  selectInput(id, "Categoria", choices = c("Todas", lista_categorias))
}
sel_anos <- function(id, valor = c(2017, 2025)) {
  sliderInput(id, "Período", min = 2015, max = 2026, value = valor,
              step = 1, sep = "")
}

# ==============================================================================
# INTERFACE
# ==============================================================================
ui <- page_navbar(
  title = "Repair Cafés pelo mundo",
  theme = tema,
  bg = cor_moldura,     # barra de navegação espresso com texto branco
  inverse = TRUE,
  fillable = FALSE,
  header = estilo,
  
  # ---- 1. Panorama ------------------------------------------------------------
  nav_panel(
    "Panorama",
    layout_sidebar(
      sidebar = sidebar(
        width = 270, title = "Filtros", bg = "#ffffff",
        sel_paises("pan_paises"),
        sel_anos("pan_anos", c(2015, 2026))
      ),
      div(class = "nota-aba",
          "Consertos registrados em Repair Cafés de 25 países entre 2015 e 2026.",
          "Fonte: Repair Monitor (Repair Café International), via TidyTuesday 2026."),
      layout_columns(
        value_box("Consertos registrados", textOutput("vb_reparos"),
                  theme = value_box_theme(bg = "#ffffff", fg = cor_texto)),
        value_box("Repair Cafés", textOutput("vb_cafes"),
                  theme = value_box_theme(bg = "#ffffff", fg = cor_texto)),
        value_box("Conserto completo", textOutput("vb_taxa"),
                  theme = value_box_theme(bg = cor_escura, fg = "#ffffff"))
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(full_screen = TRUE, height = "440px", cabecalho("gpan"),
             card_body(plotOutput("graf_pan", height = "340px"))),
        card(full_screen = TRUE, height = "440px", cabecalho("tpan"),
             card_body(DTOutput("tab_pan")))
      )
    )
  ),
  
  # ---- 2. Evolução no tempo ---------------------------------------------------
  aba("Evolução", "evo",
      list(
        radioButtons("evo_metrica", "Indicador",
                     c("Consertos registrados" = "n",
                       "Cafés ativos" = "cafes",
                       "Taxa de conserto completo" = "taxa")),
        sel_paises("evo_paises", max = 4, rotulo = "Comparar países (até 4)"),
        sel_anos("evo_anos")
      ),
      "A queda em 2020 e 2021 coincide com a pandemia de COVID-19."),
  
  # ---- 3. Países (mapa) -------------------------------------------------------
  aba("Países", "pai",
      list(
        radioButtons("pai_metrica", "Indicador no mapa",
                     c("Taxa de conserto completo" = "taxa",
                       "Consertos registrados" = "n",
                       "Cafés" = "cafes")),
        radioButtons("pai_regiao", "Região", c("Europa", "Mundo"), inline = TRUE),
        sliderInput("pai_min", "Mínimo de consertos para colorir o país",
                    min = 0, max = 2000, value = 100, step = 50),
        sel_categoria("pai_cat")
      ),
      "Países com poucos registros ficam em cinza, porque a taxa deles é instável."),
  
  # ---- 4. Crescimento dos cafés ----------------------------------------------
  aba("Crescimento", "cre",
      list(
        selectInput("cre_ini", "Ano inicial", anos_completos, selected = 2019),
        selectInput("cre_fim", "Ano final", anos_completos, selected = 2025),
        sliderInput("cre_top", "Quantos países mostrar", 3, 17, 10)
      ),
      "Café ativo = café com pelo menos um conserto registrado no ano."),
  
  # ---- 5. Categorias ----------------------------------------------------------
  aba("Categorias", "cat",
      list(
        sel_paises("cat_paises"),
        sel_anos("cat_anos", c(2015, 2026)),
        radioButtons("cat_modo", "Visualização",
                     c("Resultado completo (3 níveis)" = "todos",
                       "Taxa de conserto com IC 95%" = "taxa"))
      )),
  
  # ---- 6. Produtos e marcas ---------------------------------------------------
  aba("Produtos", "pro",
      list(
        radioButtons("pro_tipo", "Agrupar por",
                     c("Tipo de produto" = "produto", "Marca" = "marca"),
                     inline = TRUE),
        sel_categoria("pro_cat"),
        sliderInput("pro_min", "Mínimo de consertos", 50, 2000, 300, step = 50),
        sliderInput("pro_top", "Quantos mostrar", 5, 25, 12),
        radioButtons("pro_lado", "Ranking", inline = TRUE,
                     c("Maiores taxas" = "maior", "Menores taxas" = "menor"))
      ),
      "Marcas desconhecidas (44% dos registros) não entram no ranking de marcas."),
  
  # ---- 7. Idade do produto ----------------------------------------------------
  aba("Idade", "ida",
      list(
        sel_categoria("ida_cat"),
        radioButtons("ida_largura", "Tamanho das faixas",
                     c("5 anos" = 5, "10 anos" = 10), inline = TRUE),
        sliderInput("ida_max", "Idade máxima (anos)", 10, 80, 50, step = 5),
        radioButtons("ida_tabela", "Tabela",
                     c("Taxa por faixa de idade" = "faixa",
                       "Modelo logístico (razões de chances)" = "modelo"))
      ),
      "Só 36% dos registros informam o ano de fabricação."),
  
  # ---- 8. Reparabilidade ------------------------------------------------------
  aba("Reparabilidade", "rep",
      list(
        sel_categoria("rep_cat"),
        sel_paises("rep_paises"),
        radioButtons("rep_graf", "Visualização",
                     c("Taxa de conserto por nota" = "taxa",
                       "Distribuição da nota por resultado" = "dist"))
      ),
      "A nota (1 = difícil, 10 = fácil) é dada pelo reparador, provavelmente depois da tentativa de conserto."),
  
  # ---- 9. Motivos de falha ----------------------------------------------------
  aba("Falhas", "mot",
      list(
        sel_categoria("mot_cat"),
        sel_anos("mot_anos", c(2015, 2026)),
        checkboxGroupInput("mot_res", "Resultado do conserto",
                           c("Não consertado", "Parcial"),
                           selected = c("Não consertado", "Parcial"))
      ),
      "O motivo só é registrado quando o conserto não foi concluído."),
  
  # ---- 10. Fontes de informação -----------------------------------------------
  aba("Fontes", "fon",
      list(
        checkboxGroupInput("fon_fontes", "Fontes", names(cores_fontes),
                           selected = names(cores_fontes)),
        sel_anos("fon_anos"),
        radioButtons("fon_modo", "Indicador",
                     c("Participação nas consultas" = "prop",
                       "Número de consultas" = "n"))
      ),
      "A fonte consultada só foi registrada em cerca de 3 mil consertos.")
)

# ==============================================================================
# SERVIDOR
# ==============================================================================
server <- function(input, output, session) {
  
  # ---- 1. Panorama ------------------------------------------------------------
  dados_pan <- reactive({
    reparos %>%
      filtrar_paises(input$pan_paises) %>%
      filter(between(ano, input$pan_anos[1], input$pan_anos[2]))
  })
  
  output$vb_reparos <- renderText(num(nrow(dados_pan())))
  output$vb_cafes   <- renderText(num(n_distinct(dados_pan()$cafe_id)))
  output$vb_taxa    <- renderText(pct(mean(dados_pan()$consertado)))
  
  output$tit_gpan <- renderText("Como terminam os consertos")
  output$sub_gpan <- renderText(paste0("Proporção por resultado · ",
                                       desc_paises(input$pan_paises), " · ",
                                       desc_anos(input$pan_anos)))
  output$tit_tpan <- renderText("Consertos por país")
  output$sub_tpan <- renderText("Ordenado pelo número de consertos")
  
  output$graf_pan <- renderPlot(res = 96, {
    req(nrow(dados_pan()) > 0)
    dados_pan() %>%
      count(resultado) %>%
      mutate(prop = n / sum(n)) %>%
      ggplot(aes(x = prop, y = resultado, fill = resultado)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = paste0(pct(prop), "  (", num(n), ")")),
                hjust = -0.08) +
      scale_fill_manual(values = cores_resultado) +
      scale_x_continuous(labels = label_percent(decimal.mark = ","),
                         limits = c(0, 0.85), expand = c(0, 0)) +
      labs(x = NULL, y = NULL) +
      tema_app() +
      theme(panel.grid.major.y = element_blank())
  })
  
  output$tab_pan <- renderDT({
    dados_pan() %>%
      group_by(País = pais) %>%
      summarise(Consertos = n(), Cafés = n_distinct(cafe_id),
                taxa = mean(consertado)) %>%
      arrange(desc(Consertos)) %>%
      mutate(Taxa = pct(taxa), Consertos = num(Consertos)) %>%
      select(-taxa) %>%
      tabela("270px")
  })
  
  # ---- 2. Evolução ------------------------------------------------------------
  nomes_metrica <- c(n = "Consertos registrados", cafes = "Cafés ativos",
                     taxa = "Taxa de conserto completo")
  
  dados_evo <- reactive({
    d <- reparos %>% filter(between(ano, input$evo_anos[1], input$evo_anos[2]))
    # sem países escolhidos mostra o total; com países, uma linha por país
    if (length(input$evo_paises) == 0) {
      d <- d %>% mutate(grupo = "Todos os países")
    } else {
      d <- d %>% filter(pais %in% input$evo_paises) %>%
        mutate(grupo = factor(pais, levels = input$evo_paises))
    }
    d %>%
      group_by(grupo, ano) %>%
      summarise(n = n(), cafes = n_distinct(cafe_id),
                taxa = mean(consertado), .groups = "drop")
  })
  
  output$tit_gevo <- renderText(paste(nomes_metrica[input$evo_metrica], "por ano"))
  output$sub_gevo <- renderText(paste0(desc_paises(input$evo_paises), " · ",
                                       desc_anos(input$evo_anos)))
  output$tit_tevo <- renderText("Valores anuais")
  output$sub_tevo <- renderText(desc_paises(input$evo_paises))
  
  output$graf_evo <- renderPlot(res = 96, {
    d <- dados_evo()
    req(nrow(d) > 0)
    g <- ggplot(d, aes(x = ano, y = .data[[input$evo_metrica]], color = grupo)) +
      geom_line(linewidth = 1) +
      geom_point(size = 3, stroke = 1.2, fill = "white", shape = 21) +
      scale_x_continuous(breaks = seq(2015, 2026, 1)) +
      scale_color_manual(values = paleta_series) +
      labs(x = NULL, y = NULL, color = NULL) +
      tema_app() +
      theme(panel.grid.major.x = element_blank())
    if (input$evo_metrica == "taxa") {
      g <- g + scale_y_continuous(labels = label_percent(decimal.mark = ","))
    } else {
      g <- g + scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                                  limits = c(0, NA))
    }
    if (length(input$evo_paises) == 0) g <- g + theme(legend.position = "none")
    g
  })
  
  output$tab_evo <- renderDT({
    d <- dados_evo() %>%
      transmute(País = grupo, Ano = ano, Consertos = num(n),
                `Cafés ativos` = cafes, `Conserto completo` = pct(taxa))
    if (length(input$evo_paises) == 0) d <- select(d, -País)
    tabela(d)
  })
  
  # ---- 3. Países (mapa) -------------------------------------------------------
  dados_pai <- reactive({
    base <- reparos %>% filtrar_categoria(input$pai_cat)
    base %>%
      group_by(pais_sigla, pais) %>%
      resumir_taxa() %>%
      left_join(base %>% group_by(pais_sigla) %>%
                  summarise(cafes = n_distinct(cafe_id)),
                by = "pais_sigla")
  })
  
  nomes_metrica_mapa <- c(taxa = "Taxa de conserto completo",
                          n = "Consertos registrados", cafes = "Repair Cafés")
  
  output$tit_gpai <- renderText(paste(nomes_metrica_mapa[input$pai_metrica], "por país"))
  output$sub_gpai <- renderText(paste0(desc_categoria(input$pai_cat),
                                       " · cinza = menos de ", num(input$pai_min),
                                       " consertos ou sem dados"))
  output$tit_tpai <- renderText("Ranking dos países")
  output$sub_tpai <- renderText(paste0("Taxa de conserto com IC 95% · ",
                                       desc_categoria(input$pai_cat)))
  
  output$graf_pai <- renderPlot(res = 96, {
    d <- dados_pai() %>%
      filter(n >= input$pai_min) %>%
      mutate(region = unname(nome_no_mapa[pais_sigla]))
    mapa <- mapa_mundo %>% left_join(d, by = "region")
    metrica <- input$pai_metrica
    
    g <- ggplot(mapa, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = .data[[metrica]]), color = "white", linewidth = 0.2)
    
    if (metrica == "taxa") {
      g <- g + scale_fill_gradientn(colors = rampa_mapa, na.value = cinza_sem_dado,
                                    labels = rotulo_pct,
                                    name = "Conserto\ncompleto")
    } else {
      g <- g + scale_fill_gradientn(colors = rampa_mapa, na.value = cinza_sem_dado,
                                    trans = "log10",
                                    labels = label_number(big.mark = ".", decimal.mark = ","),
                                    name = nomes_metrica_mapa[metrica])
    }
    
    limites <- if (input$pai_regiao == "Europa") {
      coord_quickmap(xlim = c(-11, 30), ylim = c(35, 62))
    } else {
      coord_quickmap(ylim = c(-50, 80))
    }
    g + limites + tema_mapa()
  })
  
  output$tab_pai <- renderDT({
    dados_pai() %>%
      filter(n >= input$pai_min) %>%
      arrange(desc(taxa)) %>%
      transmute(País = pais, Consertos = num(n), Cafés = cafes,
                `Conserto completo` = pct(taxa),
                `IC 95%` = paste0(num(100 * ic_inf, 1), "–", pct(ic_sup))) %>%
      tabela()
  })
  
  # ---- 4. Crescimento ---------------------------------------------------------
  dados_cre <- reactive({
    ini <- as.numeric(input$cre_ini)
    fim <- as.numeric(input$cre_fim)
    validate(need(fim > ini, "O ano final precisa ser maior que o inicial."))
    reparos %>%
      filter(ano %in% c(ini, fim)) %>%
      group_by(pais) %>%
      summarise(inicio = n_distinct(cafe_id[ano == ini]),
                fim = n_distinct(cafe_id[ano == fim])) %>%
      mutate(crescimento = fim - inicio) %>%
      arrange(desc(crescimento)) %>%
      slice_head(n = input$cre_top)
  })
  
  output$tit_gcre <- renderText(paste("Cafés ativos em", input$cre_ini, "e em", input$cre_fim))
  output$sub_gcre <- renderText(paste("Os", input$cre_top, "países que mais ganharam cafés ativos"))
  output$tit_tcre <- renderText("Crescimento de cafés ativos por país")
  output$sub_tcre <- renderText(paste(input$cre_ini, "→", input$cre_fim))
  
  output$graf_cre <- renderPlot(res = 96, {
    d <- dados_cre() %>% mutate(pais = fct_reorder(pais, fim))
    ggplot(d, aes(y = pais)) +
      geom_segment(aes(x = inicio, xend = fim, yend = pais),
                   color = cor_clara, linewidth = 2.5) +
      geom_point(aes(x = inicio, color = input$cre_ini), size = 4) +
      geom_point(aes(x = fim, color = input$cre_fim), size = 4) +
      geom_text(aes(x = fim, label = paste0("+", crescimento)),
                hjust = -0.6, color = cinza_texto) +
      scale_color_manual(values = c(cor_media, cor_escura),
                         breaks = c(input$cre_ini, input$cre_fim)) +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.1))) +
      labs(x = "Cafés ativos", y = NULL, color = NULL) +
      tema_app() +
      theme(panel.grid.major.y = element_blank())
  })
  
  output$tab_cre <- renderDT({
    dados_cre() %>%
      rename(País = pais,
             !!paste("Cafés em", input$cre_ini) := inicio,
             !!paste("Cafés em", input$cre_fim) := fim,
             Crescimento = crescimento) %>%
      tabela()
  })
  
  # ---- 5. Categorias ----------------------------------------------------------
  dados_cat <- reactive({
    reparos %>%
      filtrar_paises(input$cat_paises) %>%
      filter(between(ano, input$cat_anos[1], input$cat_anos[2]))
  })
  
  output$tit_gcat <- renderText(
    if (input$cat_modo == "todos") "Resultado dos consertos por categoria"
    else "Taxa de conserto completo por categoria"
  )
  output$sub_gcat <- renderText(paste0(desc_paises(input$cat_paises), " · ",
                                       desc_anos(input$cat_anos),
                                       " · ordenado pela taxa de conserto"))
  output$tit_tcat <- renderText("Taxa de conserto por categoria")
  output$sub_tcat <- renderText("Com intervalo de confiança de 95% (Wilson)")
  
  output$graf_cat <- renderPlot(res = 96, {
    d <- dados_cat()
    req(nrow(d) > 0)
    if (input$cat_modo == "todos") {
      ordem <- d %>% group_by(categoria) %>% summarise(t = mean(consertado))
      d %>%
        count(categoria, resultado) %>%
        group_by(categoria) %>%
        mutate(prop = n / sum(n)) %>%
        left_join(ordem, by = "categoria") %>%
        ggplot(aes(x = prop, y = fct_reorder(categoria, t), fill = resultado)) +
        geom_col(width = 0.72, color = "white", linewidth = 0.6) +
        scale_fill_manual(values = cores_resultado) +
        guides(fill = guide_legend(reverse = TRUE)) +   # legenda na mesma ordem das barras
        scale_x_continuous(labels = label_percent(decimal.mark = ","), expand = c(0, 0)) +
        labs(x = NULL, y = NULL, fill = NULL) +
        tema_app() +
        theme(panel.grid.major.y = element_blank())
    } else {
      d %>%
        group_by(categoria) %>%
        resumir_taxa() %>%
        ggplot(aes(x = taxa, y = fct_reorder(categoria, taxa))) +
        geom_col(width = 0.65, fill = cor_escura) +
        geom_errorbar(aes(xmin = ic_inf, xmax = ic_sup), width = 0.3,
                      color = cor_detalhe) +
        geom_text(aes(label = pct(taxa)), hjust = -0.35) +
        scale_x_continuous(labels = label_percent(decimal.mark = ","),
                           limits = c(0, 1.05), expand = c(0, 0)) +
        labs(x = NULL, y = NULL) +
        tema_app() +
        theme(panel.grid.major.y = element_blank())
    }
  })
  
  output$tab_cat <- renderDT({
    dados_cat() %>%
      group_by(Categoria = categoria) %>%
      resumir_taxa() %>%
      arrange(desc(taxa)) %>%
      formatar_taxas() %>%
      rename(Consertos = n) %>%
      tabela()
  })
  
  # ---- 6. Produtos e marcas ---------------------------------------------------
  dados_pro <- reactive({
    reparos %>%
      filtrar_categoria(input$pro_cat) %>%
      filter(!is.na(.data[[input$pro_tipo]])) %>%
      group_by(item = .data[[input$pro_tipo]]) %>%
      resumir_taxa() %>%
      filter(n >= input$pro_min)
  })
  
  nome_tipo <- reactive(if (input$pro_tipo == "produto") "produtos" else "marcas")
  
  output$tit_gpro <- renderText(paste0(
    str_to_sentence(nome_tipo()), " com ",
    if (input$pro_lado == "maior") "maior" else "menor", " taxa de conserto"))
  output$sub_gpro <- renderText(paste0(desc_categoria(input$pro_cat),
                                       " · mínimo de ", num(input$pro_min),
                                       " consertos · barras = IC 95%"))
  output$tit_tpro <- renderText(paste("Todos os", nome_tipo(), "do filtro"))
  output$sub_tpro <- renderText(paste("Com pelo menos", num(input$pro_min),
                                      "consertos · ordenado por volume"))
  
  output$graf_pro <- renderPlot(res = 96, {
    d <- dados_pro()
    validate(need(nrow(d) > 0, "Nenhum item com esse número mínimo de consertos."))
    d <- if (input$pro_lado == "maior") slice_max(d, taxa, n = input$pro_top, with_ties = FALSE)
    else slice_min(d, taxa, n = input$pro_top, with_ties = FALSE)
    ggplot(d, aes(x = taxa, y = fct_reorder(item, taxa))) +
      geom_col(width = 0.65, fill = cor_escura) +
      geom_errorbar(aes(xmin = ic_inf, xmax = ic_sup), width = 0.3, color = cor_detalhe) +
      geom_text(aes(label = pct(taxa)), hjust = -0.35) +
      scale_x_continuous(labels = label_percent(decimal.mark = ","),
                         limits = c(0, 1.08), expand = c(0, 0)) +
      labs(x = NULL, y = NULL) +
      tema_app() +
      theme(panel.grid.major.y = element_blank())
  })
  
  output$tab_pro <- renderDT({
    nome <- if (input$pro_tipo == "produto") "Produto" else "Marca"
    dados_pro() %>%
      arrange(desc(n)) %>%
      rename(!!nome := item) %>%
      formatar_taxas() %>%
      rename(Consertos = n) %>%
      tabela()
  })
  
  # ---- 7. Idade do produto ----------------------------------------------------
  dados_ida <- reactive({
    largura <- as.numeric(input$ida_largura)
    reparos %>%
      filtrar_categoria(input$ida_cat) %>%
      filter(!is.na(idade_produto), idade_produto <= input$ida_max) %>%
      mutate(faixa_ini = floor(idade_produto / largura) * largura,
             faixa = paste0(faixa_ini, " a ", faixa_ini + largura - 1, " anos")) %>%
      group_by(faixa_ini, faixa) %>%
      resumir_taxa() %>%
      filter(n >= 30)   # faixas com muito poucos casos atrapalham a leitura
  })
  
  output$tit_gida <- renderText("Taxa de conserto completo por idade do produto")
  output$sub_gida <- renderText(paste0(desc_categoria(input$ida_cat),
                                       " · faixas de ", input$ida_largura,
                                       " anos · área sombreada = IC 95%"))
  output$tit_tida <- renderText(
    if (input$ida_tabela == "modelo") "Modelo logístico: chance de conserto completo"
    else "Taxa de conserto por faixa de idade"
  )
  output$sub_tida <- renderText(
    if (input$ida_tabela == "modelo")
      "Razões de chances · referências: eletrodomésticos elétricos e produtos com até 5 anos"
    else desc_categoria(input$ida_cat)
  )
  
  output$graf_ida <- renderPlot(res = 96, {
    d <- dados_ida()
    validate(need(nrow(d) > 0, "Sem dados suficientes para essa seleção."))
    ggplot(d, aes(x = faixa_ini, y = taxa)) +
      geom_ribbon(aes(ymin = ic_inf, ymax = ic_sup), fill = cor_clara, alpha = 0.7) +
      geom_line(color = cor_escura, linewidth = 1) +
      geom_point(aes(size = n), color = cor_escura, fill = "white", shape = 21, stroke = 1.3) +
      scale_y_continuous(labels = label_percent(decimal.mark = ",")) +
      scale_x_continuous(breaks = seq(0, 80, 10)) +
      scale_size_continuous(range = c(2.5, 6),
                            labels = label_number(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Idade do produto no momento do conserto (anos)", y = NULL,
           size = "Consertos na faixa") +
      tema_app()
  })
  
  output$tab_ida <- renderDT({
    if (input$ida_tabela == "modelo") {
      # resultado do modelo logístico ajustado em R/02_analise.R
      resultados$tabela_modelo %>%
        transmute(Termo = termo,
                  `Razão de chances` = num(razao_chances, 2),
                  `IC 95%` = paste0(num(ic_inf, 2), "–", num(ic_sup, 2)),
                  `p-valor` = if_else(p_valor < 0.001, "< 0,001", num(p_valor, 3))) %>%
        tabela()
    } else {
      dados_ida() %>%
        ungroup() %>%
        select(-faixa_ini) %>%
        rename(Idade = faixa) %>%
        formatar_taxas() %>%
        rename(Consertos = n) %>%
        tabela()
    }
  })
  
  # ---- 8. Reparabilidade ------------------------------------------------------
  dados_rep <- reactive({
    reparos %>%
      filtrar_categoria(input$rep_cat) %>%
      filtrar_paises(input$rep_paises) %>%
      filter(!is.na(reparabilidade))
  })
  
  output$tit_grep <- renderText(
    if (input$rep_graf == "taxa") "Taxa de conserto completo por nota de reparabilidade"
    else "Nota de reparabilidade segundo o resultado"
  )
  output$sub_grep <- renderText(paste0(desc_categoria(input$rep_cat), " · ",
                                       desc_paises(input$rep_paises),
                                       " · 1 = difícil, 10 = fácil"))
  output$tit_trep <- renderText("Resumo da nota por resultado")
  output$sub_trep <- renderText(paste0(desc_categoria(input$rep_cat), " · ",
                                       desc_paises(input$rep_paises)))
  
  output$graf_rep <- renderPlot(res = 96, {
    d <- dados_rep()
    validate(need(nrow(d) > 0, "Sem notas de reparabilidade para essa seleção."))
    if (input$rep_graf == "taxa") {
      d %>%
        group_by(reparabilidade) %>%
        resumir_taxa() %>%
        ggplot(aes(x = reparabilidade, y = taxa)) +
        geom_col(width = 0.7, fill = cor_escura) +
        geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.25, color = cor_detalhe) +
        geom_text(aes(label = pct(taxa, 0)), vjust = -1.2) +
        scale_x_continuous(breaks = 1:10) +
        scale_y_continuous(labels = label_percent(decimal.mark = ","),
                           limits = c(0, 1.05), expand = c(0, 0)) +
        labs(x = "Nota de reparabilidade", y = NULL) +
        tema_app() +
        theme(panel.grid.major.x = element_blank())
    } else {
      ggplot(d, aes(x = resultado, y = reparabilidade, fill = resultado)) +
        geom_violin(alpha = 0.35, color = NA, show.legend = FALSE) +
        geom_boxplot(width = 0.14, fill = "white", outlier.shape = NA,
                     aes(color = resultado), show.legend = FALSE, linewidth = 0.8) +
        scale_fill_manual(values = cores_resultado) +
        scale_color_manual(values = cores_resultado) +
        scale_y_continuous(breaks = 1:10) +
        labs(x = NULL, y = "Nota de reparabilidade") +
        tema_app() +
        theme(panel.grid.major.x = element_blank())
    }
  })
  
  output$tab_rep <- renderDT({
    dados_rep() %>%
      group_by(Resultado = resultado) %>%
      summarise(Consertos = num(n()),
                Média = num(mean(reparabilidade), 2),
                Mediana = median(reparabilidade),
                `Desvio padrão` = num(sd(reparabilidade), 2)) %>%
      tabela()
  })
  
  # ---- 9. Motivos de falha ----------------------------------------------------
  dados_mot <- reactive({
    validate(need(length(input$mot_res) > 0, "Escolha pelo menos um resultado."))
    reparos %>%
      filtrar_categoria(input$mot_cat) %>%
      filter(between(ano, input$mot_anos[1], input$mot_anos[2]),
             resultado %in% input$mot_res,
             !is.na(motivo_falha)) %>%
      count(motivo_falha, sort = TRUE) %>%
      mutate(prop = n / sum(n))
  })
  
  output$tit_gmot <- renderText("Por que os consertos não são concluídos")
  output$sub_gmot <- renderText(paste0("% das falhas com motivo registrado · ",
                                       desc_categoria(input$mot_cat), " · ",
                                       desc_anos(input$mot_anos)))
  output$tit_tmot <- renderText("Motivos de falha")
  output$sub_tmot <- renderText(paste(num(sum(dados_mot()$n)), "falhas com motivo registrado"))
  
  output$graf_mot <- renderPlot(res = 96, {
    d <- dados_mot()
    validate(need(nrow(d) > 0, "Nenhum motivo registrado para essa seleção."))
    ggplot(d, aes(x = prop, y = fct_reorder(motivo_falha, prop))) +
      geom_col(width = 0.65, fill = cores_resultado[["Não consertado"]]) +
      geom_text(aes(label = pct(prop)), hjust = -0.15) +
      scale_x_continuous(labels = label_percent(decimal.mark = ","),
                         expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = NULL) +
      tema_app() +
      theme(panel.grid.major.y = element_blank())
  })
  
  output$tab_mot <- renderDT({
    dados_mot() %>%
      transmute(Motivo = motivo_falha, Registros = num(n), Proporção = pct(prop)) %>%
      tabela()
  })
  
  # ---- 10. Fontes de informação -----------------------------------------------
  dados_fon <- reactive({
    validate(need(length(input$fon_fontes) > 0, "Escolha pelo menos uma fonte."))
    reparos %>%
      filter(!is.na(fonte_info),
             between(ano, input$fon_anos[1], input$fon_anos[2]))
  })
  
  output$tit_gfon <- renderText(
    if (input$fon_modo == "prop") "Participação de cada fonte nas consultas"
    else "Número de consultas por fonte"
  )
  output$sub_gfon <- renderText(paste0("Consertos em que o reparador registrou a fonte · ",
                                       desc_anos(input$fon_anos)))
  output$tit_tfon <- renderText("Taxa de conserto segundo a fonte consultada")
  output$sub_tfon <- renderText(paste0(desc_anos(input$fon_anos), " · IC 95%"))
  
  output$graf_fon <- renderPlot(res = 96, {
    d <- dados_fon() %>%
      count(ano, fonte_info) %>%
      complete(ano, fonte_info, fill = list(n = 0)) %>%   # ano sem uso = 0
      group_by(ano) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup() %>%
      filter(fonte_info %in% input$fon_fontes)
    g <- ggplot(d, aes(x = ano, y = .data[[input$fon_modo]], color = fonte_info)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.8, fill = "white", shape = 21, stroke = 1.2) +
      scale_color_manual(values = cores_fontes) +
      scale_x_continuous(breaks = seq(2015, 2026, 1)) +
      labs(x = NULL, y = NULL, color = NULL) +
      tema_app() +
      theme(panel.grid.major.x = element_blank())
    if (input$fon_modo == "prop") g + scale_y_continuous(labels = label_percent(decimal.mark = ","))
    else g
  })
  
  output$tab_fon <- renderDT({
    dados_fon() %>%
      filter(fonte_info %in% input$fon_fontes) %>%
      group_by(Fonte = fonte_info) %>%
      resumir_taxa() %>%
      arrange(desc(n)) %>%
      formatar_taxas() %>%
      rename(Consultas = n) %>%
      tabela()
  })
}

shinyApp(ui, server)
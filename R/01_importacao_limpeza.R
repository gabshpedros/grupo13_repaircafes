# ------------------------------------------------------------------------------
# 01_importacao_limpeza.R
# Importa os dados do TidyTuesday (Repair Cafes, 2026-04-07), faz a limpeza e salva a base tratada usada no relatório e no app.
# ------------------------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)

url_base <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-07/"
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)

for (arq in c("repairs.csv", "repairs_text.csv")) {
  destino <- file.path("data/raw", arq)
  if (!file.exists(destino)) download.file(paste0(url_base, arq), destino, mode = "wb")
}

repairs      <- read_csv("data/raw/repairs.csv", show_col_types = FALSE)
repairs_text <- read_csv("data/raw/repairs_text.csv", show_col_types = FALSE)

n_original <- nrow(repairs)

# ---- 1. IDs repetidos ---------------------------------------------------------
# Alguns repair_id aparecem em 2 linhas que são consertos diferentes (produto e data diferentes). Como não dá pra saber qual linha do texto corresponde a qual, esses casos saem das duas tabelas.
ids_repetidos <- repairs %>%
  count(repair_id) %>%
  filter(n > 1) %>%
  pull(repair_id)

n_linhas_repetidas <- sum(repairs$repair_id %in% ids_repetidos)

repairs      <- repairs %>% filter(!repair_id %in% ids_repetidos)
repairs_text <- repairs_text %>% filter(!repair_id %in% ids_repetidos)

# ---- 2. Tabelas de tradução ---------------------------------------------------
paises <- c(
  AT = "Áustria", AU = "Austrália", BE = "Bélgica", CA = "Canadá",
  CH = "Suíça", DE = "Alemanha", ES = "Espanha", FI = "Finlândia",
  FR = "França", GB = "Reino Unido", GF = "Guiana Francesa", HK = "Hong Kong",
  IE = "Irlanda", IL = "Israel", IT = "Itália", KR = "Coreia do Sul",
  LI = "Liechtenstein", LU = "Luxemburgo", NL = "Países Baixos",
  NZ = "Nova Zelândia", PT = "Portugal", SE = "Suécia", TW = "Taiwan",
  US = "Estados Unidos", ZA = "África do Sul"
)

categorias <- c(
  "Household appliances electric"     = "Eletrodomésticos elétricos",
  "Household appliances non-electric" = "Utensílios domésticos não elétricos",
  "Textile"                           = "Têxteis",
  "Display and sound equipment"       = "Imagem e som",
  "Computer equipment / phones"       = "Informática e celulares",
  "Tools electric"                    = "Ferramentas elétricas",
  "Tools non-electric"                = "Ferramentas não elétricas",
  "Clocks / alarm clocks"             = "Relógios",
  "Bicycles"                          = "Bicicletas",
  "Jewelry"                           = "Joias e bijuterias",
  "Toys electric"                     = "Brinquedos elétricos",
  "Toys non-electric"                 = "Brinquedos não elétricos",
  "Furniture"                         = "Móveis",
  "Other"                             = "Outros"
)

motivos <- c(
  "No way to fix the product"                   = "Sem solução técnica",
  "Spare parts not available at repair session" = "Peça indisponível no evento",
  "Spare parts not available on the market"     = "Peça indisponível no mercado",
  "Spare parts too expensive"                   = "Peça cara demais",
  "Unidentified failure"                        = "Defeito não identificado",
  "Too worn out"                                = "Desgaste excessivo",
  "No way to open the product"                  = "Produto não abre",
  "Too time consuming"                          = "Conserto demorado demais",
  "Lack of equipment at repair location"        = "Falta de equipamento",
  "Repair information not available"            = "Falta de informação técnica"
)

# ---- 3. Limpeza da tabela principal -------------------------------------------
data_publicacao <- as.Date("2026-04-07")

repairs_limpo <- repairs %>%
  mutate(
    # resultado: padroniza e corrige o "ja" (sim, em holandês)
    resultado = str_to_lower(str_trim(repaired)),
    resultado = if_else(resultado == "ja", "yes", resultado),
    resultado = factor(resultado, levels = c("no", "half", "yes"),
                       labels = c("Não consertado", "Parcial", "Consertado")),
    consertado = resultado == "Consertado",

    # datas
    ano = year(repair_date),
    mes = month(repair_date),

    # ano de fabricação: fora de 1900 até o ano do conserto vira NA
    ano_fabricacao = if_else(
      between(estimated_year_of_production, 1900, ano),
      estimated_year_of_production, NA_real_
    ),
    idade_produto = ano - ano_fabricacao,

    # reparabilidade: a escala vai de 1 a 10, então 0 é inválido
    reparabilidade = if_else(repairability %in% 1:10, repairability, NA_real_),

    # nomes em português
    pais      = unname(paises[country]),
    categoria = unname(categorias[category]),
    categoria = replace_na(categoria, "Outros"),
    produto   = str_to_sentence(str_trim(kind_of_product)),
    marca     = str_to_upper(str_trim(brand)),  # "Lg", "lg" e "LG" viram a mesma marca
    marca     = na_if(marca, "UNKNOWN/N.A.")
  ) %>%
  filter(
    !is.na(resultado),                  # 3 casos sem resultado válido
    repair_date <= data_publicacao      # datas depois da publicação do banco
  ) %>%
  select(repair_id, data = repair_date, ano, mes, cafe = repair_cafe_name,
         cafe_id = repair_cafe_number, pais_sigla = country, pais,
         categoria, produto, marca, ano_fabricacao, idade_produto,
         resultado, consertado, reparabilidade)

# ---- 4. Limpeza dos textos ----------------------------------------------------
textos_limpo <- repairs_text %>%
  mutate(
    motivo_falha = unname(motivos[failure_reasons]),
    consultou_info = case_when(
      used_repair_info == "yes" ~ "Sim",
      used_repair_info == "no"  ~ "Não",
      TRUE ~ NA_character_      # "unknown" e vazio contam como sem informação
    ),
    # a fonte é texto livre (em várias línguas), então agrupamos por palavras-chave
    fonte_txt = str_to_lower(repair_info_source),
    fonte_info = case_when(
      is.na(fonte_txt) | fonte_txt %in% c("nan", "_x000d_\n") ~ NA_character_,
      str_detect(fonte_txt, "chat ?gpt|chap gpt|\\bgpt|\\bai\\b|copilot|gemini|perplexity|claude|\\bki\\b") ~ "IA generativa",
      str_detect(fonte_txt, "you ?tube|\\byt\\b") ~ "YouTube",
      str_detect(fonte_txt, "ifixit") ~ "iFixit",
      str_detect(fonte_txt, "manual|handleiding|fabrikant|gebruiksaanwijzing|notice|datasheet|schema") ~ "Manual ou fabricante",
      str_detect(fonte_txt, "google|internet|website|site|web|online|forum|\\.com|\\.nl") ~ "Internet (sites e buscas)",
      TRUE ~ "Outra"
    )
  ) %>%
  select(repair_id, defeito = defect_found, motivo_falha,
         consultou_info, fonte_info)

# ---- 5. Junta as duas tabelas ---------------------------------------------------
reparos <- repairs_limpo %>%
  left_join(textos_limpo, by = "repair_id", relationship = "one-to-one")

# ---- 6. Salva -----------------------------------------------------------------
saveRDS(reparos, "data/reparos.rds")

# resumo da limpeza (usado no texto de Materiais e Métodos)
resumo_limpeza <- list(
  n_original       = n_original,
  n_ids_repetidos  = n_linhas_repetidas,
  n_final          = nrow(reparos),
  n_paises         = n_distinct(reparos$pais),
  n_cafes          = n_distinct(reparos$cafe_id),
  n_categorias     = n_distinct(reparos$categoria)
)
saveRDS(resumo_limpeza, "data/resumo_limpeza.rds")

glimpse(reparos)

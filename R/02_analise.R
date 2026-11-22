# ------------------------------------------------------------------------------
# 02_analise.R
# Análise estatística da base tratada (data/reparos.rds).
# Gera as tabelas e testes usados no relatório e no app e salva tudo em
# data/resultados.rds.
# ------------------------------------------------------------------------------

library(tidyverse)

reparos <- readRDS("data/reparos.rds")

# Para análises no tempo usamos só anos completos e com volume razoável:
# 2015-2016 têm poucos registros e 2026 só vai até abril.
anos_completos <- 2017:2025

# Função auxiliar: proporção de sucesso com intervalo de confiança de 95%
# (prop.test usa o intervalo de Wilson com correção de continuidade)
taxa_ic <- function(sucessos, total) {
  teste <- prop.test(sucessos, total)
  tibble(taxa = sucessos / total,
         ic_inf = teste$conf.int[1],
         ic_sup = teste$conf.int[2])
}

# ---- 1. Visão geral -----------------------------------------------------------
visao_geral <- reparos %>%
  summarise(
    n_reparos  = n(),
    n_cafes    = n_distinct(cafe_id),
    n_paises   = n_distinct(pais),
    n_produtos = n_distinct(produto),
    data_ini   = min(data),
    data_fim   = max(data)
  )

dist_resultado <- reparos %>%
  count(resultado) %>%
  mutate(prop = n / sum(n))

taxa_geral <- taxa_ic(sum(reparos$consertado), nrow(reparos))

# ---- 2. Evolução no tempo -----------------------------------------------------
por_ano <- reparos %>%
  filter(ano %in% anos_completos) %>%
  group_by(ano) %>%
  summarise(
    n_reparos  = n(),
    n_cafes    = n_distinct(cafe_id),
    n_paises   = n_distinct(pais),
    consertados = sum(consertado)
  ) %>%
  mutate(taxa = consertados / n_reparos)

# teste de tendência na proporção de sucesso ao longo dos anos
teste_tendencia <- prop.trend.test(por_ano$consertados, por_ano$n_reparos)

# crescimento de cafés ativos por país (2019 = antes da pandemia x 2025)
crescimento_paises <- reparos %>%
  filter(ano %in% c(2019, 2025)) %>%
  group_by(pais, ano) %>%
  summarise(cafes_ativos = n_distinct(cafe_id), .groups = "drop") %>%
  pivot_wider(names_from = ano, values_from = cafes_ativos,
              names_prefix = "cafes_", values_fill = 0) %>%
  mutate(crescimento = cafes_2025 - cafes_2019) %>%
  arrange(desc(crescimento))

# ---- 3. Sucesso por categoria -------------------------------------------------
por_categoria <- reparos %>%
  group_by(categoria) %>%
  summarise(n = n(), consertados = sum(consertado)) %>%
  mutate(map2_dfr(consertados, n, taxa_ic)) %>%
  arrange(desc(taxa))

# associação entre categoria e resultado (3 níveis)
tab_cat_res  <- table(reparos$categoria, reparos$resultado)
teste_qui_cat <- chisq.test(tab_cat_res)

# V de Cramér: tamanho do efeito do qui-quadrado (0 = nenhuma, 1 = total)
v_cramer <- function(teste) {
  tab <- teste$observed
  sqrt(teste$statistic / (sum(tab) * (min(dim(tab)) - 1))) %>% unname()
}
v_cat <- v_cramer(teste_qui_cat)

# ---- 4. Sucesso por país ------------------------------------------------------
# só países com pelo menos 1000 reparos, para as taxas serem estáveis
por_pais <- reparos %>%
  group_by(pais) %>%
  summarise(n = n(), consertados = sum(consertado)) %>%
  filter(n >= 1000) %>%
  mutate(map2_dfr(consertados, n, taxa_ic)) %>%
  arrange(desc(taxa))

reparos_paises_grandes <- reparos %>% filter(pais %in% por_pais$pais)
teste_qui_pais <- chisq.test(table(reparos_paises_grandes$pais,
                                   reparos_paises_grandes$consertado))
v_pais <- v_cramer(teste_qui_pais)

# ---- 5. Reparabilidade --------------------------------------------------------
# nota dada pelo reparador, de 1 (difícil) a 10 (fácil)
reparab_resultado <- reparos %>%
  filter(!is.na(reparabilidade)) %>%
  group_by(resultado) %>%
  summarise(n = n(),
            media   = mean(reparabilidade),
            mediana = median(reparabilidade),
            dp      = sd(reparabilidade))

teste_kw_reparab <- kruskal.test(reparabilidade ~ resultado, data = reparos)

taxa_por_nota <- reparos %>%
  filter(!is.na(reparabilidade)) %>%
  group_by(reparabilidade) %>%
  summarise(n = n(), taxa = mean(consertado))

# ---- 6. Idade do produto e modelo logístico -----------------------------------
reparos_modelo <- reparos %>%
  filter(!is.na(idade_produto)) %>%
  mutate(
    faixa_idade = cut(idade_produto,
                      breaks = c(-Inf, 5, 10, 20, Inf),
                      labels = c("Até 5 anos", "6 a 10 anos",
                                 "11 a 20 anos", "Mais de 20 anos")),
    # categoria de referência: a mais frequente (eletrodomésticos elétricos)
    categoria = fct_relevel(factor(categoria), "Eletrodomésticos elétricos")
  )

por_idade <- reparos_modelo %>%
  group_by(faixa_idade) %>%
  summarise(n = n(), consertados = sum(consertado)) %>%
  mutate(map2_dfr(consertados, n, taxa_ic))

# regressão logística: chance de conserto completo segundo categoria e idade
modelo <- glm(consertado ~ categoria + faixa_idade,
              data = reparos_modelo, family = binomial)

# razões de chance (odds ratios) com IC de 95% (Wald)
tabela_modelo <- broom::tidy(modelo, conf.int = TRUE, conf.method = "Wald",
                             exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = str_remove(term, "^categoria|^faixa_idade")) %>%
  select(termo = term, razao_chances = estimate,
         ic_inf = conf.low, ic_sup = conf.high, p_valor = p.value)

# ---- 7. Motivos de falha ------------------------------------------------------
motivos <- reparos %>%
  filter(!is.na(motivo_falha)) %>%
  count(motivo_falha, sort = TRUE) %>%
  mutate(prop = n / sum(n))

motivo_por_categoria <- reparos %>%
  filter(!is.na(motivo_falha)) %>%
  count(categoria, motivo_falha) %>%
  group_by(categoria) %>%
  mutate(prop = n / sum(n)) %>%
  slice_max(prop, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(prop))

# ---- 8. Fontes de informação --------------------------------------------------
consulta_sucesso <- reparos %>%
  filter(!is.na(consultou_info)) %>%
  group_by(consultou_info) %>%
  summarise(n = n(), consertados = sum(consertado)) %>%
  mutate(map2_dfr(consertados, n, taxa_ic))

teste_qui_consulta <- chisq.test(table(reparos$consultou_info, reparos$consertado))

fontes_por_ano <- reparos %>%
  filter(!is.na(fonte_info), ano %in% anos_completos) %>%
  count(ano, fonte_info) %>%
  group_by(ano) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# ---- 9. Salva tudo ------------------------------------------------------------
# (o objeto do glm não é salvo inteiro porque guarda a base toda e fica pesado)
resultados <- list(
  visao_geral = visao_geral,
  dist_resultado = dist_resultado,
  taxa_geral = taxa_geral,
  por_ano = por_ano,
  teste_tendencia = teste_tendencia,
  crescimento_paises = crescimento_paises,
  por_categoria = por_categoria,
  teste_qui_cat = teste_qui_cat,
  v_cat = v_cat,
  por_pais = por_pais,
  teste_qui_pais = teste_qui_pais,
  v_pais = v_pais,
  reparab_resultado = reparab_resultado,
  teste_kw_reparab = teste_kw_reparab,
  taxa_por_nota = taxa_por_nota,
  por_idade = por_idade,
  n_modelo = nrow(reparos_modelo),
  aic_modelo = AIC(modelo),
  tabela_modelo = tabela_modelo,
  motivos = motivos,
  motivo_por_categoria = motivo_por_categoria,
  consulta_sucesso = consulta_sucesso,
  teste_qui_consulta = teste_qui_consulta,
  fontes_por_ano = fontes_por_ano
)

saveRDS(resultados, "data/resultados.rds")

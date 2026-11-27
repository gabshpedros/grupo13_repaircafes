# Repair Cafés pelo mundo: o que dá para consertar?

**Grupo:** grupo13

**Banco de dados:** [Repair Cafes Worldwide](https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-04-07/readme.md) (TidyTuesday, 07/04/2026), com dados do [Repair Monitor](https://dashboard.repairmonitor.org/?language=en).

**Integrantes:**

- Igor Marcon
- Kevin Schelesting 
- Gabriele Henrique Pedro Sardá
- Gabriel
- Vinicius

**Links:**

- Aplicativo Shiny: *(em construção)*
- Vídeo no YouTube: *(em construção)*

## Estrutura do repositório

```
├── R/
│   ├── 01_importacao_limpeza.R   # baixa os dados, limpa e salva data/reparos.rds
│   └── 02_analise.R              # testes e modelos, salva data/resultados.rds
├── data/
│   ├── raw/                      # dados originais (não versionados, baixados pelo script)
│   ├── reparos.rds               # base tratada usada no relatório e no app
│   └── resultados.rds            # tabelas e testes da análise
├── relatorio/                    # relatório em Quarto (.qmd)
└── app/
    ├── app.R                     # interface e servidor do Shiny (10 abas)
    ├── funcoes.R                 # pacotes, dados e funções auxiliares
    └── dados/                    # cópia de reparos.rds e resultados.rds usada pelo app
```

## Como reproduzir

1. Abra o arquivo `grupo13_repaircafes.Rproj` no RStudio.
2. Rode `R/01_importacao_limpeza.R` para baixar e tratar os dados.
3. Rode `R/02_analise.R` para gerar os resultados da análise.
4. Para abrir o app: `shiny::runApp("app")`. 

## Declaração de uso de IA

*(a preencher ao final do trabalho)*

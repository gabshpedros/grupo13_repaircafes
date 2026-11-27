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

O relatório foi feito inteiramente pelo grupo, sem uso de inteligência artificial.

No aplicativo Shiny, usamos o Claude (Anthropic) e o Google AI Studio em duas tarefas pontuais, para ganhar tempo:

escrever os comentários que explicam o código do app (app/app.R e app/funcoes.R);
revisar nomes e acentuação dos textos que aparecem na interface.

A estrutura do app, a lógica do código, os gráficos e a escolha do que mostrar em cada aba foram feitos pelo grupo. Tudo o que as ferramentas sugeriram foi lido e conferido antes de entrar na versão final.

Prompt utilizado (reconstruído pelo grupo, já que não guardamos o texto original; a ideia e o pedido foram estes):

Vou te mandar o código de um aplicativo Shiny em R. Não mude a lógica, os nomes das funções nem o funcionamento do app. Quero só duas coisas: (1) adicione comentários curtos, em português, explicando o que cada parte do código faz; (2) corrija os nomes e a acentuação dos textos que aparecem para o usuário (títulos, abas, rótulos e legendas). Me devolva o código completo com essas alterações.

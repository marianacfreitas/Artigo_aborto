library(readr)
library(writexl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gt)
library(sf)
library(janitor)
library(webshot2)

# Lendo bases de dados
planilha_indicadores_aborto <- read_csv("databases/planilha_indicadores_aborto.csv", show_col_types = FALSE)
df_aux_municipios <- read_csv("databases_auxiliares/df_aux_municipios.csv", show_col_types = FALSE)

# Indicadores para todo o país
df_brasil <- planilha_indicadores_aborto |>
  group_by(ano) |>
  summarise(
    sus_tx_abortos_mil_mulheres_valor_medio =
      round((((sum(abortos_sus_menor_30) * 0.9) +
                (sum(abortos_sus_30_a_39) * 0.85) +
                (sum(abortos_sus_40_a_49) * 0.75)) * 4) /
              sum(pop_fem_sus_10_49) * 1000, 1),
    
    ans_tx_abortos_mil_mulheres_valor_medio =
      round((((sum(abortos_ans_menor_30) * 0.9) +
                (sum(abortos_ans_30_a_39) * 0.85) +
                (sum(abortos_ans_40_a_49) * 0.75)) * 5) /
              sum(pop_fem_ans_10_49) * 1000, 1),
    
    sus_tx_abortos_cem_nascidos_vivos_valor_medio =
      round((((sum(abortos_sus_menor_30) * 0.9) +
                (sum(abortos_sus_30_a_39) * 0.85) +
                (sum(abortos_sus_40_a_49) * 0.75)) * 4) /
              sum(total_de_nascidos_vivos_10_a_49_sus) * 100, 1),
    
    ans_tx_abortos_cem_nascidos_vivos_valor_medio =
      round((((sum(abortos_ans_menor_30) * 0.9) +
                (sum(abortos_ans_30_a_39) * 0.85) +
                (sum(abortos_ans_40_a_49) * 0.75)) * 5) /
              sum(total_de_nascidos_vivos_10_a_49_ans) * 100, 1)
  ) |>
  rename(
    `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` =
      sus_tx_abortos_mil_mulheres_valor_medio,
    `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar` =
      ans_tx_abortos_mil_mulheres_valor_medio,
    `Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS` =
      sus_tx_abortos_cem_nascidos_vivos_valor_medio,
    `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar` =
      ans_tx_abortos_cem_nascidos_vivos_valor_medio
  ) |>
  arrange(ano)

# Correlações no SUS
indicador1 <- "Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS"
indicador2 <- "Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS"

# Correlações
x <- df_brasil[[indicador1]]
y <- df_brasil[[indicador2]]

r_pearson <- cor(x, y, method = "pearson", use = "complete.obs")
test_pearson <- cor.test(x, y, method = "pearson")

r_spearman <- cor(x, y, method = "spearman", use = "complete.obs")
test_spearman <- cor.test(x, y, method = "spearman")


# Resultados organizados

results_tbl <- tibble::tibble(
  Método  = c("Pearson", "Spearman"),
  Estatística = c(
    unname(test_pearson$estimate),
    unname(test_spearman$estimate)  ),
  `p-valor` = c(
    test_pearson$p.value,
    test_spearman$p.value  )
)

gt_results <- results_tbl |>
  gt() |>
  tab_header(
    title = "Correlação entre a razão de abortos inseguros por 100 NV e taxa de abortos inseguros por 1000 MIF no SUS"  ) |>
  fmt_number(columns = c(Estatística), decimals = 3) |>
  fmt_number(columns = c(`p-valor`), decimals = 8) |>
  cols_label(Estatística = "Coeficiente") |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

gtsave(gt_results, file = file.path("tabelas/resultados_correlacao_sus.png"))

label_fmt <- function(x) sprintf("%.1f", x)

# Série temporal 
df_long <- df_brasil |>
  select(ano, all_of(indicador1), all_of(indicador2)) |>
  pivot_longer(cols = c(all_of(indicador1), all_of(indicador2)),
               names_to = "Indicador",
               values_to = "Valor") |>
  mutate(Label = label_fmt(Valor))

p_series <- ggplot(df_long, aes(x = ano, y = Valor, group = Indicador, color = Indicador)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = Label), vjust = -0.7, size = 3, show.legend = FALSE) +
  labs(
    title = "Indicadores do SUS ao longo do tempo (Brasil)",
    x = "Ano",
    y = "Valor"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",     # força empilhar
    legend.direction = "vertical",
    legend.margin = margin(t = 6),
    plot.margin = margin(10, 10, 20, 10)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))  # um abaixo do outro

ggsave(filename = file.path("figuras/serie_temporal_indicadores_sus.png"),
       plot = p_series, width = 10, height = 6, dpi = 300)

## Correlações na saúde suplementar

indicador1 <- "Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar"
indicador2 <- "Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar"

# Correlações
x <- df_brasil[[indicador1]]
y <- df_brasil[[indicador2]]

r_pearson <- cor(x, y, method = "pearson", use = "complete.obs")
test_pearson <- cor.test(x, y, method = "pearson")

r_spearman <- cor(x, y, method = "spearman", use = "complete.obs")
test_spearman <- cor.test(x, y, method = "spearman")


# Resultados organizados

results_tbl <- tibble::tibble(
  Método  = c("Pearson", "Spearman"),
  Estatística = c(
    unname(test_pearson$estimate),
    unname(test_spearman$estimate)  ),
  `p-valor` = c(
    test_pearson$p.value,
    test_spearman$p.value  )
)

gt_results <- results_tbl |>
  gt() |>
  tab_header(
    title = "Correlação entre a razão de abortos inseguros por 100 NV e taxa de abortos inseguros por 1000 MIF na saúde suplementar"  ) |>
  fmt_number(columns = c(Estatística), decimals = 3) |>
  fmt_number(columns = c(`p-valor`), decimals = 8) |>
  cols_label(Estatística = "Coeficiente") |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

gtsave(gt_results, file = file.path("tabelas/resultados_correlacao_ans.png"))

label_fmt <- function(x) sprintf("%.1f", x)

# Série temporal 
df_long <- df_brasil |>
  select(ano, all_of(indicador1), all_of(indicador2)) |>
  pivot_longer(cols = c(all_of(indicador1), all_of(indicador2)),
               names_to = "Indicador",
               values_to = "Valor") |>
  mutate(Label = label_fmt(Valor))

p_series <- ggplot(df_long, aes(x = ano, y = Valor, group = Indicador, color = Indicador)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = Label), vjust = -0.7, size = 3, show.legend = FALSE) +
  labs(
    title = "Indicadores da saúde suplementar ao longo do tempo (Brasil)",
    x = "Ano",
    y = "Valor"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",     # força empilhar
    legend.direction = "vertical",
    legend.margin = margin(t = 6),
    plot.margin = margin(10, 10, 20, 10)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))  # um abaixo do outro

ggsave(filename = file.path("figuras/serie_temporal_indicadores_ans.png"),
       plot = p_series, width = 10, height = 6, dpi = 300)




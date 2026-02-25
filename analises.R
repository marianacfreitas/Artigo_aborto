library(readr)
library(writexl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(trend)
library(gt)
library(sf)
library(geobr)
library(janitor)

# Planiha com informações por município
planilha_indicadores_aborto <- read_csv("databases/planilha_indicadores_aborto.csv")
df_aux_municipios <- read_csv("databases_auxiliares/df_aux_municipios.csv")

# Indicadores para todo o país
df_brasil <- planilha_indicadores_aborto |>
  group_by(ano) |>
  summarise(
    #geral_tx_abortos_mil_mulheres_lim_inf =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4)) / sum(pop_fem_10_49 ) * 1000, 1) ,
   # geral_tx_abortos_mil_mulheres_valor_medio =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5)) / sum(pop_fem_10_49 ) * 1000, 1) ,
    #geral_tx_abortos_mil_mulheres_lim_sup =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6)) / sum(pop_fem_10_49 ) * 1000, 1) ,
    #sus_tx_abortos_mil_mulheres_lim_inf =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    sus_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    #sus_tx_abortos_mil_mulheres_lim_sup =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    #ans_tx_abortos_mil_mulheres_lim_inf =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4) / sum(pop_fem_ans_10_49 ) * 1000, 1) ,
    ans_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5) / sum(pop_fem_ans_10_49 ) * 1000, 1) ,
    #ans_tx_abortos_mil_mulheres_lim_sup =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6) / sum(pop_fem_ans_10_49 ) * 1000, 1) ,
    #geral_tx_abortos_cem_nascidos_vivos_lim_inf =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4)) / sum(total_de_nascidos_vivos_10_a_49 ) * 100, 1) ,
    #geral_tx_abortos_cem_nascidos_vivos_valor_medio =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5)) / sum(total_de_nascidos_vivos_10_a_49 ) * 100, 1) ,
    #geral_tx_abortos_cem_nascidos_vivos_lim_sup =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6)) / sum(total_de_nascidos_vivos_10_a_49 ) * 100, 1) ,
    #sus_tx_abortos_cem_nascidos_vivos_lim_inf =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1) ,
    sus_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1) ,
    #sus_tx_abortos_cem_nascidos_vivos_lim_sup =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1) ,
    #ans_tx_abortos_cem_nascidos_vivos_lim_inf =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_ans ) * 100, 1) ,
    ans_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_ans ) * 100, 1) ,
    #ans_tx_abortos_cem_nascidos_vivos_lim_sup =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6) / sum(total_de_nascidos_vivos_10_a_49_ans ) * 100, 1)
  ) |>
  rename(
    #`Limite inferior da taxa de abortos inseguros por mil mulheres em idade fértil (geral)` = geral_tx_abortos_mil_mulheres_lim_inf,
    #`Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil (geral)` = geral_tx_abortos_mil_mulheres_valor_medio,
    #`Limite superior da taxa de abortos inseguros por mil mulheres em idade fértil (geral)` = geral_tx_abortos_mil_mulheres_lim_sup,
    #`Limite inferior da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_lim_inf,
    `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_valor_medio,
    #`Limite superior da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_lim_sup,
    #`Limite inferior da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplemetar` = ans_tx_abortos_mil_mulheres_lim_inf,
    `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar` = ans_tx_abortos_mil_mulheres_valor_medio,
    #`Limite superior da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar` = ans_tx_abortos_mil_mulheres_lim_sup,
    #`Limite inferior da razão de abortos inseguros por 100 nascidos vivos  (geral)` = geral_tx_abortos_cem_nascidos_vivos_lim_inf,
    #`Valor médio da razão de abortos inseguros por 100 nascidos vivos (geral)` = geral_tx_abortos_cem_nascidos_vivos_valor_medio,
    #`Limite superior da razão de abortos inseguros por 100 nascidos vivos (geral)` = geral_tx_abortos_cem_nascidos_vivos_lim_sup,
    #`Limite inferior da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_lim_inf,
    `Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_valor_medio,
    #`Limite superior da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_lim_sup,
    #`Limite inferior da razão de abortos inseguros por 100 nascidos vivos na saúde suplemetar` = ans_tx_abortos_cem_nascidos_vivos_lim_inf,
    `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar` = ans_tx_abortos_cem_nascidos_vivos_valor_medio,
    #`Limite superior da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar` = ans_tx_abortos_cem_nascidos_vivos_lim_sup
  )

# Indicadores por UF
df_uf <- planilha_indicadores_aborto |>
  left_join(df_aux_municipios) |>
  group_by(ano, uf) |>
  summarise(
    #geral_tx_abortos_mil_mulheres_lim_inf =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4)) / sum(pop_fem_10_49 ) * 1000, 1) ,
    #geral_tx_abortos_mil_mulheres_valor_medio =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5)) / sum(pop_fem_10_49 ) * 1000, 1) ,
    #geral_tx_abortos_mil_mulheres_lim_sup =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6)) / sum(pop_fem_10_49 ) * 1000, 1) ,
    #sus_tx_abortos_mil_mulheres_lim_inf =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    sus_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    #sus_tx_abortos_mil_mulheres_lim_sup =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    #ans_tx_abortos_mil_mulheres_lim_inf =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4) / sum(pop_fem_ans_10_49 ) * 1000, 1) ,
    ans_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5) / sum(pop_fem_ans_10_49 ) * 1000, 1) ,
    #ans_tx_abortos_mil_mulheres_lim_sup =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6) / sum(pop_fem_ans_10_49 ) * 1000, 1) ,
    #geral_tx_abortos_cem_nascidos_vivos_lim_inf =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4)) / sum(total_de_nascidos_vivos_10_a_49 ) * 100, 1) ,
    #geral_tx_abortos_cem_nascidos_vivos_valor_medio =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5)) / sum(total_de_nascidos_vivos_10_a_49 ) * 100, 1) ,
    #geral_tx_abortos_cem_nascidos_vivos_lim_sup =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6)) / sum(total_de_nascidos_vivos_10_a_49 ) * 100, 1) ,
    #sus_tx_abortos_cem_nascidos_vivos_lim_inf =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1) ,
    sus_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1) ,
    #sus_tx_abortos_cem_nascidos_vivos_lim_sup =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1) ,
    #ans_tx_abortos_cem_nascidos_vivos_lim_inf =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_ans ) * 100, 1) ,
    ans_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_ans ) * 100, 1) ,
    #ans_tx_abortos_cem_nascidos_vivos_lim_sup =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6) / sum(total_de_nascidos_vivos_10_a_49_ans ) * 100, 1)
  ) |>
  rename(
    #`Limite inferior da taxa de abortos inseguros por mil mulheres em idade fértil (geral)` = geral_tx_abortos_mil_mulheres_lim_inf,
    #`Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil (geral)` = geral_tx_abortos_mil_mulheres_valor_medio,
    #`Limite superior da taxa de abortos inseguros por mil mulheres em idade fértil (geral)` = geral_tx_abortos_mil_mulheres_lim_sup,
    #`Limite inferior da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_lim_inf,
    `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_valor_medio,
    #`Limite superior da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_lim_sup,
    #`Limite inferior da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplemetar` = ans_tx_abortos_mil_mulheres_lim_inf,
    `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar` = ans_tx_abortos_mil_mulheres_valor_medio,
    #`Limite superior da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar` = ans_tx_abortos_mil_mulheres_lim_sup,
    #`Limite inferior da razão de abortos inseguros por 100 nascidos vivos  (geral)` = geral_tx_abortos_cem_nascidos_vivos_lim_inf,
    #`Valor médio da razão de abortos inseguros por 100 nascidos vivos (geral)` = geral_tx_abortos_cem_nascidos_vivos_valor_medio,
    #`Limite superior da razão de abortos inseguros por 100 nascidos vivos (geral)` = geral_tx_abortos_cem_nascidos_vivos_lim_sup,
    #`Limite inferior da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_lim_inf,
    `Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_valor_medio,
    #`Limite superior da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_lim_sup,
    #`Limite inferior da razão de abortos inseguros por 100 nascidos vivos na saúde suplemetar` = ans_tx_abortos_cem_nascidos_vivos_lim_inf,
    `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar` = ans_tx_abortos_cem_nascidos_vivos_valor_medio,
    #`Limite superior da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar` = ans_tx_abortos_cem_nascidos_vivos_lim_sup
  )

# ------------- Séries temporais: Brasil - Plots ---------------

# Série temporal para o Brasil para a taxa de aborto

df_brasil_1000_mulheres <- df_brasil |>
  select(ano, `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS`, `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar`) |>
  pivot_longer(
    names_to = "Atendimento",
    values_to = "Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil",
    cols = c(`Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS`, `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar`)
  ) |>
  mutate(
    Atendimento = case_when(
      Atendimento == "Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS" ~ "SUS",
      Atendimento == "Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar" ~ "Saúde suplementar"
    )
  )
  
  
serie_temporal_br_plot_1000_mulheres <- ggplot(data = df_brasil_1000_mulheres, mapping = aes(x = ano, y = `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil`, color = Atendimento)) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = Atendimento)) +
  scale_x_continuous(breaks = unique(df_brasil_1000_mulheres$ano), guide = guide_axis(angle = 45)) +
  theme_bw(base_size = 15) +
  labs(
    title = "Série temporal do valor médio da taxa de abortos \n inseguros por mil mulheres em idade fértil",
    x = "Ano",
    y = "Valor médio da taxa de abortos inseguros",
    color = "Atendimento",
    shape = "Atendimento",
  ) +
  geom_text(label = df_brasil_1000_mulheres$`Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil`, nudge_y = 0.3, show.legend = FALSE) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Salmon", "DodgerBlue"))
serie_temporal_br_plot_1000_mulheres

ggsave(
  "figuras/br_taxa_aborto_por_1000_mulheres.png", serie_temporal_br_plot_1000_mulheres,
  width = 7.5, height = 6, units = "in", 
  dpi = 600
)

## Série temporal para o Brasil para a razão de aborto

df_brasil_100_nv <- df_brasil |>
  select(ano, `Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS`, `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar`) |>
  pivot_longer(
    names_to = "Atendimento",
    values_to = "Valor médio da razão de abortos inseguros por cem nascidos vivos",
    cols = c(`Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS`, `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar`)
  ) |>
  mutate(
    Atendimento = case_when(
      Atendimento == "Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS" ~ "SUS",
      Atendimento == "Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar" ~ "Saúde suplementar"
    )
  )


serie_temporal_br_plot_100_nv <- ggplot(data = df_brasil_100_nv, mapping = aes(x = ano, y = `Valor médio da razão de abortos inseguros por cem nascidos vivos`, color = Atendimento)) +
  geom_line(linewidth = 1) +
  geom_point(aes(shape = Atendimento)) +
  scale_x_continuous(breaks = unique(df_brasil_100_nv$ano), guide = guide_axis(angle = 45)) +
  theme_bw(base_size = 14) +
  labs(
    title = "Série temporal do valor médio da razão de abortos \n inseguros por 100 nascidos vivos",
    x = "Ano",
    y = "Valor médio da razão de abortos inseguros",
    color = "Atendimento",
    shape = "Atendimento",
  ) +
  geom_text(label = df_brasil_100_nv$`Valor médio da razão de abortos inseguros por cem nascidos vivos`, nudge_y = 0.7, show.legend = FALSE) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Salmon", "DodgerBlue"))
serie_temporal_br_plot_100_nv

ggsave(
  "figuras/br_razao_aborto_por_100_nascidos_vivos.png", serie_temporal_br_plot_100_nv,
  width = 7.5, height = 6, units = "in", 
  dpi = 600
)




# ------------- Séries temporais: Algumas UFs - Plots ---------------

ufs_analisadas_taxa <- c("Acre", "Distrito Federal", "Maranhão", "Mato Grosso")
ufs_analisadas_razao <- c("Acre", "Amazonas", "Distrito Federal", "Maranhão", "Mato Grosso",
                          "Pará", "Rio Grande do Sul", "Rio de Janeiro",
                          "Santa Catarina")

# Série temporal para a para a taxa de aborto

for(i in ufs_analisadas_taxa){
  
  df_uf_1000_mulheres <- df_uf |> filter(uf == i) |>
    select(ano, `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS`, `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar`) |>
    pivot_longer(
      names_to = "Atendimento",
      values_to = "Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil",
      cols = c(`Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS`, `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar`)
    ) |>
    mutate(
      Atendimento = case_when(
        Atendimento == "Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS" ~ "SUS",
        Atendimento == "Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar" ~ "Saúde suplementar"
      )
    )
  
  
  serie_temporal_uf_plot_1000_mulheres <- ggplot(data = df_uf_1000_mulheres, mapping = aes(x = ano, y = `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil`, color = Atendimento)) +
    geom_line(linewidth = 1) +
    geom_point(aes(shape = Atendimento)) +
    scale_x_continuous(breaks = unique(df_uf_1000_mulheres$ano), guide = guide_axis(angle = 45)) +
    theme_bw(base_size = 15) +
    labs(
      title = paste0(i, ": Série temporal do valor médio da taxa de abortos \n inseguros por mil mulheres em idade fértil"),
      x = "Ano",
      y = "Valor médio da taxa de abortos inseguros",
      color = "Atendimento",
      shape = "Atendimento",
    ) +
    geom_text(label = df_uf_1000_mulheres$`Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil`, nudge_y = 0.3, show.legend = FALSE) +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("Salmon", "DodgerBlue"))

  ggsave(
    paste0("figuras/", i, "_taxa_aborto_por_1000_mulheres.png"), serie_temporal_uf_plot_1000_mulheres,
    width = 7.5, height = 6, units = "in", 
    dpi = 600
  )
  
}


## Série temporal para a razão de aborto

for(i in ufs_analisadas_razao){
  
  df_uf_100_nv <- df_uf |> filter(uf == i) |>
    select(ano, `Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS`, `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar`) |>
    pivot_longer(
      names_to = "Atendimento",
      values_to = "Valor médio da razão de abortos inseguros por cem nascidos vivos",
      cols = c(`Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS`, `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar`)
    ) |>
    mutate(
      Atendimento = case_when(
        Atendimento == "Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS" ~ "SUS",
        Atendimento == "Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar" ~ "Saúde suplementar"
      )
    )
  
  
  serie_temporal_uf_plot_100_nv <- ggplot(data = df_uf_100_nv, mapping = aes(x = ano, y = `Valor médio da razão de abortos inseguros por cem nascidos vivos`, color = Atendimento)) +
    geom_line(linewidth = 1) +
    geom_point(aes(shape = Atendimento)) +
    scale_x_continuous(breaks = unique(df_uf_100_nv$ano), guide = guide_axis(angle = 45)) +
    theme_bw(base_size = 14) +
    labs(
      title = paste0(i, ": Série temporal do valor médio da razão de abortos \n inseguros por 100 nascidos vivos"),
      x = "Ano",
      y = "Valor médio da razão de abortos inseguros",
      color = "Atendimento",
      shape = "Atendimento",
    ) +
    geom_text(label = df_uf_100_nv$`Valor médio da razão de abortos inseguros por cem nascidos vivos`, nudge_y = 0.7, show.legend = FALSE) +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("Salmon", "DodgerBlue"))
  
  ggsave(
    paste0("figuras/", i, "_razao_aborto_por_100_nascidos_vivos.png"), serie_temporal_uf_plot_100_nv,
    width = 7.5, height = 6, units = "in", 
    dpi = 600
  )
  
}


# ----------- Teste de tendência: Mann-Kendall ----------

# Mann-Kendall
df_brasil$uf <- "Brasil"
df_tendencia <- rbind(
  df_brasil, df_uf)

variaveis <- c(
  "Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS",              
  "Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar",
  "Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS",                       
  "Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar"
)

results_table_completa <- data.frame()

for (i in 1:length(unique(df_tendencia$uf))) {
  localidade <- unique(df_tendencia$uf)[i]
  
  mann_kendall_results <- lapply(df_tendencia |> filter(uf == localidade) |> dplyr::select(all_of(variaveis)), function(x) {
    mann_kendall_test <- mk.test(x)
    return(c(uf = localidade, p_value = mann_kendall_test$p.value, z_value = mann_kendall_test$statistic))
  })
  
  p_value <- round(as.numeric(unlist(lapply(mann_kendall_results, `[[`, "p_value"))), 5)
  
  results_table <- data.frame(
    local = as.vector(unlist(lapply(mann_kendall_results, `[[`, "uf"))),
    Variavel = names(mann_kendall_results), 
    valor_2015 = lapply(variaveis, function(variavel) df_tendencia |> filter(uf == localidade, ano == 2015) |> pull(variavel)) |> as.numeric(),
    valor_2024 = lapply(variaveis, function(variavel) df_tendencia |> filter(uf == localidade, ano == 2024) |> pull(variavel)) |> as.numeric(),
    Mann_Kendall_z = round(as.numeric(unlist(lapply(mann_kendall_results, `[[`, "z_value.z"))), 3),
    Mann_Kendall_p = p_value
  )
  
  results_table_completa <- bind_rows(results_table_completa, results_table)
}

results_table_completa_organizada <- results_table_completa |>
  pivot_wider(
    names_from = Variavel,
    values_from = c(
      valor_2015, 
      valor_2024, Mann_Kendall_z, Mann_Kendall_p
    )
  ) 

 results_table_completa_organizada <- results_table_completa_organizada |>
   mutate(`Mann_Kendall_p_Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = 
            ifelse(`Mann_Kendall_p_Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` < 0.001,
                   "< 0.001",
                   as.character(round(`Mann_Kendall_p_Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS`,3))),
          
          `Mann_Kendall_p_Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS` = 
            ifelse(`Mann_Kendall_p_Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS` < 0.001,
                   "< 0.001",
                   as.character(round(`Mann_Kendall_p_Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS`,3))),
          
          )

results_table_completa_organizada

tabela_tendencia_taxa <- tibble(
  localidade = results_table_completa_organizada$local,
  
  valor_2015_taxa_por_1000_mulheres_sus = results_table_completa_organizada$`valor_2015_Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS`,
  valor_2015_taxa_por_1000_mulheres_saude_suplementar = results_table_completa_organizada$`valor_2015_Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar`,
  valor_2024_taxa_por_1000_mulheres_sus = results_table_completa_organizada$`valor_2024_Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS`,
  valor_2024_taxa_por_1000_mulheres_saude_suplementar = results_table_completa_organizada$`valor_2024_Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar`,
  
  mann_kendal_z_taxa_por_1000_mulheres_sus = results_table_completa_organizada$`Mann_Kendall_z_Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS`,
  mann_kendal_z_taxa_por_1000_mulheres_saude_suplementar = results_table_completa_organizada$`Mann_Kendall_z_Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar`,
  mann_kendal_p_taxa_por_1000_mulheres_sus = results_table_completa_organizada$`Mann_Kendall_p_Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS`,
  mann_kendal_p_taxa_por_1000_mulheres_saude_suplementar = results_table_completa_organizada$`Mann_Kendall_p_Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar`,
)

write_csv(
  tabela_tendencia_taxa,
  "databases/mann_kendall_taxa_por_1000_mulheres.csv"
)

write_xlsx(tabela_tendencia_taxa,
           "databases/mann_kendall_taxa_por_1000_mulheres.xlsx")

# Ajustar layout
gt_tbl_taxa <- gt(tabela_tendencia_taxa) |>
  tab_header(
    title = "Análise de tendência para a taxa de abortos inseguros por 1000 mulheres em idade fértil",
    subtitle = "Teste de tendência de Mann–Kendall, para Brasil e UFs (2015–2024)"
  ) |>
  tab_spanner(
    label = "SUS",
    columns = c(
      valor_2015_taxa_por_1000_mulheres_sus, valor_2024_taxa_por_1000_mulheres_sus,
      mann_kendal_z_taxa_por_1000_mulheres_sus, mann_kendal_p_taxa_por_1000_mulheres_sus)
  ) |>
  tab_spanner(
    label = "Saúde suplementar",
    columns = c(
      valor_2015_taxa_por_1000_mulheres_saude_suplementar, valor_2024_taxa_por_1000_mulheres_saude_suplementar,
      mann_kendal_z_taxa_por_1000_mulheres_saude_suplementar, mann_kendal_p_taxa_por_1000_mulheres_saude_suplementar
    )
  ) |>
  cols_label(
    localidade = "Local",
    valor_2015_taxa_por_1000_mulheres_sus = "Taxa em 2015",
    valor_2024_taxa_por_1000_mulheres_sus = "Taxa em 2024",
    mann_kendal_z_taxa_por_1000_mulheres_sus     = "Mann–Kendall",
    mann_kendal_p_taxa_por_1000_mulheres_sus      = "P-valor",
    valor_2015_taxa_por_1000_mulheres_saude_suplementar = "Taxa em 2015",
    valor_2024_taxa_por_1000_mulheres_saude_suplementar = "Taxa em 2024",
    mann_kendal_z_taxa_por_1000_mulheres_saude_suplementar     = "Mann–Kendall",
    mann_kendal_p_taxa_por_1000_mulheres_saude_suplementar      = "P-valor"
  ) |>
  fmt_number(
    columns = starts_with("valor"),
    decimals = 1
  ) |>
  fmt_number(
    columns = starts_with("mann"),
    decimals = 3
  )

gt_tbl_taxa

# salvando como imagem (PNG)
gtsave(
  gt_tbl_taxa,
  filename = "tabelas/tabela_mk_taxa_aborto_por_1000_mulheres.png"
)

gtsave(
  gt_tbl_taxa,
  filename = "tabelas/tabela_mk_taxa_aborto_por_1000_mulheres.html"
)

gtsave(
  gt_tbl_taxa,
  filename = "tabelas/tabela_mk_taxa_aborto_por_1000_mulheres.tex"
)



tabela_tendencia_razao <- tibble(
  localidade = results_table_completa_organizada$local,
  
  valor_2015_taxa_por_100_nascidos_vivos_sus = results_table_completa_organizada$`valor_2015_Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS`,
  valor_2015_taxa_por_100_nascidos_vivos_saude_suplementar = results_table_completa_organizada$`valor_2015_Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar`,
  valor_2024_taxa_por_100_nascidos_vivos_sus = results_table_completa_organizada$`valor_2024_Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS`,
  valor_2024_taxa_por_100_nascidos_vivos_saude_suplementar = results_table_completa_organizada$`valor_2024_Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar`,
  
  mann_kendal_z_taxa_por_100_nascidos_vivos_sus = results_table_completa_organizada$`Mann_Kendall_z_Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS`,
  mann_kendal_z_taxa_por_100_nascidos_vivos_saude_suplementar = results_table_completa_organizada$`Mann_Kendall_z_Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar`,
  mann_kendal_p_taxa_por_100_nascidos_vivos_sus = results_table_completa_organizada$`Mann_Kendall_p_Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS`,
  mann_kendal_p_taxa_por_100_nascidos_vivos_saude_suplementar = results_table_completa_organizada$`Mann_Kendall_p_Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar`,
)

write_csv(
  tabela_tendencia_razao,
  "databases/mann_kendall_razao_por_100_nascidos_vivos.csv"
)

write_xlsx(
  tabela_tendencia_razao,
  "databases/mann_kendall_razao_por_100_nascidos_vivos.xlsx"
)

# Ajustar layout
gt_tbl_razao <- gt(tabela_tendencia_razao) |>
  tab_header(
    title = "Análise de tendência para a razão de abortos inseguros por 100 nascidos vivos",
    subtitle = "Teste de tendência de Mann–Kendall, para Brasil e UFs (2015–2024)"
  ) |>
  tab_spanner(
    label = "SUS",
    columns = c(
      valor_2015_taxa_por_100_nascidos_vivos_sus, valor_2024_taxa_por_100_nascidos_vivos_sus,
      mann_kendal_z_taxa_por_100_nascidos_vivos_sus, mann_kendal_p_taxa_por_100_nascidos_vivos_sus)
  ) |>
  tab_spanner(
    label = "Saúde suplementar",
    columns = c(
      valor_2015_taxa_por_100_nascidos_vivos_saude_suplementar, valor_2024_taxa_por_100_nascidos_vivos_saude_suplementar,
      mann_kendal_z_taxa_por_100_nascidos_vivos_saude_suplementar, mann_kendal_p_taxa_por_100_nascidos_vivos_saude_suplementar
    )
  ) |>
  cols_label(
    localidade = "Local",
    valor_2015_taxa_por_100_nascidos_vivos_sus = "Razão em 2015",
    valor_2024_taxa_por_100_nascidos_vivos_sus = "Razão em 2024",
    mann_kendal_z_taxa_por_100_nascidos_vivos_sus     = "Mann–Kendall",
    mann_kendal_p_taxa_por_100_nascidos_vivos_sus      = "P-valor",
    valor_2015_taxa_por_100_nascidos_vivos_saude_suplementar = "Razão em 2015",
    valor_2024_taxa_por_100_nascidos_vivos_saude_suplementar = "Razão em 2024",
    mann_kendal_z_taxa_por_100_nascidos_vivos_saude_suplementar     = "Mann–Kendall",
    mann_kendal_p_taxa_por_100_nascidos_vivos_saude_suplementar      = "P-valor"
  ) |>
  fmt_number(
    columns = starts_with("valor"),
    decimals = 1
  ) |>
  fmt_number(
    columns = starts_with("mann"),
    decimals = 3
  )

gt_tbl_razao

# salvando como imagem (PNG)
gtsave(
  gt_tbl_razao,
  filename = "tabelas/tabela_mk_razao_aborto_por_100_nascidos_vivos.png", vwidth = 1050
)

gtsave(
  gt_tbl_razao,
  filename = "tabelas/tabela_mk_razao_aborto_por_100_nascidos_vivos.html"
)

gtsave(
  gt_tbl_razao,
  filename = "tabelas/tabela_mk_razao_aborto_por_100_nascidos_vivos.tex"
)

# -------------- Análise Espacial por Município -----------

df_municipio <- planilha_indicadores_aborto |>
  group_by(ano, codmunres) |>
  summarise(
    #geral_tx_abortos_mil_mulheres_lim_inf =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4)) / sum(pop_fem_10_49 ) * 1000, 1) ,
    #geral_tx_abortos_mil_mulheres_valor_medio =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5)) / sum(pop_fem_10_49 ) * 1000, 1) ,
    #geral_tx_abortos_mil_mulheres_lim_sup =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6)) / sum(pop_fem_10_49 ) * 1000, 1) ,
    #sus_tx_abortos_mil_mulheres_lim_inf =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    sus_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    #sus_tx_abortos_mil_mulheres_lim_sup =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    #ans_tx_abortos_mil_mulheres_lim_inf =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4) / sum(pop_fem_ans_10_49 ) * 1000, 1) ,
    ans_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5) / sum(pop_fem_ans_10_49 ) * 1000, 1) ,
    #ans_tx_abortos_mil_mulheres_lim_sup =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6) / sum(pop_fem_ans_10_49 ) * 1000, 1) ,
    #geral_tx_abortos_cem_nascidos_vivos_lim_inf =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4)) / sum(total_de_nascidos_vivos_10_a_49 ) * 100, 1) ,
    #geral_tx_abortos_cem_nascidos_vivos_valor_medio =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5)) / sum(total_de_nascidos_vivos_10_a_49 ) * 100, 1) ,
    #geral_tx_abortos_cem_nascidos_vivos_lim_sup =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6)) / sum(total_de_nascidos_vivos_10_a_49 ) * 100, 1) ,
    #sus_tx_abortos_cem_nascidos_vivos_lim_inf =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1) ,
    sus_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1) ,
    #sus_tx_abortos_cem_nascidos_vivos_lim_sup =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1) ,
    #ans_tx_abortos_cem_nascidos_vivos_lim_inf =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_ans ) * 100, 1) ,
    ans_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_ans ) * 100, 1) ,
    #ans_tx_abortos_cem_nascidos_vivos_lim_sup =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6) / sum(total_de_nascidos_vivos_10_a_49_ans ) * 100, 1)
  ) |>
  rename(
    #`Limite inferior da taxa de abortos inseguros por mil mulheres em idade fértil (geral)` = geral_tx_abortos_mil_mulheres_lim_inf,
    #`Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil (geral)` = geral_tx_abortos_mil_mulheres_valor_medio,
    #`Limite superior da taxa de abortos inseguros por mil mulheres em idade fértil (geral)` = geral_tx_abortos_mil_mulheres_lim_sup,
    #`Limite inferior da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_lim_inf,
    `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_valor_medio,
    #`Limite superior da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_lim_sup,
    #`Limite inferior da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplemetar` = ans_tx_abortos_mil_mulheres_lim_inf,
    `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar` = ans_tx_abortos_mil_mulheres_valor_medio,
    #`Limite superior da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar` = ans_tx_abortos_mil_mulheres_lim_sup,
    #`Limite inferior da razão de abortos inseguros por 100 nascidos vivos  (geral)` = geral_tx_abortos_cem_nascidos_vivos_lim_inf,
    #`Valor médio da razão de abortos inseguros por 100 nascidos vivos (geral)` = geral_tx_abortos_cem_nascidos_vivos_valor_medio,
    #`Limite superior da razão de abortos inseguros por 100 nascidos vivos (geral)` = geral_tx_abortos_cem_nascidos_vivos_lim_sup,
    #`Limite inferior da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_lim_inf,
    `Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_valor_medio,
    #`Limite superior da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_lim_sup,
    #`Limite inferior da razão de abortos inseguros por 100 nascidos vivos na saúde suplemetar` = ans_tx_abortos_cem_nascidos_vivos_lim_inf,
    `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar` = ans_tx_abortos_cem_nascidos_vivos_valor_medio,
    #`Limite superior da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar` = ans_tx_abortos_cem_nascidos_vivos_lim_sup
  )

df_muni_sf_2024 <- read_municipality(year = 2020, showProgress = FALSE) |>
  mutate(codmunres = substr(code_muni, 1, 6)) |>
  mutate(codmunres = as.numeric(codmunres))

df_ufs_sf <- read_state(year = 2020, showProgress = FALSE)

df_municipio_2024 <- df_municipio |> filter(ano == 2024)
df_municipio_2015 <- df_municipio |> filter(ano == 2015)

df_mapa_2024 <- left_join(df_municipio_2024, df_muni_sf_2024) |>
  st_as_sf()

df_mapa_2015 <- left_join(df_municipio_2015, df_muni_sf_2024) |>
  st_as_sf()

mapa_2024_tx_sus <- ggplot() +
  geom_sf(data = df_mapa_2024, aes(fill = `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS`), color = NA) +
  labs(title = "Mapa da taxa de abortos inseguros por 1000 mulheres em idade fértil \n no SUS dos municípios no ano de 2024") +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Taxa", limits = c(0, 70)) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.05, color = "#505050") +
  theme_bw()
mapa_2024_tx_sus

ggsave("figuras/mapa_taxa_por_1000mif_municipios_sus_2024.png", mapa_2024_tx_sus, width = 12, height = 8)

mapa_2024_tx_ans <- ggplot() +
  geom_sf(data = df_mapa_2024, aes(fill = `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar`), color = NA) +
  labs(title = "Mapa da taxa de abortos inseguros por 1000 mulheres em idade fértil \n na saúde suplementar dos municípios no ano de 2024") +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Taxa", limits = c(0, 70)) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.05, color = "#505050") +
  theme_bw()
mapa_2024_tx_ans

ggsave("figuras/mapa_taxa_por_1000mif_municipios_saude_suplementar_2024.png", mapa_2024_tx_ans, width = 12, height = 8)

mapa_2024_razao_sus <- ggplot() +
  geom_sf(data = df_mapa_2024, aes(fill = `Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS`), color = NA) +
  labs(title = "Mapa da razão de abortos inseguros por 100 nascidos vivos \n no SUS dos municípios no ano de 2024") +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Razão", limits = c(0, 200)) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.05, color = "#505050") +
  theme_bw()
mapa_2024_razao_sus

ggsave("figuras/mapa_razao_por_100nv_municipios_sus_2024.png", mapa_2024_razao_sus, width = 12, height = 8)

mapa_2024_razao_ans <- ggplot() +
  geom_sf(data = df_mapa_2024, aes(fill = `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar`), color = NA) +
  labs(title = "Mapa da razão de abortos inseguros por 100 nascidos vivos \n na saúde suplementar dos municípios no ano de 2024") +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Razão", limits = c(0, 200)) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.05, color = "#505050") +
  theme_bw()
mapa_2024_razao_ans

ggsave("figuras/mapa_razao_por_100nv_municipios_saude_suplementar_2024.png", mapa_2024_razao_ans, width = 12, height = 8)

mapa_2015_tx_sus <- ggplot() +
  geom_sf(data = df_mapa_2015, aes(fill = `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS`), color = NA) +
  labs(title = "Mapa da taxa de abortos inseguros por 1000 mulheres em idade fértil \n no SUS dos municípios no ano de 2015") +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Taxa", limits = c(0, 70)) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.05, color = "#505050") +
  theme_bw()
mapa_2015_tx_sus

ggsave("figuras/mapa_taxa_por_1000mif_municipios_sus_2015.png", mapa_2015_tx_sus, width = 12, height = 8)

mapa_2015_tx_ans <- ggplot() +
  geom_sf(data = df_mapa_2015, aes(fill = `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar`), color = NA) +
  labs(title = "Mapa da taxa de abortos inseguros por 1000 mulheres em idade fértil \n na saúde suplementar dos municípios no ano de 2015") +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Taxa", limits = c(0, 70)) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.05, color = "#505050") +
  theme_bw()
mapa_2015_tx_ans

ggsave("figuras/mapa_taxa_por_1000mif_municipios_saude_suplementar_2015.png", mapa_2015_tx_ans, width = 12, height = 8)

mapa_2015_razao_sus <- ggplot() +
  geom_sf(data = df_mapa_2015, aes(fill = `Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS`), color = NA) +
  labs(title = "Mapa da razão de abortos inseguros por 100 nascidos vivos \n no SUS dos municípios no ano de 2015") +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Razão", limits = c(0, 200)) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.05, color = "#505050") +
  theme_bw()
mapa_2015_razao_sus

ggsave("figuras/mapa_razao_por_100nv_municipios_sus_2015.png", mapa_2015_razao_sus, width = 12, height = 8)

mapa_2015_razao_ans <- ggplot() +
  geom_sf(data = df_mapa_2015, aes(fill = `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar`), color = NA) +
  labs(title = "Mapa da razão de abortos inseguros por 100 nascidos vivos \n na saúde suplementar dos municípios no ano de 2015") +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Razão", limits = c(0, 200)) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.05, color = "#505050") +
  theme_bw()
mapa_2015_razao_ans

ggsave("figuras/mapa_razao_por_100nv_municipios_saude_suplementar_2015.png", mapa_2015_razao_ans, width = 12, height = 8)


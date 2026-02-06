library(readr)
library(writexl)
library(dplyr)

planilha_indicadores_aborto <- read_csv("databases/planilha_indicadores_aborto.csv")

df <- planilha_indicadores_aborto |>
  group_by(ano) |>
  summarise(
    geral_tx_abortos_mil_mulheres_lim_inf =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4)) / sum(pop_fem_10_49 ) * 1000, 1) ,
    geral_tx_abortos_mil_mulheres_valor_medio =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5)) / sum(pop_fem_10_49 ) * 1000, 1) ,
    geral_tx_abortos_mil_mulheres_lim_sup =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6)) / sum(pop_fem_10_49 ) * 1000, 1) ,
    sus_tx_abortos_mil_mulheres_lim_inf =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    sus_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    sus_tx_abortos_mil_mulheres_lim_sup =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    ans_tx_abortos_mil_mulheres_lim_inf =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4) / sum(pop_fem_ans_10_49 ) * 1000, 1) ,
    ans_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5) / sum(pop_fem_ans_10_49 ) * 1000, 1) ,
    ans_tx_abortos_mil_mulheres_lim_sup =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6) / sum(pop_fem_ans_10_49 ) * 1000, 1) ,
    geral_tx_abortos_cem_nascidos_vivos_lim_inf =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4)) / sum(total_de_nascidos_vivos_10_a_49 ) * 100, 1) ,
    geral_tx_abortos_cem_nascidos_vivos_valor_medio =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5)) / sum(total_de_nascidos_vivos_10_a_49 ) * 100, 1) ,
    geral_tx_abortos_cem_nascidos_vivos_lim_sup =  round(((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) + (((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6)) / sum(total_de_nascidos_vivos_10_a_49 ) * 100, 1) ,
    sus_tx_abortos_cem_nascidos_vivos_lim_inf =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 3) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1) ,
    sus_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1) ,
    sus_tx_abortos_cem_nascidos_vivos_lim_sup =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1) ,
    ans_tx_abortos_cem_nascidos_vivos_lim_inf =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_ans ) * 100, 1) ,
    ans_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_ans ) * 100, 1) ,
    ans_tx_abortos_cem_nascidos_vivos_lim_sup =  round((((sum(abortos_ans_menor_30 ) * 0.9) + (sum(abortos_ans_30_a_39 ) * 0.85) + (sum(abortos_ans_40_a_49 ) * 0.75)) * 6) / sum(total_de_nascidos_vivos_10_a_49_ans ) * 100, 1)
  )


final <- df |>
  rename(
    `Limite inferior da taxa de abortos inseguros por mil mulheres em idade fértil (geral)` = geral_tx_abortos_mil_mulheres_lim_inf,
    `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil (geral)` = geral_tx_abortos_mil_mulheres_valor_medio,
    `Limite superior da taxa de abortos inseguros por mil mulheres em idade fértil (geral)` = geral_tx_abortos_mil_mulheres_lim_sup,
    `Limite inferior da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_lim_inf,
    `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_valor_medio,
    `Limite superior da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_lim_sup,
    `Limite inferior da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplemetar` = ans_tx_abortos_mil_mulheres_lim_inf,
    `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar` = ans_tx_abortos_mil_mulheres_valor_medio,
    `Limite superior da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar` = ans_tx_abortos_mil_mulheres_lim_sup,
    `Limite inferior da razão de abortos inseguros por 100 nascidos vivos  (geral)` = geral_tx_abortos_cem_nascidos_vivos_lim_inf,
    `Valor médio da razão de abortos inseguros por 100 nascidos vivos (geral)` = geral_tx_abortos_cem_nascidos_vivos_valor_medio,
    `Limite superior da razão de abortos inseguros por 100 nascidos vivos (geral)` = geral_tx_abortos_cem_nascidos_vivos_lim_sup,
    `Limite inferior da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_lim_inf,
    `Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_valor_medio,
    `Limite superior da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_lim_sup,
    `Limite inferior da razão de abortos inseguros por 100 nascidos vivos na saúde suplemetar` = ans_tx_abortos_cem_nascidos_vivos_lim_inf,
    `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar` = ans_tx_abortos_cem_nascidos_vivos_valor_medio,
    `Limite superior da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar` = ans_tx_abortos_cem_nascidos_vivos_lim_sup
  )


write_xlsx(final, "indicadores_abortos.xlsx")



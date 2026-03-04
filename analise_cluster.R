library(dplyr)
library(janitor)
library(clusterSim)
library(clValid)
library(rstatix)
library(sf)
library(geobr)
library(readr)
library(factoextra)

# Planiha com informações por município
planilha_indicadores_aborto <- read_csv("databases/planilha_indicadores_aborto.csv") |>
  mutate(across(contains("ans"),
                ~ if_else(ano < 2017, NA, .)))

df_aux_municipios <- read_csv("databases_auxiliares/df_aux_municipios.csv")

# Indicadores da ANS de 2017 a 2019 (agrupados)
df_ans_inicio <- planilha_indicadores_aborto |>
  filter(ano >= 2017 & ano <= 2019) |>
  group_by(codmunres) |>
  summarise(
    ans_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_ans_menor_30, na.rm = T) * 0.9) + (sum(abortos_ans_30_a_39 , na.rm = T) * 0.85) + (sum(abortos_ans_40_a_49 , na.rm = T) * 0.75)) * 5) / sum(pop_fem_ans_10_49 , na.rm = T) * 1000, 1),
    ans_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_ans_menor_30 , na.rm = T) * 0.9) + (sum(abortos_ans_30_a_39 , na.rm = T) * 0.85) + (sum(abortos_ans_40_a_49 , na.rm = T) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_ans , na.rm = T) * 100, 1))#|>
  # rename(
  #   `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar` = ans_tx_abortos_mil_mulheres_valor_medio,
  #   `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar` = ans_tx_abortos_cem_nascidos_vivos_valor_medio
  # )

# Indicadores do SUS de 2015 a 2017 (agrupados)
df_sus_inicio <- planilha_indicadores_aborto |>
  filter(ano >= 2015 & ano <= 2017) |>
  group_by(codmunres) |>
  summarise(
    sus_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    sus_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1)
   ) #|>
  # rename(
  #   `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_valor_medio,
  #   `Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_valor_medio
  # )

# Indicadores da ANS de 2022 a 2024 (agrupados)
df_ans_final <- planilha_indicadores_aborto |>
  filter(ano >= 2022 & ano <= 2024) |>
  group_by(codmunres) |>
  summarise(
  ans_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 5) / sum(pop_fem_ans_10_49) * 1000, 1),
   ans_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_ans) * 100, 1)) #|>
  # rename(
  #   `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil na saúde suplementar` = ans_tx_abortos_mil_mulheres_valor_medio,
  #   `Valor médio da razão de abortos inseguros por 100 nascidos vivos na saúde suplementar` = ans_tx_abortos_cem_nascidos_vivos_valor_medio
  # )

df_ans_final[is.na(df_ans_final)] <- 0

# Indicadores do SUS de 2022 a 2024 (agrupados)
df_sus_final <- planilha_indicadores_aborto |>
  filter(ano >= 2022 & ano <= 2024) |>
  group_by(codmunres) |>
  summarise(
    sus_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(pop_fem_sus_10_49 ) * 1000, 1) ,
    sus_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1)
   ) #|>
  # rename(
  #   `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_valor_medio,
  #   `Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_valor_medio
  # )

# Análise de cluster utilizando a distância euclidiana
df_ans_inicio_var <- df_ans_inicio |> select(c(ans_tx_abortos_mil_mulheres_valor_medio, ans_tx_abortos_cem_nascidos_vivos_valor_medio))
df_ans_final_var <- df_ans_final |> select(c(ans_tx_abortos_mil_mulheres_valor_medio, ans_tx_abortos_cem_nascidos_vivos_valor_medio))
df_sus_inicio_var <- df_sus_inicio |> select(c(sus_tx_abortos_mil_mulheres_valor_medio, sus_tx_abortos_cem_nascidos_vivos_valor_medio))
df_sus_final_var <- df_sus_final |> select(c(sus_tx_abortos_mil_mulheres_valor_medio, sus_tx_abortos_cem_nascidos_vivos_valor_medio))

df_ans_inicio_dist <- dist(df_ans_inicio_var, method = "euclidean")
df_ans_final_dist <- dist(df_ans_final_var, method = "euclidean")
df_sus_inicio_dist <- dist(df_sus_inicio_var, method = "euclidean")
df_sus_final_dist <- dist(df_sus_final_var, method = "euclidean")

## K-means ----------------------------------------------------------------
### Gráficos do cotovelo
fviz_nbclust(df_ans_inicio_var, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (ANS de 2017 a 2019)"
  )  # k = 4 ou 5

fviz_nbclust(df_ans_final_var, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (ANS de 2022 a 2024)"
  ) # k = 4 ou k = 5

fviz_nbclust(df_sus_inicio_var, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (SUS de 2015 a 2017)"
  ) # k = 4 ou k = 5


fviz_nbclust(df_sus_final_var, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (SUS de 2022 a 2024)"
  ) # k = 4 ou k = 5

### Ajustando o k-means com os números de grupos escolhidos
set.seed(2402)
df_ans_inicio_kmeans5 <- kmeans(df_ans_inicio_var, 5)
round(df_ans_inicio_kmeans5$centers, 3)
table(df_ans_inicio_kmeans5$cluster)

set.seed(2402)
df_ans_inicio_kmeans4 <- kmeans(df_ans_inicio_var, 4)
round(df_ans_inicio_kmeans4$centers, 3)
table(df_ans_inicio_kmeans4$cluster)

set.seed(2402)
df_ans_inicio_kmeans3 <- kmeans(df_ans_inicio_var, 3)
round(df_ans_inicio_kmeans3$centers, 3)
table(df_ans_inicio_kmeans3$cluster)

set.seed(2402)
df_ans_final_kmeans5 <- kmeans(df_ans_final_var, 5)
round(df_ans_final_kmeans5$centers, 3)
table(df_ans_final_kmeans5$cluster)

set.seed(2402)
df_ans_final_kmeans4 <- kmeans(df_ans_final_var, 4)
round(df_ans_final_kmeans4$centers, 3)
table(df_ans_final_kmeans4$cluster)

set.seed(2402)
df_ans_final_kmeans3 <- kmeans(df_ans_final_var, 3)
round(df_ans_final_kmeans3$centers, 3)
table(df_ans_final_kmeans3$cluster)

set.seed(2402)
df_sus_inicio_kmeans5 <- kmeans(df_sus_inicio_var, 5)
round(df_sus_inicio_kmeans5$centers, 3)
table(df_sus_inicio_kmeans5$cluster)

set.seed(2402)
df_sus_final_kmeans5 <- kmeans(df_sus_final_var, 5)
round(df_sus_final_kmeans5$centers, 3)
table(df_sus_final_kmeans5$cluster)

## K-medoids ----------------------------------------------------------------
### Gráficos do cotovelo
fviz_nbclust(df_ans_inicio_var, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (ANS de 2017 a 2019)"
  )  # k = 4 ou 5

fviz_nbclust(df_ans_final_var, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (ANS de 2022 a 2024)"
  ) # k = 4 ou k = 5

fviz_nbclust(df_sus_inicio_var, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (SUS de 2015 a 2017)"
  ) # k = 4 ou k = 5


fviz_nbclust(df_sus_final_var, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (SUS de 2022 a 2024)"
  ) # k = 4 ou k = 5

set.seed(2402)
df_ans_inicio_pam5 <- cluster::pam(df_ans_inicio_var, 5)
round(df_ans_inicio_pam5$medoids, 3)
table(df_ans_inicio_pam5$cluster)

set.seed(2402)
df_ans_final_pam5 <- cluster::pam(df_ans_final_var, 5)
round(df_ans_final_pam$medoids, 3)
table(df_ans_final_pam$cluster)

set.seed(2402)
df_sus_inicio_pam5 <- cluster::pam(df_sus_inicio_var, 5)
round(df_sus_inicio_pam$medoids, 3)
table(df_sus_inicio_pam$cluster)

set.seed(2402)
df_sus_final_pam <- cluster::pam(df_sus_final_var, 5)
round(df_sus_final_pam$medoids, 3)
table(df_sus_final_pam$cluster)

## Ajustando métodos hierárquicos -----------------------------------------
### Método de Ward
ans_inicio_ward <- hclust(df_ans_inicio_dist, method = "ward.D2")
plot(as.dendrogram(ans_inicio_ward), main = "Dendrograma do método de Ward (ANS 2017 a 2019)", ylab = "Altura")
rect.hclust(ans_inicio_ward, k = 3, border = 2:5)
ans_inicio_ward3_class <- cutree(ans_inicio_ward, k = 3)
table(ans_inicio_ward3_class)

ans_final_ward <- hclust(df_ans_final_dist, method = "ward.D2")
plot(as.dendrogram(ans_final_ward), main = "Dendrograma do método de Ward (ANS 2022 a 2024)", ylab = "Altura")
rect.hclust(ans_final_ward, k = 3, border = 2:5)
ans_final_ward3_class <- cutree(ans_final_ward, k = 3)
table(ans_final_ward3_class)

sus_inicio_ward <- hclust(df_sus_inicio_dist, method = "ward.D2")
plot(as.dendrogram(sus_inicio_ward), main = "Dendrograma do método de Ward (SUS 2015 a 2017)", ylab = "Altura")
rect.hclust(sus_inicio_ward, k = 3, border = 2:5)
sus_inicio_ward3_class <- cutree(sus_inicio_ward, k = 3)
table(sus_inicio_ward3_class)

sus_final_ward <- hclust(df_sus_final_dist, method = "ward.D2")
plot(as.dendrogram(sus_final_ward), main = "Dendrograma do método de Ward (SUS 2022 a 2024)", ylab = "Altura")
rect.hclust(sus_final_ward, k = 3, border = 2:5)
sus_final_ward3_class <- cutree(sus_final_ward, k = 3)
table(sus_final_ward3_class)

### Single linkage
ans_inicio_single <- hclust(df_ans_inicio_dist, method = "single")
plot(as.dendrogram(ans_inicio_single), main = "Dendrograma do método de single linkage (ANS 2017 a 2019)", ylab = "Altura")

ans_final_single <- hclust(df_ans_final_dist, method = "single")
plot(as.dendrogram(ans_final_single), main = "Dendrograma do método de single linkage (ANS 2022 a 2024)", ylab = "Altura")

sus_inicio_single <- hclust(df_sus_inicio_dist, method = "single")
plot(as.dendrogram(sus_inicio_single), main = "Dendrograma do método de single linkage (SUS 2015 a 2017)", ylab = "Altura")

sus_final_single <- hclust(df_sus_final_dist, method = "single")
plot(as.dendrogram(sus_final_single), main = "Dendrograma do método de single linkage (SUS 2022 a 2024)", ylab = "Altura")


### Complete linkage
ans_inicio_complete <- hclust(df_ans_inicio_dist, method = "complete")
plot(as.dendrogram(ans_inicio_complete), main = "Dendrograma do método de complete linkage (ANS 2017 a 2019)", ylab = "Altura")

ans_final_complete <- hclust(df_ans_final_dist, method = "complete")
plot(as.dendrogram(ans_final_complete), main = "Dendrograma do método de complete linkage (ANS 2022 a 2024)", ylab = "Altura")

sus_inicio_complete <- hclust(df_sus_inicio_dist, method = "complete")
plot(as.dendrogram(sus_inicio_complete), main = "Dendrograma do método de complete linkage (SUS 2015 a 2017)", ylab = "Altura")

sus_final_complete <- hclust(df_sus_final_dist, method = "complete")
plot(as.dendrogram(sus_final_complete), main = "Dendrograma do método de complete linkage (SUS 2022 a 2024)", ylab = "Altura")


### Average linkage
ans_inicio_average <- hclust(df_ans_inicio_dist, method = "average")
plot(as.dendrogram(ans_inicio_average), main = "Dendrograma do método de average linkage (ANS 2017 a 2019)", ylab = "Altura")

ans_final_average <- hclust(df_ans_final_dist, method = "average")
plot(as.dendrogram(ans_final_average), main = "Dendrograma do método de average linkage (ANS 2022 a 2024)", ylab = "Altura")

sus_inicio_average <- hclust(df_sus_inicio_dist, method = "average")
plot(as.dendrogram(sus_inicio_average), main = "Dendrograma do método de average linkage (SUS 2015 a 2017)", ylab = "Altura")

sus_final_average <- hclust(df_sus_final_dist, method = "average")
plot(as.dendrogram(sus_final_average), main = "Dendrograma do método de average linkage (SUS 2022 a 2024)", ylab = "Altura")


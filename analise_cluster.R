library(dplyr)
library(janitor)
library(clusterSim)
library(clValid)
library(rstatix)
library(sf)
library(geobr)
library(readr)
library(factoextra)
library(knitr)
library(htmltools)
library(gridExtra)

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
  )  # k = 4, 5 ou 6

fviz_nbclust(df_ans_final_var, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (ANS de 2022 a 2024)"
  ) # k = 4, 5 ou k = 6

fviz_nbclust(df_sus_inicio_var, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (SUS de 2015 a 2017)"
  ) # k = 4, 5 ou k = 6


fviz_nbclust(df_sus_final_var, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (SUS de 2022 a 2024)"
  ) # k = 4 k = 5, 6

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
df_ans_inicio_kmeans6 <- kmeans(df_ans_inicio_var, 6)
round(df_ans_inicio_kmeans6$centers, 3)
table(df_ans_inicio_kmeans6$cluster)

set.seed(2402)
df_ans_final_kmeans6 <- kmeans(df_ans_final_var, 6)
round(df_ans_final_kmeans6$centers, 3)
table(df_ans_final_kmeans6$cluster)

set.seed(2402)
df_ans_final_kmeans5 <- kmeans(df_ans_final_var, 5)
round(df_ans_final_kmeans5$centers, 3)
table(df_ans_final_kmeans5$cluster)

set.seed(2402)
df_ans_final_kmeans4 <- kmeans(df_ans_final_var, 4)
round(df_ans_final_kmeans4$centers, 3)
table(df_ans_final_kmeans4$cluster)

set.seed(2402)
df_sus_inicio_kmeans5 <- kmeans(df_sus_inicio_var, 5)
round(df_sus_inicio_kmeans5$centers, 3)
table(df_sus_inicio_kmeans5$cluster)

set.seed(2402)
df_sus_inicio_kmeans6 <- kmeans(df_sus_inicio_var, 6)
round(df_sus_inicio_kmeans6$centers, 3)
table(df_sus_inicio_kmeans6$cluster)

set.seed(2402)
df_sus_inicio_kmeans4 <- kmeans(df_sus_inicio_var, 4)
round(df_sus_inicio_kmeans4$centers, 3)
table(df_sus_inicio_kmeans4$cluster)

set.seed(2402)
df_sus_final_kmeans5 <- kmeans(df_sus_final_var, 5)
round(df_sus_final_kmeans5$centers, 3)
table(df_sus_final_kmeans5$cluster)

set.seed(2402)
df_sus_final_kmeans4 <- kmeans(df_sus_final_var, 4)
round(df_sus_final_kmeans4$centers, 3)
table(df_sus_final_kmeans4$cluster)

set.seed(2402)
df_sus_final_kmeans6 <- kmeans(df_sus_final_var, 6)
round(df_sus_final_kmeans6$centers, 3)
table(df_sus_final_kmeans6$cluster)

## K-medoids ----------------------------------------------------------------
### Gráficos do cotovelo
fviz_nbclust(df_ans_inicio_var, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (ANS de 2017 a 2019)"
  )  # k = 6

fviz_nbclust(df_ans_final_var, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (ANS de 2022 a 2024)"
  ) # k = 6 ou k = 9

fviz_nbclust(df_sus_inicio_var, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (SUS de 2015 a 2017)"
  ) # k = 5 ou k = 6


fviz_nbclust(df_sus_final_var, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (SUS de 2022 a 2024)"
  ) # k = 5 ou k = 6

set.seed(2402)
df_ans_inicio_pam6 <- cluster::pam(df_ans_inicio_var, 6)
round(df_ans_inicio_pam6$medoids, 3)
table(df_ans_inicio_pam6$cluster)

set.seed(2402)
df_ans_inicio_pam5 <- cluster::pam(df_ans_inicio_var, 5)
round(df_ans_inicio_pam5$medoids, 3)
table(df_ans_inicio_pam5$cluster)

set.seed(2402)
df_ans_final_pam6 <- cluster::pam(df_ans_final_var, 6)
round(df_ans_final_pam6$medoids, 3)
table(df_ans_final_pam6$cluster)

set.seed(2402)
df_ans_final_pam9 <- cluster::pam(df_ans_final_var, 9)
round(df_ans_final_pam9$medoids, 3)
table(df_ans_final_pam9$cluster)

set.seed(2402)
df_sus_inicio_pam5 <- cluster::pam(df_sus_inicio_var, 5)
round(df_sus_inicio_pam5$medoids, 3)
table(df_sus_inicio_pam5$cluster)

set.seed(2402)
df_sus_inicio_pam6 <- cluster::pam(df_sus_inicio_var, 6)
round(df_sus_inicio_pam6$medoids, 3)
table(df_sus_inicio_pam6$cluster)

set.seed(2402)
df_sus_final_pam5 <- cluster::pam(df_sus_final_var, 5)
round(df_sus_final_pam5$medoids, 3)
table(df_sus_final_pam5$cluster)

set.seed(2402)
df_sus_final_pam6 <- cluster::pam(df_sus_final_var, 6)
round(df_sus_final_pam6$medoids, 3)
table(df_sus_final_pam6$cluster)


## Ajustando métodos hierárquicos -----------------------------------------
### Método de Ward
ans_inicio_ward <- hclust(df_ans_inicio_dist, method = "ward.D2")
plot(as.dendrogram(ans_inicio_ward), main = "Dendrograma do método de Ward (ANS 2017 a 2019)", ylab = "Altura")
rect.hclust(ans_inicio_ward, k = 3, border = 2:5)
rect.hclust(ans_inicio_ward, k = 4, border = 2:5)

ans_inicio_ward3_class <- cutree(ans_inicio_ward, k = 3)
table(ans_inicio_ward3_class)
ans_inicio_ward4_class <- cutree(ans_inicio_ward, k = 4)
table(ans_inicio_ward4_class)

ans_final_ward <- hclust(df_ans_final_dist, method = "ward.D2")
plot(as.dendrogram(ans_final_ward), main = "Dendrograma do método de Ward (ANS 2022 a 2024)", ylab = "Altura")
rect.hclust(ans_final_ward, k = 3, border = 2:5)
rect.hclust(ans_final_ward, k = 4, border = 2:5)
ans_final_ward3_class <- cutree(ans_final_ward, k = 3)
ans_final_ward4_class <- cutree(ans_final_ward, k = 4)
table(ans_final_ward3_class)
table(ans_final_ward4_class)


sus_inicio_ward <- hclust(df_sus_inicio_dist, method = "ward.D2")
plot(as.dendrogram(sus_inicio_ward), main = "Dendrograma do método de Ward (SUS 2015 a 2017)", ylab = "Altura")
rect.hclust(sus_inicio_ward, k = 3, border = 2:5)
sus_inicio_ward3_class <- cutree(sus_inicio_ward, k = 3)
table(sus_inicio_ward3_class)
rect.hclust(sus_inicio_ward, k = 4, border = 2:5)
sus_inicio_ward4_class <- cutree(sus_inicio_ward, k = 4)
table(sus_inicio_ward4_class)

sus_final_ward <- hclust(df_sus_final_dist, method = "ward.D2")
plot(as.dendrogram(sus_final_ward), main = "Dendrograma do método de Ward (SUS 2022 a 2024)", ylab = "Altura")
rect.hclust(sus_final_ward, k = 3, border = 2:5)
sus_final_ward3_class <- cutree(sus_final_ward, k = 3)
table(sus_final_ward3_class)
rect.hclust(sus_final_ward, k = 4, border = 2:5)
sus_final_ward4_class <- cutree(sus_final_ward, k = 4)
table(sus_final_ward4_class)

### Single linkage
ans_inicio_single <- hclust(df_ans_inicio_dist, method = "single")
plot(as.dendrogram(ans_inicio_single), main = "Dendrograma do método de single linkage (ANS 2017 a 2019)", ylab = "Altura")
rect.hclust(ans_inicio_single, k = 3, border = 2:5)

ans_final_single <- hclust(df_ans_final_dist, method = "single")
plot(as.dendrogram(ans_final_single), main = "Dendrograma do método de single linkage (ANS 2022 a 2024)", ylab = "Altura")

sus_inicio_single <- hclust(df_sus_inicio_dist, method = "single")
plot(as.dendrogram(sus_inicio_single), main = "Dendrograma do método de single linkage (SUS 2015 a 2017)", ylab = "Altura")
rect.hclust(sus_inicio_single, k = 3, border = 2:5)

sus_final_single <- hclust(df_sus_final_dist, method = "single")
plot(as.dendrogram(sus_final_single), main = "Dendrograma do método de single linkage (SUS 2022 a 2024)", ylab = "Altura")
rect.hclust(sus_final_single, k = 3, border = 2:5)

# conclusão - nenhum dos single linkage ficou bom

### Complete linkage
ans_inicio_complete <- hclust(df_ans_inicio_dist, method = "complete")
plot(as.dendrogram(ans_inicio_complete), main = "Dendrograma do método de complete linkage (ANS 2017 a 2019)", ylab = "Altura")
rect.hclust(ans_inicio_complete, k = 4, border = 2:5)
ans_inicio_complete4_class <- cutree(ans_inicio_complete, k = 4)
table(ans_inicio_complete4_class) # um cluster concentrou quase todos os municípios

ans_final_complete <- hclust(df_ans_final_dist, method = "complete")
plot(as.dendrogram(ans_final_complete), main = "Dendrograma do método de complete linkage (ANS 2022 a 2024)", ylab = "Altura")
rect.hclust(ans_final_complete, k = 3, border = 2:5)
ans_final_complete3_class <- cutree(ans_final_complete, k = 3)
table(ans_final_complete3_class) # um cluster concentrou quase todos os municípios

sus_inicio_complete <- hclust(df_sus_inicio_dist, method = "complete")
plot(as.dendrogram(sus_inicio_complete), main = "Dendrograma do método de complete linkage (SUS 2015 a 2017)", ylab = "Altura")
rect.hclust(sus_inicio_complete, k = 4, border = 2:5)
sus_inicio_complete4_class <- cutree(sus_inicio_complete, k = 4)
table(sus_inicio_complete4_class) # dois cluster concentram quase todos os municípios

sus_final_complete <- hclust(df_sus_final_dist, method = "complete")
plot(as.dendrogram(sus_final_complete), main = "Dendrograma do método de complete linkage (SUS 2022 a 2024)", ylab = "Altura")
rect.hclust(sus_final_complete, k = 4, border = 2:5)
sus_final_complete4_class <- cutree(sus_final_complete, k = 4)
table(sus_final_complete4_class) # um cluster concentra quase todos os municípios

# conclusão - nenhum dos complete linkage ficou bom

### Average linkage
ans_inicio_average <- hclust(df_ans_inicio_dist, method = "average")
plot(as.dendrogram(ans_inicio_average), main = "Dendrograma do método de average linkage (ANS 2017 a 2019)", ylab = "Altura")
rect.hclust(ans_inicio_average, k = 4, border = 2:5)

ans_final_average <- hclust(df_ans_final_dist, method = "average")
plot(as.dendrogram(ans_final_average), main = "Dendrograma do método de average linkage (ANS 2022 a 2024)", ylab = "Altura")

sus_inicio_average <- hclust(df_sus_inicio_dist, method = "average")
plot(as.dendrogram(sus_inicio_average), main = "Dendrograma do método de average linkage (SUS 2015 a 2017)", ylab = "Altura")

sus_final_average <- hclust(df_sus_final_dist, method = "average")
plot(as.dendrogram(sus_final_average), main = "Dendrograma do método de average linkage (SUS 2022 a 2024)", ylab = "Altura")
rect.hclust(sus_final_average, k = 4, border = 2:5)
sus_final_average4_class <- cutree(sus_final_average, k = 4)
table(sus_final_average4_class) # um cluster concentra quase todos os municípios

## Escolhendo o melhor método para cada análise ---------------------------
### ANS 2017 a 2019  ----------------------------------------------------------
ls()[grep("^df_ans_inicio_kmeans", ls())]
ans_inicio_kmeans_index <- data.frame(
  metodo = unlist(lapply(4:6, function(i) paste0("kmeans", i))),
  db_index_cent = unlist(lapply(4:6, function(i) index.DB(df_ans_inicio_var, get(paste0("df_ans_inicio_kmeans", i))$cluster)$DB)),
  db_index_med = unlist(lapply(4:6, function(i) index.DB(df_ans_inicio_var, get(paste0("df_ans_inicio_kmeans", i))$cluster, d = df_ans_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(4:6, function(i) dunn(distance = df_ans_inicio_dist, get(paste0("df_ans_inicio_kmeans", i))$cluster))),
  silh_index = unlist(lapply(4:6, function(i) index.S(df_ans_inicio_dist, get(paste0("df_ans_inicio_kmeans", i))$cluster))),
  ch_index_cent = unlist(lapply(4:6, function(i) index.G1(df_ans_inicio_var, get(paste0("df_ans_inicio_kmeans", i))$cluster))),
  ch_index_med = unlist(lapply(4:6, function(i) index.G1(df_ans_inicio_var, get(paste0("df_ans_inicio_kmeans", i))$cluster, d = df_ans_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^df_ans_inicio_pam", ls())]
ans_inicio_pam_index <- data.frame(
  metodo = unlist(lapply(5:6, function(i) paste0("pam", i))),
  db_index_cent = unlist(lapply(5:6, function(i) index.DB(df_ans_inicio_var, get(paste0("df_ans_inicio_pam", i))$cluster)$DB)),
  db_index_med = unlist(lapply(5:6, function(i) index.DB(df_ans_inicio_var, get(paste0("df_ans_inicio_pam", i))$cluster, d = df_ans_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(5:6, function(i) dunn(distance = df_ans_inicio_dist, get(paste0("df_ans_inicio_pam", i))$cluster))),
  silh_index = unlist(lapply(5:6, function(i) index.S(df_ans_inicio_dist, get(paste0("df_ans_inicio_pam", i))$cluster))),
  ch_index_cent = unlist(lapply(5:6, function(i) index.G1(df_ans_inicio_var, get(paste0("df_ans_inicio_pam", i))$cluster))),
  ch_index_med = unlist(lapply(5:6, function(i) index.G1(df_ans_inicio_var, get(paste0("df_ans_inicio_pam", i))$cluster, d = df_ans_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^ans_inicio_ward", ls())]
ans_inicio_ward_index <- data.frame(
  metodo = unlist(lapply(3:4, function(i) paste0("ward", i))),
  db_index_cent = unlist(lapply(3:4, function(i) index.DB(df_ans_inicio_var, get(paste0("ans_inicio_ward", i, "_class")))$DB)),
  db_index_med = unlist(lapply(3:4, function(i) index.DB(df_ans_inicio_var, get(paste0("ans_inicio_ward", i, "_class")), d = df_ans_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:4, function(i) dunn(distance = df_ans_inicio_dist, get(paste0("ans_inicio_ward", i, "_class"))))),
  silh_index = unlist(lapply(3:4, function(i) index.S(df_ans_inicio_dist, get(paste0("ans_inicio_ward", i, "_class"))))),
  ch_index_cent = unlist(lapply(3:4, function(i) index.G1(df_ans_inicio_var, get(paste0("ans_inicio_ward", i, "_class"))))),
  ch_index_med = unlist(lapply(3:4, function(i) index.G1(df_ans_inicio_var, get(paste0("ans_inicio_ward", i, "_class")), d = df_ans_inicio_dist, centrotypes = "medoids")))
)

ans_inicio_avaliacao <- rbind(
  ans_inicio_pam_index,
  ans_inicio_kmeans_index, 
  ans_inicio_ward_index
)

#### db_index_cent: menor melhor (pam6, seguido de kmeans4 e ward4)
#### db_index_med: menor melhor (pam6, seguido de ward4 e kmeans4)
#### dunn_index: maior melhor (ward3, seguido de kmeans5 e kmeans4)
#### silh_index: maior melhor (ward3, seguido de kmeans3 e kmeans4)
#### ch_index_cent: maior melhor (kmeans5, seguido de kmeans4 e ward4)
#### ch_index_med: maior melhor (kmeans5, seguido de kmeans4 e ward4)

#### Escolha: K-means 5 foi bem nas métricas em geral e as classes não estão tão desequilibradas, a maioria dos métodos teve classe com poucos município.
table(df_ans_inicio_kmeans5$cluster) # cluster 4 com poucos municípios
table(df_ans_inicio_kmeans4$cluster) # cluster 4 com poucos municípios
table(ans_inicio_ward3_class) # cluster 2 concentra a maioria dos município
table(df_ans_inicio_pam6$cluster)


df_ans_inicio$cluster_ans_inicio <- as.factor(df_ans_inicio_kmeans5$cluster)

### ANS 2022 a 2024 ----------------------------------------------------------
ls()[grep("^df_ans_final_kmeans", ls())]
ans_final_kmeans_index <- data.frame(
  metodo = unlist(lapply(4:6, function(i) paste0("kmeans", i))),
  db_index_cent = unlist(lapply(4:6, function(i) index.DB(df_ans_final_var, get(paste0("df_ans_final_kmeans", i))$cluster)$DB)),
  db_index_med = unlist(lapply(4:6, function(i) index.DB(df_ans_final_var, get(paste0("df_ans_final_kmeans", i))$cluster, d = df_ans_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(4:6, function(i) dunn(distance = df_ans_final_dist, get(paste0("df_ans_final_kmeans", i))$cluster))),
  silh_index = unlist(lapply(4:6, function(i) index.S(df_ans_final_dist, get(paste0("df_ans_final_kmeans", i))$cluster))),
  ch_index_cent = unlist(lapply(4:6, function(i) index.G1(df_ans_final_var, get(paste0("df_ans_final_kmeans", i))$cluster))),
  ch_index_med = unlist(lapply(4:6, function(i) index.G1(df_ans_final_var, get(paste0("df_ans_final_kmeans", i))$cluster, d = df_ans_final_dist, centrotypes = "medoids")))
)

ls()[grep("^df_ans_final_pam", ls())]
ans_final_pam_index <- data.frame(
  metodo = unlist(lapply(c(6, 9), function(i) paste0("pam", i))),
  db_index_cent = unlist(lapply(c(6, 9), function(i) index.DB(df_ans_final_var, get(paste0("df_ans_final_pam", i))$cluster)$DB)),
  db_index_med = unlist(lapply(c(6, 9), function(i) index.DB(df_ans_final_var, get(paste0("df_ans_final_pam", i))$cluster, d = df_ans_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(c(6, 9), function(i) dunn(distance = df_ans_final_dist, get(paste0("df_ans_final_pam", i))$cluster))),
  silh_index = unlist(lapply(c(6, 9), function(i) index.S(df_ans_final_dist, get(paste0("df_ans_final_pam", i))$cluster))),
  ch_index_cent = unlist(lapply(c(6, 9), function(i) index.G1(df_ans_final_var, get(paste0("df_ans_final_pam", i))$cluster))),
  ch_index_med = unlist(lapply(c(6, 9), function(i) index.G1(df_ans_final_var, get(paste0("df_ans_final_pam", i))$cluster, d = df_ans_final_dist, centrotypes = "medoids")))
)

ls()[grep("^ans_final_ward", ls())]
ans_final_ward_index <- data.frame(
  metodo = unlist(lapply(3:4, function(i) paste0("ward", i))),
  db_index_cent = unlist(lapply(3:4, function(i) index.DB(df_ans_final_var, get(paste0("ans_final_ward", i, "_class")))$DB)),
  db_index_med = unlist(lapply(3:4, function(i) index.DB(df_ans_final_var, get(paste0("ans_final_ward", i, "_class")), d = df_ans_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:4, function(i) dunn(distance = df_ans_final_dist, get(paste0("ans_final_ward", i, "_class"))))),
  silh_index = unlist(lapply(3:4, function(i) index.S(df_ans_final_dist, get(paste0("ans_final_ward", i, "_class"))))),
  ch_index_cent = unlist(lapply(3:4, function(i) index.G1(df_ans_final_var, get(paste0("ans_final_ward", i, "_class"))))),
  ch_index_med = unlist(lapply(3:4, function(i) index.G1(df_ans_final_var, get(paste0("ans_final_ward", i, "_class")), d = df_ans_final_dist, centrotypes = "medoids")))
)

ans_final_avaliacao <- rbind(
  ans_final_pam_index,
  ans_final_kmeans_index, 
  ans_final_ward_index
)

#### db_index_cent: menor melhor (kmeans5, seguido de pam9 e ward4)
#### db_index_med: menor melhor (ward4, seguido de pam9 e ward3)
#### dunn_index: maior melhor (kmeans5, seguido de pam9 e ward3)
#### silh_index: maior melhor (ward3, seguido de kmeans3 e pam6)
#### ch_index_cent: maior melhor (kmeans5, seguido de kmeans4 e kmeans3)
#### ch_index_med: maior melhor (kmeans5, seguido de kmeans4 e pam9)

#### Escolha: K-means 5 foi bem nas métricas em geral e as classes não estão tão desequilibradas, a maioria dos métodos teve classe com poucos município.
table(df_ans_final_kmeans5$cluster) # cluster 3 com apenas 12 municípios

df_ans_final$cluster_ans_final <- as.factor(df_ans_final_kmeans5$cluster)

### SUS 2015 a 2017  ----------------------------------------------------------
ls()[grep("^df_sus_inicio_kmeans", ls())]
sus_inicio_kmeans_index <- data.frame(
  metodo = unlist(lapply(4:6, function(i) paste0("kmeans", i))),
  db_index_cent = unlist(lapply(4:6, function(i) index.DB(df_sus_inicio_var, get(paste0("df_sus_inicio_kmeans", i))$cluster)$DB)),
  db_index_med = unlist(lapply(4:6, function(i) index.DB(df_sus_inicio_var, get(paste0("df_sus_inicio_kmeans", i))$cluster, d = df_sus_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(4:6, function(i) dunn(distance = df_sus_inicio_dist, get(paste0("df_sus_inicio_kmeans", i))$cluster))),
  silh_index = unlist(lapply(4:6, function(i) index.S(df_sus_inicio_dist, get(paste0("df_sus_inicio_kmeans", i))$cluster))),
  ch_index_cent = unlist(lapply(4:6, function(i) index.G1(df_sus_inicio_var, get(paste0("df_sus_inicio_kmeans", i))$cluster))),
  ch_index_med = unlist(lapply(4:6, function(i) index.G1(df_sus_inicio_var, get(paste0("df_sus_inicio_kmeans", i))$cluster, d = df_sus_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^df_sus_inicio_pam", ls())]
sus_inicio_pam_index <- data.frame(
  metodo = unlist(lapply(5:6, function(i) paste0("pam", i))),
  db_index_cent = unlist(lapply(5:6, function(i) index.DB(df_sus_inicio_var, get(paste0("df_sus_inicio_pam", i))$cluster)$DB)),
  db_index_med = unlist(lapply(5:6, function(i) index.DB(df_sus_inicio_var, get(paste0("df_sus_inicio_pam", i))$cluster, d = df_sus_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(5:6, function(i) dunn(distance = df_sus_inicio_dist, get(paste0("df_sus_inicio_pam", i))$cluster))),
  silh_index = unlist(lapply(5:6, function(i) index.S(df_sus_inicio_dist, get(paste0("df_sus_inicio_pam", i))$cluster))),
  ch_index_cent = unlist(lapply(5:6, function(i) index.G1(df_sus_inicio_var, get(paste0("df_sus_inicio_pam", i))$cluster))),
  ch_index_med = unlist(lapply(5:6, function(i) index.G1(df_sus_inicio_var, get(paste0("df_sus_inicio_pam", i))$cluster, d = df_sus_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^sus_inicio_ward", ls())]
sus_inicio_ward_index <- data.frame(
  metodo = unlist(lapply(3:4, function(i) paste0("ward", i))),
  db_index_cent = unlist(lapply(3:4, function(i) index.DB(df_sus_inicio_var, get(paste0("sus_inicio_ward", i, "_class")))$DB)),
  db_index_med = unlist(lapply(3:4, function(i) index.DB(df_sus_inicio_var, get(paste0("sus_inicio_ward", i, "_class")), d = df_sus_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:4, function(i) dunn(distance = df_sus_inicio_dist, get(paste0("sus_inicio_ward", i, "_class"))))),
  silh_index = unlist(lapply(3:4, function(i) index.S(df_sus_inicio_dist, get(paste0("sus_inicio_ward", i, "_class"))))),
  ch_index_cent = unlist(lapply(3:4, function(i) index.G1(df_sus_inicio_var, get(paste0("sus_inicio_ward", i, "_class"))))),
  ch_index_med = unlist(lapply(3:4, function(i) index.G1(df_sus_inicio_var, get(paste0("sus_inicio_ward", i, "_class")), d = df_sus_inicio_dist, centrotypes = "medoids")))
)

sus_inicio_avaliacao <- rbind(
  sus_inicio_pam_index,
  sus_inicio_kmeans_index, 
  sus_inicio_ward_index
)

#### db_index_cent: menor melhor (ward4, seguido de pam5 e kmeans5)
#### db_index_med: menor melhor (ward4, seguido de pam6 e pam5)
#### dunn_index: maior melhor (kmeans5, seguido de kmeans3 e pam3)
#### silh_index: maior melhor (kmeans5, seguido de ward4 e pam5)
#### ch_index_cent: maior melhor (kmeans5, seguido de pam6 e pam5)
#### ch_index_med: maior melhor (kmeans5, seguido de pam6 e pam5)

#### Escolha: K-means 5 tem boas métricas e clusters equilibrados.
table(df_sus_inicio_kmeans5$cluster)
table(df_sus_inicio_pam5$cluster)
table(df_sus_inicio_pam6$cluster)


df_sus_inicio$cluster_sus_inicio <- as.factor(df_sus_inicio_kmeans5$cluster)

### SUS 2022 a 2024 ----------------------------------------------------------
ls()[grep("^df_sus_final_kmeans", ls())]
sus_final_kmeans_index <- data.frame(
  metodo = unlist(lapply(4:6, function(i) paste0("kmeans", i))),
  db_index_cent = unlist(lapply(4:6, function(i) index.DB(df_sus_final_var, get(paste0("df_sus_final_kmeans", i))$cluster)$DB)),
  db_index_med = unlist(lapply(4:6, function(i) index.DB(df_sus_final_var, get(paste0("df_sus_final_kmeans", i))$cluster, d = df_sus_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(4:6, function(i) dunn(distance = df_sus_final_dist, get(paste0("df_sus_final_kmeans", i))$cluster))),
  silh_index = unlist(lapply(4:6, function(i) index.S(df_sus_final_dist, get(paste0("df_sus_final_kmeans", i))$cluster))),
  ch_index_cent = unlist(lapply(4:6, function(i) index.G1(df_sus_final_var, get(paste0("df_sus_final_kmeans", i))$cluster))),
  ch_index_med = unlist(lapply(4:6, function(i) index.G1(df_sus_final_var, get(paste0("df_sus_final_kmeans", i))$cluster, d = df_sus_final_dist, centrotypes = "medoids")))
)

ls()[grep("^df_sus_final_pam", ls())]
sus_final_pam_index <- data.frame(
  metodo = unlist(lapply(5:6, function(i) paste0("pam", i))),
  db_index_cent = unlist(lapply(5:6, function(i) index.DB(df_sus_final_var, get(paste0("df_sus_final_pam", i))$cluster)$DB)),
  db_index_med = unlist(lapply(5:6, function(i) index.DB(df_sus_final_var, get(paste0("df_sus_final_pam", i))$cluster, d = df_sus_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(5:6, function(i) dunn(distance = df_sus_final_dist, get(paste0("df_sus_final_pam", i))$cluster))),
  silh_index = unlist(lapply(5:6, function(i) index.S(df_sus_final_dist, get(paste0("df_sus_final_pam", i))$cluster))),
  ch_index_cent = unlist(lapply(5:6, function(i) index.G1(df_sus_final_var, get(paste0("df_sus_final_pam", i))$cluster))),
  ch_index_med = unlist(lapply(5:6, function(i) index.G1(df_sus_final_var, get(paste0("df_sus_final_pam", i))$cluster, d = df_sus_final_dist, centrotypes = "medoids")))
)

ls()[grep("^sus_final_ward", ls())]
sus_final_ward_index <- data.frame(
  metodo = unlist(lapply(3:4, function(i) paste0("ward", i))),
  db_index_cent = unlist(lapply(3:4, function(i) index.DB(df_sus_final_var, get(paste0("sus_final_ward", i, "_class")))$DB)),
  db_index_med = unlist(lapply(3:4, function(i) index.DB(df_sus_final_var, get(paste0("sus_final_ward", i, "_class")), d = df_sus_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:4, function(i) dunn(distance = df_sus_final_dist, get(paste0("sus_final_ward", i, "_class"))))),
  silh_index = unlist(lapply(3:4, function(i) index.S(df_sus_final_dist, get(paste0("sus_final_ward", i, "_class"))))),
  ch_index_cent = unlist(lapply(3:4, function(i) index.G1(df_sus_final_var, get(paste0("sus_final_ward", i, "_class"))))),
  ch_index_med = unlist(lapply(3:4, function(i) index.G1(df_sus_final_var, get(paste0("sus_final_ward", i, "_class")), d = df_sus_final_dist, centrotypes = "medoids")))
)

sus_final_avaliacao <- rbind(
  sus_final_pam_index,
  sus_final_kmeans_index, 
  sus_final_ward_index
)

#### db_index_cent: menor melhor (ward4, seguido de pam5, kmeans5)
#### db_index_med: menor melhor (ward4, seguido de pam5, kmeans5)
#### dunn_index: maior melhor (kmeans5, seguido de pam6 e pam5)
#### silh_index: maior melhor (kmeans5, seguido de ward4, pam5)
#### ch_index_cent: maior melhor (kmeans5, seguido de pam6, pam5)
#### ch_index_med: maior melhor (kmeans5, seguido de pam6 e pam5)

#### Escolha: K-means 5 foi bem nas métricas em geral e as classes não estão tão desequilibradas, a maioria dos métodos teve classe com poucos município.
table(df_sus_final_kmeans5$cluster)

df_sus_final$cluster_sus_final <- as.factor(df_sus_final_kmeans$cluster)


# Comparando os grupos  ---------------
#Criando as funções que serão utilizadas para a construção das tabelas e boxplots

cria_tabelas <- function(variaveis = nomes_variaveis, df = df_indicadores, var_grupos, atendimento) {
  grupos <- levels(df[[var_grupos]])
  
  for (variavel in variaveis) {
    tabela <- kable(
      data.frame(
        grupo = grupos,
        n = unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> nrow())),
        minimo = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> min(na.rm = TRUE))), 2),
        primeiro_qt = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> quantile(0.25, na.rm = TRUE))), 2),
        media = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> mean(na.rm = TRUE))), 2),
        mediana = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> median(na.rm = TRUE))), 2),
        dp = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> sd(na.rm = TRUE))), 2),
        terceiro_qt = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> quantile(0.75, na.rm = TRUE))), 2),
        maximo = round(unlist(lapply(grupos, function(grupo) df[df[[var_grupos]] == grupo, ] |> pull(variavel) |> max(na.rm = TRUE))), 2)
      ),
      align = "cccccccc",
      col.names = c("Grupo (cluster)", "n", "Mín.", "1º Quartil", "Média", "Mediana", "D.P.", "3º Quartil", "Máx."),
      caption = HTML(ifelse(variavel == variaveis[1], paste0(glue::glue("Medidas resumo para os grupos de municípios agrupados pelos indicadores de aborto inseguro ({atendimento}). <br><br>"), variavel), variavel))    )
    
    print(tabela)
  }
}

cria_tabelas(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_ans_inicio, var_grupos = "cluster_ans_inicio", atendimento = "saúde suplementar 2017 a 2019")

cria_tabelas(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_ans_final, var_grupos = "cluster_ans_final", atendimento = "saúde suplementar 2022 a 2024")

cria_tabelas(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_sus_inicio, var_grupos = "cluster_sus_inicio", atendimento = "SUS 2015 a 2017")

cria_tabelas(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_sus_final, var_grupos = "cluster_sus_final", atendimento = "SUS 2022 a 2024")

cria_boxplots <- function(variaveis, df, var_grupos) {
  boxplots <- list()
  
  for (i in 1:length(variaveis)) {
    variavel <- variaveis[i]
    
    boxplots[[i]] <- ggplot(df) +
      geom_boxplot(aes(.data[[var_grupos]], .data[[variavel]]), fill = "lightblue") +
      labs(y = variavel, x = "Grupo") +
      ggtitle(variavel) +
      theme_classic() + 
      theme(legend.position = "top") +
      theme(plot.title = element_text(size = 13), axis.text.x = element_text(size = 12), legend.text = element_text(size = 11), plot.margin = unit(c(1, 0, 0, 0.5), "cm"))
  }
  
  grid.arrange(
    grobs = matrix(boxplots, nrow = 1, byrow = FALSE),
    ncol = 3
  )
}

cria_boxplots(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
              df = df_ans_inicio, var_grupos = "cluster_ans_inicio")
cria_boxplots(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
              df = df_ans_final, var_grupos = "cluster_ans_final")
cria_boxplots(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
              df = df_sus_inicio, var_grupos = "cluster_sus_inicio")
cria_boxplots(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
              df = df_sus_final, var_grupos = "cluster_sus_final")


cria_tabelas_testes <- function(variaveis, df, var_grupos, atendimento) {
  for (variavel in variaveis) {
    
    formula <- as.formula(glue::glue("{variavel} ~ {var_grupos}"))
    resultados_dunn <- dunn_test(data = df, formula = formula, p.adjust.method = "bonferroni")
    
    # Calcula a medida de efeito d de Cohen
    efeito_d <- cohens_d(data = df, formula = formula)
    
    monta_linhas <- function(num_grupo) {
      paste(
        -1 * round(resultados_dunn$statistic[which(resultados_dunn$group1 == unique(resultados_dunn$group1)[num_grupo])], 3),
        "(Z) <br>",
        ifelse(
          round(resultados_dunn$p.adj[which(resultados_dunn$group1 == unique(resultados_dunn$group1)[num_grupo])], 3) < 0.05,
          ifelse(
            round(resultados_dunn$p.adj[which(resultados_dunn$group1 == unique(resultados_dunn$group1)[num_grupo])], 3) == 0, 
            "< 0.001*",
            paste0(round(resultados_dunn$p.adj[which(resultados_dunn$group1 == unique(resultados_dunn$group1)[num_grupo])], 3), "*")
          ),
          round(resultados_dunn$p.adj[which(resultados_dunn$group1 == unique(resultados_dunn$group1)[num_grupo])], 3)
        ),
        "(valor-p) <br>",
        round(efeito_d$effsize[efeito_d$group1 == unique(efeito_d$group1)[num_grupo]], 3),
        "(D de Cohen)"
      )
    }
    
      df_teste <- data.frame(
        grupo1 = monta_linhas(1),
        grupo2 = c(rep("", 1), monta_linhas(2)),
        grupo3 = c(rep("", 2), monta_linhas(3)),
        row.names = unique(resultados_dunn$group2)
      )
      
      #colnames(df_teste) <- unique(resultados_dunn$group1)
      
      tabela <- kable(
        df_teste,
        align = c("ccc"),
        caption = HTML(ifelse(variavel == variaveis[1],
                              paste0(glue::glue("Resultados dos testes de Dunn (com correção de Bonferroni) para as comparações múltiplas entre os pares de grupos de municípios agrupados pelos indicadores de aborto inseguro {atendimento}. <br><br>"), variavel), variavel))      )
    
    print(tabela)
  }
}

cria_tabelas_testes(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_ans_inicio, var_grupos = "cluster_ans_inicio", atendimento = "saúde suplementar 2017 a 2019")

cria_tabelas_testes(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_ans_final, var_grupos = "cluster_ans_final", atendimento = "saúde suplementar 2022 a 2024")

cria_tabelas_testes(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_sus_inicio, var_grupos = "cluster_sus_inicio", atendimento = "SUS 2015 a 2017")

cria_tabelas_testes(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_sus_final, var_grupos = "cluster_sus_final", atendimento = "SUS 2022 a 2024")


# Mapas do Brasil dos clusters --------------------------------------------
## Baixando os dados de geometria
df_muni_sf <- read_municipality(year = 2020, showProgress = FALSE) |>
  mutate(codmunres = as.numeric(substr(code_muni, 1, 6)))

df_ufs_sf <- read_state(year = 2020, showProgress = FALSE)

get_dupes(df_muni_sf, codmunres)

## Juntando os dois dataframes
df_mapa_ans_inicio <- left_join(df_ans_inicio, df_muni_sf) |>
  st_as_sf()

## Criando os mapas
ggplot() +
  geom_sf(data = df_mapa_ans_inicio, aes(fill = cluster_ans_inicio), color = NA) +
  labs(title = "Mapa do Brasil com os grupos de municípios agrupados pelos inidicaores de aborto inseguro  \n na saúde suplementar dos anos de 2017 a 2019") +
  scale_fill_viridis_d(name = "Grupo", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw()

## Juntando os dois dataframes
df_mapa_ans_final <- left_join(df_ans_final, df_muni_sf) |>
  st_as_sf()

## Criando os mapas
ggplot() +
  geom_sf(data = df_mapa_ans_final, aes(fill = cluster_ans_final), color = NA) +
  labs(title = "Mapa do Brasil com os grupos de municípios agrupados pelos inidicaores de aborto inseguro  \n na saúde suplementar dos anos de 2022 a 2024") +
  scale_fill_viridis_d(name = "Grupo", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw()

## Juntando os dois dataframes
df_mapa_sus_inicio <- left_join(df_sus_inicio, df_muni_sf) |>
  st_as_sf()

## Criando os mapas
ggplot() +
  geom_sf(data = df_mapa_sus_inicio, aes(fill = cluster_sus_inicio), color = NA) +
  labs(title = "Mapa do Brasil com os grupos de municípios agrupados pelos inidicaores de aborto inseguro  \n no SUS dos anos de 2015 a 2017") +
  scale_fill_viridis_d(name = "Grupo", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw()

## Juntando os dois dataframes
df_mapa_sus_final <- left_join(df_sus_final, df_muni_sf) |>
  st_as_sf()

## Criando os mapas
ggplot() +
  geom_sf(data = df_mapa_sus_final, aes(fill = cluster_sus_final), color = NA) +
  labs(title = "Mapa do Brasil com os grupos de municípios agrupados pelos inidicaores de aborto inseguro  \n no SUS dos anos de 2022 a 2024") +
  scale_fill_viridis_d(name = "Grupo", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw()





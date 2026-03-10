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
library(writexl)
library(glue)
library(patchwork)

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
p1 <- fviz_nbclust(df_ans_inicio_var, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (ANS de 2017 a 2019)"
  )  # k = 4, 5 ou 6
p1
ggsave("figuras/clustering/elbow_kmeans_ans_inicio.png", p1, width = 10, height = 6)

p2 <- fviz_nbclust(df_ans_final_var, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (ANS de 2022 a 2024)"
  ) # k = 4, 5 ou 6
p2
ggsave("figuras/clustering/elbow_kmeans_ans_final.png", p2, width = 10, height = 6)


p3 <- fviz_nbclust(df_sus_inicio_var, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (SUS de 2015 a 2017)"
  ) # k = 4, 5 ou k = 6
p3
ggsave("figuras/clustering/elbow_kmeans_sus_inicio.png", p3, width = 10, height = 6)



p4 <- fviz_nbclust(df_sus_final_var, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (SUS de 2022 a 2024)"
  ) # k = 4 k = 5, 6
p4
ggsave("figuras/clustering/elbow_kmeans_sus_final.png", width = 10, height = 6)


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
### Gráficos de cotovelo
p5 <- fviz_nbclust(df_ans_inicio_var, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (ANS de 2017 a 2019)"
  )  # k = 6
p5
ggsave("figuras/clustering/elbow_kmedoids_ans_inicio.png", p5, width = 10, height = 6)


p6 <- fviz_nbclust(df_ans_final_var, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (ANS de 2022 a 2024)"
  ) # k = 6, 7
p6
ggsave("figuras/clustering/elbow_kmedoids_ans_final.png", p6, width = 10, height = 6)


p7 <- fviz_nbclust(df_sus_inicio_var, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (SUS de 2015 a 2017)"
  ) # k = 5 ou k = 6
p7
ggsave("figuras/clustering/elbow_kmedoids_sus_inicio.png", p7,  width = 10, height = 6)

p8 <- fviz_nbclust(df_sus_final_var, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (SUS de 2022 a 2024)"
  ) # k = 6 ou k = 7
p8
ggsave("figuras/clustering/elbow_kmedoids_sus_final.png", p8, width = 10, height = 6)


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
df_ans_final_pam7 <- cluster::pam(df_ans_final_var, 7)
round(df_ans_final_pam7$medoids, 3)
table(df_ans_final_pam7$cluster)

set.seed(2402)
df_sus_inicio_pam5 <- cluster::pam(df_sus_inicio_var, 5)
round(df_sus_inicio_pam5$medoids, 3)
table(df_sus_inicio_pam5$cluster)

set.seed(2402)
df_sus_inicio_pam6 <- cluster::pam(df_sus_inicio_var, 6)
round(df_sus_inicio_pam6$medoids, 3)
table(df_sus_inicio_pam6$cluster)

set.seed(2402)
df_sus_final_pam7 <- cluster::pam(df_sus_final_var, 7)
round(df_sus_final_pam7$medoids, 3)
table(df_sus_final_pam7$cluster)

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

#### db_index_cent: menor melhor (kmeans6, seguido de pam6 e kmeans4)
#### db_index_med: menor melhor (kmeans6, seguido de pam6 e ward4)
#### dunn_index: maior melhor (kmeans6, seguido de ward3 e kmeans5)
#### silh_index: maior melhor (ward3, seguido de kmeans4 e pam6)
#### ch_index_cent: maior melhor (kmeans6, seguido de kmeans5 e kmeans4)
#### ch_index_med: maior melhor (kmeans6, seguido de kmeans5 e kmeans4)

#### Escolha: K-means 5 foi bem nas métricas em geral e as classes não estão tão desequilibradas, a maioria dos métodos teve classe com poucos município.
table(df_ans_inicio_kmeans6$cluster) # clusters 3 e 6 com pouquíssimos municípios (4 e 51)
table(df_ans_inicio_kmeans5$cluster) # cluster 4 com poucos municípios (45)
table(df_ans_inicio_kmeans4$cluster) # cluster 4 com poucos municípios (51)
table(ans_inicio_ward3_class) # quase todos os municípios conecentrados em um único grupo
table(ans_inicio_ward4_class) # maioria dos municípios conecentrados em um único grupo

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
  metodo = unlist(lapply(c(6, 7), function(i) paste0("pam", i))),
  db_index_cent = unlist(lapply(c(6, 7), function(i) index.DB(df_ans_final_var, get(paste0("df_ans_final_pam", i))$cluster)$DB)),
  db_index_med = unlist(lapply(c(6, 7), function(i) index.DB(df_ans_final_var, get(paste0("df_ans_final_pam", i))$cluster, d = df_ans_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(c(6, 7), function(i) dunn(distance = df_ans_final_dist, get(paste0("df_ans_final_pam", i))$cluster))),
  silh_index = unlist(lapply(c(6, 7), function(i) index.S(df_ans_final_dist, get(paste0("df_ans_final_pam", i))$cluster))),
  ch_index_cent = unlist(lapply(c(6, 7), function(i) index.G1(df_ans_final_var, get(paste0("df_ans_final_pam", i))$cluster))),
  ch_index_med = unlist(lapply(c(6, 7), function(i) index.G1(df_ans_final_var, get(paste0("df_ans_final_pam", i))$cluster, d = df_ans_final_dist, centrotypes = "medoids")))
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

#### db_index_cent: menor melhor (kmeans6, seguido de kmeans5 e ward4)
#### db_index_med: menor melhor (ward4, seguido de ward3 e kmeans6)
#### dunn_index: maior melhor (kmeans6, seguido de kmeans5 e ward3)
#### silh_index: maior melhor (ward3, seguido de pam6 e kmeans4)
#### ch_index_cent: maior melhor (kmeans6, seguido de kmeans5 e kmeans4)
#### ch_index_med: maior melhor (kmeans6, seguido de kmeans5 e kmeans4)

#### Escolha: Ward 4 foi bem nas métricas em geral e as classes não estão tão desequilibradas, a maioria dos métodos teve classe com poucos município.
table(df_ans_final_kmeans6$cluster) # cluster 2 com apenas 12 municípios
table(df_ans_final_kmeans5$cluster) # cluster 3 com apenas 12 municípios
table(df_ans_final_kmeans4$cluster) # cluster 4 com apenas 30 municípios, grande concectração de municípios no cluster 3
table(ans_final_ward4_class) # cluster 2 com apenas 30 municípios
table(ans_final_ward3_class) # cluster 3 com apenas 31 municípios, grande concentração de municípios no cluster 1



df_ans_final$cluster_ans_final <- as.factor(ans_final_ward4_class)

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

#### db_index_cent: menor melhor (kmeans4, seguido de ward4 e pam5)
#### db_index_med: menor melhor (kmeans4, seguido de ward4 e pam5)
#### dunn_index: maior melhor (kmeans6, seguido de kmeans5 e pam6)
#### silh_index: maior melhor (kmeans4, seguido de kmeans5 e ward4)
#### ch_index_cent: maior melhor (kmeans6, seguido de kmeans5 e kmeans4)
#### ch_index_med: maior melhor (kmeans6, seguido de kmeans5 e kmeans4)

#### Escolha: K-means 4 tem boas métricas e clusters equilibrados.
table(df_sus_inicio_kmeans4$cluster)
table(df_sus_inicio_kmeans6$cluster)


df_sus_inicio$cluster_sus_inicio <- as.factor(df_sus_inicio_kmeans4$cluster)

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
  metodo = unlist(lapply(6:7, function(i) paste0("pam", i))),
  db_index_cent = unlist(lapply(6:7, function(i) index.DB(df_sus_final_var, get(paste0("df_sus_final_pam", i))$cluster)$DB)),
  db_index_med = unlist(lapply(6:7, function(i) index.DB(df_sus_final_var, get(paste0("df_sus_final_pam", i))$cluster, d = df_sus_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(6:7, function(i) dunn(distance = df_sus_final_dist, get(paste0("df_sus_final_pam", i))$cluster))),
  silh_index = unlist(lapply(6:7, function(i) index.S(df_sus_final_dist, get(paste0("df_sus_final_pam", i))$cluster))),
  ch_index_cent = unlist(lapply(6:7, function(i) index.G1(df_sus_final_var, get(paste0("df_sus_final_pam", i))$cluster))),
  ch_index_med = unlist(lapply(6:7, function(i) index.G1(df_sus_final_var, get(paste0("df_sus_final_pam", i))$cluster, d = df_sus_final_dist, centrotypes = "medoids")))
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

#### db_index_cent: menor melhor (ward4, seguido de kmeans6, kmeans5)
#### db_index_med: menor melhor (ward4, seguido de kmeans6, kmeans5)
#### dunn_index: maior melhor (ward4, seguido de kmeans6 e kmeans5)
#### silh_index: maior melhor (kmeans4, seguido de kmeans5, ward3)
#### ch_index_cent: maior melhor (kmeans6, seguido de kmeans5, kmeans4)
#### ch_index_med: maior melhor (kmeans6, seguido de kmeans5 e kmeans4)

#### Escolha: K-means 5 foi bem nas métricas em geral e as classes estão mais equilibradas.
table(df_sus_final_kmeans6$cluster)
table(sus_final_ward4_class)
table(df_sus_final_kmeans4$cluster)
table(df_sus_final_kmeans5$cluster)



df_sus_final$cluster_sus_final <- as.factor(df_sus_final_kmeans5$cluster)

# Juntando todas as métricas de avaliação para 
ans_inicio_avaliacao$atendimento_periodo <- "ans_inicio"
ans_final_avaliacao$atendimento_periodo <- "ans_final"
sus_inicio_avaliacao$atendimento_periodo <- "sus_inicio"
sus_final_avaliacao$atendimento_periodo <- "sus_final"


df_avaliacao <- rbind(ans_inicio_avaliacao, ans_final_avaliacao, 
                      sus_inicio_avaliacao, sus_final_avaliacao)


write_xlsx(df_avaliacao, "databases/metricas_avaliacao_clusters.xlsx")

## --------- Comparação dos clusters com apenas um dos indicadores para a análise ---------
library(mclust)    
library(aricode)

df_ans_inicio_rz <- df_ans_inicio |> select(c(ans_tx_abortos_cem_nascidos_vivos_valor_medio))
df_ans_final_rz <- df_ans_final |> select(c(ans_tx_abortos_cem_nascidos_vivos_valor_medio))
df_sus_inicio_rz <- df_sus_inicio |> select(c(sus_tx_abortos_cem_nascidos_vivos_valor_medio))
df_sus_final_rz <- df_sus_final |> select(c(sus_tx_abortos_cem_nascidos_vivos_valor_medio))

df_ans_inicio_tx <- df_ans_inicio |> select(c(ans_tx_abortos_mil_mulheres_valor_medio))
df_ans_final_tx <- df_ans_final |> select(c(ans_tx_abortos_mil_mulheres_valor_medio))
df_sus_inicio_tx <- df_sus_inicio |> select(c(sus_tx_abortos_mil_mulheres_valor_medio))
df_sus_final_tx <- df_sus_final |> select(c(sus_tx_abortos_mil_mulheres_valor_medio))

df_ans_inicio_rz_dist <- dist(df_ans_inicio_rz, method = "euclidean")
df_ans_final_rz_dist <- dist(df_ans_final_rz, method = "euclidean")
df_sus_inicio_rz_dist <- dist(df_sus_inicio_rz, method = "euclidean")
df_sus_final_rz_dist <- dist(df_sus_final_rz, method = "euclidean")

df_ans_inicio_tx_dist <- dist(df_ans_inicio_tx, method = "euclidean")
df_ans_final_tx_dist <- dist(df_ans_final_tx, method = "euclidean")
df_sus_inicio_tx_dist <- dist(df_sus_inicio_tx, method = "euclidean")
df_sus_final_tx_dist <- dist(df_sus_final_tx, method = "euclidean")

# Análise de cluster para ANS de 2017 a 2019 - K-means com k=5
set.seed(2402)
df_ans_inicio_kmeans5_rz <- kmeans(df_ans_inicio_rz, 5)
round(df_ans_inicio_kmeans5_rz$centers, 3)
table(df_ans_inicio_kmeans5_rz$cluster)

set.seed(2402)
df_ans_inicio_kmeans5_tx <- kmeans(df_ans_inicio_tx, 5)
round(df_ans_inicio_kmeans5_tx$centers, 3)
table(df_ans_inicio_kmeans5_tx$cluster)

## ---- Comparação das análises: ANS de 2017 a 2019

# Número de municípios em grupos diferentes
table(df_ans_inicio_kmeans5$cluster)
table(df_ans_inicio_kmeans5_rz$cluster)
table(df_ans_inicio_kmeans5_tx$cluster)
# análise conjunta vs apenas com a razão
cj_rz_ans_inicio <- mclust::adjustedRandIndex(df_ans_inicio_kmeans5$cluster, df_ans_inicio_kmeans5_rz$cluster) # 0.9823526
# análise conjunta vs apenas com a taxa
cj_tx_ans_inicio <- mclust::adjustedRandIndex(df_ans_inicio_kmeans5$cluster, df_ans_inicio_kmeans5_tx$cluster) # 0.8904534
# análise apenas com a taxa vs apenas com a razão
tx_rz_ans_inicio <- mclust::adjustedRandIndex(df_ans_inicio_kmeans5_tx$cluster, df_ans_inicio_kmeans5_rz$cluster) # 0.8820779

# Análise de cluster para ANS de 2022 a 2024 - Ward com k=4
ans_final_ward_rz <- hclust(df_ans_final_rz_dist, method = "ward.D2")
ans_final_ward4_class_rz <- cutree(ans_final_ward_rz, k = 4)
table(ans_final_ward4_class_rz)

ans_final_ward_tx <- hclust(df_ans_final_tx_dist, method = "ward.D2")
ans_final_ward4_class_tx <- cutree(ans_final_ward_tx, k = 4)
table(ans_final_ward4_class_tx)

## ---- Comparação das análises: ANS de 2022 a 2024
# Número de municípios em grupos diferentes
table(ans_final_ward4_class)
table(ans_final_ward4_class_rz)
table(ans_final_ward4_class_tx)
# análise conjunta vs apenas com a razão
cj_rz_ans_final <- mclust::adjustedRandIndex(ans_final_ward4_class, ans_final_ward4_class_rz) # 0.8054838
# análise conjunta vs apenas com a taxa
cj_tx_ans_final <- mclust::adjustedRandIndex(ans_final_ward4_class, ans_final_ward4_class_tx) # 0.450833
# análise apenas com a taxa vs apenas com a razão
tx_rz_ans_final <- mclust::adjustedRandIndex(ans_final_ward4_class_tx, ans_final_ward4_class_rz) # 0.452628

# Análise de cluster para SUS de 2015 a 2017 - K-means com k=4
set.seed(2402)
df_sus_inicio_kmeans4_rz <- kmeans(df_sus_inicio_rz, 4)
round(df_sus_inicio_kmeans4_rz$centers, 3)
table(df_sus_inicio_kmeans4_rz$cluster)

set.seed(2402)
df_sus_inicio_kmeans4_tx <- kmeans(df_sus_inicio_tx, 4)
round(df_sus_inicio_kmeans4_tx$centers, 3)
table(df_sus_inicio_kmeans4_tx$cluster)

## ---- Comparação das análises: SUS de 2015 a 2017
# Número de municípios em grupos diferentes
table(df_sus_inicio_kmeans4$cluster)
table(df_sus_inicio_kmeans4_rz$cluster)
table(df_sus_inicio_kmeans4_tx$cluster)
# análise conjunta vs apenas com a razão
cj_rz_sus_inicio <- mclust::adjustedRandIndex(df_sus_inicio_kmeans4$cluster, df_sus_inicio_kmeans4_rz$cluster) # 0.9270969
# análise conjunta vs apenas com a taxa
cj_tx_sus_inicio <- mclust::adjustedRandIndex(df_sus_inicio_kmeans4$cluster, df_sus_inicio_kmeans4_tx$cluster) # 0.638541
# análise apenas com a taxa vs apenas com a razão
tx_rz_sus_inicio <- mclust::adjustedRandIndex(df_sus_inicio_kmeans4_tx$cluster, df_sus_inicio_kmeans4_rz$cluster) # 0.5892951

# Análise de cluster para SUS de 2022 a 2024 - K-means com k=5
set.seed(2402)
df_sus_final_kmeans5_rz <- kmeans(df_sus_final_rz, 5)
round(df_sus_final_kmeans5_rz$centers, 3)
table(df_sus_final_kmeans5_rz$cluster)

set.seed(2402)
df_sus_final_kmeans5_tx <- kmeans(df_sus_final_tx, 5)
round(df_sus_final_kmeans5_tx$centers, 3)
table(df_sus_final_kmeans5_tx$cluster)

## ---- Comparação das análises: SUS de 2022 a 2024
# Número de municípios em grupos diferentes
table(df_sus_final_kmeans5$cluster)
table(df_sus_final_kmeans5_rz$cluster)
table(df_sus_final_kmeans5_tx$cluster)
# análise conjunta vs apenas com a razão
cj_rz_sus_final <- mclust::adjustedRandIndex(df_sus_final_kmeans5$cluster, df_sus_final_kmeans5_rz$cluster) # 0.8966685
# análise conjunta vs apenas com a taxa
cj_tx_sus_final <- mclust::adjustedRandIndex(df_sus_final_kmeans5$cluster, df_sus_final_kmeans5_tx$cluster) # 0.5930783
# análise apenas com a taxa vs apenas com a razão
tx_rz_sus_final <- mclust::adjustedRandIndex(df_sus_final_kmeans5_tx$cluster, df_sus_final_kmeans5_rz$cluster) # 0.5342077

#-- Conclusões da comparação entre análises de cluster com os indicadores juntos e isolados: --
# Adjusted Rand Index (ARI): compara as duas análises olhando pares de observações, 1.0 indica concordância perfeita
# ANS 2017 a 2019: ARI para os três pares muito altos, clusters os três clusters são bem parecidos.
# ANS 2022 a 2024: ARI entre a análise conjunta e apenas a razão é bem alta, quando comparamos com a análise apenas com a taxa ARI fica baixo.
# SUS 2015 a 2017: ARI entre a análise conjunta e apenas a razão é bem alta, quando comparamos com a análise apenas com a taxa ARI fica baixo.
# SUS 2022 a 2024: ARI entre a análise conjunta e apenas a razão é bem alta, quando comparamos com a análise apenas com a taxa ARI fica baixo.

# Salvando resultados
`ARI para análise conjunta e análise apenas com razão por 100 NV` <- c(cj_rz_ans_inicio, cj_rz_ans_final, cj_rz_sus_inicio, cj_rz_sus_final)
`ARI para análise conjunta e análise apenas com taxa por 1000 MIF` <- c(cj_tx_ans_inicio, cj_tx_ans_final, cj_tx_sus_inicio, cj_tx_sus_final)
`ARI para análise apenas com razão por 100 NV e análise apenas com taxa por 1000 MIF` <- c(tx_rz_ans_inicio, tx_rz_ans_final, tx_rz_sus_inicio, tx_rz_sus_final)
`Análise` <- c("ANS, 2017 a 2019", "ANS, 2022 a 2024", "SUS, 2015 a 2017", "SUS, 2022 a 2024")

df_comparacao_clusters <- data.frame(`Análise`, 
                                    `ARI para análise conjunta e análise apenas com razão por 100 NV`,
                                    `ARI para análise conjunta e análise apenas com taxa por 1000 MIF`,
                                    `ARI para análise apenas com razão por 100 NV e análise apenas com taxa por 1000 MIF`)

write_xlsx(df_comparacao_clusters, "databases/ari_comparacao_clusters.xlsx")


# Comparando os grupos  ---------------
#Criando as funções que serão utilizadas para a construção das tabelas e boxplots

cria_tabelas <- function(variaveis, df, var_grupos, atendimento, salvar_excel = FALSE, nome_arquivo = "tabelas_resumo.xlsx") {
  
  grupos <- levels(as.factor(df[[var_grupos]]))
  # Criamos uma lista vazia para armazenar os data frames
  lista_dfs <- list()
  
  for (variavel in variaveis) {
    
    # Criamos o data frame
    df_resumo <- data.frame(
      variavel = variavel, # Coluna extra para identificar no Excel
      grupo = grupos,
      n = unlist(lapply(grupos, function(g) nrow(df[df[[var_grupos]] == g, ]))),
      minimo = round(unlist(lapply(grupos, function(g) min(df[df[[var_grupos]] == g, ][[variavel]], na.rm = TRUE))), 2),
      primeiro_qt = round(unlist(lapply(grupos, function(g) quantile(df[df[[var_grupos]] == g, ][[variavel]], 0.25, na.rm = TRUE))), 2),
      media = round(unlist(lapply(grupos, function(g) mean(df[df[[var_grupos]] == g, ][[variavel]], na.rm = TRUE))), 2),
      mediana = round(unlist(lapply(grupos, function(g) median(df[df[[var_grupos]] == g, ][[variavel]], na.rm = TRUE))), 2),
      dp = round(unlist(lapply(grupos, function(g) sd(df[df[[var_grupos]] == g, ][[variavel]], na.rm = TRUE))), 2),
      terceiro_qt = round(unlist(lapply(grupos, function(g) quantile(df[df[[var_grupos]] == g, ][[variavel]], 0.75, na.rm = TRUE))), 2),
      maximo = round(unlist(lapply(grupos, function(g) max(df[df[[var_grupos]] == g, ][[variavel]], na.rm = TRUE))), 2)
    )
    
    # Armazenamos na lista
    lista_dfs[[variavel]] <- df_resumo
    
    # Geramos o kable para visualização
    tabela_kable <- kable(
      df_resumo[, -1], # Removemos a coluna "variavel" apenas no print
      align = "cccccccc",
      col.names = c("Grupo (cluster)", "n", "Mín.", "1º Quartil", "Média", "Mediana", "D.P.", "3º Quartil", "Máx."),
      caption = ifelse(variavel == variaveis[1], 
                       paste0(glue("Medidas resumo para os grupos de municípios ({atendimento}). <br><br>"), variavel), 
                       variavel)
    )
    
    print(tabela_kable)
  }
  
  # Opção de salvar em Excel
  if (salvar_excel) {
    # Une todas as tabelas em uma só para o Excel
    df_final_excel <- bind_rows(lista_dfs)
    write_xlsx(df_final_excel, nome_arquivo)
    message(paste("Arquivo salvo com sucesso:", nome_arquivo))
  }
  
  # Retorna a lista de data frames para que você possa usar p9 <- cria_tabelas(...)
  return(invisible(lista_dfs))
}

# Apenas visualizar
cria_tabelas(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_ans_inicio, 
             var_grupos = "cluster_ans_inicio", 
             atendimento = "saúde suplementar 2017 a 2019")

# Visualizar e salvar tabelas
df_resumo_clusters_ans_inicio <- cria_tabelas(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
                         df = df_ans_inicio, 
                         var_grupos = "cluster_ans_inicio", 
                         atendimento = "saúde suplementar 2017 a 2019",
                         salvar_excel = TRUE, 
                         nome_arquivo = "databases/tabela_resumo_clusters_ans_inicio.xlsx")

cria_tabelas(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_ans_inicio, var_grupos = "cluster_ans_inicio", atendimento = "saúde suplementar 2017 a 2019")

df_resumo_clusters_ans_final <- cria_tabelas(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
                                              df = df_ans_final, 
                                              var_grupos = "cluster_ans_final", 
                                              atendimento = "saúde suplementar 2022 a 2024",
                                              salvar_excel = TRUE, 
                                              nome_arquivo = "databases/tabela_resumo_clusters_ans_final.xlsx")

cria_tabelas(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_ans_final, var_grupos = "cluster_ans_final", atendimento = "saúde suplementar 2022 a 2024")


cria_tabelas(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_sus_inicio, var_grupos = "cluster_sus_inicio", atendimento = "SUS 2015 a 2017")

df_resumo_clusters_sus_inicio <- cria_tabelas(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
                                              df = df_sus_inicio, 
                                              var_grupos = "cluster_sus_inicio", 
                                              atendimento = "SUS 2015 a 2017",
                                              salvar_excel = TRUE, 
                                              nome_arquivo = "databases/tabela_resumo_clusters_sus_inicio.xlsx")

cria_tabelas(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_sus_final, var_grupos = "cluster_sus_final", atendimento = "SUS 2022 a 2024")

df_resumo_clusters_sus_final <- cria_tabelas(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
                                              df = df_sus_final, 
                                              var_grupos = "cluster_sus_final", 
                                              atendimento = "SUS 2022 a 2024",
                                              salvar_excel = TRUE, 
                                              nome_arquivo = "databases/tabela_resumo_clusters_sus_final.xlsx")

cria_boxplots <- function(variaveis, df, var_grupos, labels_y, titulo_geral, nome_arquivo = "boxplots_final.png") {
  
  boxplots <- list()
  
  for (i in 1:length(variaveis)) {
    variavel <- variaveis[i]
    label_atual <- labels_y[i] # Pega o label correspondente à variável
    
    boxplots[[i]] <- ggplot(df) +
      geom_boxplot(aes(x = as.factor(.data[[var_grupos]]), y = .data[[variavel]]), 
                   fill = "lightblue", outlier.alpha = 0.5) +
      # Removemos o ggtitle(variavel) para os gráficos não terem títulos individuais
      labs(y = label_atual, x = "Grupo") + 
      theme_classic() + 
      theme(
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 11),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
      )
  }
  
  # Combinando os gráficos com patchwork
  # wrap_plots transforma a lista em um layout; ncol define as colunas
  grafico_final <- wrap_plots(boxplots, ncol = length(variaveis)) + 
    plot_annotation(
      title = titulo_geral,
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )
  
  # Exibe o gráfico no RStudio
  print(grafico_final)
  
  # Salva o gráfico
  ggsave(nome_arquivo, plot = grafico_final, width = 5 * length(variaveis), height = 6, dpi = 300)
  
  message(paste("Gráfico salvo como:", nome_arquivo))
  
  return(invisible(grafico_final))
}
cria_boxplots(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
              df = df_ans_inicio, var_grupos = "cluster_ans_inicio",
              labels_y = c("Taxa de abortos inseguros por 1000 \n mulheres em idade fértil", "Razão de abortos inseguros por 100 \n nascidos vivos"),
              titulo_geral = "Indicadores de aborto inseguro da saúde suplementar \n por grupo entre os anos de 2017 a 2019",
              nome_arquivo = "figuras/clustering/boxplot_clusters_ans_inicio.png")

cria_boxplots(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
              df = df_ans_final, var_grupos = "cluster_ans_final",
              labels_y = c("Taxa de abortos inseguros por 1000 \n mulheres em idade fértil", "Razão de abortos inseguros por 100 \n nascidos vivos"),
              titulo_geral = "Indicadores de aborto inseguro da saúde suplementar \n por grupo entre os anos de 2022 a 2024",
              nome_arquivo = "figuras/clustering/boxplot_clusters_ans_final.png")


cria_boxplots(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
              df = df_sus_inicio, var_grupos = "cluster_sus_inicio",
              labels_y = c("Taxa de abortos inseguros por 1000 \n mulheres em idade fértil", "Razão de abortos inseguros por 100 \n nascidos vivos"),
              titulo_geral = "Indicadores de aborto inseguro do SUS \n por grupo entre os anos de 2015 a 2017",
              nome_arquivo = "figuras/clustering/boxplot_clusters_sus_inicio.png")


cria_boxplots(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
              df = df_sus_final, var_grupos = "cluster_sus_final",
              labels_y = c("Taxa de abortos inseguros por 1000 \n mulheres em idade fértil", "Razão de abortos inseguros por 100 \n nascidos vivos"),
              titulo_geral = "Indicadores de aborto inseguro do SUS \n por grupo entre os anos de 2022 a 2024",
              nome_arquivo = "figuras/clustering/boxplot_clusters_sus_final.png")





cria_tabelas_testes <- function(variaveis, df, var_grupos, atendimento, salvar_excel = FALSE, nome_arquivo = "testes_post_hoc.xlsx") {
  
  lista_para_excel <- list()
  
  for (variavel in variaveis) {
    
    formula <- as.formula(glue("{variavel} ~ {var_grupos}"))
    
    # Executa os testes
    resultados_dunn <- dunn_test(data = df, formula = formula, p.adjust.method = "bonferroni")
    efeito_d <- cohens_d(data = df, formula = formula)
    
    # Unimos os resultados do Dunn e Cohen em um único dataframe plano
    df_excel <- resultados_dunn %>%
      left_join(efeito_d %>% select(group1, group2, effsize), by = c("group1", "group2")) %>%
      mutate(variavel = variavel) %>%
      select(variavel, group1, group2, statistic, p.adj, p.adj.signif, effsize)
    
    lista_para_excel[[variavel]] <- df_excel
    
    monta_linhas <- function(num_grupo) {
      # Filtra os índices para o grupo específico
      idx <- which(resultados_dunn$group1 == unique(resultados_dunn$group1)[num_grupo])
      
      # Formatação do p-valor com asterisco
      p_formatado <- ifelse(resultados_dunn$p.adj[idx] < 0.05,
                            ifelse(round(resultados_dunn$p.adj[idx], 3) == 0, "< 0.001*", 
                                   paste0(round(resultados_dunn$p.adj[idx], 3), "*")),
                            round(resultados_dunn$p.adj[idx], 3))
      
      paste0(
        -1 * round(resultados_dunn$statistic[idx], 3), " (Z)<br>",
        p_formatado, " (p)<br>",
        round(efeito_d$effsize[idx], 3), " (d)"
      )
    }
    
    # Montagem da matriz triangular para o kable
    df_teste_kable <- data.frame(
      grupo1 = monta_linhas(1),
      grupo2 = c("", monta_linhas(2)), 
      grupo3 = c("", "", monta_linhas(3)),
      row.names = unique(resultados_dunn$group2)
    )
    
    tabela <- kable(
      df_teste_kable,
      escape = FALSE,
      align = "ccc",
      caption = HTML(ifelse(variavel == variaveis[1],
                            paste0(glue("Resultados dos testes de Dunn e D de Cohen ({atendimento}). <br><br>"), variavel), 
                            variavel))
    )
    
    print(tabela)
  }
  
  # Salvamento no Excel
  if (salvar_excel) {
    df_final_excel <- bind_rows(lista_para_excel)
    write_xlsx(df_final_excel, nome_arquivo)
    message(paste("Arquivo de testes salvo:", nome_arquivo))
  }
  
  return(invisible(lista_para_excel))
}

cria_tabelas_testes(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_ans_inicio, var_grupos = "cluster_ans_inicio", atendimento = "saúde suplementar 2017 a 2019")

tabela_testes_ans_inicio <- cria_tabelas_testes(
  variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
  df = df_ans_inicio, 
  var_grupos = "cluster_ans_inicio", 
  atendimento = "saúde suplementar 2017 a 2019",
  salvar_excel = TRUE,
  nome_arquivo = "databases/tabela_testes_clusters_ans_inicio.xlsx"
)

cria_tabelas_testes(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_ans_final, var_grupos = "cluster_ans_final", atendimento = "saúde suplementar 2022 a 2024")

tabela_testes_ans_final <- cria_tabelas_testes(
  variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio", "ans_tx_abortos_cem_nascidos_vivos_valor_medio"),
  df = df_ans_final, 
  var_grupos = "cluster_ans_final", 
  atendimento = "SUS 2022 a 2024",
  salvar_excel = TRUE,
  nome_arquivo = "databases/tabela_testes_clusters_ans_final.xlsx"
)



cria_tabelas_testes(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_sus_inicio, var_grupos = "cluster_sus_inicio", atendimento = "SUS 2015 a 2017")

tabela_testes_sus_inicio <- cria_tabelas_testes(
  variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
  df = df_sus_inicio, 
  var_grupos = "cluster_sus_inicio", 
  atendimento = "SUS 2015 a 2017",
  salvar_excel = TRUE,
  nome_arquivo = "databases/tabela_testes_clusters_sus_inicio.xlsx"
)



cria_tabelas_testes(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
             df = df_sus_final, var_grupos = "cluster_sus_final", atendimento = "SUS 2022 a 2024")

tabela_testes_sus_final <- cria_tabelas_testes(
  variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio", "sus_tx_abortos_cem_nascidos_vivos_valor_medio"),
  df = df_sus_final, 
  var_grupos = "cluster_sus_final", 
  atendimento = "SUS 2022 a 2024",
  salvar_excel = TRUE,
  nome_arquivo = "databases/tabela_testes_clusters_sus_final.xlsx"
)


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
mp1 <- ggplot() +
  geom_sf(data = df_mapa_ans_inicio, aes(fill = cluster_ans_inicio), color = NA) +
  labs(title = "Mapa do Brasil com os grupos de municípios agrupados pelos indicadores de aborto inseguro  \n na saúde suplementar dos anos de 2017 a 2019") +
  scale_fill_viridis_d(name = "Grupo", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw()
mp1
ggsave("figuras/clustering/mapa_cluster_ans_inicio.png", mp1,  width = 10, height = 6)


## Juntando os dois dataframes
df_mapa_ans_final <- left_join(df_ans_final, df_muni_sf) |>
  st_as_sf()

## Criando os mapas
mp2 <- ggplot() +
  geom_sf(data = df_mapa_ans_final, aes(fill = cluster_ans_final), color = NA) +
  labs(title = "Mapa do Brasil com os grupos de municípios agrupados pelos indicadores de aborto inseguro  \n na saúde suplementar dos anos de 2022 a 2024") +
  scale_fill_viridis_d(name = "Grupo", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw()
mp2
ggsave("figuras/clustering/mapa_cluster_ans_final.png", mp2,  width = 10, height = 6)


## Juntando os dois dataframes
df_mapa_sus_inicio <- left_join(df_sus_inicio, df_muni_sf) |>
  st_as_sf()

## Criando os mapas
mp3 <- ggplot() +
  geom_sf(data = df_mapa_sus_inicio, aes(fill = cluster_sus_inicio), color = NA) +
  labs(title = "Mapa do Brasil com os grupos de municípios agrupados pelos indicadores de aborto inseguro  \n no SUS dos anos de 2015 a 2017") +
  scale_fill_viridis_d(name = "Grupo", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw()
mp3
ggsave("figuras/clustering/mapa_cluster_sus_inicio.png", mp3,  width = 10, height = 6)


## Juntando os dois dataframes
df_mapa_sus_final <- left_join(df_sus_final, df_muni_sf) |>
  st_as_sf()

## Criando os mapas
mp4 <- ggplot() +
  geom_sf(data = df_mapa_sus_final, aes(fill = cluster_sus_final), color = NA) +
  labs(title = "Mapa do Brasil com os grupos de municípios agrupados pelos indicadores de aborto inseguro  \n no SUS dos anos de 2022 a 2024") +
  scale_fill_viridis_d(name = "Grupo", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw()
mp4
ggsave("figuras/clustering/mapa_cluster_final.png", mp4,  width = 10, height = 6)






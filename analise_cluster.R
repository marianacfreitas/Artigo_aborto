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
  filter(ano >= 2017)

# Planiha com informações por município sem o município 270150
planilha_indicadores_aborto_sem_mun <- read_csv("databases/planilha_indicadores_aborto.csv") |>
  filter(ano >= 2017) |>
  filter(codmunres != 270150)

df_aux_municipios <- read_csv("databases_auxiliares/df_aux_municipios.csv")

# Indicadores da ANS de 2017 a 2019 (agrupados)
df_ans_inicio <- planilha_indicadores_aborto_sem_mun |>
  filter(ano >= 2017 & ano <= 2019) |>
  group_by(codmunres) |>
  summarise(
    ans_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_ans_menor_30, na.rm = T) * 0.9) + (sum(abortos_ans_30_a_39 , na.rm = T) * 0.85) + (sum(abortos_ans_40_a_49 , na.rm = T) * 0.75)) * 5) / sum(pop_fem_ans_10_49 , na.rm = T) * 1000, 1) #,
   # ans_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_ans_menor_30 , na.rm = T) * 0.9) + (sum(abortos_ans_30_a_39 , na.rm = T) * 0.85) + (sum(abortos_ans_40_a_49 , na.rm = T) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_ans , na.rm = T) * 100, 1))
   )

# Indicadores do SUS de 2017 a 2019 (agrupados)
df_sus_inicio <- planilha_indicadores_aborto |>
  filter(ano >= 2017 & ano <= 2019) |>
  group_by(codmunres) |>
  summarise(
    sus_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(pop_fem_sus_10_49 ) * 1000, 1) #,
   # sus_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1)
   ) #|>
  # rename(
  #   `Valor médio da taxa de abortos inseguros por mil mulheres em idade fértil no SUS` = sus_tx_abortos_mil_mulheres_valor_medio,
  #   `Valor médio da razão de abortos inseguros por 100 nascidos vivos no SUS` = sus_tx_abortos_cem_nascidos_vivos_valor_medio
  # )

# Indicadores da ANS de 2022 a 2024 (agrupados)
df_ans_final <- planilha_indicadores_aborto_sem_mun |>
  filter(ano >= 2022 & ano <= 2024) |>
  group_by(codmunres) |>
  summarise(
  ans_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 5) / sum(pop_fem_ans_10_49) * 1000, 1)
  # ans_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_ans_menor_30) * 0.9) + (sum(abortos_ans_30_a_39) * 0.85) + (sum(abortos_ans_40_a_49) * 0.75)) * 5) / sum(total_de_nascidos_vivos_10_a_49_ans) * 100, 1)) 
   )

df_ans_final[is.na(df_ans_final)] <- 0

# Indicadores do SUS de 2022 a 2024 (agrupados)
df_sus_final <- planilha_indicadores_aborto |>
  filter(ano >= 2022 & ano <= 2024) |>
  group_by(codmunres) |>
  summarise(
    sus_tx_abortos_mil_mulheres_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(pop_fem_sus_10_49 ) * 1000, 1) # ,
    #sus_tx_abortos_cem_nascidos_vivos_valor_medio =  round((((sum(abortos_sus_menor_30 ) * 0.9) + (sum(abortos_sus_30_a_39 ) * 0.85) + (sum(abortos_sus_40_a_49 ) * 0.75)) * 4) / sum(total_de_nascidos_vivos_10_a_49_sus ) * 100, 1)
   ) 

# Análise de cluster utilizando a distância euclidiana
df_ans_inicio_var <- df_ans_inicio |> select(c(ans_tx_abortos_mil_mulheres_valor_medio))
df_ans_final_var <- df_ans_final |> select(c(ans_tx_abortos_mil_mulheres_valor_medio))
df_sus_inicio_var <- df_sus_inicio |> select(c(sus_tx_abortos_mil_mulheres_valor_medio))
df_sus_final_var <- df_sus_final |> select(c(sus_tx_abortos_mil_mulheres_valor_medio))

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
    title = "Gráfico do cotovelo para o método K-médias (SUS de 2017 a 2019)"
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
df_ans_inicio_kmeans2 <- kmeans(df_ans_inicio_var, 2)
round(df_ans_inicio_kmeans2$centers, 3)
table(df_ans_inicio_kmeans2$cluster)

set.seed(2402)
df_ans_inicio_kmeans3 <- kmeans(df_ans_inicio_var, 3)
round(df_ans_inicio_kmeans3$centers, 3)
table(df_ans_inicio_kmeans3$cluster)

set.seed(2402)
df_ans_inicio_kmeans4 <- kmeans(df_ans_inicio_var, 4)
round(df_ans_inicio_kmeans4$centers, 3)
table(df_ans_inicio_kmeans4$cluster)

set.seed(2402)
df_ans_inicio_kmeans5 <- kmeans(df_ans_inicio_var, 5)
round(df_ans_inicio_kmeans5$centers, 3)
table(df_ans_inicio_kmeans5$cluster)

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
df_ans_final_kmeans3 <- kmeans(df_ans_final_var, 3)
round(df_ans_final_kmeans3$centers, 3)
table(df_ans_final_kmeans3$cluster)

set.seed(2402)
df_ans_final_kmeans2 <- kmeans(df_ans_final_var, 2)
round(df_ans_final_kmeans2$centers, 3)
table(df_ans_final_kmeans2$cluster)

set.seed(2402)
df_sus_inicio_kmeans2 <- kmeans(df_sus_inicio_var, 2)
round(df_sus_inicio_kmeans2$centers, 3)
table(df_sus_inicio_kmeans2$cluster)

set.seed(2402)
df_sus_inicio_kmeans3 <- kmeans(df_sus_inicio_var, 3)
round(df_sus_inicio_kmeans3$centers, 3)
table(df_sus_inicio_kmeans3$cluster)

set.seed(2402)
df_sus_inicio_kmeans4 <- kmeans(df_sus_inicio_var, 4)
round(df_sus_inicio_kmeans4$centers, 3)
table(df_sus_inicio_kmeans4$cluster)

set.seed(2402)
df_sus_inicio_kmeans5 <- kmeans(df_sus_inicio_var, 5)
round(df_sus_inicio_kmeans5$centers, 3)
table(df_sus_inicio_kmeans5$cluster)

set.seed(2402)
df_sus_inicio_kmeans6 <- kmeans(df_sus_inicio_var, 6)
round(df_sus_inicio_kmeans6$centers, 3)
table(df_sus_inicio_kmeans6$cluster)

set.seed(2402)
df_sus_final_kmeans2 <- kmeans(df_sus_final_var, 2)
round(df_sus_final_kmeans2$centers, 3)
table(df_sus_final_kmeans2$cluster)

set.seed(2402)
df_sus_final_kmeans3 <- kmeans(df_sus_final_var, 3)
round(df_sus_final_kmeans3$centers, 3)
table(df_sus_final_kmeans3$cluster)

set.seed(2402)
df_sus_final_kmeans4 <- kmeans(df_sus_final_var, 4)
round(df_sus_final_kmeans4$centers, 3)
table(df_sus_final_kmeans4$cluster)

set.seed(2402)
df_sus_final_kmeans5 <- kmeans(df_sus_final_var, 5)
round(df_sus_final_kmeans5$centers, 3)
table(df_sus_final_kmeans5$cluster)

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
    title = "Gráfico do cotovelo para o método método K-medoides (PAM) (SUS de 2017 a 2019)"
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
df_ans_inicio_kmedoids2 <- cluster::pam(df_ans_inicio_var, 2)
round(df_ans_inicio_kmedoids2$medoids, 3)
table(df_ans_inicio_kmedoids2$cluster)

set.seed(2402)
df_ans_inicio_kmedoids3 <- cluster::pam(df_ans_inicio_var, 3)
round(df_ans_inicio_kmedoids3$medoids, 3)
table(df_ans_inicio_kmedoids3$cluster)

set.seed(2402)
df_ans_inicio_kmedoids4 <- cluster::pam(df_ans_inicio_var, 4)
round(df_ans_inicio_kmedoids4$medoids, 3)
table(df_ans_inicio_kmedoids4$cluster)

set.seed(2402)
df_ans_inicio_kmedoids5 <- cluster::pam(df_ans_inicio_var, 5)
round(df_ans_inicio_kmedoids5$medoids, 3)
table(df_ans_inicio_kmedoids5$cluster)

set.seed(2402)
df_ans_inicio_kmedoids6 <- cluster::pam(df_ans_inicio_var, 6)
round(df_ans_inicio_kmedoids6$medoids, 3)
table(df_ans_inicio_kmedoids6$cluster)

set.seed(2402)
df_ans_final_kmedoids2 <- cluster::pam(df_ans_final_var, 2)
round(df_ans_final_kmedoids2$medoids, 3)
table(df_ans_final_kmedoids2$cluster)

set.seed(2402)
df_ans_final_kmedoids3 <- cluster::pam(df_ans_final_var, 3)
round(df_ans_final_kmedoids3$medoids, 3)
table(df_ans_final_kmedoids3$cluster)

set.seed(2402)
df_ans_final_kmedoids4 <- cluster::pam(df_ans_final_var, 4)
round(df_ans_final_kmedoids4$medoids, 3)
table(df_ans_final_kmedoids4$cluster)

set.seed(2402)
df_ans_final_kmedoids5 <- cluster::pam(df_ans_final_var, 5)
round(df_ans_final_kmedoids5$medoids, 3)
table(df_ans_final_kmedoids5$cluster)

set.seed(2402)
df_ans_final_kmedoids6 <- cluster::pam(df_ans_final_var, 6)
round(df_ans_final_kmedoids6$medoids, 3)
table(df_ans_final_kmedoids6$cluster)

set.seed(2402)
df_sus_inicio_kmedoids2 <- cluster::pam(df_sus_inicio_var, 2)
round(df_sus_inicio_kmedoids2$medoids, 3)
table(df_sus_inicio_kmedoids2$cluster)

set.seed(2402)
df_sus_inicio_kmedoids3 <- cluster::pam(df_sus_inicio_var, 3)
round(df_sus_inicio_kmedoids3$medoids, 3)
table(df_sus_inicio_kmedoids3$cluster)

set.seed(2402)
df_sus_inicio_kmedoids4 <- cluster::pam(df_sus_inicio_var, 4)
round(df_sus_inicio_kmedoids4$medoids, 3)
table(df_sus_inicio_kmedoids4$cluster)

set.seed(2402)
df_sus_inicio_kmedoids5 <- cluster::pam(df_sus_inicio_var, 5)
round(df_sus_inicio_kmedoids5$medoids, 3)
table(df_sus_inicio_kmedoids5$cluster)

set.seed(2402)
df_sus_inicio_kmedoids6 <- cluster::pam(df_sus_inicio_var, 6)
round(df_sus_inicio_kmedoids6$medoids, 3)
table(df_sus_inicio_kmedoids6$cluster)

set.seed(2402)
df_sus_final_kmedoids2 <- cluster::pam(df_sus_final_var, 2)
round(df_sus_final_kmedoids2$medoids, 3)
table(df_sus_final_kmedoids2$cluster)

set.seed(2402)
df_sus_final_kmedoids3 <- cluster::pam(df_sus_final_var, 3)
round(df_sus_final_kmedoids3$medoids, 3)
table(df_sus_final_kmedoids3$cluster)

set.seed(2402)
df_sus_final_kmedoids4 <- cluster::pam(df_sus_final_var, 4)
round(df_sus_final_kmedoids4$medoids, 3)
table(df_sus_final_kmedoids4$cluster)

set.seed(2402)
df_sus_final_kmedoids5 <- cluster::pam(df_sus_final_var, 5)
round(df_sus_final_kmedoids5$medoids, 3)
table(df_sus_final_kmedoids5$cluster)

set.seed(2402)
df_sus_final_kmedoids6 <- cluster::pam(df_sus_final_var, 6)
round(df_sus_final_kmedoids6$medoids, 3)
table(df_sus_final_kmedoids6$cluster)


## Ajustando métodos hierárquicos -----------------------------------------
### Método de Ward
ans_inicio_ward <- hclust(df_ans_inicio_dist, method = "ward.D2")
plot(as.dendrogram(ans_inicio_ward), main = "Dendrograma do método de Ward (ANS 2017 a 2019)", ylab = "Altura")
ans_inicio_ward2_class <- cutree(ans_inicio_ward, k = 2)
ans_inicio_ward3_class <- cutree(ans_inicio_ward, k = 3)
ans_inicio_ward4_class <- cutree(ans_inicio_ward, k = 4)
ans_inicio_ward5_class <- cutree(ans_inicio_ward, k = 5)
ans_inicio_ward6_class <- cutree(ans_inicio_ward, k = 6)

ans_final_ward <- hclust(df_ans_final_dist, method = "ward.D2")
plot(as.dendrogram(ans_final_ward), main = "Dendrograma do método de Ward (ANS 2022 a 2024)", ylab = "Altura")
ans_final_ward2_class <- cutree(ans_final_ward, k = 2)
ans_final_ward3_class <- cutree(ans_final_ward, k = 3)
ans_final_ward4_class <- cutree(ans_final_ward, k = 4)
ans_final_ward5_class <- cutree(ans_final_ward, k = 5)
ans_final_ward6_class <- cutree(ans_final_ward, k = 6)


sus_inicio_ward <- hclust(df_sus_inicio_dist, method = "ward.D2")
plot(as.dendrogram(sus_inicio_ward), main = "Dendrograma do método de Ward (SUS 2017 a 2019)", ylab = "Altura")
sus_inicio_ward2_class <- cutree(sus_inicio_ward, k = 2)
sus_inicio_ward3_class <- cutree(sus_inicio_ward, k = 3)
sus_inicio_ward4_class <- cutree(sus_inicio_ward, k = 4)
sus_inicio_ward5_class <- cutree(sus_inicio_ward, k = 5)
sus_inicio_ward6_class <- cutree(sus_inicio_ward, k = 6)

sus_final_ward <- hclust(df_sus_final_dist, method = "ward.D2")
plot(as.dendrogram(sus_final_ward), main = "Dendrograma do método de Ward (SUS 2022 a 2024)", ylab = "Altura")
sus_final_ward2_class <- cutree(sus_final_ward, k = 2)
sus_final_ward3_class <- cutree(sus_final_ward, k = 3)
sus_final_ward4_class <- cutree(sus_final_ward, k = 4)
sus_final_ward5_class <- cutree(sus_final_ward, k = 5)
sus_final_ward6_class <- cutree(sus_final_ward, k = 6)

### Single linkage
ans_inicio_single <- hclust(df_ans_inicio_dist, method = "single")
plot(as.dendrogram(ans_inicio_single), main = "Dendrograma do método de single linkage (ANS 2017 a 2019)", ylab = "Altura")
ans_inicio_single2_class <- cutree(ans_inicio_single, k = 2)
ans_inicio_single3_class <- cutree(ans_inicio_single, k = 3)
ans_inicio_single4_class <- cutree(ans_inicio_single, k = 4)
ans_inicio_single5_class <- cutree(ans_inicio_single, k = 5)
ans_inicio_single6_class <- cutree(ans_inicio_single, k = 6)

ans_final_single <- hclust(df_ans_final_dist, method = "single")
plot(as.dendrogram(ans_final_single), main = "Dendrograma do método de single linkage (ANS 2022 a 2024)", ylab = "Altura")
ans_final_single2_class <- cutree(ans_final_single, k = 2)
ans_final_single3_class <- cutree(ans_final_single, k = 3)
ans_final_single4_class <- cutree(ans_final_single, k = 4)
ans_final_single5_class <- cutree(ans_final_single, k = 5)
ans_final_single6_class <- cutree(ans_final_single, k = 6)

sus_inicio_single <- hclust(df_sus_inicio_dist, method = "single")
plot(as.dendrogram(sus_inicio_single), main = "Dendrograma do método de single linkage (SUS 2017 a 2019)", ylab = "Altura")
sus_inicio_single2_class <- cutree(sus_inicio_single, k = 2)
sus_inicio_single3_class <- cutree(sus_inicio_single, k = 3)
sus_inicio_single4_class <- cutree(sus_inicio_single, k = 4)
sus_inicio_single5_class <- cutree(sus_inicio_single, k = 5)
sus_inicio_single6_class <- cutree(sus_inicio_single, k = 6)

sus_final_single <- hclust(df_sus_final_dist, method = "single")
plot(as.dendrogram(sus_final_single), main = "Dendrograma do método de single linkage (SUS 2022 a 2024)", ylab = "Altura")
sus_final_single2_class <- cutree(sus_final_single, k = 2)
sus_final_single3_class <- cutree(sus_final_single, k = 3)
sus_final_single4_class <- cutree(sus_final_single, k = 4)
sus_final_single5_class <- cutree(sus_final_single, k = 5)
sus_final_single6_class <- cutree(sus_final_single, k = 6)

### Complete linkage
ans_inicio_complete <- hclust(df_ans_inicio_dist, method = "complete")
plot(as.dendrogram(ans_inicio_complete), main = "Dendrograma do método de complete linkage (ANS 2017 a 2019)", ylab = "Altura")
ans_inicio_complete2_class <- cutree(ans_inicio_complete, k = 2)
ans_inicio_complete3_class <- cutree(ans_inicio_complete, k = 3)
ans_inicio_complete4_class <- cutree(ans_inicio_complete, k = 4)
ans_inicio_complete5_class <- cutree(ans_inicio_complete, k = 5)
ans_inicio_complete6_class <- cutree(ans_inicio_complete, k = 6)

ans_final_complete <- hclust(df_ans_final_dist, method = "complete")
plot(as.dendrogram(ans_final_complete), main = "Dendrograma do método de complete linkage (ANS 2022 a 2024)", ylab = "Altura")
ans_final_complete2_class <- cutree(ans_final_complete, k = 2)
ans_final_complete3_class <- cutree(ans_final_complete, k = 3)
ans_final_complete4_class <- cutree(ans_final_complete, k = 4)
ans_final_complete5_class <- cutree(ans_final_complete, k = 5)
ans_final_complete6_class <- cutree(ans_final_complete, k = 6)

sus_inicio_complete <- hclust(df_sus_inicio_dist, method = "complete")
plot(as.dendrogram(sus_inicio_complete), main = "Dendrograma do método de complete linkage (SUS 2017 a 2019)", ylab = "Altura")
sus_inicio_complete2_class <- cutree(sus_inicio_complete, k = 2)
sus_inicio_complete3_class <- cutree(sus_inicio_complete, k = 3)
sus_inicio_complete4_class <- cutree(sus_inicio_complete, k = 4)
sus_inicio_complete5_class <- cutree(sus_inicio_complete, k = 5)
sus_inicio_complete6_class <- cutree(sus_inicio_complete, k = 6)

sus_final_complete <- hclust(df_sus_final_dist, method = "complete")
plot(as.dendrogram(sus_final_complete), main = "Dendrograma do método de complete linkage (SUS 2022 a 2024)", ylab = "Altura")
sus_final_complete2_class <- cutree(sus_final_complete, k = 2)
sus_final_complete3_class <- cutree(sus_final_complete, k = 3)
sus_final_complete4_class <- cutree(sus_final_complete, k = 4)
sus_final_complete5_class <- cutree(sus_final_complete, k = 5)
sus_final_complete6_class <- cutree(sus_final_complete, k = 6)

# conclusão - nenhum dos complete linkage ficou bom

### Average linkage
ans_inicio_average <- hclust(df_ans_inicio_dist, method = "average")
plot(as.dendrogram(ans_inicio_average), main = "Dendrograma do método de average linkage (ANS 2017 a 2019)", ylab = "Altura")
ans_inicio_average2_class <- cutree(ans_inicio_average, k = 2)
ans_inicio_average3_class <- cutree(ans_inicio_average, k = 3)
ans_inicio_average4_class <- cutree(ans_inicio_average, k = 4)
ans_inicio_average5_class <- cutree(ans_inicio_average, k = 5)
ans_inicio_average6_class <- cutree(ans_inicio_average, k = 6)

ans_final_average <- hclust(df_ans_final_dist, method = "average")
plot(as.dendrogram(ans_final_average), main = "Dendrograma do método de average linkage (ANS 2022 a 2024)", ylab = "Altura")
ans_final_average2_class <- cutree(ans_final_average, k = 2)
ans_final_average3_class <- cutree(ans_final_average, k = 3)
ans_final_average4_class <- cutree(ans_final_average, k = 4)
ans_final_average5_class <- cutree(ans_final_average, k = 5)
ans_final_average6_class <- cutree(ans_final_average, k = 6)

sus_inicio_average <- hclust(df_sus_inicio_dist, method = "average")
plot(as.dendrogram(sus_inicio_average), main = "Dendrograma do método de average linkage (SUS 2017 a 2019)", ylab = "Altura")
sus_inicio_average2_class <- cutree(sus_inicio_average, k = 2)
sus_inicio_average3_class <- cutree(sus_inicio_average, k = 3)
sus_inicio_average4_class <- cutree(sus_inicio_average, k = 4)
sus_inicio_average5_class <- cutree(sus_inicio_average, k = 5)
sus_inicio_average6_class <- cutree(sus_inicio_average, k = 6)

sus_final_average <- hclust(df_sus_final_dist, method = "average")
plot(as.dendrogram(sus_final_average), main = "Dendrograma do método de average linkage (SUS 2022 a 2024)", ylab = "Altura")
sus_final_average2_class <- cutree(sus_final_average, k = 2)
sus_final_average3_class <- cutree(sus_final_average, k = 3)
sus_final_average4_class <- cutree(sus_final_average, k = 4)
sus_final_average5_class <- cutree(sus_final_average, k = 5)
sus_final_average6_class <- cutree(sus_final_average, k = 6)

## Escolhendo o melhor método para cada análise ---------------------------
### ANS 2017 a 2019  ----------------------------------------------------------
ls()[grep("^df_ans_inicio_kmeans", ls())]
ans_inicio_kmeans_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("kmeans", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_ans_inicio_var, get(paste0("df_ans_inicio_kmeans", i))$cluster)$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_ans_inicio_var, get(paste0("df_ans_inicio_kmeans", i))$cluster, d = df_ans_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_ans_inicio_dist, get(paste0("df_ans_inicio_kmeans", i))$cluster))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_ans_inicio_dist, get(paste0("df_ans_inicio_kmeans", i))$cluster))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_ans_inicio_var[[1]], get(paste0("df_ans_inicio_kmeans", i))$cluster))),  
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_ans_inicio_var, get(paste0("df_ans_inicio_kmeans", i))$cluster, d = df_ans_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^df_ans_inicio_kmedoids", ls())]
ans_inicio_kmedoids_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("kmedoids", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_ans_inicio_var, get(paste0("df_ans_inicio_kmedoids", i))$cluster)$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_ans_inicio_var, get(paste0("df_ans_inicio_kmedoids", i))$cluster, d = df_ans_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_ans_inicio_dist, get(paste0("df_ans_inicio_kmedoids", i))$cluster))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_ans_inicio_dist, get(paste0("df_ans_inicio_kmedoids", i))$cluster))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_ans_inicio_var[[1]], get(paste0("df_ans_inicio_kmedoids", i))$cluster))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_ans_inicio_var, get(paste0("df_ans_inicio_kmedoids", i))$cluster, d = df_ans_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^ans_inicio_ward", ls())]
ans_inicio_ward_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("ward", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_ans_inicio_var, get(paste0("ans_inicio_ward", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_ans_inicio_var, get(paste0("ans_inicio_ward", i, "_class")), d = df_ans_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_ans_inicio_dist, get(paste0("ans_inicio_ward", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_ans_inicio_dist, get(paste0("ans_inicio_ward", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_ans_inicio_var[[1]], get(paste0("ans_inicio_ward", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_ans_inicio_var, get(paste0("ans_inicio_ward", i, "_class")), d = df_ans_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^ans_inicio_single", ls())]
ans_inicio_single_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("single", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_ans_inicio_var, get(paste0("ans_inicio_single", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_ans_inicio_var, get(paste0("ans_inicio_single", i, "_class")), d = df_ans_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_ans_inicio_dist, get(paste0("ans_inicio_single", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_ans_inicio_dist, get(paste0("ans_inicio_single", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_ans_inicio_var[[1]], get(paste0("ans_inicio_single", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_ans_inicio_var, get(paste0("ans_inicio_single", i, "_class")), d = df_ans_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^ans_inicio_complete", ls())]
ans_inicio_complete_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("complete", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_ans_inicio_var, get(paste0("ans_inicio_complete", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_ans_inicio_var, get(paste0("ans_inicio_complete", i, "_class")), d = df_ans_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_ans_inicio_dist, get(paste0("ans_inicio_complete", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_ans_inicio_dist, get(paste0("ans_inicio_complete", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_ans_inicio_var[[1]], get(paste0("ans_inicio_complete", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_ans_inicio_var, get(paste0("ans_inicio_complete", i, "_class")), d = df_ans_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^ans_inicio_average", ls())]
ans_inicio_average_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("average", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_ans_inicio_var, get(paste0("ans_inicio_average", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_ans_inicio_var, get(paste0("ans_inicio_average", i, "_class")), d = df_ans_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_ans_inicio_dist, get(paste0("ans_inicio_average", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_ans_inicio_dist, get(paste0("ans_inicio_average", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_ans_inicio_var[[1]], get(paste0("ans_inicio_average", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_ans_inicio_var, get(paste0("ans_inicio_average", i, "_class")), d = df_ans_inicio_dist, centrotypes = "medoids")))
)



ans_inicio_avaliacao <- rbind(
  ans_inicio_kmedoids_index,
  ans_inicio_kmeans_index, 
  ans_inicio_ward_index,
  ans_inicio_single_index,
  ans_inicio_complete_index,
  ans_inicio_average_index
)

### ANS 2022 a 2024 ----------------------------------------------------------
ls()[grep("^df_ans_final_kmeans", ls())]
ans_final_kmeans_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("kmeans", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_ans_final_var, get(paste0("df_ans_final_kmeans", i))$cluster)$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_ans_final_var, get(paste0("df_ans_final_kmeans", i))$cluster, d = df_ans_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_ans_final_dist, get(paste0("df_ans_final_kmeans", i))$cluster))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_ans_final_dist, get(paste0("df_ans_final_kmeans", i))$cluster))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_ans_final_var[[1]], get(paste0("df_ans_final_kmeans", i))$cluster))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_ans_final_var, get(paste0("df_ans_final_kmeans", i))$cluster, d = df_ans_final_dist, centrotypes = "medoids")))
)

ls()[grep("^df_ans_final_kmedoids", ls())]
ans_final_kmedoids_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("kmedoids", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_ans_final_var, get(paste0("df_ans_final_kmedoids", i))$cluster)$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_ans_final_var, get(paste0("df_ans_final_kmedoids", i))$cluster, d = df_ans_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_ans_final_dist, get(paste0("df_ans_final_kmedoids", i))$cluster))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_ans_final_dist, get(paste0("df_ans_final_kmedoids", i))$cluster))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_ans_final_var[[1]], get(paste0("df_ans_final_kmedoids", i))$cluster))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_ans_final_var, get(paste0("df_ans_final_kmedoids", i))$cluster, d = df_ans_final_dist, centrotypes = "medoids")))
)

ls()[grep("^ans_final_ward", ls())]
ans_final_ward_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("ward", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_ans_final_var, get(paste0("ans_final_ward", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_ans_final_var, get(paste0("ans_final_ward", i, "_class")), d = df_ans_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_ans_final_dist, get(paste0("ans_final_ward", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_ans_final_dist, get(paste0("ans_final_ward", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_ans_final_var[[1]], get(paste0("ans_final_ward", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_ans_final_var, get(paste0("ans_final_ward", i, "_class")), d = df_ans_final_dist, centrotypes = "medoids")))
)

ls()[grep("^ans_final_single", ls())]
ans_final_single_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("single", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_ans_final_var, get(paste0("ans_final_single", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_ans_final_var, get(paste0("ans_final_single", i, "_class")), d = df_ans_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_ans_final_dist, get(paste0("ans_final_single", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_ans_final_dist, get(paste0("ans_final_single", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_ans_final_var[[1]], get(paste0("ans_final_single", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_ans_final_var, get(paste0("ans_final_single", i, "_class")), d = df_ans_final_dist, centrotypes = "medoids")))
)

ls()[grep("^ans_final_complete", ls())]
ans_final_complete_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("complete", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_ans_final_var, get(paste0("ans_final_complete", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_ans_final_var, get(paste0("ans_final_complete", i, "_class")), d = df_ans_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_ans_final_dist, get(paste0("ans_final_complete", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_ans_final_dist, get(paste0("ans_final_complete", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_ans_final_var[[1]], get(paste0("ans_final_complete", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_ans_final_var, get(paste0("ans_final_complete", i, "_class")), d = df_ans_final_dist, centrotypes = "medoids")))
)

ls()[grep("^ans_final_average", ls())]
ans_final_average_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("average", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_ans_final_var, get(paste0("ans_final_average", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_ans_final_var, get(paste0("ans_final_average", i, "_class")), d = df_ans_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_ans_final_dist, get(paste0("ans_final_average", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_ans_final_dist, get(paste0("ans_final_average", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_ans_final_var[[1]], get(paste0("ans_final_average", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_ans_final_var, get(paste0("ans_final_average", i, "_class")), d = df_ans_final_dist, centrotypes = "medoids")))
)

ans_final_avaliacao <- rbind(
  ans_final_kmedoids_index,
  ans_final_kmeans_index, 
  ans_final_ward_index,
  ans_final_average_index,
  ans_final_single_index,
  ans_final_complete_index
)

### SUS 2017 a 2019  ----------------------------------------------------------
ls()[grep("^df_sus_inicio_kmeans", ls())]
sus_inicio_kmeans_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("kmeans", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_sus_inicio_var, get(paste0("df_sus_inicio_kmeans", i))$cluster)$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_sus_inicio_var, get(paste0("df_sus_inicio_kmeans", i))$cluster, d = df_sus_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_sus_inicio_dist, get(paste0("df_sus_inicio_kmeans", i))$cluster))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_sus_inicio_dist, get(paste0("df_sus_inicio_kmeans", i))$cluster))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_sus_inicio_var[[1]], get(paste0("df_sus_inicio_kmeans", i))$cluster))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_sus_inicio_var, get(paste0("df_sus_inicio_kmeans", i))$cluster, d = df_sus_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^df_sus_inicio_kmedoids", ls())]
sus_inicio_kmedoids_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("kmedoids", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_sus_inicio_var, get(paste0("df_sus_inicio_kmedoids", i))$cluster)$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_sus_inicio_var, get(paste0("df_sus_inicio_kmedoids", i))$cluster, d = df_sus_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_sus_inicio_dist, get(paste0("df_sus_inicio_kmedoids", i))$cluster))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_sus_inicio_dist, get(paste0("df_sus_inicio_kmedoids", i))$cluster))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_sus_inicio_var[[1]], get(paste0("df_sus_inicio_kmedoids", i))$cluster))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_sus_inicio_var, get(paste0("df_sus_inicio_kmedoids", i))$cluster, d = df_sus_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^sus_inicio_ward", ls())]
sus_inicio_ward_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("ward", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_sus_inicio_var, get(paste0("sus_inicio_ward", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_sus_inicio_var, get(paste0("sus_inicio_ward", i, "_class")), d = df_sus_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_sus_inicio_dist, get(paste0("sus_inicio_ward", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_sus_inicio_dist, get(paste0("sus_inicio_ward", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_sus_inicio_var[[1]], get(paste0("sus_inicio_ward", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_sus_inicio_var, get(paste0("sus_inicio_ward", i, "_class")), d = df_sus_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^sus_inicio_single", ls())]
sus_inicio_single_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("single", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_sus_inicio_var, get(paste0("sus_inicio_single", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_sus_inicio_var, get(paste0("sus_inicio_single", i, "_class")), d = df_sus_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_sus_inicio_dist, get(paste0("sus_inicio_single", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_sus_inicio_dist, get(paste0("sus_inicio_single", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_sus_inicio_var[[1]], get(paste0("sus_inicio_single", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_sus_inicio_var, get(paste0("sus_inicio_single", i, "_class")), d = df_sus_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^sus_inicio_average", ls())]
sus_inicio_average_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("average", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_sus_inicio_var, get(paste0("sus_inicio_average", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_sus_inicio_var, get(paste0("sus_inicio_average", i, "_class")), d = df_sus_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_sus_inicio_dist, get(paste0("sus_inicio_average", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_sus_inicio_dist, get(paste0("sus_inicio_average", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_sus_inicio_var[[1]], get(paste0("sus_inicio_average", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_sus_inicio_var, get(paste0("sus_inicio_average", i, "_class")), d = df_sus_inicio_dist, centrotypes = "medoids")))
)

ls()[grep("^sus_inicio_complete", ls())]
sus_inicio_complete_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("complete", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_sus_inicio_var, get(paste0("sus_inicio_complete", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_sus_inicio_var, get(paste0("sus_inicio_complete", i, "_class")), d = df_sus_inicio_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_sus_inicio_dist, get(paste0("sus_inicio_complete", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_sus_inicio_dist, get(paste0("sus_inicio_complete", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_sus_inicio_var[[1]], get(paste0("sus_inicio_complete", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_sus_inicio_var, get(paste0("sus_inicio_complete", i, "_class")), d = df_sus_inicio_dist, centrotypes = "medoids")))
)


sus_inicio_avaliacao <- rbind(
  sus_inicio_kmedoids_index,
  sus_inicio_kmeans_index, 
  sus_inicio_ward_index,
  sus_inicio_average_index,
  sus_inicio_complete_index,
  sus_inicio_single_index
)

### SUS 2022 a 2024 ----------------------------------------------------------
ls()[grep("^df_sus_final_kmeans", ls())]
sus_final_kmeans_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("kmeans", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_sus_final_var, get(paste0("df_sus_final_kmeans", i))$cluster)$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_sus_final_var, get(paste0("df_sus_final_kmeans", i))$cluster, d = df_sus_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_sus_final_dist, get(paste0("df_sus_final_kmeans", i))$cluster))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_sus_final_dist, get(paste0("df_sus_final_kmeans", i))$cluster))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_sus_final_var[[1]], get(paste0("df_sus_final_kmeans", i))$cluster))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_sus_final_var, get(paste0("df_sus_final_kmeans", i))$cluster, d = df_sus_final_dist, centrotypes = "medoids")))
)

ls()[grep("^df_sus_final_kmedoids", ls())]
sus_final_kmedoids_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("kmedoids", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_sus_final_var, get(paste0("df_sus_final_kmedoids", i))$cluster)$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_sus_final_var, get(paste0("df_sus_final_kmedoids", i))$cluster, d = df_sus_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_sus_final_dist, get(paste0("df_sus_final_kmedoids", i))$cluster))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_sus_final_dist, get(paste0("df_sus_final_kmedoids", i))$cluster))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_sus_final_var[[1]], get(paste0("df_sus_final_kmedoids", i))$cluster))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_sus_final_var, get(paste0("df_sus_final_kmedoids", i))$cluster, d = df_sus_final_dist, centrotypes = "medoids")))
)

ls()[grep("^sus_final_ward", ls())]
sus_final_ward_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("ward", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_sus_final_var, get(paste0("sus_final_ward", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_sus_final_var, get(paste0("sus_final_ward", i, "_class")), d = df_sus_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_sus_final_dist, get(paste0("sus_final_ward", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_sus_final_dist, get(paste0("sus_final_ward", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_sus_final_var[[1]], get(paste0("sus_final_ward", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_sus_final_var, get(paste0("sus_final_ward", i, "_class")), d = df_sus_final_dist, centrotypes = "medoids")))
)

ls()[grep("^sus_final_single", ls())]
sus_final_single_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("single", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_sus_final_var, get(paste0("sus_final_single", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_sus_final_var, get(paste0("sus_final_single", i, "_class")), d = df_sus_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_sus_final_dist, get(paste0("sus_final_single", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_sus_final_dist, get(paste0("sus_final_single", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_sus_final_var[[1]], get(paste0("sus_final_single", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_sus_final_var, get(paste0("sus_final_single", i, "_class")), d = df_sus_final_dist, centrotypes = "medoids")))
)

ls()[grep("^sus_final_average", ls())]
sus_final_average_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("average", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_sus_final_var, get(paste0("sus_final_average", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_sus_final_var, get(paste0("sus_final_average", i, "_class")), d = df_sus_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_sus_final_dist, get(paste0("sus_final_average", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_sus_final_dist, get(paste0("sus_final_average", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_sus_final_var[[1]], get(paste0("sus_final_average", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_sus_final_var, get(paste0("sus_final_average", i, "_class")), d = df_sus_final_dist, centrotypes = "medoids")))
)

ls()[grep("^sus_final_complete", ls())]
sus_final_complete_index <- data.frame(
  metodo = unlist(lapply(2:6, function(i) paste0("complete", i))),
  db_index_cent = unlist(lapply(2:6, function(i) index.DB(df_sus_final_var, get(paste0("sus_final_complete", i, "_class")))$DB)),
  db_index_med = unlist(lapply(2:6, function(i) index.DB(df_sus_final_var, get(paste0("sus_final_complete", i, "_class")), d = df_sus_final_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(2:6, function(i) dunn(distance = df_sus_final_dist, get(paste0("sus_final_complete", i, "_class"))))),
  silh_index = unlist(lapply(2:6, function(i) index.S(df_sus_final_dist, get(paste0("sus_final_complete", i, "_class"))))),
  ch_index_cent = unlist(lapply(2:6, function(i) index.G1(df_sus_final_var[[1]], get(paste0("sus_final_complete", i, "_class"))))),
  ch_index_med = unlist(lapply(2:6, function(i) index.G1(df_sus_final_var, get(paste0("sus_final_complete", i, "_class")), d = df_sus_final_dist, centrotypes = "medoids")))
)

sus_final_avaliacao <- rbind(
  sus_final_kmedoids_index,
  sus_final_kmeans_index, 
  sus_final_ward_index,
  sus_final_single_index,
  sus_final_average_index,
  sus_final_complete_index
)

# Juntando todas as métricas de avaliação para 
ans_inicio_avaliacao$atendimento_periodo <- "ans_inicio"
ans_final_avaliacao$atendimento_periodo <- "ans_final"
sus_inicio_avaliacao$atendimento_periodo <- "sus_inicio"
sus_final_avaliacao$atendimento_periodo <- "sus_final"


df_avaliacao <- rbind(ans_inicio_avaliacao, ans_final_avaliacao, 
                      sus_inicio_avaliacao, sus_final_avaliacao)


write_xlsx(df_avaliacao, "databases/metricas_avaliacao_clusters.xlsx")

##-- Escolha dos melhores clusters

# Métodos escolhidos ao final
# SUS 2017-2019: Kmeans3
# SUS 2022-2024: Kmeans3
# ANS 2017-2019: Kmeans3
# ANS 2022-1014: Kmeans3

df_ans_inicio$cluster_kmeans3 <- as.factor(df_ans_inicio_kmeans3$cluster)
df_ans_final$cluster_kmeans3 <- as.factor(df_ans_final_kmeans3$cluster)
df_sus_inicio$cluster_kmeans3 <- as.factor(df_sus_inicio_kmeans3$cluster)
df_sus_final$cluster_kmeans3 <- as.factor(df_sus_final_kmeans3$cluster)

# Tranformando os nomes dos clusters
df_ans_inicio <- df_ans_inicio |>
  mutate(
    cluster_kmeans3 = case_when(
      cluster_kmeans3 == 2 ~ "1: lowest unsafe abortion rates",
      cluster_kmeans3 == 3 ~ "2: intermediary unsafe abortion rates",
      cluster_kmeans3 == 1 ~ "3: highest unsafe abortion rates"
    )
  ) |>
  mutate(
    cluster_kmeans3_num = case_when(
      cluster_kmeans3 == "1: lowest unsafe abortion rates" ~ "Lowest rates",
      cluster_kmeans3 == "2: intermediary unsafe abortion rates" ~ "Intermediary rates",
      cluster_kmeans3 == "3: highest unsafe abortion rates" ~ "Highest rates"
    )
  )

df_ans_final <- df_ans_final |>
  mutate(
    cluster_kmeans3 = case_when(
      cluster_kmeans3 == 1 ~ "1: lowest unsafe abortion rates",
      cluster_kmeans3 == 2 ~ "2: intermediary unsafe abortion rates",
      cluster_kmeans3 == 3 ~ "3: highest unsafe abortion rates"
    )
  ) |>
  mutate(
    cluster_kmeans3_num = case_when(
      cluster_kmeans3 == "1: lowest unsafe abortion rates" ~ "Lowest rates",
      cluster_kmeans3 == "2: intermediary unsafe abortion rates" ~ "Intermediary rates",
      cluster_kmeans3 == "3: highest unsafe abortion rates" ~ "Highest rates"
    )  
    )


df_sus_inicio <- df_sus_inicio |>
  mutate(
    cluster_kmeans3 = case_when(
      cluster_kmeans3 == 1 ~ "1: lowest unsafe abortion rates",
      cluster_kmeans3 == 2 ~ "2: intermediary unsafe abortion rates",
      cluster_kmeans3 == 3 ~ "3: highest unsafe abortion rates"
    )
  ) |>
  mutate(
    cluster_kmeans3_num = case_when(
      cluster_kmeans3 == "1: lowest unsafe abortion rates" ~ "Lowest rates",
      cluster_kmeans3 == "2: intermediary unsafe abortion rates" ~ "Intermediary rates",
      cluster_kmeans3 == "3: highest unsafe abortion rates" ~ "Highest rates"
    )
  )

df_sus_final <- df_sus_final |>
  mutate(
    cluster_kmeans3 = case_when(
      cluster_kmeans3 == 2 ~ "1: lowest unsafe abortion rates",
      cluster_kmeans3 == 1 ~ "2: intermediary unsafe abortion rates",
      cluster_kmeans3 == 3 ~ "3: highest unsafe abortion rates"
    )
  ) |>
  mutate(
    cluster_kmeans3_num = case_when(
      cluster_kmeans3 == "1: lowest unsafe abortion rates" ~ "Lowest rates",
      cluster_kmeans3 == "2: intermediary unsafe abortion rates" ~ "Intermediary rates",
      cluster_kmeans3 == "3: highest unsafe abortion rates" ~ "Highest rates"
    )
  )


# ----- Criando as funções que serão utilizadas para a construção das tabelas e boxplots ------

cria_tabelas <- function(variaveis, df, var_grupos, atendimento, salvar_excel = FALSE, nome_arquivo = "tabelas_resumo.xlsx") {
  
  grupos <- levels(as.factor(df[[var_grupos]]))
  # Criamos uma lista vazia para armazenar os data frames
  lista_dfs <- list()
  
  for (variavel in variaveis) {
    
    # Criamos o data frame
    df_resumo <- data.frame(
      Variable = variavel, # Coluna para identificar no Excel
      Cluster = grupos,
      n = unlist(lapply(grupos, function(g) nrow(df[df[[var_grupos]] == g, ]))),
      Min = round(unlist(lapply(grupos, function(g) min(df[df[[var_grupos]] == g, ][[variavel]], na.rm = TRUE))), 2),
      `1Q` = round(unlist(lapply(grupos, function(g) quantile(df[df[[var_grupos]] == g, ][[variavel]], 0.25, na.rm = TRUE))), 2),
      Mean = round(unlist(lapply(grupos, function(g) mean(df[df[[var_grupos]] == g, ][[variavel]], na.rm = TRUE))), 2),
      Median = round(unlist(lapply(grupos, function(g) median(df[df[[var_grupos]] == g, ][[variavel]], na.rm = TRUE))), 2),
      SD = round(unlist(lapply(grupos, function(g) sd(df[df[[var_grupos]] == g, ][[variavel]], na.rm = TRUE))), 2),
      `3Q` = round(unlist(lapply(grupos, function(g) quantile(df[df[[var_grupos]] == g, ][[variavel]], 0.75, na.rm = TRUE))), 2),
      Max = round(unlist(lapply(grupos, function(g) max(df[df[[var_grupos]] == g, ][[variavel]], na.rm = TRUE))), 2)
    )
    
    # Armazenamos na lista
    lista_dfs[[variavel]] <- df_resumo
    
    # Geramos o kable para visualização
    tabela_kable <- kable(
      df_resumo[, -1], # Removemos a coluna "variavel" apenas no print
      align = "cccccccc",
      col.names = c("Cluster", "n", "Min.", "1Q", "Mean", "Median", "S.D.", "3Q", "Max."),
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

cria_boxplots <- function(variaveis, df, var_grupos, labels_y, titulo_geral, nome_arquivo = "boxplots_final.png") {
  
    variavel <- variaveis[1]
    label_atual <- labels_y[1] # Pega o label correspondente à variável
    
    boxplots <- ggplot(df) +
      geom_boxplot(aes(x = as.factor(.data[[var_grupos]]), y = .data[[variavel]]), 
                   fill = "lightblue", outlier.alpha = 0.5) +
      labs(y = label_atual, x = "Group", title = titulo_geral) + 
      theme_classic() + 
      theme(
        legend.position = "right",
        legend.direction = "vertical"
      )
  
  # Combinando os gráficos com patchwork
  # wrap_plots transforma a lista em um layout; ncol define as colunas
  grafico_final <- boxplots
  
  # Exibe o gráfico no RStudio
  print(grafico_final)
  
  # Salva o gráfico
  ggsave(nome_arquivo, plot = grafico_final, width = 5 * length(variaveis), height = 6, dpi = 300)
  
  message(paste("Gráfico salvo como:", nome_arquivo))
  
  return(invisible(grafico_final))
}

## -- Análises para os melhores métodos de cada categoria de atendimento + período

# Visualizar e salvar tabelas
df_resumo_clusters_ans_inicio <- cria_tabelas(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio"),
                         df = df_ans_inicio, 
                         var_grupos = "cluster_kmeans3", 
                         atendimento = " ",
                         salvar_excel = TRUE, 
                         nome_arquivo = "databases/tabela_resumo_clusters_ans_inicio_kmeans3.xlsx")

df_resumo_clusters_ans_final <- cria_tabelas(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio"),
                                              df = df_ans_final, 
                                              var_grupos = "cluster_kmeans3", 
                                              atendimento = " ",
                                              salvar_excel = TRUE, 
                                              nome_arquivo = "databases/tabela_resumo_clusters_ans_final_kmeans3.xlsx")


df_resumo_clusters_sus_inicio <- cria_tabelas(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio"),
                                              df = df_sus_inicio, 
                                              var_grupos = "cluster_kmeans3", 
                                              atendimento = " ",
                                              salvar_excel = TRUE, 
                                              nome_arquivo = "databases/tabela_resumo_clusters_sus_inicio_kmeans3.xlsx")

df_resumo_clusters_sus_final <- cria_tabelas(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio"),
                                              df = df_sus_final, 
                                              var_grupos = "cluster_kmeans3", 
                                              atendimento = " ",
                                              salvar_excel = TRUE, 
                                              nome_arquivo = "databases/tabela_resumo_clusters_sus_final_kmeans3.xlsx")


cria_boxplots(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio"),
              df = df_ans_inicio, var_grupos = "cluster_kmeans3_num",
              labels_y = c("Unsafe abortion rate per \n 1000 women in reproductive age"),
              titulo_geral = " ",
              nome_arquivo = "figuras/clustering/boxplot_clusters_ans_inicio_kmeans3.png")

cria_boxplots(variaveis = c("ans_tx_abortos_mil_mulheres_valor_medio"),
              df = df_ans_final, var_grupos = "cluster_kmeans3_num",
              labels_y = c("Unsafe abortion rate per \n 1000 women in reproductive age"),
              titulo_geral = " ",
              nome_arquivo = "figuras/clustering/boxplot_clusters_ans_final_kmeans3.png")


cria_boxplots(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio"),
              df = df_sus_inicio, var_grupos = "cluster_kmeans3_num",
              labels_y = c("Unsafe abortion rate per \n 1000 women in reproductive age"),
              titulo_geral = " ",
              nome_arquivo = "figuras/clustering/boxplot_clusters_sus_inicio_kmeans3.png")


cria_boxplots(variaveis = c("sus_tx_abortos_mil_mulheres_valor_medio"),
              df = df_sus_final, var_grupos = "cluster_kmeans3_num",
              labels_y = c("Unsafe abortion rate per \n 1000 women in reproductive age"),
              titulo_geral = " ",
              nome_arquivo = "figuras/clustering/boxplot_clusters_sus_final_kmeans3.png")

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
  geom_sf(data = df_mapa_ans_inicio, aes(fill = cluster_kmeans3), color = NA) +
  labs(title = " ") +
  scale_fill_viridis_d(name = "Groups", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
mp1
ggsave("figuras/clustering/mapa_cluster_ans_inicio_kmeans3.png", mp1,  width = 10, height = 6)


## Juntando os dois dataframes
df_mapa_ans_final <- left_join(df_ans_final, df_muni_sf) |>
  st_as_sf()

## Criando os mapas
mp2 <- ggplot() +
  geom_sf(data = df_mapa_ans_final, aes(fill = cluster_kmeans3), color = NA) +
  labs(title = " ") +
  scale_fill_viridis_d(name = "Groups", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
mp2
ggsave("figuras/clustering/mapa_cluster_ans_final_kmeans3.png", mp2,  width = 10, height = 6)


## Juntando os dois dataframes
df_mapa_sus_inicio <- left_join(df_sus_inicio, df_muni_sf) |>
  st_as_sf()

## Criando os mapas
mp3 <- ggplot() +
  geom_sf(data = df_mapa_sus_inicio, aes(fill = cluster_kmeans3), color = NA) +
  labs(title = " ") +
  scale_fill_viridis_d(name = "Groups", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
mp3
ggsave("figuras/clustering/mapa_cluster_sus_inicio_kmeans3.png", mp3,  width = 10, height = 6)


## Juntando os dois dataframes
df_mapa_sus_final <- left_join(df_sus_final, df_muni_sf) |>
  st_as_sf()

## Criando os mapas
mp4 <- ggplot() +
  geom_sf(data = df_mapa_sus_final, aes(fill = cluster_kmeans3), color = NA) +
  labs(title = " ") +
  scale_fill_viridis_d(name = "Groups", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
mp4
ggsave("figuras/clustering/mapa_cluster_sus_final_kmeans3.png", mp4,  width = 10, height = 6)
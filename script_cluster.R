library(dplyr)
library(janitor)
library(factoextra)
library(clusterSim)
library(clValid)
library(knitr)
library(htmltools)
library(gridExtra)
library(rstatix)
library(sf)
library(geobr)

# Preparando os dados -----------------------------------------------------
## Lendo uma tabela com dados auxiliares para os municípios
df_aux_municios <- read.csv("databases/df_aux_municipios.csv") |>
  select(codmunres, municipio, uf, idhm) 

## Lendo os dados necessários para o cálculo do indicadores de taxa de fecundidade 
df_fecundidade_menor_20 <- read.csv("databases/indicadores_bloco2_planejamento_reprodutivo_SUS_ANS_2012_2023.csv") |>
  select(codmunres, ano, nvm_menor_que_20, pop_feminina_10_a_19) |>
  filter(ano < 2022)

df_fecundidade_outras_idades <- read.csv("databases/dados_faixas_etarias_adicionais.csv") 

## Juntando as duas bases e calculando os indicadores de fecundidade para o ano de 2019
df_fecundidade <- full_join(df_fecundidade_menor_20, df_fecundidade_outras_idades) |>
  left_join(df_aux_municios) |>
  filter(ano == 2019) |>
  mutate(
    tx_fecundidade_menor_20 = round(nvm_menor_que_20 / pop_feminina_10_a_19 * 1000, 1),
    tx_fecundidade_10_a_14 = round(nvm_10_a_14 / pop_feminina_10_a_14 * 1000, 1),
    tx_fecundidade_15_a_19 = round(nvm_15_a_19 / pop_feminina_15_a_19 * 1000, 1),
    .keep = "unused"
  )

## Lendo os dados para o cálculo da cobertura AB e % de usuárias exclusivas do SUS
df_bloco1 <- read.csv("databases/indicadores_bloco1_socioeconomicos_2012-2023.csv") |>
  select(codmunres, ano, media_cobertura_esf, populacao_total, pop_fem_10_49_com_plano_saude, populacao_feminina_10_a_49) |>
  filter(ano == 2019) |>
  mutate(
    cobertura_ab = round(media_cobertura_esf / populacao_total * 100, 1),
    porc_exclusivas_sus = round((populacao_feminina_10_a_49 - pop_fem_10_49_com_plano_saude) / populacao_feminina_10_a_49 * 100, 1),
    .keep = "unused"
  ) 

## Juntando com os dados da taxa de fecundidade
df_indicadores <- left_join(df_fecundidade, df_bloco1) 

rm(df_fecundidade, df_fecundidade_menor_20, df_fecundidade_outras_idades, df_aux_municios, df_bloco1)


# Análise de clusters (utilizando apenas as taxas de fecundidade) ---------
dados1 <- df_indicadores |> select(tx_fecundidade_10_a_14) |> scale()
dados2 <- df_indicadores |> select(tx_fecundidade_15_a_19) |> scale()
dados3 <- df_indicadores |> select(tx_fecundidade_menor_20) |> scale()

dados1_dist <- dist(dados1, method = "euclidean")
dados2_dist <- dist(dados2, method = "euclidean")
dados3_dist <- dist(dados2, method = "euclidean")

## K-means ----------------------------------------------------------------
### Gráficos do cotovelo
fviz_nbclust(dados1, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (10 a 14 anos)"
  )  # k = 3 ou k = 4

fviz_nbclust(dados2, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (15 a 19 anos)"
  )  # k = 3, 4 ou 5

fviz_nbclust(dados3, kmeans, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-médias (menores de 20 anos)"
  ) # k = 3, 4 ou 5

### Ajustando o k-means com os números de grupos escolhidos
set.seed(1504)
d1_kmeans3 <- kmeans(dados1, 3)
round(d1_kmeans3$centers, 3)
table(d1_kmeans3$cluster)

set.seed(1504)
d1_kmeans4 <- kmeans(dados1, 4)
round(d1_kmeans4$centers, 3)
table(d1_kmeans4$cluster)

set.seed(1504)
d2_kmeans3 <- kmeans(dados2, 3)
round(d2_kmeans3$centers, 3)
table(d2_kmeans3$cluster)

set.seed(1504)
d2_kmeans4 <- kmeans(dados2, 4)
round(d2_kmeans4$centers, 3)
table(d2_kmeans4$cluster)

set.seed(1504)
d2_kmeans5 <- kmeans(dados2, 5)
round(d2_kmeans5$centers, 3)
table(d2_kmeans5$cluster)

set.seed(1504)
d3_kmeans3 <- kmeans(dados3, 3)
round(d3_kmeans3$centers, 3)
table(d3_kmeans3$cluster)

set.seed(1504)
d3_kmeans4 <- kmeans(dados3, 4)
round(d3_kmeans4$centers, 3)
table(d3_kmeans4$cluster)

set.seed(1504)
d3_kmeans5 <- kmeans(dados3, 5)
round(d3_kmeans5$centers, 3)
table(d3_kmeans5$cluster)


## K-medoids --------------------------------------------------------------
### Gráficos do cotovelo
fviz_nbclust(dados1, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-medoides (PAM) (10 a 14 anos)"
  )  # k = 3 ou k = 4

fviz_nbclust(dados2, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-medoides (PAM) (15 a 19 anos)"
  )  # k = 4 ou k = 5

fviz_nbclust(dados3, cluster::pam, method = "wss") +
  labs(
    x = "Número de clusters", y = "Variância total intragrupo",
    title = "Gráfico do cotovelo para o método K-medoides (PAM) (menores de 20 anos)"
  ) # k = 4

### Ajustando o k-medoids com os números de grupos escolhidos 
set.seed(1504)
d1_pam3 <- cluster::pam(dados1, k = 3)
round(d1_pam3$medoids, 3)
table(d1_pam3$cluster)

set.seed(1504)
d1_pam4 <- cluster::pam(dados1, k = 4)
round(d1_pam4$medoids, 3)
table(d1_pam4$cluster)

set.seed(1504)
d2_pam4 <- cluster::pam(dados1, k = 4)
round(d2_pam4$medoids, 3)
table(d2_pam4$cluster)

set.seed(1504)
d2_pam5 <- cluster::pam(dados1, k = 5)
round(d2_pam5$medoids, 3)
table(d2_pam5$cluster)

set.seed(1504)
d3_pam4 <- cluster::pam(dados1, k = 4)
round(d3_pam4$medoids, 3)
table(d3_pam4$cluster)


## Ajustando métodos hierárquicos -----------------------------------------
### Método de Ward
d1_ward <- hclust(dados1_dist, method = "ward.D2")
plot(as.dendrogram(d1_ward), main = "Dendrograma do método de Ward (10 a 14 anos)", ylab = "Altura") # K = 3
rect.hclust(d1_ward, k = 3, border = 2:5)
d1_ward3_class <- cutree(d1_ward, k = 3)
table(d1_ward3_class)

d2_ward <- hclust(dados2_dist, method = "ward.D2")
plot(as.dendrogram(d2_ward), main = "Dendrograma do método de Ward (15 a 19 anos)", ylab = "Altura") # K = 3
rect.hclust(d2_ward, k = 3, border = 2:5)
d2_ward3_class <- cutree(d2_ward, k = 3)
table(d2_ward3_class)

d3_ward <- hclust(dados3_dist, method = "ward.D2")
plot(as.dendrogram(d3_ward), main = "Dendrograma do método de Ward (menores de 20 anos)", ylab = "Altura") # K = 3
rect.hclust(d3_ward, k = 3, border = 2:5)
d3_ward3_class <- cutree(d3_ward, k = 3)
table(d3_ward3_class)


### Single linkage
d1_single <- hclust(dados1_dist, method = "single")
plot(as.dendrogram(d1_single), main = "Dendrograma do método de single linkage (10 a 14 anos)", ylab = "Altura") # Não ficou bom

d2_single <- hclust(dados2_dist, method = "single")
plot(as.dendrogram(d2_single), main = "Dendrograma do método de single linkage (15 a 19 anos)", ylab = "Altura") # Não ficou bom

d3_single <- hclust(dados3_dist, method = "single")
plot(as.dendrogram(d3_single), main = "Dendrograma do método de single linkage (menores de 20 anos)", ylab = "Altura") # Não ficou bom


### Complete linkage
d1_complete <- hclust(dados1_dist, method = "complete")
plot(as.dendrogram(d1_complete), main = "Dendrograma do método de complete linkage (10 a 14 anos)", ylab = "Altura") # Não ficou bom

d2_complete <- hclust(dados2_dist, method = "complete")
plot(as.dendrogram(d2_complete), main = "Dendrograma do método de complete linkage (15 a 19 anos)", ylab = "Altura") # Não ficou bom

d3_complete <- hclust(dados3_dist, method = "complete")
plot(as.dendrogram(d3_complete), main = "Dendrograma do método de complete linkage (menores de 20 anos)", ylab = "Altura") # Não ficou bom


### Average linkage
d1_average <- hclust(dados1_dist, method = "average")
plot(as.dendrogram(d1_average), main = "Dendrograma do método de average linkage (10 a 14 anos)", ylab = "Altura") # Não ficou bom

d2_average <- hclust(dados2_dist, method = "average")
plot(as.dendrogram(d2_average), main = "Dendrograma do método de average linkage (15 a 19 anos)", ylab = "Altura") # Não ficou bom

d3_average <- hclust(dados3_dist, method = "average")
plot(as.dendrogram(d3_average), main = "Dendrograma do método de average linkage (menores de 20 anos)", ylab = "Altura") # Não ficou bom

### Em resumo: dos métodos hierárquicos, só o de Ward apresentou resultados aceitáveis

## Escolhendo o melhor método para cada análise ---------------------------
### 10 a 14 anos ----------------------------------------------------------
ls()[grep("^d1_kmeans", ls())]
d1_kmeans_index <- data.frame(
  metodo = unlist(lapply(3:4, function(i) paste0("kmeans", i))),
  db_index_cent = unlist(lapply(3:4, function(i) index.DB(dados1, get(paste0("d1_kmeans", i))$cluster)$DB)),
  db_index_med = unlist(lapply(3:4, function(i) index.DB(dados1, get(paste0("d1_kmeans", i))$cluster, d = dados1_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:4, function(i) dunn(distance = dados1_dist, get(paste0("d1_kmeans", i))$cluster))),
  silh_index = unlist(lapply(3:4, function(i) index.S(dados1_dist, get(paste0("d1_kmeans", i))$cluster))),
  ch_index_cent = unlist(lapply(3:4, function(i) index.G1(dados1, get(paste0("d1_kmeans", i))$cluster))),
  ch_index_med = unlist(lapply(3:4, function(i) index.G1(dados1, get(paste0("d1_kmeans", i))$cluster, d = dados1_dist, centrotypes = "medoids")))
)

ls()[grep("^d1_pam", ls())]
d1_pam_index <- data.frame(
  metodo = unlist(lapply(3:4, function(i) paste0("pam", i))),
  db_index_cent = unlist(lapply(3:4, function(i) index.DB(dados1, get(paste0("d1_pam", i))$cluster)$DB)),
  db_index_med = unlist(lapply(3:4, function(i) index.DB(dados1, get(paste0("d1_pam", i))$cluster, d = dados1_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:4, function(i) dunn(distance = dados1_dist, get(paste0("d1_pam", i))$cluster))),
  silh_index = unlist(lapply(3:4, function(i) index.S(dados1_dist, get(paste0("d1_pam", i))$cluster))),
  ch_index_cent = unlist(lapply(3:4, function(i) index.G1(dados1, get(paste0("d1_pam", i))$cluster))),
  ch_index_med = unlist(lapply(3:4, function(i) index.G1(dados1, get(paste0("d1_pam", i))$cluster, d = dados1_dist, centrotypes = "medoids")))
)

ls()[grep("^d1_ward", ls())]
d1_ward_index <- data.frame(
  metodo = unlist(lapply(3, function(i) paste0("ward", i))),
  db_index_cent = unlist(lapply(3, function(i) index.DB(dados1, get(paste0("d1_ward", i, "_class")))$DB)),
  db_index_med = unlist(lapply(3, function(i) index.DB(dados1, get(paste0("d1_ward", i, "_class")), d = dados1_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3, function(i) dunn(distance = dados1_dist, get(paste0("d1_ward", i, "_class"))))),
  silh_index = unlist(lapply(3, function(i) index.S(dados1_dist, get(paste0("d1_ward", i, "_class"))))),
  ch_index_cent = unlist(lapply(3, function(i) index.G1(dados1, get(paste0("d1_ward", i, "_class"))))),
  ch_index_med = unlist(lapply(3, function(i) index.G1(dados1, get(paste0("d1_ward", i, "_class")), d = dados1_dist, centrotypes = "medoids")))
)

d1_avaliacao <- rbind(
  d1_pam_index,
  d1_kmeans_index, 
  d1_ward_index
)

#### db_index_cent: menor melhor (kmeans4, seguido de pam4 e ward3)
#### db_index_med: menor melhor (kmeans4, seguido de kmeans3 e pam4)
#### dunn_index: maior melhor (kmeans4, seguido de kmeans3 e pam3)
#### silh_index: maior melhor (pam4, seguido de kmeans4 e kmeans3)
#### ch_index_cent: maior melhor (kmeans4, seguido de kmeans3)
#### ch_index_med: maior melhor (kmeans4, seguido de kmeans3)

#### Escolha: apesar de o kmeans4 ter desempenhado melhor, um dos clusters ficou com poucos municípios (60). Assim, vou de K-means com K = 3.
table(d1_kmeans4$cluster)
table(d1_kmeans3$cluster)

df_indicadores$cluster_10_a_14 <- as.factor(d1_kmeans3$cluster)

### 15 a 19 anos ----------------------------------------------------------
ls()[grep("^d2_kmeans", ls())]
d2_kmeans_index <- data.frame(
  metodo = unlist(lapply(3:5, function(i) paste0("kmeans", i))),
  db_index_cent = unlist(lapply(3:5, function(i) index.DB(dados2, get(paste0("d2_kmeans", i))$cluster)$DB)),
  db_index_med = unlist(lapply(3:5, function(i) index.DB(dados2, get(paste0("d2_kmeans", i))$cluster, d = dados2_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:5, function(i) dunn(distance = dados2_dist, get(paste0("d2_kmeans", i))$cluster))),
  silh_index = unlist(lapply(3:5, function(i) index.S(dados2_dist, get(paste0("d2_kmeans", i))$cluster))),
  ch_index_cent = unlist(lapply(3:5, function(i) index.G1(dados2, get(paste0("d2_kmeans", i))$cluster))),
  ch_index_med = unlist(lapply(3:5, function(i) index.G1(dados2, get(paste0("d2_kmeans", i))$cluster, d = dados2_dist, centrotypes = "medoids")))
)

ls()[grep("^d2_pam", ls())]
d2_pam_index <- data.frame(
  metodo = unlist(lapply(4:5, function(i) paste0("pam", i))),
  db_index_cent = unlist(lapply(4:5, function(i) index.DB(dados2, get(paste0("d2_pam", i))$cluster)$DB)),
  db_index_med = unlist(lapply(4:5, function(i) index.DB(dados2, get(paste0("d2_pam", i))$cluster, d = dados2_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(4:5, function(i) dunn(distance = dados2_dist, get(paste0("d2_pam", i))$cluster))),
  silh_index = unlist(lapply(4:5, function(i) index.S(dados2_dist, get(paste0("d2_pam", i))$cluster))),
  ch_index_cent = unlist(lapply(4:5, function(i) index.G1(dados2, get(paste0("d2_pam", i))$cluster))),
  ch_index_med = unlist(lapply(4:5, function(i) index.G1(dados2, get(paste0("d2_pam", i))$cluster, d = dados2_dist, centrotypes = "medoids")))
)

ls()[grep("^d2_ward", ls())]
d2_ward_index <- data.frame(
  metodo = unlist(lapply(3, function(i) paste0("ward", i))),
  db_index_cent = unlist(lapply(3, function(i) index.DB(dados2, get(paste0("d2_ward", i, "_class")))$DB)),
  db_index_med = unlist(lapply(3, function(i) index.DB(dados2, get(paste0("d2_ward", i, "_class")), d = dados2_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3, function(i) dunn(distance = dados2_dist, get(paste0("d2_ward", i, "_class"))))),
  silh_index = unlist(lapply(3, function(i) index.S(dados2_dist, get(paste0("d2_ward", i, "_class"))))),
  ch_index_cent = unlist(lapply(3, function(i) index.G1(dados2, get(paste0("d2_ward", i, "_class"))))),
  ch_index_med = unlist(lapply(3, function(i) index.G1(dados2, get(paste0("d2_ward", i, "_class")), d = dados2_dist, centrotypes = "medoids")))
)

d2_avaliacao <- rbind(
  d2_pam_index,
  d2_kmeans_index, 
  d2_ward_index
)

#### db_index_cent: menor melhor (kmeans5, seguido de kmeans4)
#### db_index_med: menor melhor (kmeans5, seguido de kmeans4)
#### dunn_index: maior melhor (kmeans5, seguido de kmeans4)
#### silh_index: maior melhor (kmeans3, seguido de kmeans4 (muito próximos))
#### ch_index_cent: maior melhor (kmeans5, seguido de kmeans4)
#### ch_index_med: maior melhor (kmeans5, seguido de kmeans4)

#### Escolha: apesar de o kmeans5 ter apresentado os melhores resultados, um dos grupos ficou muito pequenos (n = 45). Assim, vou no método K-means com K = 4.
table(d2_kmeans5$cluster)
table(d2_kmeans4$cluster)

df_indicadores$cluster_15_a_19 <- as.factor(d2_kmeans4$cluster)

### Menores de 20 anos ----------------------------------------------------
ls()[grep("^d3_kmeans", ls())]
d3_kmeans_index <- data.frame(
  metodo = unlist(lapply(3:5, function(i) paste0("kmeans", i))),
  db_index_cent = unlist(lapply(3:5, function(i) index.DB(dados3, get(paste0("d3_kmeans", i))$cluster)$DB)),
  db_index_med = unlist(lapply(3:5, function(i) index.DB(dados3, get(paste0("d3_kmeans", i))$cluster, d = dados3_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3:5, function(i) dunn(distance = dados3_dist, get(paste0("d3_kmeans", i))$cluster))),
  silh_index = unlist(lapply(3:5, function(i) index.S(dados3_dist, get(paste0("d3_kmeans", i))$cluster))),
  ch_index_cent = unlist(lapply(3:5, function(i) index.G1(dados3, get(paste0("d3_kmeans", i))$cluster))),
  ch_index_med = unlist(lapply(3:5, function(i) index.G1(dados3, get(paste0("d3_kmeans", i))$cluster, d = dados3_dist, centrotypes = "medoids")))
)

ls()[grep("^d3_pam", ls())]
d3_pam_index <- data.frame(
  metodo = unlist(lapply(4, function(i) paste0("pam", i))),
  db_index_cent = unlist(lapply(4, function(i) index.DB(dados3, get(paste0("d3_pam", i))$cluster)$DB)),
  db_index_med = unlist(lapply(4, function(i) index.DB(dados3, get(paste0("d3_pam", i))$cluster, d = dados3_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(4, function(i) dunn(distance = dados3_dist, get(paste0("d3_pam", i))$cluster))),
  silh_index = unlist(lapply(4, function(i) index.S(dados3_dist, get(paste0("d3_pam", i))$cluster))),
  ch_index_cent = unlist(lapply(4, function(i) index.G1(dados3, get(paste0("d3_pam", i))$cluster))),
  ch_index_med = unlist(lapply(4, function(i) index.G1(dados3, get(paste0("d3_pam", i))$cluster, d = dados3_dist, centrotypes = "medoids")))
)

ls()[grep("^d3_ward", ls())]
d3_ward_index <- data.frame(
  metodo = unlist(lapply(3, function(i) paste0("ward", i))),
  db_index_cent = unlist(lapply(3, function(i) index.DB(dados3, get(paste0("d3_ward", i, "_class")))$DB)),
  db_index_med = unlist(lapply(3, function(i) index.DB(dados3, get(paste0("d3_ward", i, "_class")), d = dados3_dist, centrotypes = "medoids")$DB)),
  dunn_index = unlist(lapply(3, function(i) dunn(distance = dados3_dist, get(paste0("d3_ward", i, "_class"))))),
  silh_index = unlist(lapply(3, function(i) index.S(dados3_dist, get(paste0("d3_ward", i, "_class"))))),
  ch_index_cent = unlist(lapply(3, function(i) index.G1(dados3, get(paste0("d3_ward", i, "_class"))))),
  ch_index_med = unlist(lapply(3, function(i) index.G1(dados3, get(paste0("d3_ward", i, "_class")), d = dados3_dist, centrotypes = "medoids")))
)

d3_avaliacao <- rbind(
  d3_pam_index,
  d3_kmeans_index, 
  d3_ward_index
)

#### db_index_cent: menor melhor (kmeans5, seguido de kmeans4)
#### db_index_med: menor melhor (kmeans4, seguido de kmeans5)
#### dunn_index: maior melhor (ward3)
#### silh_index: maior melhor (kmeans3, seguido de ward3 e kmeans4)
#### ch_index_cent: maior melhor (kmeans5, seguido de kmeans4)
#### ch_index_med: maior melhor (kmeans5, seguido de kmeans4)

#### Comentário: apesar de o kmeans5 ter apresentado os melhores resultados, um dos grupos ficou com poucos municípios (46).
#### Assim, vou optar pelo kmeans 4, uma vez que o ward3 não apresentou bons resultados além do índice de silhueta (o de Dunn ficou esquisito)
table(d3_kmeans5$cluster)
table(d3_kmeans4$cluster)

#### Escolha: método K-means com K = 4.
df_indicadores$cluster_20 <- as.factor(d3_kmeans4$cluster)

rm(list = ls()[ls() != "df_indicadores"])

# Comparando os grupos em relação às variáveis de interesse ---------------
#Criando as funções que serão utilizadas para a construção das tabelas e boxplots
nomes_variaveis <- c("idhm", "cobertura_ab", "porc_exclusivas_sus")

cria_tabelas <- function(variaveis = nomes_variaveis, df = df_indicadores, var_grupos, label_tabela, faixa_etaria) {
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
      caption = HTML(ifelse(variavel == variaveis[1], paste0(glue::glue("Medidas resumo das variáveis de interesse para os grupos de municípios agrupados pela taxa de fecundidade em mulheres {faixa_etaria}. <br><br>"), variavel), variavel)),
      label = ifelse(variavel == variaveis[1], paste0("tabela", label_tabela), NA)
    )
    
    print(tabela)
  }
}

cria_tabelas(var_grupos = "cluster_10_a_14", label_tabela = "1", faixa_etaria = "de 10 a 14 anos")
cria_tabelas(var_grupos = "cluster_15_a_19", label_tabela = "2", faixa_etaria = "de 15 a 19 anos")
cria_tabelas(var_grupos = "cluster_20", label_tabela = "3", faixa_etaria = "menores de 20 anos")

cria_boxplots <- function(variaveis = nomes_variaveis, df = df_indicadores, var_grupos) {
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

cria_boxplots(var_grupos = "cluster_10_a_14")
cria_boxplots(var_grupos = "cluster_15_a_19")
cria_boxplots(var_grupos = "cluster_20")


cria_tabelas_testes <- function(variaveis = nomes_variaveis, df = df_indicadores, var_grupos, label_tabela, faixa_etaria) {
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
    
    if (faixa_etaria == "de 10 a 14 anos") {
      df_teste <- data.frame(
        grupo1 = monta_linhas(1),
        grupo2 = c(rep("", 1), monta_linhas(2)),
        row.names = unique(resultados_dunn$group2)
      )
      
      colnames(df_teste) <- unique(resultados_dunn$group1)
      
      tabela <- kable(
        df_teste,
        align = c("cc"),
        caption = HTML(ifelse(variavel == variaveis[1], paste0(glue::glue("Resultados dos testes de Dunn (com correção de Bonferroni) para as comparações múltiplas entre os pares de grupos de municípios agrupados pela taxa de fecundidade em mulheres {faixa_etaria}. <br><br>"), variavel), variavel)),
        label = ifelse(variavel == variaveis[1], paste0("tabela-", label_tabela), NA)
      )
      
    } else {
      df_teste <- data.frame(
        grupo1 = monta_linhas(1),
        grupo2 = c(rep("", 1), monta_linhas(2)),
        grupo3 = c(rep("", 2), monta_linhas(3)),
        row.names = unique(resultados_dunn$group2)
      )
      
      colnames(df_teste) <- unique(resultados_dunn$group1)
      
      tabela <- kable(
        df_teste,
        align = c("ccc"),
        caption = HTML(ifelse(variavel == variaveis[1], paste0(glue::glue("Resultados dos testes de Dunn (com correção de Bonferroni) para as comparações múltiplas entre os pares de grupos de municípios agrupados pela taxa de fecundidade em mulheres {faixa_etaria}. <br><br>"), variavel), variavel)),
        label = ifelse(variavel == variaveis[1], paste0("tabela-", label_tabela), NA)
      )
    }
    
    print(tabela)
  }
}

cria_tabelas_testes(var_grupos = "cluster_10_a_14", faixa_etaria = "de 10 a 14 anos", label_tabela = "1")
cria_tabelas_testes(var_grupos = "cluster_15_a_19", faixa_etaria = "de 15 a 19 anos", label_tabela = "2")
cria_tabelas_testes(var_grupos = "cluster_20", faixa_etaria = "menores de 20 anos", label_tabela = "3")


# Mapas do Brasil dos clusters --------------------------------------------
## Baixando os dados de geometria
df_muni_sf <- read_municipality(year = 2019, showProgress = FALSE) |>
  mutate(codmunres = as.numeric(substr(code_muni, 1, 6)))

df_ufs_sf <- read_state(year = 2019, showProgress = FALSE)

get_dupes(df_muni_sf, codmunres)

## Juntando os dois dataframes
df_dados_mapa <- left_join(df_indicadores, df_muni_sf) |>
  st_as_sf()

## Criando os mapas
ggplot() +
  geom_sf(data = df_dados_mapa, aes(fill = cluster_10_a_14), color = NA) +
  labs(title = "Mapa do Brasil com os grupos de municípios agrupados pela taxa de fecundidade de \nmulheres de 10 a 14 anos de idade (por mil)") +
  scale_fill_viridis_d(name = "Grupo", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw()

ggplot() +
  geom_sf(data = df_dados_mapa, aes(fill = cluster_15_a_19), color = NA) +
  labs(title = "Mapa do Brasil com os grupos de municípios agrupados pela taxa de fecundidade de \nmulheres de 15 a 19 anos de idade (por mil)") +
  scale_fill_viridis_d(name = "Grupo", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw()

ggplot() +
  geom_sf(data = df_dados_mapa, aes(fill = cluster_20), color = NA) +
  labs(title = "Mapa do Brasil com os grupos de municípios agrupados pela taxa de fecundidade de \nmulheres menores de 20 anos de idade (por mil)") +
  scale_fill_viridis_d(name = "Grupo", end = 0.8, alpha = 0.6) +
  geom_sf(data = df_ufs_sf, fill = NA, linewidth = 0.08, color = "black") +
  theme_bw()


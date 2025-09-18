# ---- Setup ----
library(tidyverse)
library(purrr)          # for map_df
library(cluster)        # daisy, pam, silhouette
library(factoextra)     # visuals
library(NbClust)
library(ComplexHeatmap) # heatmaps
library(circlize)       # colors

set.seed(42)

# ---- Example data (mixed numeric + categorical) ----
n <- 150
ihc <- tibble(
  case_id = paste0("C", seq_len(n)),
  GATA6   = rnorm(n, 150, 40),           # H-score 0-300
  CK17    = rbinom(n, 1, 0.4),           # binary
  p63     = sample(0:3, n, TRUE),        # 0/1/2/3 ordinal intensity
  CK5_6   = rnorm(n, 100, 35),           # H-score
  CD44    = rbeta(n, 2, 5) * 100,        # % positivity
  FOXI1   = rnorm(n, 30, 20)             # % positivity
)

# Encode categorical as factors / ordered
ihc_pre <- ihc %>%
  mutate(
    CK17 = factor(CK17, levels = c(0, 1), labels = c("neg", "pos")),
    p63  = ordered(p63, levels = 0:3)
  )

markers <- c("GATA6","CK17","p63","CK5_6","CD44","FOXI1")
X <- ihc_pre %>% dplyr::select(all_of(markers))
row_ids <- ihc_pre$case_id

# ---- Gower distance (mixed types) ----
# optional weights (example: emphasize GATA6 & CK5_6)
w <- rep(1, length(markers)); names(w) <- markers
w[c("GATA6","CK5_6")] <- 1.2

d <- daisy(X, metric = "gower", weights = w)
summary(d)

# ---- Choose k by silhouette (PAM on Gower) ----
sil_df <- map_df(2:8, function(k) {
  pam_fit <- pam(d, k = k, diss = TRUE)
  sil     <- silhouette(pam_fit)                        # <-- call the function
  tibble(k = k, avg_sil = mean(sil[, "sil_width"]))
})

sil_df %>% arrange(desc(avg_sil))
best_k <- sil_df$k[which.max(sil_df$avg_sil)]

# ---- Final PAM fit & silhouette plot ----
pam_fit    <- pam(d, k = best_k, diss = TRUE)
cluster_id <- factor(pam_fit$clustering, labels = paste0("Cluster", seq_len(best_k)))
fviz_silhouette(silhouette(pam_fit), label = FALSE)

# ---- Summaries by cluster ----
ihc_clusters <- ihc_pre %>% mutate(cluster = cluster_id)

# numeric summary (mean/SD)
num_vars <- X %>% dplyr::select(where(is.numeric)) %>% names()
summ_num <- ihc_clusters %>%
  group_by(cluster) %>%
  summarise(across(all_of(num_vars),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        sd   = ~ sd(.x,   na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# categorical summary (counts/%)
cat_vars <- names(X)[!vapply(X, is.numeric, logical(1))]
summ_cat <- ihc_clusters %>%
  mutate(across(all_of(cat_vars), as.character)) %>%        # unify type for pivot_longer
  dplyr::select(cluster, all_of(cat_vars)) %>%
  pivot_longer(-cluster, names_to = "marker", values_to = "level") %>%
  group_by(cluster, marker, level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster, marker) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  arrange(marker, cluster, desc(pct))

summ_num
summ_cat

# ---- Heatmap (convert to numeric for plotting only) ----
# Note: This is a visualization step; clustering already used mixed-types Gower
X_num <- ihc_pre %>%
  transmute(
    GATA6 = as.numeric(GATA6),
    CK17  = as.numeric(as.integer(CK17) - 1),  # neg=0, pos=1
    p63   = as.numeric(p63),                   # 0..3
    CK5_6 = as.numeric(CK5_6),
    CD44  = as.numeric(CD44),
    FOXI1 = as.numeric(FOXI1)
  ) %>% as.matrix()

# z-score continuous columns only
cont_cols <- c("GATA6","CK5_6","CD44","FOXI1")
X_scaled <- X_num
X_scaled[, cont_cols] <- scale(X_num[, cont_cols])

ord    <- order(cluster_id)
mat    <- t(X_scaled[ord, ])
cl_anno <- data.frame(Cluster = cluster_id[ord]); rownames(cl_anno) <- row_ids[ord]
col_fun <- colorRamp2(c(-2, 0, 2), c("#2c7bb6","#ffffbf","#d7191c"))

Heatmap(mat,
        name = "z/score", col = col_fun,
        column_title = "Cases", row_title = "IHC markers",
        top_annotation = HeatmapAnnotation(df = cl_anno),
        cluster_rows = TRUE, cluster_columns = FALSE, show_column_names = FALSE
)

# ---- Optional: hierarchical clustering on the numeric view (visual check) ----
d_euc <- dist(X_scaled, method = "euclidean")
hc    <- hclust(d_euc, method = "ward.D2")
plot(hc)
hc_k <- cutree(hc, k = best_k)
table(hc_k)

# ---- Optional: bootstrap stability (PAM on Gower) ----
B <- 100
assignments <- matrix(NA, nrow = nrow(X), ncol = B, dimnames = list(row_ids, NULL))
for (b in 1:B) {
  idx   <- sample(seq_len(nrow(X)), replace = TRUE)
  d_b   <- daisy(X[idx, , drop = FALSE], metric = "gower")
  fit_b <- pam(d_b, k = best_k, diss = TRUE)
  assignments[row_ids[idx], b] <- paste0("C", fit_b$clustering)
}

co <- matrix(0, nrow = nrow(X), ncol = nrow(X), dimnames = list(row_ids, row_ids))
for (b in 1:B) {
  lab   <- assignments[, b]
  inbag <- !is.na(lab)
  same  <- outer(lab[inbag], lab[inbag], `==`)
  co[inbag, inbag] <- co[inbag, inbag] + same
}
co <- co / B

Heatmap(co[ord, ord], name = "co-cluster",
        col = colorRamp2(c(0, 1), c("white", "black")),
        show_row_names = FALSE, show_column_names = FALSE)

# ---- Example association test ----
kruskal.test(GATA6 ~ cluster, data = ihc_clusters)
# chisq.test(table(ihc_clusters$cluster, ihc_clusters$grade))  # if you have a grade variable

# ---- End ----

# Example categorical dataset
set.seed(42)
n <- 150
ihc <- tibble(
  case_id    = paste0("C", seq_len(n)),
  GATA6_cat  = sample(c("low","medium","high"), n, TRUE, c(0.3,0.4,0.3)),
  CK17_cat   = sample(c("neg","pos"), n, TRUE, c(0.6,0.4)),
  p63_cat    = sample(c("0","1","2","3"), n, TRUE, c(0.4,0.3,0.2,0.1)),
  CK5_6_cat  = sample(c("neg","weak","strong"), n, TRUE, c(0.5,0.3,0.2)),
  CD44_cat   = sample(c("low","high"), n, TRUE, c(0.7,0.3)),
  FOXI1_cat  = sample(c("absent","focal","diffuse"), n, TRUE, c(0.4,0.4,0.2))
)

markers <- c("GATA6_cat","CK17_cat","p63_cat","CK5_6_cat","CD44_cat","FOXI1_cat")
X <- ihc %>% dplyr::select(all_of(markers)) %>% mutate(across(everything(), as.factor))

# PAM on Gower
d <- daisy(X, metric = "gower")
sil_df <- map_df(2:8, function(k) {
  pam_fit <- pam(d, k = k, diss = TRUE)
  sil     <- silhouette(pam_fit)
  tibble(k = k, avg_sil = mean(sil[, "sil_width"]))
})
best_k <- sil_df$k[which.max(sil_df$avg_sil)]
pam_fit <- pam(d, k = best_k, diss = TRUE)
clusters <- factor(pam_fit$clustering, labels = paste0("C", seq_len(best_k)))

# k-modes


library(klaR)
library(purrr)
library(dplyr)

# 1) Build a clean, categorical-only data.frame for k-modes
X_km <- ihc %>%
  dplyr::select(all_of(markers)) %>%
  mutate(across(everything(), as.factor)) %>%
  droplevels() %>%
  as.data.frame(stringsAsFactors = FALSE, check.names = TRUE)

# 2) Drop constant columns (k-modes can choke if a variable has only 1 level)
keep <- vapply(X_km, function(x) dplyr::n_distinct(x, na.rm = TRUE) >= 2, logical(1))
if (!all(keep)) {
  message("Dropping constant variables for k-modes: ",
          paste(names(X_km)[!keep], collapse = ", "))
}
X_km <- X_km[, keep, drop = FALSE]

# 3) Coerce factors to character (safest for klaR::kmodes)
X_km2 <- data.frame(lapply(X_km, as.character),
                    stringsAsFactors = FALSE, check.names = TRUE)

# 4) Choose k by minimizing within-cluster mismatch cost
grid <- purrr::map_df(2:8, function(k) {
  fit <- kmodes(X_km2, modes = k, iter.max = 50, weighted = FALSE)
  tibble(k = k, cost = fit$withindiff)
})

grid %>% arrange(cost)
best_k <- grid$k[which.min(grid$cost)]

# 5) Final k-modes fit
fit <- kmodes(X_km2, modes = best_k, iter.max = 100, weighted = FALSE)
clusters_km <- factor(fit$cluster, labels = paste0("C", seq_len(best_k)))

# (Optional) Inspect mode profiles per cluster
fit$modes
table(clusters_km)




# MCA + k-means
library(FactoMineR)
mca <- MCA(X, graph = FALSE)
eig <- mca$eig
cumpct <- cumsum(eig[, "cumulative percentage of variance"])
nc <- which(cumpct >= 75)[1]              # <-- FIXED (was which[...] earlier)
scores <- mca$ind$coord[, 1:nc, drop = FALSE]

sil_df_mca <- map_df(2:8, function(k) {
  cl  <- kmeans(scale(scores), centers = k, nstart = 50)
  sil <- cluster::silhouette(cl$cluster, dist(scale(scores)))
  tibble(k = k, avg_sil = mean(sil[, "sil_width"]))
})
best_k_mca <- sil_df_mca$k[which.max(sil_df_mca$avg_sil)]
km <- kmeans(scale(scores), centers = best_k_mca, nstart = 100)
clusters_mca <- factor(km$cluster, labels = paste0("C", seq_len(best_k_mca)))

# Distributions by cluster for PAM result
summary_tbl <- ihc %>%
  mutate(cluster = clusters) %>%
  pivot_longer(all_of(markers), names_to = "marker", values_to = "level") %>%
  group_by(cluster, marker, level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster, marker) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  arrange(marker, cluster, desc(pct))
summary_tbl

source("code/functions/experiment.R")

# Experiment 1: points (= multivariate Gaussians), varying distance and variance

dists <- seq(from = 2, to = 8, by = 0.125)
sds <- seq(from = 0.5, to = 2, by = 0.05)
n_exp <- length(dists)*length(sds)

set.seed(12345)

params_df_E1 <- data.frame(
  ID = paste0("E1_", 1:n_exp),
  seed_data = sample(1:10000, size = n_exp),
  n1 = 500,
  n2 = 500,
  manif = "point",
  dist = NA, 
  cov_1 = NA, 
  cov_2 = NA, 
  dim_noise = 2,
  n_irrev_features = 0,
  modif_data = FALSE,
  dbscan_rawData = TRUE,
  eps_min = 0.01, 
  eps_max = 10,
  eps_by = 0.01,
  minPts = 5,
  comp_umap = TRUE,
  seed_umap = sample(1:10000, size = n_exp), 
  n_neighbors_umap = 15, 
  n_components_umap = 2, 
  stringsAsFactors = FALSE
)
params_df_E1$dist <- expand.grid(dists, sds)$Var1
params_df_E1$cov_1 <- params_df_E1$cov_2 <- expand.grid(dists, sds)$Var2^2
saveRDS(params_df_E1, "results/experiments/params_E1.rds")

params_list_E1 <- split(params_df_E1, seq(nrow(params_df_E1)))


cores_to_use <- 20
parallel::mclapply(params_list_E1, 
                                experiment,
                                mc.cores = cores_to_use
)



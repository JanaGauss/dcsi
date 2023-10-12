source("code/functions/experiment.R")

# Experiment 2: points (= multivariate Gaussians), different standard deviation in the components

dists <- seq(from = 2, to = 5, by = 0.125)
sds <- seq(from = 0.5, to = 3.5, by = 0.05)
n_exp <- length(dists)*length(sds)

set.seed(123)

params_df_E2 <- data.frame(
  ID = paste0("E2_", 1:n_exp),
  seed_data = sample(1:10000, size = n_exp),
  n1 = 500,
  n2 = 500,
  manif = "point",
  dist = NA, 
  cov_1 = 0.5^2, 
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
params_df_E2$dist <- expand.grid(dists, sds)$Var1
params_df_E2$cov_2 <- expand.grid(dists, sds)$Var2^2
saveRDS(params_df_E2, "results/experiments/params_E2.rds")

params_list_E2 <- split(params_df_E2, seq(nrow(params_df_E2)))


cores_to_use <- 20
parallel::mclapply(params_list_E2, 
                                 experiment,
                                 mc.cores = cores_to_use
)

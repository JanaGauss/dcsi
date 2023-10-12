source("code/functions/experiment.R")

# Experiment 5: points (= multivariate Gaussians), increasing number of dimensions

dists <- c(seq(from = 1.5, to = 5, by = 0.25), 10, 20, 50)
dim <- c(seq(from = 2, to = 10, by = 1), 15, 20, 50, 100, 500, 1000, 2000)
n_exp <- length(dists)*length(dim)

set.seed(678)

params_df_E5 <- data.frame(
  ID = paste0("E5_", 1:n_exp),
  seed_data = sample(1:10000, size = n_exp),
  n1 = 500,
  n2 = 500,
  manif = "point",
  dist = NA, #
  cov_1 = 0.5^2, 
  cov_2 = 0.5^2, 
  dim_noise = NA,
  n_irrev_features = 0, 
  modif_data = FALSE,
  dbscan_rawData = TRUE,
  eps_min = 0.01, 
  eps_max = 50, 
  eps_by = 0.01,
  minPts = 5,
  comp_umap = TRUE,
  seed_umap = sample(1:10000, size = n_exp), 
  n_neighbors_umap = 15, 
  n_components_umap = 2, 
  stringsAsFactors = FALSE
)
params_df_E5$dist <- expand.grid(dists, dim)$Var1
params_df_E5$dim_noise <- expand.grid(dists, dim)$Var2
saveRDS(params_df_E5, "results/experiments/params_E5.rds")

params_list_E5 <- split(params_df_E5, seq(nrow(params_df_E5)))


cores_to_use <- 20
parallel::mclapply(params_list_E5, 
                                 experiment,
                                 mc.cores = cores_to_use
                   )


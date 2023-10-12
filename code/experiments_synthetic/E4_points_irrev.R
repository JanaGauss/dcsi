source("code/functions/experiment.R")

# Experiment 4: points (= multivariate Gaussians), increasing number of irrelevant features

dists <- c(seq(from = 1.5, to = 5, by = 0.25), 10, 20, 50)
n_irrev <- c(seq(from = 0, to = 10, by = 1), 15, 20, 50, 100, 500, 1000, 2000)
n_exp <- length(dists)*length(n_irrev)

set.seed(456)

params_df_E4 <- data.frame(
  ID = paste0("E4_", 1:n_exp),
  seed_data = sample(1:10000, size = n_exp),
  n1 = 500,
  n2 = 500,
  manif = "point",
  dist = NA, #
  cov_1 = 0.5^2, 
  cov_2 = 0.5^2, 
  dim_noise = 2,
  n_irrev_features = NA, 
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
params_df_E4$dist <- expand.grid(dists, n_irrev)$Var1
params_df_E4$n_irrev_features <- expand.grid(dists, n_irrev)$Var2
saveRDS(params_df_E4, "results/experiments/params_E4.rds")

params_list_E4 <- split(params_df_E4, seq(nrow(params_df_E4)))


cores_to_use <- 20
parallel::mclapply(params_list_E4, 
                                 experiment,
                                 mc.cores = cores_to_use
                   )

source("code/functions/experiment.R")

# Experiment 7: circles, varying distance (radius) and variance

r_2 <- seq(from = 5, to = 10, by = 0.125)
sds <-  seq(from = 0, to = 1, by = 0.05)
n_exp <- length(r_2)*length(sds)

set.seed(45678)

params_df_E7 <- data.frame(
  ID = paste0("E7_", 1:n_exp),
  seed_data = sample(1:10000, size = n_exp),
  n1 = 500,
  n2 = 500,
  manif = "circle",
  r_2 = NA,
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
params_df_E7$r_2 <- expand.grid(r_2, sds)$Var1
params_df_E7$cov_1 <- params_df_E7$cov_2 <- expand.grid(r_2, sds)$Var2^2
saveRDS(params_df_E7, "results/experiments/params_E7.rds")

params_list_E7 <- split(params_df_E7, seq(nrow(params_df_E7)))

cores_to_use <- 20
parallel::mclapply(params_list_E7, 
                                 experiment,
                                 mc.cores = cores_to_use
                   )


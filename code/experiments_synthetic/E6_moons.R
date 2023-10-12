source("code/functions/experiment.R")

# Experiment 6: moons, varying distance and variance

moon_shift <- seq(from = 0, to = 0.95, by = 0.05)
sds <-  seq(from = 0, to = 2, by = 0.05)
n_exp <- length(moon_shift)*length(sds)

set.seed(4567)

params_df_E6 <- data.frame(
  ID = paste0("E6_", 1:n_exp),
  seed_data = sample(1:10000, size = n_exp),
  n1 = 500,
  n2 = 500,
  manif = "moon",
  r_moon = 6,
  moon_shift = NA,
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
params_df_E6$moon_shift <- expand.grid(moon_shift, sds)$Var1
params_df_E6$cov_1 <- params_df_E6$cov_2 <- expand.grid(moon_shift, sds)$Var2^2
saveRDS(params_df_E6, "results/experiments/params_E6.rds")

params_list_E6 <- split(params_df_E6, seq(nrow(params_df_E6)))

cores_to_use <- 20
parallel::mclapply(params_list_E6, 
                                 experiment,
                                 mc.cores = cores_to_use
                   )

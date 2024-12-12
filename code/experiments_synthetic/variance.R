source("code/functions/experiment.R")

# Monte Carlo simulations: selected settings from Experiment 1
# distance of means: 4.5
# standard deviation: 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2

# ToDo: add dbscan clustering

dist_means <- 4.5
n_MCsamples <- 200
sds <- seq(from = 0.5, to = 2, by = 0.25)
n_exp <- n_MCsamples*length(sds)

set.seed(48629)

params_df_Var <- data.frame(
  ID = paste0("Var_", 1:n_exp),
  seed_data = sample(1:10000, size = n_exp),
  n1 = 500,
  n2 = 500,
  manif = "point",
  dist = dist_means, 
  cov_1 = NA, 
  cov_2 = NA, 
  dim_noise = 2,
  n_irrev_features = 0,
  modif_data = FALSE,
  dbscan_rawData = TRUE,
  eps_min = 0.01, 
  eps_max = 1,
  eps_by = 0.01,
  minPts = 5,
  comp_umap = FALSE,
  seed_umap = sample(1:10000, size = n_exp), 
  n_neighbors_umap = 15, 
  n_components_umap = 2, 
  stringsAsFactors = FALSE
)
params_df_Var$cov_1 <- params_df_Var$cov_2 <- rep(sds^2, n_MCsamples)
saveRDS(params_df_Var, "results/experiments/params_Var.rds")

params_list_Var <- split(params_df_Var, seq(nrow(params_df_Var)))


cores_to_use <- 8
parallel::mclapply(params_list_Var, 
                   experiment_Var,
                   mc.cores = cores_to_use
)

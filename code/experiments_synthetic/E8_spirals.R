source("code/functions/experiment.R")

# Experiment 8: spirals, varying variance

sds <-  seq(from = 0, to = 2.5, by = 0.05)
n_exp <- length(sds)

set.seed(876)

params_df_E8 <- data.frame(
  ID = paste0("E8_", 1:n_exp),
  seed_data = sample(1:10000, size = n_exp),
  n1 = 500,
  n2 = 500,
  manif = "spiral",
  cov_1 = NA, 
  cov_2 = NA, 
  dim_noise = 2,
  n_irrev_features = 0,
  modif_data = FALSE,
  dbscan_rawData = TRUE,
  eps_min = 0.01, 
  eps_max = 20,
  eps_by = 0.01,
  minPts = 5,
  comp_umap = TRUE,
  seed_umap = sample(1:10000, size = n_exp), 
  n_neighbors_umap = 15, 
  n_components_umap = 2, 
  perpl_tsne = 30, 
  stringsAsFactors = FALSE
)
params_df_E8$cov_1 <- params_df_E8$cov_2 <- sds^2
saveRDS(params_df_E8, "results/experiments/params_E8.rds")

params_list_E8 <- split(params_df_E8, seq(nrow(params_df_E8)))

cores_to_use <- 20
parallel::mclapply(params_list_E8, 
                                 experiment,
                                 mc.cores = cores_to_use
                   )


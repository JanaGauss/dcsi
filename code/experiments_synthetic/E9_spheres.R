source("code/functions/experiment.R")

# Experiment 9: spheres, varying dimension, variance, radius. Embeddings also in 3D

r_2 <- c(10, 20, 50)
sds <-  c(0, 0.25, 0.5)
dim <- c(seq(from = 2, to = 10, by = 1), 15, 20, 50, 100, 500, 1000) # dim_sphere = 2 = 3D data
emb_dim <- c(2, 3)
n_exp <- length(r_2)*length(sds)*length(dim)*length(emb_dim)

set.seed(3456)

params_df_E9 <- data.frame(
  ID = paste0("E9_", 1:n_exp),
  seed_data = sample(1:10000, size = n_exp),
  n1 = 500,
  n2 = 500,
  manif = "n_sphere",
  r_2 = NA,
  cov_1 = NA, 
  cov_2 = NA, 
  dim_sphere = NA,
  dim_noise = 2,
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
  n_components_umap = NA, 
  stringsAsFactors = FALSE
)

params_grid <- expand.grid(r_2, sds, dim, emb_dim)
params_df_E9$r_2 <- params_grid$Var1
params_df_E9$cov_1 <- params_df_E9$cov_2 <- params_grid$Var2^2
params_df_E9$dim_sphere <- params_grid$Var3
params_df_E9$n_components_umap <- params_grid$Var4
saveRDS(params_df_E9, "results/experiments/params_E9.rds")

params_list_E9 <- split(params_df_E9, seq(nrow(params_df_E9)))

cores_to_use <- 20
parallel::mclapply(params_list_E9, 
                                 experiment,
                                 mc.cores = cores_to_use
                   )


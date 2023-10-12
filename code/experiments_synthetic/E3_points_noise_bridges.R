source("code/functions/experiment.R")

# Experiment 3: points (= multivariate Gaussians), uniform noise or bridges

#### noise - not used in the paper ####
# these data sets were not used in the paper.
# this section isn't removed as this would change the seed values for the data sets with bridges
dists <- seq(from = 2, to = 8, by = 0.25)
noise <- seq(from = 0, to = 3, by = 0.1)
n_exp <- length(dists)*length(noise)

set.seed(321)

params_df_E3_noise <- data.frame(
  ID = NA,
  seed_data = sample(1:10000, size = n_exp),
  n1 = 500,
  n2 = 500,
  manif = "point",
  dist = NA, #
  cov_1 = 0.5^2, 
  cov_2 = 0.5^2, 
  dim_noise = 2,
  n_irrev_features = 0,
  modif_data = TRUE,
  seed_mod = sample(1:10000, size = n_exp),
  mod_choice = "noise", 
  perc = NA, 
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
params_df_E3_noise$dist <- expand.grid(dists, noise)$Var1
params_df_E3_noise$perc <- expand.grid(dists, noise)$Var2

#### bridges #### 
dists <- seq(from = 4, to = 10, by = 0.25)
bridge <- seq(from = 0, to = 1.5, by = 0.05)
n_exp <- length(dists)*length(bridge)

params_df_E3_bridge <- data.frame(
  ID = paste0("E3_", 1:n_exp),
  seed_data = sample(1:10000, size = n_exp),
  n1 = 500,
  n2 = 500,
  manif = "point",
  dist = NA, #
  cov_1 = 0.5^2, 
  cov_2 = 0.5^2, 
  dim_noise = 2,
  n_irrev_features = 0,
  modif_data = TRUE,
  seed_mod = sample(1:10000, size = n_exp),
  mod_choice = "bridge", 
  perc = NA, 
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
params_df_E3_bridge$dist <- expand.grid(dists, bridge)$Var1
params_df_E3_bridge$perc <- expand.grid(dists, bridge)$Var2


params_df_E3 <- rbind(params_df_E3_noise, params_df_E3_bridge)
params_df_E3$ID <-paste0("E3_", 1:nrow(params_df_E3))
saveRDS(params_df_E3, "results/experiments/params_E3.rds")


params_list_E3 <- split(params_df_E3, seq(nrow(params_df_E3)))


cores_to_use <- 20
parallel::mclapply(params_list_E3, 
                                 experiment,
                                 mc.cores = cores_to_use
                   )

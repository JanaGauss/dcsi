# Example to rerun the experiment on one synthetic data set

source("code/functions/experiment.R")
source("code/functions/plot_functions.R")

params_list <- list(
  ID = "E1_example",
  seed_data = 123,
  n1 = 500,
  n2 = 500,
  manif = "point",
  dist = 4.5,
  cov_1 = 1,
  cov_2 = 1,
  dim_noise = 2,
  n_irrev_features = 0,
  modif_data = FALSE,
  dbscan_rawData = TRUE,
  eps_min = 0.05, 
  eps_max = 2,
  eps_by = 0.05,
  minPts = 5,
  comp_umap = TRUE,
  seed_umap = 123, 
  n_neighbors_umap = 15, 
  n_components_umap = 2
)
# Parameters for experiment function:
# The first parameters (seed_data - modif_data) are used to create a synthetic data set
# eps_min/max/by: epsilon parameter for DBSCAN:
# For each epsilon in seq(from = eps_min, to = eps_max, by = eps_by), a DBSCAN clustering is computed
# ARI is used to measure the quality of the clustering
# In order to evaluate the separability measures, they are compared to maximum ARI, 
# as the separability measures aim to quantify to difficulty of the clustering problem.
# An UMAP embedding is computed and the separability measures and DBSCAN are again evaluated in order to measure
# if there is a change in separability.

experiment(params_list)

example <- readRDS("results/experiments_rawData/E1_example.rds")

# plot original data
plot_2_3d_data(example$data)

# plot UMAP embedding
plot_2_3d_data(example$dat_umap)

# separability measures on raw data
example$sep_raw

# separability measures on UMAP embedding
example$sep_umap
# most measures indicate an increase in separability

# ARI of DBSCAN clustering depending on eps parameter, raw data
ggplot(data = filter(example$res_dbscan_raw_long, measure == "ARI"),
       aes(x = eps, y = performance)) + geom_line() +
  labs(x = "epsilon", y = "ARI") +
  ylim(c(-0.05, 1))

# ARI of DBSCAN clustering depending on eps parameter, UMAP embedding
ggplot(data = filter(example$res_dbscan_umap_long, measure == "ARI"),
       aes(x = eps, y = performance)) + geom_line() +
  labs(x = "epsilon", y = "ARI") +
  ylim(c(-0.05, 1))



library(tidyverse)
library(tdaunif)
library(ggplot2)
library(cowplot)

# generate data from a disk and add data from two normal distributions
gen_data <- function(n_disk, n_gauss, sd, seed){
  set.seed(seed)
  data <- data.frame(sample_disk(n_disk, sd = 0, bins = 1L))
  data <- rbind(rbind(data, data.frame(x = rnorm(n_gauss, mean = -0.4, sd = sd), y = rnorm(n_gauss, mean = 0, sd = sd))),
                data.frame(x = rnorm(n_gauss, mean = 0.4, sd = sd), y = rnorm(n_gauss, mean = 0, sd = sd)))
  data
}

# calculate vector of distances to the minPts nearest neighbor
get_dist_neighb <- function(dist_mat, minPts){
  knn_graph <- cccd::nng(dx = dist_mat, k = minPts)
  knn_matrix <- as.matrix(igraph::as_adjacency_matrix(knn_graph))
  knn_weights <- matrixcalc::hadamard.prod(knn_matrix, dist_mat) # add distances to knn-graph
  
  dist_kth_neighbor <- apply(knn_weights, 1, max)
  return(dist_kth_neighbor)
}

# compute connectedness and associated core points
get_conn <- function(dist_mat, dist_vec, eps){
  ind <- which(dist_vec < eps)
  dist_ind <- dist_mat[ind, ind]
  mst <- pegas::mst(dist_ind)
  ind_mst <- which.max(mst[, 3])
  list(conn = mst[ind_mst, 3],
       x1 = as.integer(row.names(dist_ind)[mst[ind_mst, 1]]),
       x2 = as.integer(row.names(dist_ind)[mst[ind_mst, 2]]))
}

# compute separation
get_sep <- function(dist_mat, dist_vec, eps){
  ind <- which(dist_vec < eps)
  dist_ind <- dist_mat[ind, ind]
  mst <- pegas::mst(dist_ind)
  ind_mst <- which.max(mst[, 3])
  list(conn = mst[ind_mst, 3],
       x1 = as.integer(row.names(dist_ind)[mst[ind_mst, 1]]),
       x2 = as.integer(row.names(dist_ind)[mst[ind_mst, 2]]))
}
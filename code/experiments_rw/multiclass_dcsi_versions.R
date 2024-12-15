source("code/functions/experiment_realWorld.R")
source("code/functions/separability_functions.R")
library(tidyverse)

##### prepare data #####
fmnist_umap <- readRDS("results/experiments_rw/fmnist_umap.rds")
fmnist_dat_umap <- fmnist_umap$dat # 3D
fmnist_dist_umap <- fmnist_umap$dist

mnist_umap <- readRDS("results/experiments_rw/mnist_umap.rds")
mnist_dat_umap <- mnist_umap$dat # 3D
mnist_dist_umap <- mnist_umap$dist

fmnist_labels_5 <- fmnist_umap$labels_5
fmnist_labels_10 <- fmnist_umap$labels_10
mnist_labels <- mnist_umap$labels

#### calculate different multiclass versions of DCSI ####
# for fmnist-5 and -10 and mnist
# minPts = 50

fmnist_5 <- calc_DCSI_RW(fmnist_dist_umap, fmnist_labels_5, 50, returnSepConn = TRUE)
fmnist_10 <- calc_DCSI_RW(fmnist_dist_umap, fmnist_labels_10, 50, returnSepConn = TRUE)
mnist <- calc_DCSI_RW(mnist_dist_umap, mnist_labels, 50, returnSepConn = TRUE)

res_multiclass <- data.frame(fmnist5 = calc_DCSI_multiclass(fmnist_5$Sep, 
                                                           fmnist_5$Conn, fmnist_5$pair_matrix), 
                             fmnist10 = calc_DCSI_multiclass(fmnist_10$Sep, 
                                                             fmnist_10$Conn, fmnist_10$pair_matrix), 
                             mnist = calc_DCSI_multiclass(mnist$Sep, 
                                                          mnist$Conn, mnist$pair_matrix))
res_multiclass %>% round(2)
library(xtable)
xtable(res_multiclass %>% round(2), type = "latex")

# Group 1:
# mean of pairwise DCSI

# min of pairwise DCSI, = q/(1+q), where q = min(Sep/Conn)

# median pairwise DCSI, = q/(1+q), where q = median(Sep/Conn)

# q/(1+q), where q = mean(Sep/Conn) (theoretical weakness: != 1/(1+q') with q' = mean(Conn/Sep))
# -> not computed

# Group 2:
# Sep = min Sep, Conn = max Conn

# Sep = mean Sep, Conn = mean Conn

# Sep = median Sep, Conn = median Conn




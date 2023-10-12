source("code/functions/experiment_realWorld.R")
source("code/functions/separability_functions.R")
library(tidyverse)

##### prepare data #####
load_mnist() 
fmnist <- rbind(train$x, test$x)
# draw subsample of 10 000 for computational reasons
set.seed(21022)
ind_fmnist <- sample(1:nrow(fmnist), 10000, replace = FALSE)
fmnist <- fmnist[ind_fmnist,]

fmnist <- matrix(c(scale(c(fmnist))), nrow = length(ind_fmnist)) # scale whole dat set (not column-wise!)

fmnist_lbls_10 <- as.numeric(c(train$y, test$y)) + 1
# caution! these labels are the original labels + 1 because labels from 1 to ... are necessary for separability calculation
fmnist_lbls_10 <- fmnist_lbls_10[ind_fmnist]

fmnist_lbls_5 <- fmnist_lbls <- c(train$y, test$y)
fmnist_lbls_5[fmnist_lbls %in% c(0, 3)] <- 1
fmnist_lbls_5[fmnist_lbls == 1] <- 2
fmnist_lbls_5[fmnist_lbls %in% c(2, 4, 6)] <- 3
fmnist_lbls_5[fmnist_lbls == 8] <- 4
fmnist_lbls_5[fmnist_lbls %in% c(5, 7, 9)] <- 5
fmnist_lbls_5 <- fmnist_lbls_5[ind_fmnist]

dat_raw <- as.data.frame(fmnist)
dist_raw <- proxy::dist(dat_raw)

umap <- readRDS("results/experiments_rw/fmnist_umap.rds")
dat_umap <- umap$dat # 3D
dist_umap <- umap$dist

#### calculate DCSI #####
results_raw_5 <- calc_sep_RW_robust(dat_raw, dist_raw, fmnist_lbls_5, c(5, 10, 15, 20, 50))

results_umap_5 <- calc_sep_RW_robust(dat_umap, dist_umap, fmnist_lbls_5, c(5, 10, 15, 20, 50))

results_raw_10 <- calc_sep_RW_robust(dat_raw, dist_raw, fmnist_lbls_10, c(5, 10, 15, 20, 50))

results_umap_10 <- calc_sep_RW_robust(dat_umap, dist_umap, fmnist_lbls_10, c(5, 10, 15, 20, 50))

save.image(file="results/experiments_rw/robust_fmnist.RData")

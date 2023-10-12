source("code/functions/experiment_realWorld.R")
library(tidyverse)

# !!! In order to run these experiments, the MNIST training data need to be downloaded from lrz sync and share 
# (see ReadMe of https://github.com/HerrMo/topoclust)


#### prepare data sets ####
mnist_te <- read.csv("data/data_raw/mnist_test.csv")
mnist_tr <- read.csv("data/data_raw/mnist_train.csv")
mnist <- rbind(mnist_tr, mnist_te)
# draw subsample of 10 000 for computational reasons
set.seed(41022)
ind_mnist <- sample(1:nrow(mnist), 10000, replace = FALSE)
mnist <- mnist[ind_mnist,]

mnist_lbls <- mnist$label
mnist <- matrix(c(scale(c(as.matrix(mnist[, -1])))), nrow = length(mnist_lbls)) # scale whole dat set (not column-wise!)


load_mnist() # Fashion mnist
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


#### list of parameters and data ####
params_list <- list(
  mnist_raw = list(
    dat_X = as.data.frame(mnist),
    dist = NULL, labels = as.numeric(mnist_lbls) + 1,
    # caution! Label in data doesn't correspond to digit, but labels from 1 to ... are necessary for separability calculation
    choice = "raw",
    eps_range = seq(10, 40, by = 0.01),
    seed = NULL, k_umap = NULL, perpl_tsne = NULL,
    fmnist = FALSE, labels_10 = NULL, labels_5 = NULL,
    name = "mnist_raw"
  ),
  mnist_umap = list(
    dat_X = as.data.frame(mnist),
    dist = NULL, labels = as.numeric(mnist_lbls) + 1,
    # caution! Label in data doesn't correspond to digit, but labels from 1 to ... are necessary for separability calculation
    choice = "umap",
    eps_range = seq(0.01, 10, by = 0.01),
    seed = 12345, k_umap = 10, perpl_tsne = NULL,
    fmnist = FALSE, labels_10 = NULL, labels_5 = NULL,
    name = "mnist_umap"
  ),
  fmnist_raw = list(
    dat_X = as.data.frame(fmnist),
    dist = NULL, labels = NULL,
    choice = "raw",
    eps_range = seq(10, 40, by = 0.01),
    seed = NULL, k_umap = NULL, perpl_tsne = NULL,
    fmnist = TRUE, labels_10 = fmnist_lbls_10, labels_5 = fmnist_lbls_5,
    name = "fmnist_raw"
  ),
  fmnist_umap = list(
    dat_X = as.data.frame(fmnist),
    dist = NULL, labels = NULL,
    choice = "umap",
    eps_range = seq(0.01, 15, by = 0.01),
    seed = 12345, k_umap = 10, perpl_tsne = NULL,
    fmnist = TRUE, labels_10 = fmnist_lbls_10, labels_5 = fmnist_lbls_5,
    name = "fmnist_umap"
  )
)


#### calculate distance matrices ####
params_list <- lapply(params_list, calc_dist)

params_list$mnist_umap$dist <- params_list$mnist_raw$dist
params_list$fmnist_umap$dist <- params_list$fmnist_raw$dist

#### apply experiment function ####
cores_to_use <- 20
parallel::mclapply(params_list,
                   wrapper_exp_RW,
                   mc.cores = cores_to_use
)




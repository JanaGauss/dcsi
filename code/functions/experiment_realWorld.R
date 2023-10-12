# main function for experiments on real world data
library(tidyverse)
source("code/functions/separability_functions.R")

# Function to run experiments on real world data
experiment_RW <- function(dat_X, dist = NULL, labels = NULL, 
                          choice = "raw",
                          eps_range, 
                          k_umap = 10,
                          perpl_tsne = 30,
                          seed = 1234, 
                          fmnist = FALSE,
                          labels_10 = NULL,
                          labels_5 = NULL,
                          name = "mnist_raw"){
  

  checkmate::assert_choice(choice, c("raw", "umap", "tsne"))
  
  
  if(!fmnist){
    results <- list(labels = labels)
  } else{
    results <- list(labels_5 = labels_5,
                    labels_10 = labels_10)
  }
  
  if(is.null(dist)){
    dist <- proxy::dist(dat_X)
  }
  
  #### calculate embedding ####
  if(choice == "umap"){
    print(paste(Sys.time(), name, " umap-emb"))
    
    # compute 2- and 3-D embedding (2-D only for visualization)
    umap_emb_vis <- umap::umap(as.matrix(dist), random_state = seed, 
                           n_neighbors = k_umap, n_components = 2, input = "dist")
    dat_umap_vis <- as.data.frame(umap_emb_vis$layout)
    colnames(dat_umap_vis) <- paste0("X", 1:ncol(dat_umap_vis))
    
    
    umap_emb <- umap::umap(as.matrix(dist), random_state = seed, 
                           n_neighbors = k_umap, n_components = 3, input = "dist")
    dat_X <- as.data.frame(umap_emb$layout)
    colnames(dat_X) <- paste0("X", 1:ncol(dat_X))
    
    dist <- proxy::dist(dat_X)
    
    
    results <- append(results,
                      list(dat_vis = dat_umap_vis,
                           dat = dat_X,
                           dist = dist))
    
  }
  if(choice == "tsne"){
    print(paste(Sys.time(), name, " tsne-emb"))
    
    # compute 2- and 3-D embedding (2-D only for visualisation)
    set.seed(seed)
    tsne_emb_vis <- Rtsne::Rtsne(dist, dims = 2, perplexity = perpl_tsne)
    
    dat_tsne_vis <- as.data.frame(tsne_emb_vis$Y)
    colnames(dat_tsne_vis) <- paste0("X", 1:ncol(dat_tsne_vis))
    
    
    tsne_emb <- Rtsne::Rtsne(dist, dims = 3, perplexity = perpl_tsne)
    dat_X <- as.data.frame(tsne_emb$Y)
    colnames(dat_X) <- paste0("X", 1:ncol(dat_X))
    
    dist <- proxy::dist(dat_X)
    
    
    results <- append(results,
                      list(dat_vis = dat_tsne_vis,
                           dat = dat_X,
                           dist = dist))
    
  }
    
  #### calculate separability measures ####
  # for the whole data set and for each pair of two groups
  print(paste(Sys.time(), name, " sep measures"))
  
  
  if(fmnist){
    sep_10 <- calc_sep_RW(data = dat_X,
                          dist = dist,
                          labels = labels_10,
                          name_value = name)
    
    sep_5 <- calc_sep_RW(data = dat_X,
                          dist = dist,
                          labels = labels_5,
                          name_value = name)
    
    
    results <- append(results,
                      list(sep_10 = sep_10,
                           sep_5 = sep_5))
  } else{
    sep <- calc_sep_RW(data = dat_X,
                       dist = dist,
                       labels = labels,
                       name_value = name)
    
    results <- append(results,
                      list(sep = sep))
  }
  
  #### calculate dbscan ####
  print(paste(Sys.time(), name, " dbscan"))
  
  if(!fmnist){
    
    res_dbscan <- as.data.frame(cluster_res(dat = dist, eps_range = eps_range, minPts = 5, 
                                            labels = labels))
    
    res_dbscan$eps <- eps_range
    res_dbscan_long <- res_dbscan %>%
      gather("measure", "performance", c(1,2))
    
    eps_dbscan <- res_dbscan_long$eps[which.max(res_dbscan_long$performance)]
    dbscan <- dbscan::dbscan(x = dist, eps = eps_dbscan, minPts = 5)
    max_ARI <- max(res_dbscan$ARI)
    max_NMI <- max(res_dbscan$NMI)
    
    results <- append(results,
                      list(res_dbscan_long = res_dbscan_long,
                           eps_dbscan = eps_dbscan,
                           dbscan = dbscan,
                           max_ARI = max_ARI,
                           max_NMI = max_NMI))
    
  } else{
    # only compute clustering once and evaluate for both label sets
    clustering <- lapply(
      eps_range,
      function(eps) dbscan::dbscan(dist, eps = eps, minPts = 5)$cluster
    )
    
    # FMNIST-5
    res_dbscan_5_raw <- lapply(
      clustering, 
      function(clust) t(performance(clust, labels_5))
    )
    
    res_dbscan_5 <- do.call(rbind.data.frame, res_dbscan_5_raw)
    res_dbscan_5$eps <- eps_range
    
    res_dbscan_long_5 <- res_dbscan_5 %>%
      gather("measure", "performance", c(1,2))
    
    eps_dbscan_5 <- res_dbscan_long_5$eps[which.max(res_dbscan_long_5$performance)]
    dbscan_5 <- dbscan::dbscan(x = dist, eps = eps_dbscan_5, minPts = 5)
    max_ARI_5 <- max(res_dbscan_5$ARI)
    max_NMI_5 <- max(res_dbscan_5$NMI)
    
    # FMNIST-10
    res_dbscan_10_raw <- lapply(
      clustering, 
      function(clust) t(performance(clust, labels_10))
    )
    
    res_dbscan_10 <- do.call(rbind.data.frame, res_dbscan_10_raw)
    res_dbscan_10$eps <- eps_range
    
    res_dbscan_long_10 <- res_dbscan_10 %>%
      gather("measure", "performance", c(1,2))
    
    eps_dbscan_10 <- res_dbscan_long_10$eps[which.max(res_dbscan_long_10$performance)]
    dbscan_10 <- dbscan::dbscan(x = dist, eps = eps_dbscan_10, minPts = 5)
    max_ARI_10 <- max(res_dbscan_10$ARI)
    max_NMI_10 <- max(res_dbscan_10$NMI)
    
    
    results <- append(results,
                      list(res_dbscan_long_5 = res_dbscan_long_5,
                           eps_dbscan_5 = eps_dbscan_5,
                           dbscan_5 = dbscan_5,
                           max_ARI_5 = max_ARI_5,
                           max_NMI_5 = max_NMI_5,
                           res_dbscan_long_10 = res_dbscan_long_10,
                           eps_dbscan_10 = eps_dbscan_10,
                           dbscan_10 = dbscan_10,
                           max_ARI_10 = max_ARI_10,
                           max_NMI_10 = max_NMI_10))
  }
  
  
  print(paste(Sys.time(), name, " done"))
  
  saveRDS(results, file = paste0("results/experiments_rw/", name, ".rds"))
  
}

# Wrapper for above function
wrapper_exp_RW <- function(list){
  experiment_RW(dat_X = list$dat_X,
                dist = list$dist,
                labels = list$labels,
                choice = list$choice,
                eps_range = list$eps_range,
                seed = list$seed,
                k_umap = list$k_umap,
                perpl_tsne = list$perpl_tsne,
                fmnist = list$fmnist,
                labels_10 = list$labels_10,
                labels_5 = list$labels_5,
                name = list$name)
}


cluster_res <- function(dat, eps_range, minPts, labels, two_versions = FALSE) { 
  
  if(!two_versions){
    res <- vapply(eps_range,
                  function(eps) performance(dbscan::dbscan(dat, eps = eps, minPts = minPts)$cluster,
                                            labels),
                  numeric(2))
    t(res)
  } else{
    res <- vapply(eps_range,
                  function(eps) performance(dbscan::dbscan(dat, eps = eps, minPts = minPts)$cluster,
                                            labels, two_versions = TRUE),
                  numeric(4))
    t(res)
  }
  
}

performance <- function(x, y, two_versions = FALSE) {
  # modification: for data with uniform noise, only the true data is evaluated -> labels (y) contains only true data
  x <- x[1:length(y)] # filter x also for true data
  
  if(!two_versions){
    c(ARI = mclust::adjustedRandIndex(x, y),
      NMI = aricode::NMI(x, y))
  } else{ # assign each noise point to its own cluster
    x_2 <- x
    if(sum(x == 0) > 0){
      x_2[x_2 == 0] <- (max(x) + 1):(max(x) + sum(x == 0))
    }
    c(ARI = mclust::adjustedRandIndex(x, y),
      ARI_2 = mclust::adjustedRandIndex(x_2, y),
      NMI = aricode::NMI(x, y),
      NMI_2 = aricode::NMI(x_2, y))
    
  }
  
}

calc_dist <- function(list){
  
  if(grepl("raw", list$name)){
    print(paste(Sys.time(), list$name, " dist"))
  
    list$dist <- proxy::dist(list$dat_X)
  }
  
  
  return(list)
}

# Loads MNIST data, see: https://gist.github.com/sboysel/3fed0a36a5b231278089
load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('data/data_raw/train-images-idx3-ubyte')
  test <<- load_image_file('data/data_raw/t10k-images-idx3-ubyte')
  
  train$y <<- load_label_file('data/data_raw/train-labels-idx1-ubyte')
  test$y <<- load_label_file('data/data_raw/t10k-labels-idx1-ubyte')
}

# main function for experiments
library(tidyverse)

source("code/functions/generate_data.R")
source("code/functions/separability_functions.R")


#' Function to run experiments on  synthetic data
#' 
#' Generates data, calculates separability measures, calculates DBSCAN-clustering (optional),
#' calculates an UMAP with separability measures and DBSCAN clustering.
#' @param params_list list containing all relevant parameters: 
#' parameter ID: ID of experiment 
#' parameter seed_data: seed for data generation
#' parameters for data generation: see generate data or code below. Variables that aren't needed (e.g. r_2 if manif = "point") can be left out
#' parameter dbscan_rawData: should a DBSCAN clustering for the raw data be calculated?
#' parameters for DBSCAN: minPts; eps_min, eps_max, eps_by -> they form the epsilon range 
#' parameter comp_umap: should a umap embedding be computed?
#' parameters for umap: seed_umap, n_neighbors_umap, n_components_umap
experiment <- function(params_list){
  
  list2env(params_list, globalenv())
  
  print(paste(Sys.time(), ID))
  
  #### generate data ####
  set.seed(seed_data)
  data <- generate_data(n1 = n1, 
                        n2 = n2, 
                        manif = manif, 
                        dist = dist, 
                        r_2 = r_2, 
                        r_moon = r_moon, 
                        dim_sphere = dim_sphere, 
                        moon_shift = moon_shift,
                        cov_1 = cov_1, 
                        cov_2 = cov_2,
                        dim_noise = dim_noise,
                        n_irrev_features = n_irrev_features
                        )
  
  if(modif_data == TRUE){
    set.seed(seed_mod)
    data <- modify_data(data = data, mod_choice = mod_choice, perc = perc)
  }
    
  results <- list(data = data)
  
  #### calculate separability measures of raw data ####
  dist_raw <- proxy::dist(select(data, -component))
  
  sep_raw <- calc_sep_all(data = select(data, -component),
                          dist = dist_raw,
                          labels = data$component,
                          name_value = "dat_raw")
  
  
  results <- append(results,
                    list(dist_raw = dist_raw, sep_raw = sep_raw))
  
  #### calculate DBSCAN-clustering of raw data (optional) ####
  eps_range <- seq(from = eps_min, to = eps_max, by = eps_by)
  if(dbscan_rawData == TRUE){
    
    print(paste(Sys.time(), "dbscan - raw data"))
    # calculate ARI and NMI for eps_range
    labels <- data$component
    if(modif_data == TRUE && mod_choice == "noise"){
      labels <- labels[1:(n1 + n2)] # choose only "true" data, not noise
    }
    
    if(substr(ID, 1, 2) %in% c("E2", "E7", "E9")){ 
      # compute ARI, NMI as usual + with each noise point as own cluster
      res_dbscan_raw <- as.data.frame(cluster_res(dat = dist_raw, eps_range = eps_range, minPts = minPts, 
                                                  labels = labels, two_versions = TRUE)) 
      
      res_dbscan_raw$eps <- eps_range
      res_dbscan_raw_long <- res_dbscan_raw %>%
        gather("measure", "performance", c(1, 2, 3, 4))
      
      
    } else{
      res_dbscan_raw <- as.data.frame(cluster_res(dat = dist_raw, eps_range = eps_range, minPts = minPts, 
                                                  labels = labels))
      
      res_dbscan_raw$eps <- eps_range
      res_dbscan_raw_long <- res_dbscan_raw %>%
        gather("measure", "performance", c(1,2))
    }
    
    # find best epsilon and re-compute clustering solution
    eps_dbscan_raw <- res_dbscan_raw_long$eps[which.max(res_dbscan_raw_long$performance)]
    dbscan_raw <- dbscan::dbscan(x = dist_raw, eps = eps_dbscan_raw, minPts = minPts)
    
    results <- append(results,
                      list(res_dbscan_raw_long = res_dbscan_raw_long,
                           eps_dbscan_raw = eps_dbscan_raw,
                           dbscan_raw = dbscan_raw))
    print(paste(Sys.time(), "dbscan - raw data - done"))
  }
  
  #### calculate UMAP embedding and evaluate it ####
  if(comp_umap == TRUE){
    
    umap_emb <- umap::umap(as.matrix(dist_raw), random_state = seed_umap, 
                     n_neighbors = n_neighbors_umap, n_components = n_components_umap, input = "dist")
    
    dat_umap <- as.data.frame(umap_emb$layout)
    colnames(dat_umap) <- paste0("X", 1:ncol(dat_umap))
    dat_umap$component <- data$component
    
    dist_umap <- proxy::dist(select(dat_umap, -component))
    
    #### calculate separability measures ####
    sep_umap <- calc_sep_all(data = select(dat_umap, -component),
                            dist = dist_umap,
                            labels = dat_umap$component,
                            name_value = "dat_umap")
    
    
    results <- append(results,
                      list(dist_umap = dist_umap, sep_umap = sep_umap))
    
    #### calculate DBSCAN-clustering of umap embedding ####
    print(paste(Sys.time(), "dbscan - umap embedding"))
    # calculate ARI and NMI for eps_range
    labels <- data$component
    if(modif_data == TRUE && mod_choice == "noise"){
      labels <- labels[1:(n1 + n2)] # choose only "true" data, not noise
    }
    
    if(substr(ID, 1, 2) %in% c("E2", "E7", "E9")){ 
      # compute ARI, NMI as usual + with each noise point as own cluster
      res_dbscan_umap <- as.data.frame(cluster_res(dat = dist_umap, eps_range = eps_range, minPts = minPts, 
                                                  labels = labels, two_versions = TRUE)) 
      
      res_dbscan_umap$eps <- eps_range
      res_dbscan_umap_long <- res_dbscan_umap %>%
        gather("measure", "performance", c(1,2,3,4))
      
      
    } else{
      res_dbscan_umap <- as.data.frame(cluster_res(dat = dist_umap, eps_range = eps_range, minPts = minPts, 
                                                  labels = labels))
      
      res_dbscan_umap$eps <- eps_range
      res_dbscan_umap_long <- res_dbscan_umap %>%
        gather("measure", "performance", c(1,2))
    }
    
    # find best epsilon and re-compute clustering solution
    eps_dbscan_umap <- res_dbscan_umap_long$eps[which.max(res_dbscan_umap_long$performance)]
    dbscan_umap <- dbscan::dbscan(x = dist_umap, eps = eps_dbscan_umap, minPts = minPts)
    
    results <- append(results,
                      list(dat_umap = dat_umap,
                           dist_umap = dist_umap,
                           res_dbscan_umap_long = res_dbscan_umap_long,
                           eps_dbscan_umap = eps_dbscan_umap,
                           dbscan_umap = dbscan_umap))
    print(paste(Sys.time(), "dbscan - umap emb. - done"))
    
  }
  
  # return(results)
  saveRDS(results, file = paste0("results/experiments_rawData/", ID, ".rds"))
  
  
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

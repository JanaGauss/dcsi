# Script to create the results data frames in results/experiments.
# In order to run correctly run this file, all experiments must have been calculated, which takes several days
# (the results are automatically placed in the results/experiments_rawData folder, they are not provided in the git repo).

library(dplyr)
library(stringr)

#### function to create data frames ####
# for E1, E3, E4, E5, E6, E8  
readResults <- function(filenames){
  
  results <- data.frame()
  
  for(i in filenames){
    
    list <- readRDS(paste0("results/experiments_rawData/", i))
    
    
    id <- str_extract(i, "_\\d+.rds")
    id <- str_replace(id, "_", "")
    id <- str_replace(id, ".rds", "")
    print(id)
    
    res <- c(as.numeric(id), 
             list$sep_raw$dat_raw, 
             max(filter(list$res_dbscan_raw_long, measure == "ARI")$performance), max(filter(list$res_dbscan_raw_long, measure == "NMI")$performance),
             list$sep_umap$dat_umap, 
             max(filter(list$res_dbscan_umap_long, measure == "ARI")$performance), max(filter(list$res_dbscan_umap_long, measure == "NMI")$performance))
    
    results <- rbind(results, res)
  }
  
  colnames(results) <- c("ID",
                         paste0(list$sep_raw$measure, "_raw"),
                         "max_ARI_raw", "max_NMI_raw",
                         paste0(list$sep_umap$measure, "_umap"),
                         "max_ARI_umap", "max_NMI_umap")
  
  results <- results %>%
    mutate_all(as.character) %>%
    mutate_all(as.numeric) %>%
    mutate_all(function(x) round(x, 4))
  
  return(results)
  
}

# for E2, E7, E9: add ARI_2, NMI_2
# for ARI_2 and NMI_2, each noise point of the dbscan result is assigned to its own cluster. 
# Otherwise, the noise points are treated as one cluster by ARI and NMI (as dbscan assigns them to cluster "0"), which doesn't make sense. 
# This modification is mainly relevant for data sets with highly varying densities.
# E.g. for E9 (nested spheres), the density of the outer sphere is always lower, so there are epsilon-values, 
# where dbscan correctly identifies the inner sphere as one cluster and classifies all points of the outer sphere as noise points,
# which yields an ARI of 1 (perfect clustering), if the noise points aren't assigned to separate clusters.
# ARI_2 is used for evaluation in the paper for E2 and E9 (for E7, there were almost no differences between ARI and ARI_2). 
# See Gauss (2022) for more details.
readResults2 <- function(filenames){
  
  results <- data.frame()
  
  for(i in filenames){
    
    list <- readRDS(paste0("results/experiments_rawData/", i))
    
    
    id <- str_extract(i, "_\\d+.rds")
    id <- str_replace(id, "_", "")
    id <- str_replace(id, ".rds", "")
    print(id)
    
    res <- c(as.numeric(id), 
             list$sep_raw$dat_raw, 
             max(filter(list$res_dbscan_raw_long, measure == "ARI")$performance), max(filter(list$res_dbscan_raw_long, measure == "NMI")$performance),
             max(filter(list$res_dbscan_raw_long, measure == "ARI_2")$performance), max(filter(list$res_dbscan_raw_long, measure == "NMI_2")$performance),
             list$sep_umap$dat_umap, 
             max(filter(list$res_dbscan_umap_long, measure == "ARI")$performance), max(filter(list$res_dbscan_umap_long, measure == "NMI")$performance),
             max(filter(list$res_dbscan_umap_long, measure == "ARI_2")$performance), max(filter(list$res_dbscan_umap_long, measure == "NMI_2")$performance))
    
    results <- rbind(results, res)
  }
  
  colnames(results) <- c("ID",
                         paste0(list$sep_raw$measure, "_raw"),
                         "max_ARI_raw", "max_NMI_raw",
                         "max_ARI_2_raw", "max_NMI_2_raw",
                         paste0(list$sep_umap$measure, "_umap"),
                         "max_ARI_umap", "max_NMI_umap",
                         "max_ARI_2_umap", "max_NMI_2_umap")
  
  results <- results %>%
    mutate_all(as.character) %>%
    mutate_all(as.numeric) %>%
    mutate_all(function(x) round(x, 4))
  
  return(results)
  
}


#### code ####
files_E1 <- grep("E1_", list.files("results/experiments_rawData"), value = TRUE)
E1_results <- readResults(files_E1)
E1_results$ID <- paste0("E1_", E1_results$ID)

params_E1 <- readRDS("results/experiments/params_E1.rds")
E1_results <- full_join(params_E1, E1_results)
saveRDS(E1_results, "results/experiments/results_E1.rds")


files_E2 <- grep("E2_", list.files("results/experiments_rawData"), value = TRUE)
E2_results <- readResults2(files_E2)
E2_results$ID <- paste0("E2_", E2_results$ID)

params_E2 <- readRDS("results/experiments/params_E2.rds")

E2_results <- full_join(params_E2, E2_results)
saveRDS(E2_results, "results/experiments/results_E2.rds")


files_E3 <- grep("E3_", list.files("results/experiments_rawData"), value = TRUE)
E3_results <- readResults(files_E3)
E3_results$ID <- paste0("E3_", E3_results$ID)

params_E3 <- readRDS("results/experiments/params_E3.rds")

E3_results <- full_join(params_E3, E3_results)
saveRDS(E3_results, "results/experiments/results_E3.rds")



files_E4 <- grep("E4_", list.files("results/experiments_rawData"), value = TRUE)
E4_results <- readResults(files_E4)
E4_results$ID <- paste0("E4_", E4_results$ID)

params_E4 <- readRDS("results/experiments/params_E4.rds")

E4_results <- full_join(params_E4, E4_results)
saveRDS(E4_results, "results/experiments/results_E4.rds")



files_E5 <- grep("E5_", list.files("results/experiments_rawData"), value = TRUE)
E5_results <- readResults(files_E5)
E5_results$ID <- paste0("E5_", E5_results$ID)

params_E5 <- readRDS("results/experiments/params_E5.rds")

E5_results <- full_join(params_E5, E5_results)
saveRDS(E5_results, "results/experiments/results_E5.rds")


files_E6 <- grep("E6_", list.files("results/experiments_rawData"), value = TRUE)
E6_results <- readResults(files_E6)
E6_results$ID <- paste0("E6_", E6_results$ID)

params_E6 <- readRDS("results/experiments/params_E6.rds")

E6_results <- full_join(params_E6, E6_results)
saveRDS(E6_results, "results/experiments/results_E6.rds")



files_E7 <- grep("E7_", list.files("results/experiments_rawData"), value = TRUE)
E7_results <- readResults2(files_E7)
E7_results$ID <- paste0("E7_", E7_results$ID)

params_E7 <- readRDS("results/experiments/params_E7.rds")

E7_results <- full_join(params_E7, E7_results)
saveRDS(E7_results, "results/experiments/results_E7.rds")


files_E8 <- grep("E8_", list.files("results/experiments_rawData"), value = TRUE)
E8_results <- readResults(files_E8)
E8_results$ID <- paste0("E8_", E8_results$ID)

params_E8 <- readRDS("results/experiments/params_E8.rds")

E8_results <- full_join(params_E8, E8_results)
saveRDS(E8_results, "results/experiments/results_E8.rds")



files_E9 <- grep("E9_", list.files("results/experiments_rawData"), value = TRUE)
E9_results <- readResults2(files_E9)
E9_results$ID <- paste0("E9_", E9_results$ID)

params_E9 <- readRDS("results/experiments/params_E9.rds")

E9_results <- full_join(params_E9, E9_results)
saveRDS(E9_results, "results/experiments/results_E9.rds")


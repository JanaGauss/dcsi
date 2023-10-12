# Functions for calculation of separability
library(dplyr)
library(stringr)


#' Function to calculate separability measures
#' 
#' calculates some CVIs, complexity measures, DSI and a self defined separability measure
#' @param data a dataframe with the data
#' @param dist a pre-calculated distance matrix (optional)
#' @param labels a vector with labels
#' @param name_value name of the value column
calc_sep_all <- function(data, dist = NULL, labels, name_value = "value"){
  
  if(is.null(dist)){
    dist <- proxy::dist(data)
  }
  labels <- factor(labels)
  
  cvi <- c("Calinski_Harabasz", "Davies_Bouldin", "Dunn", "Silhouette")
  compl <- c("neighborhood", "network")
  
  # CVIs:
  cvi_measures <- unlist(clusterCrit::intCriteria(traj = as.matrix.data.frame(data), 
                                     part = as.integer(labels), crit = cvi))
  # correct CH:
  CH_0 <- cvi_measures[1]*(length(unique(labels)) - 1)/(nrow(data) - length(unique(labels))) # remove the factor (n-k)(k-1)
  CH_tranfs <- CH_0/(1+CH_0) # transform to [0,1] with 1 = best value. For original CH, the higher the better
  
  # correct Davies-Bouldin:
  DB_transf <- 1/(1+cvi_measures[2]) # transform to [0,1] with 1 = best value. For original DB, the lower the better
  
  # correct Dunn index:
  Dunn_transf <- cvi_measures[3]/(1+cvi_measures[3]) # transform to [0,1] with 1 = best value. For original Dunn, the higher the better
  
  # correct silhouette index:
  # silhouette index is in [-1,1] with 1 the best value 
  sil_transf <- (cvi_measures[4] + 1)/2 # transform to [0,1] by adding 1 (-> [0, 2]) and then dividing by 2
  
  cvi_measures <- c(CH_tranfs, DB_transf, Dunn_transf, sil_transf)
  names(cvi_measures) <- paste0(names(cvi_measures), "*") # star indicates correction/transformation compared to original definition
  
  # Complexity measures:
  compl_measures <- (1 - ECoL::complexity(x = data, y = labels, 
                                    summary = "mean", groups = compl)[1:8])
  # 1 - compl. measure, because for them holds 1 = complex (worst value)
  
  # DSI:
  dsi <- calc_DSI(dist, labels)
  
  # CVNN
  cvnn <- calc_CVNN(dist, labels)
  
  # DCSI
  dcsi <- calc_DCSI(dist, labels)
  
  result <- data.frame(measure = c(names(cvi_measures),
                                   "CVNN*", # star indicates that there is some sort of "correction" compared to the original measure
                                   names(compl_measures),
                                   names(dcsi),
                                   "DSI"),
                       value = c(cvi_measures, cvnn, compl_measures, unlist(dcsi), dsi))
  result$measure <- stringr::str_replace(result$measure, "neighborhood.|network.", "") # shorten names of measures
  result$measure <- stringr::str_replace(result$measure, ".mean", "")
  result$category <- c(rep("CVI", 5), rep("Complexity", 8), rep("other", 4))
  result$category2 <- c(rep("Compactness", 4), "Compactn./Neighborh.", rep("Neighborhood/Graph", 11), "Distributional")
  
  names(result)[2] <- name_value
    
  return(result)
    
}


#' Function to calculate DSI
#' 
#' calculates DSI (from Data Separability for Neural Network Classifiers and the Development of a Separability Index, Guan et al.)
#' @param dist a distance matrix
#' @param labels a vector with labels
calc_DSI <- function(dist, labels){
  
  dist <- as.matrix(dist)
  
  # calculate KS similarity between ICD and BCD for every class
  KS_list <- list()
  
  for(i in unique(labels)){
    
    ind_i <- which(labels == i)
    
    # ICD set: intra-class distances
    ICD_i <- dist[ind_i, ind_i]
    elements_ICD <- upper.tri(dist[ind_i, ind_i], diag = FALSE) # elements that must be selected from ICD_i
    ICD_vector <- ICD_i[elements_ICD]
    
    # BCD set: between-class distances
    BCD_vector <- as.vector(dist[ind_i, -ind_i])
    
    # calculate KS statistic
    KS_i <- suppressWarnings(ks.test(ICD_vector, BCD_vector, alternative = "two.sided")$statistic)
    
    KS_list <- append(KS_list, list(KS_i))
  }
  
  # average KS similarity values
  DSI <- mean(unlist(KS_list))
  
  # return result
  return(DSI)
  
}


#' Function to calculate (a modified version of) CVNN
#' 
#' calculates CVNN 
#' (with some modifications, originally from Understanding and Enhancement of Internal Clustering Validation Measures, Liu et al.)
#' Modifications: 
#' 1. to calculate the overall Compactness, the mean is used instead of the sum of intra-cluster compactness-values
#' 2. Compactness is divided by the mean distance of points, so that CVNN can be compared between different data sets
#' 3. CVNN is transformed so that its in (0,1], and 1 is the best value
#' @param dist a distance matrix
#' @param labels a vector with labels
#' @param k number of nearest neighbors
calc_CVNN <- function(dist, labels, k = 10){
  
  dist <- as.matrix(dist)
  
  # calculate separation and compactness for every class
  Sep_list <- list()
  Comp_list <- list()
  
  knn_graph <- cccd::nng(dx = dist, k = k)
  knn_matrix <- as.matrix(igraph::as_adjacency_matrix(knn_graph))
  
  for(i in unique(labels)){
    
    ind_i <- which(labels == i)
    
    # Separation: proportion of k-NN of objects in i that don't belong to i
    knn_i <- knn_matrix[ind_i, -ind_i] 
    sep_i <- sum(knn_i)/(length(ind_i)*k)
    
    Sep_list <- append(Sep_list, sep_i)
    
    
    # Compactness: average pairwise distance between objects in same cluster
    dist_i <- dist[ind_i, ind_i]
    comp_i <- mean(dist_i[upper.tri(dist_i, diag = FALSE)])
    
    Comp_list <- append(Comp_list, list(comp_i))
    
  }
  
  # calculate CVNN
  # Modification for compactness: mean instead of sum, normalize by mean distance so that CVNN of different data can be compared
  diag(dist) <- NA
  Comp <- mean(unlist(Comp_list))/mean(dist, na.rm = TRUE)
  Sep <- max(unlist(Sep_list))
  
  CVNN <- Comp + Sep
  
  # Modification: in order to have a measure between 0 and 1, CVNN is transformed
  # the lower CVNN, the better
  # for the transformed version, the higher the better (as it measures the separability)
  CVNN_transf <- 1/(1+CVNN)
  
  # return result
  return(CVNN_transf)
  
  
}
  
  

#' Function to calculate self developed "density cluster separability index" (DCSI)
#' 
#' eps = median distance to minPts*2-th neighbor for each class
#' @param dist a distance matrix
#' @param labels a vector with labels
#' @param minPts minPts argument for core point definition
calc_DCSI <- function(dist, labels, minPts = 5){
  
  dist <- as.matrix(dist)
  
  # compute core points: 
  # calculate eps for each class = median distance to minPts*2-th nearest neighbor
  # calculate distance to minPts-th nearest neighbor among points of same class
  ind_corePoints <- c()
  for(i in unique(labels)){
    
    ind_i <- which(labels == i) 
    
    dist_i <- dist[ind_i, ind_i]
    
    knn_graph_eps_i <- cccd::nng(dx = dist_i, k = minPts*2)
    knn_matrix_eps_i <- as.matrix(igraph::as_adjacency_matrix(knn_graph_eps_i))
    knn_weights_eps_i <- matrixcalc::hadamard.prod(knn_matrix_eps_i, dist_i) # add distances to knn-graph
    
    dist_kth_neighbor_eps_i <- apply(knn_weights_eps_i, 1, max) # maximum value of every row = distance to minPts*2-th neighbor
    eps_i <- median(dist_kth_neighbor_eps_i)
    
    
    # calculate core points
    knn_graph_i <- cccd::nng(dx = dist_i, k = minPts)
    knn_matrix_i <- as.matrix(igraph::as_adjacency_matrix(knn_graph_i))
    knn_weights_i <- matrixcalc::hadamard.prod(knn_matrix_i, dist_i) # add distances to knn-graph
    
    dist_kth_neighbor_i <- apply(knn_weights_i, 1, max)
    
    ind_corePoints_i <- which(dist_kth_neighbor_i <= eps_i)
    
    ind_corePoints <- c(ind_corePoints, ind_i[ind_corePoints_i])
    
  }
  
  # from now on, consider only core points
  dist_core <- dist[ind_corePoints, ind_corePoints]
  labels_core <- labels[ind_corePoints]
  
  # for each cluster i: 
  # separation = minimum distance between a core point of i and a core point that is not in i
  # connectedness = maximum distance in a MST built of the core points of cluster i
  Sep_list <- list()
  Conn_list <- list()
  
  for(i in unique(labels_core)){
    
    ind_i <- which(labels_core == i) 
    
    dist_i <- dist_core[ind_i, -ind_i]
    sep_i <- min(dist_i)
    
    
    Sep_list <- append(Sep_list, sep_i)
    
    
    # Connectedness: maximum distance in a MST built of the core points of cluster i
    mst_i <- ape::mst(dist_core[ind_i, ind_i])
    mst_weights <- matrixcalc::hadamard.prod(dist_core[ind_i, ind_i], mst_i)
    conn_i <- max(mst_weights)
    
    
    
    Conn_list <- append(Conn_list, list(conn_i))
    
  }
  
  Sep <- min(unlist(Sep_list)) # the higher the better
  Conn <- max(unlist(Conn_list)) # the smaller the better
  # consider ratio Sep/Conn -> the higher the better
  # values should be in [0,1] with 1 = highest value
  DCSI <- (Sep/Conn)/(1+Sep/Conn) # = 1/(1+Conn/Sep)
  
  result <- list("DCSI" = DCSI, "Sep_DCSI" = Sep, "Conn_DCSI" = Conn)
  
  # return result
  return(result)
  
}


#' Function to calculate separability measures on real world data
#' 
#' calculates some CVIs, complexity measures, DSI and a self defined separability measure.
#' This function is modified compared to calc_sep_all because pairwise separabilities are also calculated
#' @param data a dataframe with the data
#' @param dist a pre-calculated distance matrix (optional)
#' @param labels a vector with labels
#' @param name_value name of the value column
calc_sep_RW <- function(data, dist = NULL, labels, name_value = "value"){
  
  if(is.null(dist)){
    dist <- proxy::dist(data)
  }
  
  dist <- as.matrix(dist)
  labels <- factor(labels)
  labels_numeric <- as.numeric(labels)
  
  # calculate separability of whole data set (complexity measures and CVIs)
  CVIs_Compl_all <- calc_CVIs_Compl(data = data, dist = dist, labels = labels, name_value = paste0(name_value, "_all"))
  
  # calculate pairwise separability 
  results_pairs <- list()
  
  for(i in 1:max(labels_numeric)){
    
    for(j in i:max(labels_numeric)){
      if(i != j){
        
        ind_pair <- which(labels_numeric %in% c(i, j))
        
        dat_pair <- data[ind_pair, ]
        
        dist_pair <- dist[ind_pair, ind_pair]
        
        labels_pair <- labels[ind_pair]
        
        CVIs_Compl_pair <- calc_CVIs_Compl(data = dat_pair, dist = dist_pair, 
                                           labels = labels_pair, name_value = paste0(name_value, "_", i, "_", j))
        
        results_pairs <- append(results_pairs,
                          list(CVIs_Compl_pair))
        names(results_pairs)[length(results_pairs)] <- paste0("Sep_", i, "_", j)
        
      }
    }
    
  }
  
  # create a matrix of pairwise separation values for each measure:
  list_sep_matrices <- list()
  for(i in CVIs_Compl_all$measure){
    
    sep_matrix <- matrix(0, nrow = max(labels_numeric), ncol = max(labels_numeric))
    
    for(j in names(results_pairs)){
      
      row <- as.numeric(str_split(j, "_")[[1]][2])
      col <- as.numeric(str_split(j, "_")[[1]][3])
      
      sep_matrix[row, col] <- results_pairs[[j]][which(results_pairs[[j]]$measure == i), 2]
      
    }
    
    sep_matrix <- sep_matrix + t(sep_matrix)
    diag(sep_matrix) <- NA
    
    
    list_sep_matrices <- append(list_sep_matrices,
                                list(sep_matrix))
    names(list_sep_matrices)[length(list_sep_matrices)] <- i
    
  }
  
  # calculate DCSI
  dcsi <- calc_DCSI_RW(dist, labels) # uses precomputed results, e.g. the MSTs only have to be calculated once
  dat_dcsi <- data.frame("DCSI",
                         dcsi$all,
                         "other", 
                         "Neighborhood/Graph")
  colnames(dat_dcsi) <- colnames(CVIs_Compl_all)
  
  CVIs_Compl_all <- rbind(CVIs_Compl_all, dat_dcsi)
  
  list_sep_matrices <- append(list_sep_matrices,
                              list(dcsi$pair_matrix))
  names(list_sep_matrices)[length(list_sep_matrices)] <- "DCSI"
      


  
  result<- list(sep_all = CVIs_Compl_all,
                sep_pair = list_sep_matrices)
  
  return(result)
  
}



#' Function to calculate complexity measures and CVIs (used for real world data)
#' 
#' @param data a dataframe with the data
#' @param labels a vector with labels
#' @param name_value name of the value column
calc_CVIs_Compl <- function(data, dist, labels, name_value = "value"){
  
  labels <- factor(labels)
  
  cvi <- c("Calinski_Harabasz", "Davies_Bouldin", "Dunn", "Silhouette")
  compl <- c("neighborhood", "network")
  
  # CVIs:
  cvi_measures <- unlist(clusterCrit::intCriteria(traj = as.matrix.data.frame(data), 
                                                  part = as.integer(labels), crit = cvi))
  # correct CH:
  CH_0 <- cvi_measures[1]*(length(unique(labels)) - 1)/(nrow(data) - length(unique(labels))) # remove the factor (n-k)(k-1)
  CH_tranfs <- CH_0/(1+CH_0) # transform to [0,1] with 1 = best value. For original CH, the higher the better
  
  # correct Davies-Bouldin:
  DB_transf <- 1/(1+cvi_measures[2]) # transform to [0,1] with 1 = best value. For original DB, the lower the better
  
  # correct Dunn index:
  Dunn_transf <- cvi_measures[3]/(1+cvi_measures[3]) # transform to [0,1] with 1 = best value. For original Dunn, the higher the better
  
  # correct silhouette index:
  # silhouette index is in [-1,1] with 1 the best value 
  sil_transf <- (cvi_measures[4] + 1)/2 # transform to [0,1] by adding 1 (-> [0, 2]) and then dividing by 2
  
  cvi_measures <- c(CH_tranfs, DB_transf, Dunn_transf, sil_transf)
  names(cvi_measures) <- paste0(names(cvi_measures), "*") # star indicates correction/transformation compared to original definition
  
  # Complexity measures:
  compl_measures <- (1 - ECoL::complexity(x = data, y = labels, 
                                          summary = "mean", groups = compl)[c(1, 2, 3, 6, 7, 8)])
  # 1 - compl. measure, because for them holds 1 = complex (worst value)
  
  # DSI:
  dsi <- calc_DSI(dist, labels)
  
  # CVNN
  cvnn <- calc_CVNN(dist, labels)
  
  result <- data.frame(measure = c(names(cvi_measures),
                                   "CVNN*", # star indicates that there is some sort of "correction" compared to the original measure
                                   names(compl_measures),
                                   "DSI"),
                       value = c(cvi_measures, cvnn, compl_measures, dsi))
  result$measure <- stringr::str_replace(result$measure, "neighborhood.|network.", "") # shorten names of measures
  result$measure <- stringr::str_replace(result$measure, ".mean", "")
  result$category <- c(rep("CVI", 5), rep("Complexity", 6), "other")
  result$category2 <- c(rep("Compactness", 4), "Compactn./Neighborh.", rep("Neighborhood/Graph", 6), "Distributional")
  
  names(result)[2] <- name_value
  
  return(result)
  
}


#' Function to calculate self developed "density cluster separability index" (DCSI) (real world data)
#' 
#' eps = median distance to minPts*2-th neighbor for each class
#' @param dist a distance matrix
#' @param labels a vector with labels
#' @param minPts minPts argument for core point definition
calc_DCSI_RW <- function(dist, labels, minPts = 5){
  
  dist <- as.matrix(dist)
  
  labels_numeric <- as.numeric(labels)
  
  # compute core points: 
  # calculate eps for each class = median distance to minPts*2-th = k-th neighbor
  # calculate distance to minPts = k-th neighbor among points of same class
  ind_corePoints <- c()
  for(i in unique(labels)){
    
    ind_i <- which(labels == i) 
    
    dist_i <- dist[ind_i, ind_i]
    
    knn_graph_eps_i <- cccd::nng(dx = dist_i, k = minPts*2)
    knn_matrix_eps_i <- as.matrix(igraph::as_adjacency_matrix(knn_graph_eps_i))
    knn_weights_eps_i <- matrixcalc::hadamard.prod(knn_matrix_eps_i, dist_i) # add distances to knn-graph
    
    dist_kth_neighbor_eps_i <- apply(knn_weights_eps_i, 1, max) # maximum value of every row = distance to k-th neighbor
    eps_i <- median(dist_kth_neighbor_eps_i)
    
    
    # calculate core points
    knn_graph_i <- cccd::nng(dx = dist_i, k = minPts)
    knn_matrix_i <- as.matrix(igraph::as_adjacency_matrix(knn_graph_i))
    knn_weights_i <- matrixcalc::hadamard.prod(knn_matrix_i, dist_i) # add distances to knn-graph
    
    dist_kth_neighbor_i <- apply(knn_weights_i, 1, max)
    
    ind_corePoints_i <- which(dist_kth_neighbor_i <= eps_i)
    
    ind_corePoints <- c(ind_corePoints, ind_i[ind_corePoints_i])
    
  }
  
  # from now on, consider only core points
  dist_core <- dist[ind_corePoints, ind_corePoints]
  labels_core <- labels[ind_corePoints]
  
  # for each cluster i: calculate connectedness and pairwise separation
  # connectedness = maximum distance in a MST built of the core points of cluster i
  Conn_list <- list()
  Sep_matrix <- matrix(0, nrow = max(as.numeric(labels_core)), ncol = max(as.numeric(labels_core)))
  for(i in unique(labels_core)){
    
    ind_i <- which(labels_core == i) 
    
    mst_i <- ape::mst(dist_core[ind_i, ind_i])
    mst_weights <- matrixcalc::hadamard.prod(dist_core[ind_i, ind_i], mst_i)
    conn_i <- max(mst_weights)
    
    Conn_list <- append(Conn_list, list(conn_i))
    
    for(j in i:max(as.numeric(labels_core))){
      if(i != j){
        
        ind_j <- which(labels_core == j)
        
        dist_ij <- dist_core[ind_i, ind_j]
        Sep_matrix[as.numeric(i), j] <- min(dist_ij)
        
      }
      
    }
    
  }
  Sep_matrix <- Sep_matrix + t(Sep_matrix)
  diag(Sep_matrix) <- NA
  
  
  # calculate pairwise DCSIs
  dcsi_matrix <- matrix(0, nrow = max(labels_numeric), ncol = max(labels_numeric))
  
  for(i in unique(labels_core)){
    
    for(j in i:max(as.numeric(labels_core))){
      if(i != j){
        
        Sep <- Sep_matrix[as.numeric(i), j]
        Conn <- max(Conn_list[[i]], Conn_list[[j]])
        
        dcsi_matrix[as.numeric(i), j] <- (Sep/Conn)/(1+Sep/Conn)
        
      }
      
    }
    
  }
  dcsi_matrix <- dcsi_matrix + t(dcsi_matrix)
  diag(dcsi_matrix) <- NA
  

  # calculate DCSI for whole data set
  # !!! Note that for the final results, a more robust version was used: average pairwise separability !!!
  Sep <- min(Sep_matrix, na.rm = TRUE) # the higher the better
  Conn <- max(unlist(Conn_list)) # the smaller the better
  # consider ratio Sep/Conn -> the higher the better
  # values should be in [0,1] with 1 = highest value
  DCSI <- (Sep/Conn)/(1+Sep/Conn) # = 1/(1+Conn/Sep)
  
  result <- list("all" = DCSI, "pair_matrix" = dcsi_matrix)
  
  # return result
  return(result)
  
}

# calculate DCSI with different values of min_Pts
calc_sep_RW_robust <- function(data, dist = NULL, labels, 
                               min_Pts = c(5, 10, 15, 20, 50)){
  
  results <- list()
  
  if(is.null(dist)){
    dist <- proxy::dist(data)
  }
  
  dist <- as.matrix(dist)
  labels <- factor(labels)
  labels_numeric <- as.numeric(labels)
  
  # calculate DCSI
  for(j in min_Pts){
    print(paste0("min_Pts = ", j))
    dcsi <- calc_DCSI_RW(dist, labels, minPts = j) # uses precomputed results, e.g. the MSTs only have to be calculated once
    dcsi <- append(dcsi, list(min_Pts = j))
    
    results[[length(results) + 1]] <- dcsi
  }
  
  return(results)
  
}



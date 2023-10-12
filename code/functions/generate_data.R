# Function to generate data
library(dplyr)

#' Function to generate well-behaved data from topological manifolds + Gaussian noise
#' 
#' generates uniform samples from two manifolds (points, circles, n-spheres, moons, spirals) + multidimensional Gaussian noise
#' @param n1 number of samples from 1. component
#' @param n2 number of samples from 2. component
#' @param manif form of manifold of components: point, circle, n_sphere, moon or spiral
#' @param dist (for points) distance of components. Means are always (0, ..., 0) and (dist, 0, ..., 0)
#' @param r_2 (for circles, n_spheres) radius of second circle/n_sphere. Radius of first circle/n_sphere is 2
#' @param r_moon radius of the moons
#' @param dim_sphere (for spheres) dimension of n_sphere
#' @param moon_shift how much (relative to the radius) are the two moons shifted vertically
#' @param cov_1 covariance of Gaussian noise of component 1. Can be a covariance matrix or a number. 
#' If cov_1 is a number, the covariance will be cov_1*identity matrix and the dimension (dim) has to be specified.
#' @param cov_2 covariance of Gaussian noise of component 2. Can be a covariance matrix or a number. If NULL, cov_2 = cov_1.
#' If cov_2 is a matrix, is must have the same dimension as cov_2.
#' If cov_2 is a number, the covariance will be cov_2*identity matrix and the dimension (dim) has to be specified.
#' @param dim_noise dimensionaly of Gaussian noise, only relevant if cov_1 and cov_2 are numbers
#' @param n_irrev_features number of irrelevant features sampled uniformly from [0,1] that are added to the dataset 
generate_data <- function(n1 = 500, n2 = 500,
                          manif = "point", 
                          dist = 6,
                          r_2 = 10,
                          r_moon = 6,
                          dim_sphere = 2,
                          moon_shift = 0.25,
                          cov_1 = 0.25, cov_2 = NULL,
                          dim_noise = 2,
                          n_irrev_features = 0){
  
  checkmate::assert_choice(manif, c("point", "circle", "n_sphere", "moon", "spiral"))
  
  #### calculate covariance matrices (Gaussian noise) and generate Gaussian noise ####
  if(is.null(cov_2)){ # if cov_2 isn't specified, cov_2 is set to be cov_1
    cov_2 <- cov_1
  }
  
  if(all(dim(as.matrix(cov_1))== c(1, 1))){ # if cov_1 is a number, the covariance will be cov_1*identity matrix
    cov_1 <- diag(cov_1, nrow = dim_noise)
  }
  
  if(all(dim(as.matrix(cov_2))== c(1, 1))){ # if cov_2 is a number, the covariance will be cov_2*identity matrix
    cov_2 <- diag(cov_2, nrow = dim_noise)
  }
  
  #### check covariance matrices
  checkmate::assert_true(all(dim(as.matrix(cov_1)) == dim(as.matrix(cov_2)))) # check for same dimensionality
  checkmate::assert_true(all(t(cov_1) == cov_1)) # check if covariances are symmetric matrices. 
  checkmate::assert_true(all(t(cov_2) == cov_2)) 
  # If they are positive definite is checked in mvrnorm
  
  #### generate Gaussian noise 
  noise_1 <- MASS::mvrnorm(n1, mu = rep(0, nrow(cov_1)), Sigma = cov_1)
  noise_2 <- MASS::mvrnorm(n2, mu = rep(0, nrow(cov_2)), Sigma = cov_2)
  
  
  #### generate data from manifold ####
  if(manif == "point"){
    # two points 
    
    manif1 <- matrix(0, nrow = n1)
    manif2 <- matrix(dist, nrow = n2)
    
  } else if(manif == "circle" | manif == "n_sphere"){
    # two circles/spheres around the origin
    
    if(manif == "circle"){
      dim_sphere <- 1 # a circle is a one-dimensional sphere
    }
    checkmate::assert_true(round(dim_sphere) == dim_sphere & dim_sphere >= 0) # check that dim_sphere is a natural number or 0
    
    manif1 <- TDA::sphereUnif(n1, d = dim_sphere, r = 4) # radius of first circle/n_sphere is always 1 
    manif2 <- TDA::sphereUnif(n2, d = dim_sphere, r = r_2)
    
  } else if(manif == "moon"){
    # two moons
    
    # sample from two circles
    manif1 <- as.data.frame(TDA::circleUnif(n1*3, r = r_moon)) %>% # sample a lot of observations and later choose n1 of them
      filter(x2 >= 0) %>% # take only the upper part of a circle
      mutate(x1 = x1 + r_moon) %>% # shift upper moon horizontally
      mutate(x2 = x2 - moon_shift*r_moon) # shift upper moon vertically
    manif1 <- manif1[sample(1:nrow(manif1), n1, replace = FALSE),] # sample n1 observations
      
    manif2 <- as.data.frame(TDA::circleUnif(n2*3, r = r_moon)) %>%
      filter(x2 < 0) # take only the lower part of a circle
    manif2 <- manif2[sample(1:nrow(manif2), n2, replace = FALSE),] # sample n2 observations
    
  } else if(manif == "spiral"){
    # two spirals, one is flipped so that they don't intersect
    
    manif1 <- as.data.frame(tdaunif::sample_arch_spiral(n1, ar = 1, arms = 1L, min_wrap = 0.5, max_wrap = 2, sd = 0)*2) # data is multiplied by 2 to separate the spirals a bit more
    manif2 <- as.data.frame(tdaunif::sample_arch_spiral(n2, ar = 1, arms = 1L, min_wrap = 0.5, max_wrap = 2, sd = 0)*2)
    
    manif2 <- manif2 %>% # flip data around the origin
      mutate(x = x*-1,
             y = y*-1)
    
  }
  
  #### create final dataset ####
  #### adjust dimensionalities of data on manifolds and noise
  if(ncol(manif1) < ncol(noise_1)){ # add zeros so that data from manifolds and noise have the same dimensionality
    
    manif1 <- cbind(manif1, matrix(0, nrow = n1,
                                 ncol = ncol(noise_1) - ncol(manif1)))
    
    manif2 <- cbind(manif2, matrix(0, nrow = n2,
                                   ncol = ncol(noise_2) - ncol(manif2)))
    
  } else if(ncol(manif1) > ncol(noise_1)){
    
    noise_1 <- cbind(noise_1, matrix(0, nrow = n1,
                                     ncol = ncol(manif1) - ncol(noise_1)))
    noise_2 <- cbind(noise_2, matrix(0, nrow = n2,
                                     ncol = ncol(manif2) - ncol(noise_2)))
    
  }
  manif1 <- manif1 + noise_1
  manif2 <- manif2 + noise_2
  
  #### add irrelevant features and component/manifold variable and create final dataset
  if(n_irrev_features != 0){
    
    irrev_1 <- matrix(runif(n1*n_irrev_features, min = 0, max = 1), 
                      byrow = TRUE, ncol = n_irrev_features)
    irrev_2 <- matrix(runif(n2*n_irrev_features, min = 0, max = 1), 
                      byrow = TRUE, ncol = n_irrev_features)
    
    manif1 <- cbind(manif1, irrev_1) 
    manif2 <- cbind(manif2, irrev_2)
    
  }
  
  manif1 <- as.data.frame(manif1)
  colnames(manif1) <- paste0("X", 1:ncol(manif1))
  manif2 <- as.data.frame(manif2)
  colnames(manif2) <- paste0("X", 1:ncol(manif2))
  
  manif1$component <- "A"
  manif2$component <- "B"
  data <- rbind(manif1, manif2)
  
  return(data)
  
  
} 


#' Function to modify data: add noise or a bridge
#' 
#' adds uniform noise or a bridge between two multivariate Gaussians, only for 2-D
#' @param data generated data
#' @param mod_choice "noise" or "bridge"
#' @param perc percentage: how much noise/bridge points should be added?
modify_data <- function(data, mod_choice = "noise", perc = 0.1){
  
  checkmate::assert_choice(mod_choice, c("noise", "bridge"))
  checkmate::assert_true(ncol(data) == 3) # only for 2-dim. data (X1, X2, component as columns)
  
  n_mod <- round(nrow(data)*perc) # number of noise/bridge points
  
  if(mod_choice == "noise"){
    
    data_noise <- data.frame(
      X1 = runif(n = n_mod, min = min(data$X1) - 0.1, max = max(data$X1) + 0.1),
      X2 = runif(n = n_mod, min = min(data$X2) - 0.1, max = max(data$X2) + 0.1),
      component = sample(x = unique(data$component), size = n_mod, replace = TRUE))
    
    data_mod <- rbind(data, data_noise)
    
  } else{ # bridge
    
    means <- aggregate(data$X1, list(data$component), FUN=mean) 
    
    min_bridge <- min(means[, 2])
    max_bridge <- max(means[, 2])
    
    sd_X2 <- sqrt(var(data$X2))
    
    data_bridge <- data.frame(
      X1 = runif(n = n_mod, min = min_bridge, max = max_bridge),
      X2 = rnorm(n = n_mod, mean = 0, sd = sd_X2*0.2)) %>% # X2: mean 0, some small noise (20% sd of real data)
      mutate(component = if_else(X1 < mean(c(min_bridge, max_bridge)),
                                 "A",
                                 "B"))
    
    data_mod <- rbind(data, data_bridge)
    
  }
  
  return(data_mod)
  
}


# Function to reproduce the data used for the experiments. Takes one row of a data frame (with parameters) as input an generates the corresponding data set
dat_experiment <- function(params){
  
  list_params <- split(params, 1)[[1]]
  list2env(list_params, globalenv())
  
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
  
  return(data)
  
}
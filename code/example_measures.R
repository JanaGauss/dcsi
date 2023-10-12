# separability measures on exemplary data sets (figure 2, table 2) and figure 1 
source("code/functions/separability_functions.R")
source("code/functions/plot_functions.R")
source("code/functions/generate_data.R")
library(dplyr)
library(cowplot)

# 9 toy data sets:
# 3 times: 2 Gaussian cluster, varying distance
# 2 Gaussian clusters + outlier
# moons, nested circles
# Random labels
# Linear separable, but only one connected component
# 2 clusters, but 3 components

#### generate data ####
set.seed(123)
dat_list <- lapply(c(2, 4, 8), function(x) generate_data(dist = x))

dat_outlier <- dat_list[[3]]
dat_outlier <- rbind(dat_outlier, append(as.list(rnorm(n = 2, mean = 0, sd = sqrt(0.05))), "B"))

dat_circle <- generate_data(manif = "circle", cov_1 = 0.05)

dat_moon <- generate_data(manif = "moon", cov_1 = 0.05, moon_shift = 0.5)

dat_random <- as.data.frame(MASS::mvrnorm(1000, mu = c(0,0), Sigma = diag(0.25, nrow = 2)))
colnames(dat_random) <- c("X1", "X2")
dat_random$component <- c(rep("A", 500), rep("B", 500)) 
dat_random <- dat_random[sample(1:nrow(dat_random), replace = FALSE),] # shuffle data because otherwise blue points will be plotted last and hide red points

dat_linsep <- as.data.frame(matrix(runif(2000), ncol = 2))
colnames(dat_linsep) <- c("X1", "X2")
dat_linsep$component <- if_else(dat_linsep$X1 < 0.5, "A", "B")

dat_3cluster <- as.data.frame(rbind(MASS::mvrnorm(500, mu = c(0,0), Sigma = diag(0.25, nrow = 2)),
                                    MASS::mvrnorm(500, mu = c(6, 6), Sigma = diag(0.25, nrow = 2)),
                                    MASS::mvrnorm(500, mu = c(3, 3), Sigma = diag(0.25, nrow = 2))))
colnames(dat_3cluster) <- c("X1", "X2")
dat_3cluster$component <- c(rep("A", 1000), rep("B", 500))

dat_list <- append(dat_list, list(dat_outlier, dat_moon, dat_circle, dat_random, dat_linsep, dat_3cluster))

#### Figure 2: data sets #####
plot_list <- lapply(dat_list, plot_2_3d_data, legend = FALSE, size = 0.3, alpha = 0.4)
p_outlier <- plot_list[[4]]
dat_outl_2 <- dat_outlier[1001, ]
p_outlier <- p_outlier + geom_point(data = dat_outl_2, aes(x = X1, y = X2), color = "#D55E00", size = 2, alpha = 1)
plot_list[[4]] <- p_outlier

do.call("plot_grid", c(plot_list, ncol = 3, labels = "AUTO"))

ggsave("paper/figures/DataSepExmp.pdf", 
       do.call("plot_grid", c(plot_list, ncol = 3, labels = "AUTO")), 
       width = 7, height = 5)


#### Table 2: separability measures ####
measures <- lapply(dat_list, function(x) calc_sep_all(data = select(x, -component), labels = x$component))

df_measures <- do.call(cbind.data.frame, lapply(measures, function(x) x$value))
colnames(df_measures) <- c("dist = 2", "dist = 4", "dist = 8", 
                           "outlier", "moon", "circle", 
                           "random", "linearly separable", "3 components")
df_measures <- cbind(measures[[1]]$measure, df_measures) 
df_measures[, 2:10] <- df_measures[, 2:10] %>% round(., 2)

#### Figure 1: separability: classification vs clustering ####
set.seed(123)

dat_linsep <- as.data.frame(matrix(runif(2000), ncol = 2))
colnames(dat_linsep) <- c("X1", "X2")
dat_linsep$component <- if_else(dat_linsep$X1 < 0.5, "A", "B")

dat_3cluster <- as.data.frame(rbind(MASS::mvrnorm(500, mu = c(0,0), Sigma = diag(0.25, nrow = 2)),
                                    MASS::mvrnorm(500, mu = c(4, 6), Sigma = diag(0.25, nrow = 2)),
                                    MASS::mvrnorm(500, mu = c(8, 0), Sigma = diag(0.25, nrow = 2))))
colnames(dat_3cluster) <- c("X1", "X2")
dat_3cluster$component <- c(rep("A", 1000), rep("B", 500))

plot1 <- plot_2_3d_data(dat_linsep, size = 0.5)
plot2 <- plot_2_3d_data(dat_3cluster, size = 0.3)


plot_grid(plot1, plot2, nrow = 1, labels = "AUTO")
ggsave("paper/figures/ClusterVsClass.pdf", 
       plot_grid(plot1, plot2, nrow = 1, labels = "AUTO"), 
       width = 6, height = 2)

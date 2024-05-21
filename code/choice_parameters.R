
source("code/functions/functions_parameters.R")
source("code/functions/generate_data.R")
source("code/functions/plot_functions.R")
source("code/functions/separability_functions.R")
library(cowplot)

#### generate data set #####
# generate data set (consists of one class) with two modes

set.seed(20240417)
n <- 250
data <- data.frame(sample_disk(n, sd = 0, bins = 1L))
ggplot(data = data) + geom_point(aes(x = x, y = y))

n2 <- 125
sd <- 0.12
data <- rbind(rbind(data, data.frame(x = rnorm(n2, mean = -0.4, sd = sd), y = rnorm(n2, mean = 0, sd = sd))),
              data.frame(x = rnorm(n2, mean = 0.4, sd = sd), y = rnorm(n2, mean = 0, sd = sd)))

ggplot(data = data) + geom_point(aes(x = x, y = y, alpha = 0.7)) + 
  guides(alpha = "none") + theme_bw()

all.equal(data, gen_data(n, n2, sd, 20240417))

dist_mat <- as.matrix(dist(data))

#### (Figure 2) Version 1: MinPts = 5, eps = Quantile of distribution of distance to 10th (= MinPts*2) nearest neighbor ####
MinPts <- 5
quantiles <- c(0.1, 0.2, 0.3, 0.5, 0.6, 0.8)
dist_vec1 <- get_dist_neighb(dist_mat, MinPts) # for calculation of core points
dist_vec2 <- get_dist_neighb(dist_mat, MinPts*2) # for calculation of epsilon

dat_v1 <- data.frame()
dat_v1_res <- data.frame()
for(q in quantiles){
  dat <- data
  
  eps <- quantile(dist_vec2, q)
  conn <- get_conn(dist_mat, dist_vec1, eps)
  dat$core <- ifelse(dist_vec1 < eps,
                     "core", "none")
  dat$core[c(conn$x1, conn$x2)] <- "conn"
  dat$q <- q
  
  dat_v1_res <- rbind(dat_v1_res,
                      data.frame(q = q, conn = round(conn$conn, 2),
                                 eps = round(eps, 3)))
  dat_v1 <- rbind(dat_v1, dat)
}


alpha_none <- 0.6
size_core <- 3
labels_v1 <- paste0("q = ", dat_v1_res$q, ", Conn = ", dat_v1_res$conn,
                    ", (eps = ", dat_v1_res$eps, ")")
names(labels_v1) <- as.character(quantiles)
plot1 <- ggplot(data = dat_v1) +
  geom_point(aes(x = x, y = y, col = core, size = core, alpha = core)) +
  scale_color_manual(values = c("black", "dodgerblue", "grey")) +
  scale_size_manual(values = c(3, 1, 1)) + 
  scale_alpha_manual(values = c(1, alpha_none, alpha_none)) +
  facet_wrap(~q, ncol = 3, labeller = as_labeller(labels_v1)) + theme_bw() +
  guides(size = "none", col = "none", alpha = "none")

plot_dat <- ggplot(data = data) + geom_point(aes(x = x, y = y), size = 0.7) +
  labs(x = "X1", y = "X2") + theme_bw()

ggsave("paper/figures/param1.pdf", plot_grid(plot_dat + theme(plot.margin = unit(c(3, 0.8, 3, 0.8), "cm")), 
                                             plot1 + labs(x = "X1", y = "X2"),
                                             rel_widths = c(1, 2.3)), 
       width = 10, height = 5)

#### Version 2: MinPts = 5, eps = median of distance to kth nearest neighbor ####
MinPts <- 5
k_vec <- c(5, 8, 10, 12, 15, 20)
dist_vec1 <- get_dist_neighb(dist_mat, MinPts) # for calculation of core points

dat_v2 <- data.frame()
dat_v2_res <- data.frame()
for(k in k_vec){
  dat <- data
  
  dist_vec2 <- get_dist_neighb(dist_mat, k)
  
  eps <- median(dist_vec2)
  conn <- get_conn(dist_mat, dist_vec1, eps)
  dat$core <- ifelse(dist_vec1 < eps,
                     "core", "none")
  dat$core[c(conn$x1, conn$x2)] <- "conn"
  dat$k <- k
  
  dat_v2_res <- rbind(dat_v2_res,
                      data.frame(k = k, conn = round(conn$conn, 2),
                                 eps = round(eps, 3)))
  dat_v2 <- rbind(dat_v2, dat)
}

labels_v2 <- paste0("k = ", dat_v2_res$k, ", Conn = ", dat_v2_res$conn,
                    ", (eps = ", dat_v2_res$eps, ")")
names(labels_v2) <- as.character(k_vec)
plot2 <- ggplot(data = dat_v2) +
  geom_point(aes(x = x, y = y, col = core, size = core, alpha = core)) +
  scale_color_manual(values = c("black", "dodgerblue", "grey")) +
  scale_size_manual(values = c(size_core, 1, 1)) + 
  scale_alpha_manual(values = c(1, alpha_none, alpha_none)) +
  facet_wrap(~k, ncol = 3, labeller = as_labeller(labels_v2)) + theme_bw() +
  guides(size = "none", col = "none", alpha = "none")

#### Version 3: different values of MinPts, eps = median distance to MinPts*2th nearest neighbor ####
MinPts_vec <- c(3, 5, 8, 10, 15, 20)

dat_v3 <- data.frame()
dat_v3_res <- data.frame()
for(m in MinPts_vec){
  dat <- data
  
  dist_vec1 <- get_dist_neighb(dist_mat, m) # for calculation of core points
  dist_vec2 <- get_dist_neighb(dist_mat, m*2) # for calculation of epsilon
  
  eps <- median(dist_vec2)
  conn <- get_conn(dist_mat, dist_vec1, eps)
  dat$core <- ifelse(dist_vec1 < eps,
                     "core", "none")
  dat$core[c(conn$x1, conn$x2)] <- "conn"
  dat$m <- m
  
  dat_v3_res <- rbind(dat_v3_res,
                      data.frame(m = m, conn = round(conn$conn, 2),
                                 eps = round(eps, 3)))
  dat_v3 <- rbind(dat_v3, dat)
}

labels_v3 <- paste0("MinPts = ", dat_v3_res$m, ", Conn = ", dat_v3_res$conn,
                    ", (eps = ", dat_v3_res$eps, ")")
names(labels_v3) <- as.character(MinPts_vec)
plot3 <- ggplot(data = dat_v3) +
  geom_point(aes(x = x, y = y, col = core, size = core, alpha = core)) +
  scale_color_manual(values = c("black", "dodgerblue", "grey")) +
  scale_size_manual(values = c(size_core, 1, 1)) + 
  scale_alpha_manual(values = c(1, alpha_none, alpha_none)) +
  facet_wrap(~m, ncol = 3, labeller = as_labeller(labels_v3)) + theme_bw() +
  guides(size = "none", col = "none", alpha = "none")


plot_grid(plot1, plot2, plot3, ncol = 1, labels = c("V1", "V2", "V3"))

#### Figure 3: Example of two clearly separated classes #####
set.seed(20240426)
dat2 <- generate_data(n1 = 500, n2 = 500, manif = "point", dist = 1.5, cov_1 = 0.03)
plot_2_3d_data(dat2)
dist_mat_example <- as.matrix(dist(select(dat2, c(X1, X2))))
dist_mat_A <- as.matrix(dist(select(filter(dat2, component == "A"), c(X1, X2))))
dist_mat_B <- as.matrix(dist(select(filter(dat2, component == "B"), c(X1, X2))))
MinPts <- 5

quantiles <- seq(from = 0.1, to = 0.9, by = 0.1)
dist_vec1_A <- get_dist_neighb(dist_mat_A, MinPts) # for calculation of core points
dist_vec2_A <- get_dist_neighb(dist_mat_A, MinPts*2) # for calculation of epsilon
dist_vec1_B <- get_dist_neighb(dist_mat_B, MinPts) # for calculation of core points
dist_vec2_B <- get_dist_neighb(dist_mat_B, MinPts*2) 

dat_example_res <- data.frame()
for(q in quantiles){
  eps_A <- quantile(dist_vec2_A, q)
  cp_A <- which(dist_vec1_A < eps_A)
  
  eps_B <- quantile(dist_vec2_B, q)
  cp_B <- which(dist_vec1_B < eps_B) + 500 # get original indices
  
  conn_A <- get_conn(dist_mat_A, dist_vec1_A, eps_A)
  conn_B <- get_conn(dist_mat_B, dist_vec1_B, eps_B)
  
  sep <- min(dist_mat_example[cp_A, cp_B])
  
  dat_example_res <- rbind(dat_example_res,
                           data.frame(q = q, conn_A = conn_A$conn, 
                                      conn_B = conn_B$conn, Conn = max(conn_A$conn, conn_B$conn), 
                                      Sep = sep, 
                                      DCSI = (sep/max(conn_A$conn, conn_B$conn))/(1+sep/max(conn_A$conn, conn_B$conn))))
}

dat_long <- dat_example_res %>% gather("measure", "value", 4:6)
dat_long$measure <- factor(dat_long$measure, levels = c("Conn", "Sep", "DCSI"))
p <- ggplot(dat_long, aes(q, value)) + geom_point() + geom_line() +
  facet_wrap(~measure) + ylim(c(0, 1)) + theme_bw() + labs(x = "quantile")
plot_grid(plot_dat + guides(color = "none"), p, rel_widths = c(1, 2))
ggsave("paper/figures/param2.pdf", plot_grid(plot_2_3d_data(dat2, alpha = 0.5, size = 0.75) + 
                                               guides(color = "none"), 
                                             p, rel_widths = c(1, 2)), 
       width = 8, height = 2.3)

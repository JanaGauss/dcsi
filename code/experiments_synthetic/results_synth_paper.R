source("code/functions/plot_functions.R")
source("code/functions/generate_data.R")
library(ggplot2)
library(cowplot)

cols <- c("max_ARI",
          "DCSI", "DSI", "Density", "ClsCoef", paste0("N", c(1:3)), "LSC",
          paste0(c("calinski_harabasz", "davies_bouldin", "dunn", "silhouette", "CVNN"), "*"))

#### create data frame with results from all experiments ####
dat_list <- list()

for(i in 1:9){
  dat_list[[i]] <- readRDS(paste0("results/experiments/results_E", i, ".rds"))
}

for(i in 1:length(dat_list)){
  
  dat <- dat_list[[i]]
  
  if(i %in% c(2, 9)){ # use ARI_2 for raw data for E2 and E9
    dat$max_ARI_raw <- dat$max_ARI_2_raw
  }
  
  if(i == 9){ # only use 2D embedding, as the results of the 3D embeddings are very similar, see Gauss (2022)
    dat <- dat %>%
      filter(n_components_umap == 2)
  }
  
  if(i == 3){
    dat <- dat %>%
      filter(mod_choice == "bridge")
  }
  
  dat <- dat[, c(paste0(cols, "_raw"), paste0(cols, "_umap"))]
  
  dat$experiment <- paste0("E", i)
  
  if(i == 1){
    results <- dat
  } else{
    results <- rbind(results, dat)
  }
}

saveRDS(results, "results/experiments/results_summary.rds")

#### Figure 12: overview synthetic data sets #######
E1 <- readRDS("results/experiments/results_E1.rds")
E2 <- readRDS("results/experiments/results_E2.rds")
E3 <- readRDS("results/experiments/results_E3.rds")
E4 <- readRDS("results/experiments/results_E4.rds")
E6 <- readRDS("results/experiments/results_E6.rds")
E7 <- readRDS("results/experiments/results_E7.rds")
E8 <- readRDS("results/experiments/results_E8.rds")
E9 <- readRDS("results/experiments/results_E9.rds")

dat_1 <- rbind(dat_experiment(E1[49,]),
               dat_experiment(E1[1,]),
               dat_experiment(E1[1519,]),
               dat_experiment(E1[1471,]))
dat_1$facet <- rep(c(1:4), each = 1000)

dat_2 <- rbind(dat_experiment(E2[25,]),
               dat_experiment(E2[1,]),
               dat_experiment(E2[1525,]),
               dat_experiment(E2[1501,]))
dat_2$facet <- rep(c(1:4), each = 1000)

dat_3 <- rbind(dat_experiment(E3[800,]),
               dat_experiment(E3[776,]),
               dat_experiment(E3[1550,]),
               dat_experiment(E3[1526,]))
dat_3$facet <- c(rep(1, nrow(dat_experiment(E3[800,]))),
                 rep(2, nrow(dat_experiment(E3[776,]))),
                 rep(3, nrow(dat_experiment(E3[1550,]))),
                 rep(4, nrow(dat_experiment(E3[1526,]))))

dat_4 <- rbind(dat_experiment(E4[18,]),
               dat_experiment(E4[1,]))
dat_4$facet <- rep(c(1:2), each = 1000)

dat_6 <- rbind(dat_experiment(E6[1,]),
               dat_experiment(E6[20,]),
               dat_experiment(E6[801,]),
               dat_experiment(E6[820,]))
dat_6$facet <- rep(c(1:4), each = 1000)

dat_7 <- rbind(dat_experiment(E7[41,]),
               dat_experiment(E7[1,]),
               dat_experiment(E7[861,]),
               dat_experiment(E7[821,]))
dat_7$facet <- rep(c(1:4), each = 1000)

dat_8 <- rbind(dat_experiment(E8[1,]),
               dat_experiment(E8[51,]))
dat_8$facet <- rep(c(1:2), each = 1000)

E9$dim_sphere <- 1
dat_9 <- rbind(dat_experiment(E9[3,]),
               dat_experiment(E9[1,])) # data with 0.25 covariance not shown
dat_9$facet <- rep(c(1:2), each = 1000)


theme <- theme(strip.background = element_blank(),
               strip.text = element_blank(),
               plot.title = element_text(size = 8, face = "bold"),
               axis.text = element_text(size = 6),
               axis.title = element_text(size = 6))
a <- 0.2
plot_dataAll <- plot_grid(
  plot_2_3d_data(dat_1, legend = FALSE, alpha = a) + facet_wrap(~facet, nrow = 1) +
    theme +
    scale_size_continuous(range = c(0, 8))+
    labs(title = "E1 - hom. 2D-Gaussians"),
  plot_2_3d_data(dat_2, legend = FALSE, alpha = a) + facet_wrap(~facet, nrow = 1) +
    theme+
    labs(title = "E2 - het. 2D-Gaussians"),
  plot_2_3d_data(dat_3, legend = FALSE, alpha = a) + facet_wrap(~facet, nrow = 1) +
    theme+
    labs(title = "E3 - 2D-Gaussians w/ bridge"),
  plot_grid(plot_2_3d_data(dat_4, legend = FALSE, alpha = a) + facet_wrap(~facet, nrow = 1, scales = "free_x") +
              theme+
              labs(title = "E4 - 2D-Gaussians + irrelev. uniforms and E5 - high-dim. Gaussians w/ irrelev. dimensions"),
            ggplot() + theme_void() +
              geom_text(aes(0, 0, label = "Only data sets without irrelevant features/\nwith two-dimensional noise are shown.\nThe maximum number of irrelevant features/\nmaximum dimension of noise is 2000.
                    \nNote the different scales of the x-axes."), size = 2.5),
            nrow = 1),
  plot_2_3d_data(dat_6, legend = FALSE, alpha = a) + facet_wrap(~facet, nrow = 1) +
    theme+
    labs(title = "E6 - 2D moons"),
  plot_2_3d_data(dat_7, legend = FALSE, alpha = a) + facet_wrap(~facet, nrow = 1) +
    theme+
    labs(title = "E7 - 2D nested circles"),
  plot_grid(
    plot_2_3d_data(dat_8, legend = FALSE, alpha = a) + facet_wrap(~facet, nrow = 1) +
      theme+
      labs(title = "E8 - 2D nested spirals"),
    plot_2_3d_data(dat_9, legend = FALSE, alpha = a) + facet_wrap(~facet, nrow = 1) +
      theme+
      labs(title = "E9 - high-dim. nested spheres (corresponding 1-spheres)"),
    nrow = 1
  ),
  ncol = 1
)

ggsave("paper/figures/DataApp.pdf", plot_dataAll, width = 8, height = 12)


#### Figure 6: boxplots for each experiment #########
results <- readRDS("results/experiments/results_summary.rds")

dat_cor_2 <- results[, paste0(cols, "_raw")][, -1]
colnames(dat_cor_2) <- c("DCSI", "DSI", "Density", "ClsCoef", paste0("N", c(1:3)), "LSC",
                         paste0(c("CH", "DB", "Dunn", "Silhouette", "CVNN"), "*"))

dat_long <- dat_cor_2
dat_long <- cbind(results$max_ARI_raw, dat_long)
colnames(dat_long)[1] <- "ARI"
dat_long$experiment <- factor(results$experiment, labels = c("E1 - hom. 2D-Gaussians", "E2 - het. 2D-Gaussians", "E3 - 2D-Gaussians w/ bridge", 
                                                             "E4 - 2D-Gaussians + irrelev. uniforms", "E5 - high-dim. Gaussians w/ irrelev. dimensions", 
                                                             "E6 - 2D moons", "E7 - 2D nested circles", "E8 - 2D nested spirals", 
                                                             "E9 - high-dim. nested spheres"))

dat_long <- dat_long %>%
  tidyr::gather("Measure", "Value", 1:(ncol(dat_long) - 1))
dat_long$Measure <- factor(dat_long$Measure, levels = unique(dat_long$Measure))


plot_boxpl <- ggplot(data = dat_long) +
  geom_boxplot(aes(y = Value, x = Measure), outlier.size = 0.05, lwd = 0.2) +
  facet_wrap(~experiment, ncol = 1) +
  theme_bw() +
  theme(axis.text = element_text(size = 6), 
        strip.text = element_text(size = 7))

ggsave("paper/figures/SummPlots0.pdf", plot_boxpl, width = 8, height = 12)


#### Figure 7: correlation for each experiment #########
results <- readRDS("results/experiments/results_summary.rds")

dat_cor <- data.frame()
for(i in unique(results$experiment)){
  dat <- results %>% filter(experiment == i)
  dat_cor_temp <- rbind(cor(dat$max_ARI_raw, dat[, paste0(cols[-1], "_raw")], method = "spearman") %>% round(., 2),
                        cor(dat$max_ARI_umap, dat[, paste0(cols[-1], "_umap")], method = "spearman") %>% round(., 2))
  
  colnames(dat_cor_temp) <- c("DCSI", "DSI", "Density", "ClsCoef", paste0("N", c(1:3)), "LSC",
                              paste0(c("CH", "DB", "Dunn", "Silhouette", "CVNN"), "*"))
  dat_cor_temp <- as.data.frame(dat_cor_temp)
  dat_cor_temp$experiment <- i
  dat_cor_temp$ARI <- c("ARI raw data", "ARI umap")
  
  dat_cor <- rbind(dat_cor, dat_cor_temp)
  
}
dat_cor$ARI <- factor(dat_cor$ARI, levels = c("ARI umap", "ARI raw data"))

data <- dat_cor
data$experiment <- rep(c("E1 - hom. 2D-Gaussians", "E2 - het. 2D-Gaussians", "E3 - 2D-Gaussians w/ bridge", 
                         "E4 - 2D-Gaussians + irrelev. uniforms", "E5 - high-dim. Gaussians w/ irrelev. dimensions", 
                         "E6 - 2D moons", "E7 - 2D nested circles", "E8 - 2D nested spirals", 
                         "E9 - high-dim. nested spheres"), each = 2)
plot_cor <- reshape2::melt(data, na.rm = TRUE) #data frame to plot
size <- 5
cor_plot <- ggplot(plot_cor, aes(variable, ARI, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkorange3", high = "violetred", mid = "white", 
                       midpoint = 0, limit = c(-1,1), 
                       name = "Correlation (Spearman)",
                       guide = guide_colourbar(ticks.colour = "black")) +
  geom_text(aes(variable, ARI, label = value), color = "black",
            size = size 
            # fontface = "bold"
  ) +
  xlab("") + ylab("") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~experiment, ncol = 1)

ggsave("paper/figures/cor_synth_all.pdf", cor_plot, width = 8, height = 12)




#### Figure 5: correlation for all synthetic experiments #####
results <- readRDS("results/experiments/results_summary.rds")

dat_cor_1 <- rbind(cor(results$max_ARI_umap, results[, paste0(cols, "_umap")], method = "spearman") %>% round(., 2),
                   cor(results$max_ARI_raw, results[, paste0(cols, "_raw")], method = "spearman") %>% round(., 2))
rownames(dat_cor_1) <- c("ARI umap", "ARI raw data")
colnames(dat_cor_1) <- c("ARI", 
                         "DCSI", "DSI", "Density", "ClsCoef", paste0("N", c(1:3)), "LSC",
                         paste0(c("CH", "DB", "Dunn", "Silhouette", "CVNN"), "*"))


cor_plot <- heatmap_cor2(dat_cor_1[, -1], size = 2.5) +
  theme(plot.title = element_blank(), 
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 0))+
  theme(plot.margin=grid::unit(c(1,1,0,0), "mm"))

ggsave("paper/figures/cor_synth.pdf", cor_plot, width = 5.3, height = 0.9)


#### Figure 13: exemplary embeddings ####

x <- readRDS("results/experiments_rawData/E1_572.rds")

p1 <- plot_2_3d_data(x$data, legend = FALSE, size = 0.6, alpha = 0.6) + labs(title = "raw data")
p2 <- plot_2_3d_data(x$dat_umap, legend = FALSE, size = 0.6, alpha = 0.6) + labs(title = "UMAP emb.")
pg1 <- plot_grid(p1, p2, nrow = 1)

y <- readRDS("results/experiments_rawData/E3_819.rds")

p3 <- plot_2_3d_data(y$data, legend = FALSE, size = 0.6, alpha = 0.6) + labs(title = "raw data")
p4 <- plot_2_3d_data(y$dat_umap, legend = FALSE, size = 0.6, alpha = 0.6) + labs(title = "UMAP emb.")
pg2 <- plot_grid(p3, p4, nrow = 1)

plot_grid(pg1, pg2, labels = "AUTO", nrow = 1)
ggsave("paper/figures/emb_synth.pdf", plot_grid(pg1, pg2, labels = "AUTO", nrow = 1), width = 9, height = 2.3)

#### Figure 14: E9, example ICD and BCD ####
set.seed(1234)
dat_dim2_1 <- TDA::sphereUnif(500, d = 2, r = 4) # inner sphere
dat_dim2_2 <- TDA::sphereUnif(500, d = 2, r = 10) # outer sphere

dat_dim1000_1 <- TDA::sphereUnif(500, d = 1000, r = 4)
dat_dim1000_2 <- TDA::sphereUnif(500, d = 1000, r = 10)

ICD_2_1 <- c(proxy::dist(dat_dim2_1))
ICD_2_2 <- c(proxy::dist(dat_dim2_2))

ICD_1000_1 <- c(proxy::dist(dat_dim1000_1))
ICD_1000_2 <- c(proxy::dist(dat_dim1000_2))

BCD_2 <- c(proxy::dist(dat_dim2_1, dat_dim2_2))
BCD_1000 <- c(proxy::dist(dat_dim1000_1, dat_dim1000_2))

dim2 <- data.frame(distance = c(ICD_2_1, ICD_2_2, BCD_2),
                   set = c(
                     rep("ICD 1", length(ICD_2_1)),
                     rep("ICD 2", length(ICD_2_1)),
                     rep("BCD", length(BCD_2)))
)

dim1000 <- data.frame(distance = c(ICD_1000_1, ICD_1000_2, BCD_1000),
                      set = c(
                        rep("ICD 1", length(ICD_1000_1)),
                        rep("ICD 2", length(ICD_1000_1)),
                        rep("BCD", length(BCD_1000)))
)

dat_all <- rbind(dim2, dim1000)
dat_all$Dimension <- factor(c(rep("Dimension Sphere = 2", nrow(dim2)), rep("Dimension Sphere = 1000", nrow(dim2))),
                            levels = c("Dimension Sphere = 2", "Dimension Sphere = 1000"))

plot_E9 <- ggplot(dat_all) +
  geom_density(aes(distance, color = set, fill = set), alpha = 0.1) +
  theme_bw() +
  facet_wrap(~Dimension, nrow = 1, scales = "free_y") +
  scale_color_colorblind() +
  scale_fill_colorblind() + 
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 7))

ggsave("paper/figures/plot_E9.pdf", plot_E9, width = 7, height = 1.5)

#### table 5: E4, many irrelevant features ####
library(knitr)
library(dplyr)
library(kableExtra)

E4 <- readRDS("results/experiments/results_E4.rds")
E4$dist <- factor(E4$dist)
E4$n_irrev_features <- factor(E4$n_irrev_features)

dat_DCSI <- E4[c(307), c("dist", "n_irrev_features",
                                     "Sep_DCSI_raw", "Conn_DCSI_raw", "DCSI_raw", 
                                     "Sep_DCSI_umap", "Conn_DCSI_umap", "DCSI_umap")] %>% mutate_if(is.numeric, round, 2)

colnames(dat_DCSI) <- c("dist", "n_irrev", "Sep r", "Conn r", "DCSI r", 
                        "Sep u", "Conn u", "DCSI u")



knitr::kable(
  dat_DCSI, booktabs = TRUE, format = "latex", row.names = FALSE,
  caption = 'Experiment 4: Separation, Connectedness and DCSI on raw data (r) and the embedding (u). ARI is 0 both on the raw data and the embedding.'
)


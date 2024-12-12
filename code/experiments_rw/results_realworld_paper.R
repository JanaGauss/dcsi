source("code/functions/plot_functions.R")
source("code/functions/separability_functions.R")
source("code/functions/experiment_realWorld.R")
library(cowplot)
library(dplyr)
library(gridExtra)


### ToDo: DCSI nochmal berechnen, Grafiken updaten und Tabelle
# DCSI einfach nur mit MinPts = 50 berechnen?

colors <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "purple")
measures <- c("DCSI", "DSI", "N2", "calinski_harabasz*")


#### MNIST, Figures 10, 8, Table 4 ####
dat <- "mnist"

raw <- readRDS(paste0("results/experiments_rw/", dat, "_raw.rds"))
umap <- readRDS(paste0("results/experiments_rw/", dat, "_umap.rds"))

mnist_te <- read.csv("data/data_raw/mnist_test.csv")
mnist_tr <- read.csv("data/data_raw/mnist_train.csv")
mnist <- rbind(mnist_tr, mnist_te)
# draw subsample of 10 000 for computational reasons
set.seed(41022)
ind_mnist <- sample(1:nrow(mnist), 10000, replace = FALSE)
mnist <- mnist[ind_mnist,]

mnist_lbls <- mnist$label
mnist <- matrix(c(scale(c(as.matrix(mnist[, -1])))), nrow = length(mnist_lbls)) # scale whole dat set (not column-wise!)
dist_mnist_raw <- proxy::dist(as.data.frame(mnist))

rownames(raw$sep$sep_all) <- raw$sep$sep_all$measure
rownames(umap$sep$sep_all) <- umap$sep$sep_all$measure
tab_M <- as.data.frame(rbind(c(raw$max_ARI, raw$sep$sep_all[measures, 2]),
                             c(umap$max_ARI, umap$sep$sep_all[measures, 2])) %>% round(., 2))

# correct DCSI (old version was used, see Gauss(2022)), use MinPts = 50
dcsi_mnist_raw <- calc_DCSI_RW(dist_mnist_raw, raw$labels, minPts = 50)
dcsi_mnist_umap <- calc_DCSI_RW(umap$dist, umap$labels, minPts = 50)
raw$sep$sep_pair$DCSI <- dcsi_mnist_raw$pair_matrix
umap$sep$sep_pair$DCSI <- dcsi_mnist_umap$pair_matrix

tab_M[, 2] <- c(mean(dcsi_mnist_raw$pair_matrix, na.rm = TRUE),
                mean(dcsi_mnist_umap$pair_matrix, na.rm = TRUE)) %>% round(., 2)

dat_umap <- umap$dat_vis
dat_umap$component <- factor(umap$labels - 1)

plot_emb <- plot_2_3d_data(dat_umap, size = 0.1) +
  scale_color_manual(values = colors) +
  guides(colour = guide_legend("class", ncol = 2, override.aes = list(size=2)))

ggsave("paper/figures/mnist_vis.pdf", plot_emb, width = 4.5, height = 2) # Figure 12


dat_heatmap_raw <- data.frame()
dat_heatmap_umap <- data.frame()

for(i in measures){
  dat_measure_raw <- reshape2::melt(raw$sep$sep_pair[i]) %>% 
    filter(Var1 > Var2)
  
  dat_heatmap_raw <- rbind(dat_heatmap_raw, dat_measure_raw)
  
  dat_measure_umap <- reshape2::melt(umap$sep$sep_pair[i]) %>% 
    filter(Var1 > Var2)
  
  dat_heatmap_umap <- rbind(dat_heatmap_umap, dat_measure_umap)
}

dat_heatmap <- rbind(dat_heatmap_raw,
                     dat_heatmap_umap)
dat_heatmap$emb <- factor(rep(c("raw", "umap"), each = nrow(dat_heatmap_raw)),
                          levels = c("raw", "umap"))
dat_heatmap$L1 <- factor(dat_heatmap$L1, levels = measures)

if(dat == "mnist" | dat == "fmnist10"){
  dat_heatmap$Var1 <- dat_heatmap$Var1 - 1
  dat_heatmap$Var2 <- dat_heatmap$Var2 - 1
}

dat_heatmap$Var1 <- factor(dat_heatmap$Var1)
dat_heatmap$Var2 <- factor(dat_heatmap$Var2)

dat_heatmap$L1 <- recode_factor(dat_heatmap$L1, "calinski_harabasz*" = "CH*")
dat_heatmap$L1 <- factor(dat_heatmap$L1, levels = c("DCSI", "DSI", "N2", "CH*"))

heatmap_M <- ggplot(data = dat_heatmap) +
  geom_tile(aes(x = Var1, y = Var2, fill = value), color = "black") +
  facet_wrap(emb~L1, scales = "free", nrow = 2) +
  scale_fill_gradient2(limit = c(0,1), low = "white", high = "purple4",
                       guide = guide_legend(override.aes = list(fill = "white", color = "white"))) +
  scale_y_discrete(position = "right") +
  theme_bw() +
  labs(x = "class", y = "class", title = "MNIST") +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        strip.text = element_text(size = 7),
        plot.title = element_text(size = 9, hjust = 0.5)) 


#### FMNIST-10, Figures 9, 8, Table 4 ####
dat <- "fmnist"

raw <- readRDS(paste0("results/experiments_rw/", dat, "_raw.rds"))
umap <- readRDS(paste0("results/experiments_rw/", dat, "_umap.rds"))


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

dist_fmnist_raw <- proxy::dist(as.data.frame(fmnist))

fmnist5 <- FALSE
if(dat == "fmnist"){
  if(fmnist5){
    raw$sep <- raw$sep_5
    raw$max_ARI <- raw$max_ARI_5
    raw$res_dbscan_long <- raw$res_dbscan_long_5
    
    umap$sep <- umap$sep_5
    umap$max_ARI <- umap$max_ARI_5
    umap$res_dbscan_long <- umap$res_dbscan_long_5
    
    dat <- "fmnist5"
    
  } else{
    raw$sep <- raw$sep_10
    raw$max_ARI <- raw$max_ARI_10
    raw$res_dbscan_long <- raw$res_dbscan_long_10
    
    umap$sep <- umap$sep_10
    umap$max_ARI <- umap$max_ARI_10
    umap$res_dbscan_long <- umap$res_dbscan_long_10
    
    dat <- "fmnist10"
  }
}

rownames(raw$sep$sep_all) <- raw$sep$sep_all$measure
rownames(umap$sep$sep_all) <- umap$sep$sep_all$measure
tab_10 <- as.data.frame(rbind(c(raw$max_ARI, raw$sep$sep_all[measures, 2]),
                             c(umap$max_ARI, umap$sep$sep_all[measures, 2])) %>% round(., 2))
# correct DCSI (old version was used, see Gauss(2022))
dcsi_fmnist10_raw <- calc_DCSI_RW(dist_fmnist_raw, raw$labels_10, minPts = 50)
dcsi_fmnist10_umap <- calc_DCSI_RW(umap$dist, umap$labels_10, minPts = 50)
raw$sep$sep_pair$DCSI <- dcsi_fmnist10_raw$pair_matrix
umap$sep$sep_pair$DCSI <- dcsi_fmnist10_umap$pair_matrix

tab_10[, 2] <- c(mean(dcsi_fmnist10_raw$pair_matrix, na.rm = TRUE),
                mean(dcsi_fmnist10_umap$pair_matrix, na.rm = TRUE)) %>% round(., 2)

dat_umap <- umap$dat_vis
dat_umap$component <- factor(umap$labels_10 - 1)

plot_emb_10 <- plot_2_3d_data(dat_umap, size = 0.1) +
  scale_color_manual(values = colors) +
  guides(colour = guide_legend("class", override.aes = list(size=2)))

dat_heatmap_raw <- data.frame()
dat_heatmap_umap <- data.frame()

for(i in measures){
  dat_measure_raw <- reshape2::melt(raw$sep$sep_pair[i]) %>% 
    filter(Var1 > Var2)
  
  dat_heatmap_raw <- rbind(dat_heatmap_raw, dat_measure_raw)
  
  dat_measure_umap <- reshape2::melt(umap$sep$sep_pair[i]) %>% 
    filter(Var1 > Var2)
  
  dat_heatmap_umap <- rbind(dat_heatmap_umap, dat_measure_umap)
}

dat_heatmap <- rbind(dat_heatmap_raw,
                     dat_heatmap_umap)
dat_heatmap$emb <- factor(rep(c("raw", "umap"), each = nrow(dat_heatmap_raw)),
                          levels = c("raw", "umap"))
dat_heatmap$L1 <- factor(dat_heatmap$L1, levels = measures)


if(dat == "mnist" | dat == "fmnist10"){
  dat_heatmap$Var1 <- dat_heatmap$Var1 - 1
  dat_heatmap$Var2 <- dat_heatmap$Var2 - 1
}

dat_heatmap$Var1 <- factor(dat_heatmap$Var1)
dat_heatmap$Var2 <- factor(dat_heatmap$Var2)

dat_heatmap$L1 <- recode_factor(dat_heatmap$L1, "calinski_harabasz*" = "CH*")
dat_heatmap$L1 <- factor(dat_heatmap$L1, levels = c("DCSI", "DSI", "N2", "CH*"))

heatmap_10 <- ggplot(data = dat_heatmap) +
  geom_tile(aes(x = Var1, y = Var2, fill = value), color = "black") +
  facet_wrap(emb~L1, scales = "free", nrow = 2) +
  scale_fill_gradient2(limit = c(0,1), low = "white", high = "purple4") +
  scale_y_discrete(position = "right") +
  theme_bw() +
  labs(x = "class", y = "class", title = "FMNIST-10") +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 7),
        plot.title = element_text(size = 9, hjust = 0.5))


#### FMNIST-5, Figures 9, 8, Table 4 ####
dat <- "fmnist"

# raw <- readRDS(paste0("results/experiments_rw/", dat, "_raw.rds"))
# umap <- readRDS(paste0("results/experiments_rw/", dat, "_umap.rds"))

fmnist5 <- TRUE
if(dat == "fmnist"){
  if(fmnist5){
    raw$sep <- raw$sep_5
    raw$max_ARI <- raw$max_ARI_5
    raw$res_dbscan_long <- raw$res_dbscan_long_5
    
    umap$sep <- umap$sep_5
    umap$max_ARI <- umap$max_ARI_5
    umap$res_dbscan_long <- umap$res_dbscan_long_5
    
    dat <- "fmnist5"
    
  } else{
    raw$sep <- raw$sep_10
    raw$max_ARI <- raw$max_ARI_10
    raw$res_dbscan_long <- raw$res_dbscan_long_10
    
    umap$sep <- umap$sep_10
    umap$max_ARI <- umap$max_ARI_10
    umap$res_dbscan_long <- umap$res_dbscan_long_10
    
    dat <- "fmnist10"
  }
}

rownames(raw$sep$sep_all) <- raw$sep$sep_all$measure
rownames(umap$sep$sep_all) <- umap$sep$sep_all$measure
tab_5 <- as.data.frame(rbind(c(raw$max_ARI, raw$sep$sep_all[measures, 2]),
                             c(umap$max_ARI, umap$sep$sep_all[measures, 2])) %>% round(., 2))
# correct DCSI (old version was used, see Gauss(2022))
dcsi_fmnist5_raw <- calc_DCSI_RW(dist_fmnist_raw, raw$labels_5, minPts = 50)
dcsi_fmnist5_umap <- calc_DCSI_RW(umap$dist, umap$labels_5, minPts = 50)
raw$sep$sep_pair$DCSI <- dcsi_fmnist5_raw$pair_matrix
umap$sep$sep_pair$DCSI <- dcsi_fmnist5_umap$pair_matrix

tab_5[, 2] <- c(mean(dcsi_fmnist5_raw$pair_matrix, na.rm = TRUE),
                 mean(dcsi_fmnist5_umap$pair_matrix, na.rm = TRUE)) %>% round(., 2)

table3 <- rbind(tab_M, tab_5, tab_10)
colnames(table3) <- c("max ARI", measures)
table3$Data <- rep(c("MNIST", "FMNIST-5", "FMNIST-10"), each = 2)
table3$Embedding <- rep(c("Raw", "UMAP"), 3)
table3[, c(6, 7, 1:5)]
# Table 4


dat_umap <- umap$dat_vis
dat_umap$component <- factor(umap$labels_5)

plot_emb_5 <- plot_2_3d_data(dat_umap, size = 0.1) +
  scale_color_manual(values = colors) +
  guides(colour = guide_legend("class", override.aes = list(size=2)))


dat_heatmap_raw <- data.frame()
dat_heatmap_umap <- data.frame()

for(i in measures){
  dat_measure_raw <- reshape2::melt(raw$sep$sep_pair[i]) %>% 
    filter(Var1 > Var2)
  
  dat_heatmap_raw <- rbind(dat_heatmap_raw, dat_measure_raw)
  
  dat_measure_umap <- reshape2::melt(umap$sep$sep_pair[i]) %>% 
    filter(Var1 > Var2)
  
  dat_heatmap_umap <- rbind(dat_heatmap_umap, dat_measure_umap)
}

dat_heatmap <- rbind(dat_heatmap_raw,
                     dat_heatmap_umap)
dat_heatmap$emb <- factor(rep(c("raw", "umap"), each = nrow(dat_heatmap_raw)),
                          levels = c("raw", "umap"))
dat_heatmap$L1 <- factor(dat_heatmap$L1, levels = measures)

if(dat == "mnist" | dat == "fmnist10"){
  dat_heatmap$Var1 <- dat_heatmap$Var1 - 1
  dat_heatmap$Var2 <- dat_heatmap$Var2 - 1
}

dat_heatmap$Var1 <- factor(dat_heatmap$Var1)
dat_heatmap$Var2 <- factor(dat_heatmap$Var2)

dat_heatmap$L1 <- recode_factor(dat_heatmap$L1, "calinski_harabasz*" = "CH*")
dat_heatmap$L1 <- factor(dat_heatmap$L1, levels = c("DCSI", "DSI", "N2", "CH*"))

heatmap_5 <- ggplot(data = dat_heatmap) +
  geom_tile(aes(x = Var1, y = Var2, fill = value), color = "black") +
  facet_wrap(emb~L1, scales = "free", nrow = 2) +
  scale_fill_gradient2(limit = c(0,1), low = "white", high = "purple4",
                       guide = guide_legend(override.aes = list(fill = "white", color = "white"))) +
  scale_y_discrete(position = "right") +
  theme_bw() +
  labs(x = "class", y = "class", title = "FMNIST-5") +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"),
        strip.text = element_text(size = 7),
        plot.title = element_text(size = 9, hjust = 0.5)) 


plot_fmnist_vis <- plot_grid(plot_emb_10 + theme(plot.margin = unit(c(0.2, 1, 0.2, 0.2), "cm")), 
                             plot_emb_5 + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 1), "cm")))
plot_fmnist_vis
ggsave("paper/figures/fmnist_vis.pdf", plot_fmnist_vis, height = 3, width = 8)

plot_grid(heatmap_M, heatmap_10, heatmap_5, nrow = 3)
ggsave("paper/figures/heatmap_pair.pdf", 
       plot_grid(heatmap_M, heatmap_10, heatmap_5, nrow = 3), 
       width = 7, height = 9)




##### FMNIST-5: robustness, Figure 11 #####
load("results/experiments_rw/robust_fmnist.RData")

results_umap_5[[1]]$pair_matrix %>% round(., 2) # minPts = 5
results_umap_5[[4]]$pair_matrix %>% round(., 2) # minPts = 20
results_umap_5[[5]]$pair_matrix %>% round(., 2) # minPts = 50


dat_heatmap_robust <- data.frame()

for(i in c(1, 4, 5)){
  dat <- reshape2::melt(results_umap_5[[i]]$pair_matrix ) %>% 
    filter(Var1 > Var2)
  
  dat_heatmap_robust <- rbind(dat_heatmap_robust, dat)
}

dat_heatmap_robust$MinPts <- factor(rep(c("MinPts = 5", "MinPts = 20", "MinPts = 50"), each = nrow(dat)), 
                                    levels = c("MinPts = 5", "MinPts = 20", "MinPts = 50"))


dat_heatmap_robust$Var1 <- factor(dat_heatmap_robust$Var1)
dat_heatmap_robust$Var2 <- factor(dat_heatmap_robust$Var2)


heatmap_robust <- ggplot(data = dat_heatmap_robust) +
  geom_tile(aes(x = Var1, y = Var2, fill = value), color = "black") +
  facet_wrap(~MinPts, scales = "free", nrow = 1) +
  scale_fill_gradient2(limit = c(0,1), low = "white", high = "purple4") +
  scale_y_discrete(position = "right") +
  theme_bw() +
  labs(x = "class", y = "class") +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        strip.text = element_text(size = 7)) 
heatmap_robust

ggsave("paper/figures/heatmap_robust.pdf", heatmap_robust, width = 4, height = 1.5)


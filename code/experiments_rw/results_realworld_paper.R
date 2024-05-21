source("code/functions/plot_functions.R")
library(cowplot)
library(dplyr)
library(gridExtra)


colors <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "purple")
measures <- c("DCSI", "DSI", "N2", "calinski_harabasz*")


#### MNIST, Figures 10, 8, Table 4 ####
dat <- "mnist"

raw <- readRDS(paste0("results/experiments_rw/", dat, "_raw.rds"))
umap <- readRDS(paste0("results/experiments_rw/", dat, "_umap.rds"))

rownames(raw$sep$sep_all) <- raw$sep$sep_all$measure
rownames(umap$sep$sep_all) <- umap$sep$sep_all$measure
tab_M <- as.data.frame(rbind(c(raw$max_ARI, raw$sep$sep_all[measures, 2]),
                             c(umap$max_ARI, umap$sep$sep_all[measures, 2])) %>% round(., 2))
# correct DCSI (old version was used, see Gauss(2022))
tab_M[, 2] <- c(mean(raw$sep$sep_pair$DCSI, na.rm = TRUE),
                mean(umap$sep$sep_pair$DCSI, na.rm = TRUE)) %>% round(., 2)

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
tab_10[, 2] <- c(mean(raw$sep$sep_pair$DCSI, na.rm = TRUE),
                mean(umap$sep$sep_pair$DCSI, na.rm = TRUE)) %>% round(., 2)

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

raw <- readRDS(paste0("results/experiments_rw/", dat, "_raw.rds"))
umap <- readRDS(paste0("results/experiments_rw/", dat, "_umap.rds"))

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
tab_5[, 2] <- c(mean(raw$sep$sep_pair$DCSI, na.rm = TRUE),
                mean(umap$sep$sep_pair$DCSI, na.rm = TRUE)) %>% round(., 2)

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
results_umap_5 <- readRDS("results/experiments_rw/fmnist_robust.rds")

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


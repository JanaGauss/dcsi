source("code/functions/plot_functions.R")
source("code/functions/generate_data.R")

# read results

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(cowplot)

readResults <- function(filenames){
  
  results <- data.frame()
  
  for(i in filenames){
    
    list <- readRDS(paste0("results/experiments_rawData_Var/", i))
    
    
    id <- str_extract(i, "_\\d+.rds")
    id <- str_replace(id, "_", "")
    id <- str_replace(id, ".rds", "")
    print(id)
    
    row_params <- params_df_Var[which(params_df_Var$ID == str_replace(i, ".rds", "")),]
    
    res <- c(as.numeric(id), 
             dist = row_params$dist,
             cov = row_params$cov_1,
             maxARI = max(filter(list$res_dbscan_raw_long, measure == "ARI")$performance),
             dcsi = list$dcsi_raw$DCSI,
             Sep = list$dcsi_raw$Sep_DCSI,
             Conn = list$dcsi_raw$Conn_DCSI)
    
    results <- rbind(results, res)
  }
  
  colnames(results) <- c("ID", "dist", "cov", "maxARI", "dcsi", "Sep", "Conn")
  
  results <- results %>%
    mutate_all(as.character) %>%
    mutate_all(as.numeric) %>%
    mutate_all(function(x) round(x, 4))
  
  return(results)
  
}

filenames <- list.files("results/experiments_rawData_Var")
params_df_Var <- readRDS("results/experiments/params_Var.rds")

result <- readResults(filenames)
result$sd <- sqrt(result$cov)

# plot results
ggplot(data = result) + geom_boxplot(aes(x = factor(sd),y = dcsi))
ggplot(data = result) + geom_boxplot(aes(x = factor(sd),y = maxARI))

# correlation dcsi and ARI
cor(result$maxARI, result$dcsi, method = "spearman")

result %>% group_by(cov) %>% summarise(cor_cov = cor(dcsi, maxARI, method = "spearman"))

result$DCSI <- result$dcsi
result_long <- result %>% gather("measure", "value", c("maxARI", "DCSI"))

ggplot(data = result_long) + geom_boxplot(aes(x = factor(sd), y = value, fill = measure))

ggplot(data = result_long, aes(x = measure, y = value, group = ID)) + 
  geom_line(alpha = 0.2) +
  geom_point(aes(col = factor(sd)), position = position_jitter(width = 0.05), size = 0.5) + theme_bw()

plot_res <- ggplot(data = result_long, aes(x = factor(sd), y = value)) + geom_boxplot() +
  facet_wrap(~measure) + theme_bw() + labs(x = "standard deviation")
plot_res
ggsave("paper/figures/var_boxplots.pdf", plot_res, width = 5, height = 2.5)


set.seed(96315)
sds <-seq(from = 0.5, to = 1.75, by = 0.25)
dat_all <- data.frame()
for(sd in sds){
  dat_all <- rbind(dat_all, generate_data(dist = 4.5, cov_1 = sd^2, cov_2 = sd^2))
  
}
dat_all$SD <- rep(sds, each = 1000)
dat_all$SD <- paste0("SD = ", dat_all$SD)
plot_dat_var <- plot_2_3d_data(dat_all, alpha = 0.3, size = 0.3) + facet_wrap(~SD) + guides(color = "none") 
plot_dat_var
ggsave("paper/figures/dat_var.pdf", plot_dat_var, width = 4, height = 2.5)

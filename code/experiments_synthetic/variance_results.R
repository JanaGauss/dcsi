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

result_long <- result %>% gather("measure", "value", 4:5)

ggplot(data = result_long) + geom_boxplot(aes(x = factor(sd), y = value, fill = measure))

ggplot(data = result_long, aes(x = measure, y = value, group = ID)) + 
  geom_line(alpha = 0.2) +
  geom_point(aes(col = factor(sd)), position = position_jitter(width = 0.05), size = 0.5) + theme_bw()
# improve color palette
   
# plot some data sets
set.seed(96315)
sds <-seq(from = 0.5, to = 2, by = 0.25)
plot_list <- list()
for(sd in sds){
  plot_list[[length(plot_list) + 1]] <- plot_2_3d_data(generate_data(dist = 4.5, cov_1 = sd^2, cov_2 = sd^2)) + 
    guides(color = "none") + labs(title = paste0("SD = ", sd))
}
plot_grid(plotlist = plot_list, nrow = 2)

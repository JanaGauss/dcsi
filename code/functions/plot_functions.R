# Functions to create plots
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(scales)
library(ggthemes)

#' Function to plot 2- or 3-dimensional data
#' 
#' @param data dataframe with column component and X1, X2, ...
#' @param three_d should the data be plotted in 3D if there are 3 (or more) dimensions?
#' @param xlim limits of x axis
plot_2_3d_data <- function(data, three_d = TRUE, xlim = NULL, size = 1, legend = TRUE, alpha = 1){
  
  if("X3" %in% colnames(data) & three_d == TRUE){ # plot 3 dimensions
    plot <- plotly::plot_ly(data = data, x=~X1, y=~X2, z=~X3, type="scatter3d", mode = "markers", color = ~component)
  } else{ # plot 2 dimensions
    plot <- ggplot(data = data) + geom_point(aes(x = X1, y = X2, col = component), size = size, alpha = alpha) + 
      theme_bw() + labs(col = "class") +
      scale_color_manual(values = c("#56B4E9", "#D55E00"), labels = c("1", "2"))
  }
  
  if(!is.null(xlim)){
    plot <- plot + xlim(xlim)
  }
  
  if(legend == FALSE){
    plot <- plot + theme(legend.position="none")
  }
  
  return(plot)
}

#' Function to create a heatmap of three variables
plot_heatmap <- function(data, x, y, z, factor_all = FALSE,
                         lim = c(0, 1),
                         title = "",
                         legend = FALSE,
                         legend_title = ""){
  
  if(!factor_all){
    plot <- ggplot() +
      geom_tile(data = data, aes(x = .data[[x]], y = .data[[y]], fill = .data[[z]]))
  } else{
    plot <- ggplot() +
      geom_tile(data = data, aes(x = factor(.data[[x]]), y = factor(.data[[y]]), fill = .data[[z]]))
  }
  
  plot <- plot +
    scale_fill_gradient2(limit = c(0,1), low = "white", high = "purple4")+
    labs(title = title) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "grey80"),
          plot.title = element_text(hjust = 0.5))
  if(legend){
    plot <- plot + labs(fill = legend_title)
  } else{
    plot <- plot + theme(legend.position = "none")
  }
  
  
  return(plot)
  
}

plot_heatmap_facet <- function(data, x, y,  
                               lim = c(0, 1)){
  
  plot <- ggplot() +
    geom_tile(data = data, aes(x = .data[[x]], y = .data[[y]], fill = Value))
  
  plot <- plot +
    scale_fill_gradient2(limit = c(0,1), low = "white", high = "purple4",
                         na.value = "gray85")+
    theme_bw() +
    theme(panel.background = element_rect(fill = "grey80")) +
    facet_grid(vars(emb), vars(Measure), switch = "y")
  
  
  return(plot)
  
}

plot_heatmap_facet_E9 <- function(data, x, y,  
                                  lim = c(0, 1)){
  
  plot <- ggplot() +
    geom_tile(data = data, aes(x = .data[[x]], y = .data[[y]], fill = Value))
  
  plot <- plot +
    scale_fill_gradient2(limit = c(0,1), low = "white", high = "purple4",
                         na.value = "gray85")+
    theme_bw() +
    theme(panel.background = element_rect(fill = "grey80")) +
    facet_grid(emb ~ Measure + r_2, switch = "y")
  
  
  return(plot)
  
}

# with different limit
plot_heatmap_2 <- function(data, x, y, z, factor_all = FALSE,
                           lim = c(0, 1),
                           title = "",
                           legend = FALSE,
                           legend_title = ""){
  
  if(!factor_all){
    plot <- ggplot() +
      geom_tile(data = data, aes(x = .data[[x]], y = .data[[y]], fill = .data[[z]]))
  } else{
    plot <- ggplot() +
      geom_tile(data = data, aes(x = factor(.data[[x]]), y = factor(.data[[y]]), fill = .data[[z]]))
  }
  
  plot <- plot +
    scale_fill_gradient2(limit = c(-1,1), mid = "white", high = "purple4", low = "darkorange3",
                         na.value = "gray85")+
    labs(title = title) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "grey80"),
          plot.title = element_text(hjust = 0.5))
  if(legend){
    plot <- plot + labs(fill = legend_title)
  } else{
    plot <- plot + theme(legend.position = "none")
  }
  
  
  return(plot)
  
}

plot_heatmap_facet_2 <- function(data, x, y){
  
  plot <- ggplot() +
    geom_tile(data = data, aes(x = .data[[x]], y = .data[[y]], fill = Value))
  
  plot <- plot +
    scale_fill_gradient2(limit = c(-1,1), mid = "white", high = "purple4", low = "darkorange3")+
    theme_bw() +
    theme(panel.background = element_rect(fill = "grey80")) +
    facet_grid(vars(emb), vars(Measure), switch = "y")
  
  
  return(plot)
  
}

plot_heatmap_facet_E9_2 <- function(data, x, y){
  
  plot <- ggplot() +
    geom_tile(data = data, aes(x = .data[[x]], y = .data[[y]], fill = Value))
  
  plot <- plot +
    scale_fill_gradient2(limit = c(-1,1), mid = "white", high = "purple4", low = "darkorange3")+
    theme_bw() +
    theme(panel.background = element_rect(fill = "grey80")) +
    facet_grid(emb ~ Measure + r_2, switch = "y")
  
  
  return(plot)
  
}


# Functions to plot a correlation matrix as heatmap
heatmap_cor <- function(data, title = "", spear = TRUE, size = 5){
  
  if(spear){
    cor <- round(cor(data, method = "spearman"), 3)
  } else{
    cor <- round(cor(data, method = "pearson"), 3)
  }
  cor[lower.tri(cor)] <- NA
  plot_cor <- reshape2::melt(cor, na.rm = TRUE) # create data frame for plot
  plot <- ggplot(plot_cor, aes(Var2, Var1, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "darkorange3", high = "violetred", mid = "white", 
                         midpoint = 0, limit = c(-1,1), 
                         guide = guide_colourbar(ticks.colour = "black")) +
    geom_text(aes(Var2, Var1, label = value), color = "black",
              size = size 
              # fontface = "bold"
    ) +
    xlab("") + ylab("") + labs(title = title) +
    theme_bw() +
    theme(legend.position = "none")
  return(plot)
}

heatmap_cor2 <- function(data, title = "", size = 5){
  
  plot_cor <- reshape2::melt(data, na.rm = TRUE) #data frame to plot
  plot <- ggplot(plot_cor, aes(Var2, Var1, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "darkorange3", high = "violetred", mid = "white", 
                         midpoint = 0, limit = c(-1,1), 
                         name = "Correlation (Spearman)",
                         guide = guide_colourbar(ticks.colour = "black")) +
    geom_text(aes(Var2, Var1, label = value), color = "black",
              size = size 
              # fontface = "bold"
    ) +
    xlab("") + ylab("") + labs(title = title) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5))
  return(plot)
}


# Function to plot grouped boxplots
group_boxplot <- function(data, title = ""){
  
  dat_long <- data %>%
    tidyr::gather("Measure", "Value", 1:ncol(data))
  dat_long$Measure <- factor(dat_long$Measure,
                             levels = colnames(data),
                             labels = colnames(data))
  plot <- ggplot(data = dat_long) +
    geom_boxplot(aes(y = Value, x = Measure)) +
    theme_bw() +
    labs(x = "", y = "", title = title) +
    scale_y_continuous(limits = c(0, 1)) +
    theme(axis.text.x = element_text(angle = 90))
  
  return(plot)
  
  
}

# Functions to plot grouped boxplots + facetting
# last variable in data must be called "facet"
group_boxplot_facet <- function(data, title = ""){
  
  dat_long <- data %>%
    tidyr::gather("Measure", "Value", 1:(ncol(data) - 1))
  dat_long$Measure <- factor(dat_long$Measure,
                             levels = colnames(data),
                             labels = colnames(data))
  plot <- ggplot(data = dat_long) +
    geom_boxplot(aes(y = Value, x = Measure)) +
    theme_bw() +
    labs(x = "", y = "", title = title) +
    scale_y_continuous(limits = c(0, 1)) +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~facet, ncol = 1)
  
  return(plot)
  
  
}

# facet_wrap(~Measure) instead of ~facet
group_boxplot_facet_2 <- function(data, title = "", size = 1, lwd = 1){
  
  dat_long <- data %>%
    tidyr::gather("Measure", "Value", 1:(ncol(data) - 1))
  dat_long$Measure <- factor(dat_long$Measure,
                             levels = colnames(data),
                             labels = colnames(data))
  plot <- ggplot(data = dat_long) +
    geom_boxplot(aes(y = Value, x = facet), outlier.size = size, lwd = lwd) +
    theme_bw() +
    labs(x = "", y = "", title = title) +
    scale_y_continuous(limits = c(0, 1)) +
    # theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~Measure, ncol = 6)
  
  return(plot)
  
  
}

# ylim -1, 1 instead of 0, 1 -> plot change
group_boxplot_facet_3 <- function(data, title = "", size = 0.5, lwd = 1){
  
  dat_long <- data %>%
    tidyr::gather("Measure", "Value", 1:(ncol(data) - 1))
  dat_long$Measure <- factor(dat_long$Measure,
                             levels = colnames(data),
                             labels = colnames(data))
  
  # lim <- max(abs(dat_long$Value))
  
  plot <- ggplot(data = dat_long) +
    geom_boxplot(aes(y = Value, x = facet), outlier.size = size, lwd = lwd) +
    theme_bw() +
    labs(x = "", y = "", title = title) +
    # scale_y_continuous(limits = c(-lim, lim)) +
    # theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~Measure, ncol = 6, scales = "free") +
    geom_hline(yintercept=0, linetype="dashed", color = "grey60", size = lwd)
  
  return(plot)
  
  
}

group_boxplot_facet_4 <- function(data, title = "", size = 0.5, lwd = 1){
  
  dat_long <- data %>%
    tidyr::gather("Measure", "Value", 1:(ncol(data) - 1))
  dat_long$Measure <- factor(dat_long$Measure,
                             levels = colnames(data),
                             labels = colnames(data))
  
  # lim <- max(abs(dat_long$Value))
  
  plot <- ggplot(data = dat_long) +
    geom_boxplot(aes(y = Value, x = facet), outlier.size = size, lwd = lwd) +
    theme_bw() +
    labs(x = "", y = "", title = title) +
    # scale_y_continuous(limits = c(-lim, lim)) +
    # theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(~Measure, ncol = 6, scales = "fixed") +
    geom_hline(yintercept=0, linetype="dashed", color = "grey60", size = lwd)
  
  return(plot)
  
  
}


# Function to plot line plot 
plot_scatter <- function(y, data, ylab = "", title = "", smooth = FALSE,
                         alpha = 0.5,
                         size = 0.5,
                         size_text = 7){
  
  data$y <- y
  
  dat_long <- data %>%
    tidyr::gather("Measure", "Value", 1:(ncol(data)-1))
  dat_long$Measure <- factor(dat_long$Measure,
                             levels = colnames(data),
                             labels = colnames(data))
  
  plot <- ggplot(data = dat_long, aes(x = Value, y = y, col = Measure)) +
    geom_point(alpha = alpha, size = size) +
    theme_bw() +
    labs(y = ylab, title = title) +
    guides(colour = guide_legend(override.aes = list(size=1, alpha = 1))) +
    ylim(c(0, 1)) + 
    xlim(c(0,1)) +
    theme(legend.title = element_text(size = size_text),
          legend.text = element_text(size = size_text - 1),
          axis.text = element_text(size = size_text - 1),
          axis.title = element_text(size = size_text))
  if(smooth){
    plot <- plot + geom_smooth(method = "loess", se = FALSE, show.legend = FALSE, size = 0.5)
  }
  
  return(plot)
  
  
}

plot_scatter_facet <- function(data, ylab = "", title = "", smooth = FALSE,
                               alpha = 0.5,
                               size = 0.5,
                               size_text = 7,
                               cols = NULL){
  
  plot <- ggplot(data = data, aes(x = Value, y = y, col = Measure)) +
    geom_point(alpha = alpha, size = size) +
    theme_bw() +
    labs(y = ylab, title = title) +
    guides(colour = guide_legend(override.aes = list(size=1, alpha = 1))) +
    ylim(c(0, 1)) + 
    xlim(c(0,1)) +
    theme(legend.title = element_text(size = size_text),
          legend.text = element_text(size = size_text - 1),
          axis.text = element_text(size = size_text - 1),
          axis.title = element_text(size = size_text)) +
    facet_grid(vars(row), vars(emb)) +
    theme(strip.background.y = element_blank(),
          strip.text.y = element_blank())
  if(smooth){
    plot <- plot + geom_smooth(method = "loess", se = FALSE, show.legend = FALSE, size = 0.5)
  }
  
  if(is.null(cols)){
    # plot <- plot + scale_color_brewer(palette = "Set1")
    plot <- plot + scale_colour_colorblind()
  } else{
    plot <- plot + scale_color_manual(values = cols)
  }
  
  
  return(plot)
  
  
}


plot_scatter_E8 <- function(data, title = "", 
                            alpha = 1,
                            size = 0.5,
                            size_text = 7,
                            ylim = c(0,1)){
  
  plot <- ggplot(data = data, aes(x = sd_1, y = Value)) +
    geom_point(alpha = alpha, size = size) +
    geom_line(size = size*0.5) +
    theme_bw() +
    labs(title = title) +
    ylim(ylim) + 
    theme(legend.title = element_text(size = size_text),
          legend.text = element_text(size = size_text - 1),
          axis.text = element_text(size = size_text - 1),
          axis.title = element_text(size = size_text)) +
    facet_grid(vars(emb), vars(Measure), switch = "y")
  
  if(ylim == c(-1, 1)){
    plot <- plot  +
      geom_hline(yintercept=0, linetype="dashed", color = "grey", size = size)
  }
  
  return(plot)
  
  
}


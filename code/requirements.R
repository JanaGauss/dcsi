# required R packages:

library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(purrr)
library(ggrepel)
library(factoextra)
library(tidyverse)
library(TDA)
library(tdaunif)
library(RColorBrewer)
library(scales)
library(ggthemes)
# library(MASS) # both the MASS package and dplyr have a select()-function which can be really annoying. MASS is always used here via MASS::...
library(gridExtra)
library(umap)
library(Rtsne)
library(dbscan)

install.packages("remotes")
remotes::install_github("jlmelville/snedata")
if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}

BiocManager::install("RDRToolbox")
library(snedata)
library(RDRToolbox)

library(plotly)
library(ECoL)
install.packages("https://cran.r-project.org/src/contrib/Archive/clusterCrit/clusterCrit_1.2.8.tar.gz", repos = NULL)
library(clusterCrit)
library(ape)
library(matrixcalc)
library(cccd)
library(igraph)
library(ggforce)
library(knitr)
library(kableExtra)
library(parallel)
library(tidyr)
library(proxy)
library(reshape2)
library(mclust)
library(aricode)
library(checkmate)



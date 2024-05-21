# DCSI - An improved measure of cluster separability based on separation and connectedness

Code and data to reproduce the results of the paper 

DCSI - An improved measure of cluster separability based on separation and connectedness (<https://arxiv.org/abs/2310.12806>)

Note that some large files need to be downloaded from <https://syncandshare.lrz.de/getlink/fiBYc7saoyy1fcaFMeevv/> and placed in the correct folders. Rerunning all experiments takes several days, even when multiple cores are used, so most results can also be found in the results folder.

## Results on synthetic data sets:

The `code/experiments_synthetic` folder contains one file for each of the 9 experiments (`E1_points_dist_var.R` etc.) to run the experiments. For each data set, an rds-file containing all results is saved in `results/experiments_rawData`. These files are not provided here (except for some examples). With the code in `E1_example.R`, you can run the experiment on one exemplary data set.

After running all experiments, data frames containing the most important results (one row for each data set) can be reproduced with `read_results.R`. These data frames can also be found in the `results/experiments` folder and are used for the evaluation of the experiments. The plots are created in `results_synth_paper.R`.

## Results on real-world data sets:

The `code/experiments_rw` folder contains the code to run the experiments on real-world data sets (`experiments_realworld.R` and `fmnist_robustness.R`). The result files can be found in `results/experiments_rw`. The plots and tables are created in `results_realworld_paper.R`. Note that the MNIST training data set as well as two result files need to be downloaded from the above mentioned link. 

## Abstract

Whether class labels in a given data set correspond to meaningful clusters is crucial for the evaluation of clustering algorithms using real-world data sets. This property can be quantified by separability measures. The central aspects of separability for density-based clustering are between-class separation and within-class connectedness, and neither classification-based complexity measures nor cluster validity indices (CVIs) adequately incorporate them. A newly developed measure (density cluster separability index, DCSI) aims to quantify these two characteristics and can also be used as a CVI. Extensive experiments on synthetic data indicate that DCSI correlates strongly with the performance of DBSCAN measured via the adjusted rand index (ARI) but lacks robustness when it comes to multi-class data sets with overlapping classes that are ill-suited for density-based hard clustering. Detailed evaluation on frequently used real-world data sets shows that DCSI can correctly identify touching or overlapping classes that do not correspond to meaningful density-based clusters.


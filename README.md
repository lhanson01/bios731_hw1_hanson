

Project directory for mini data analysis project HW1 for BIOS 731

The data used are from a single fMRI scan of an individual subject undergoing an LSD experience. Imaging data is from Robin Carhart-Harris et al and was obtained from OpenNeuro (https://openneuro.org/datasets/ds003059/versions/1.0.0). Images were already pre-processed before download.

Brain volumes 50 through 100 were extracted using script Shrink Image.R found in the data folder to reduce the size of the data used in the analysis so it could be uploaded to GitHub.

Analysis: Contains the Quarto Markdown document for running the entire analysis and its companion files. Also contains a bibliography file for generating report references.

Data: 

- T1-weighted anatomical image in the folder sub-001_T1w.nii for generating the brain mask and plotting brain slices
- Compressed image file "shortened_fMRI_data.nii" containing pre-processed BOLD signal time series for all voxels only including brain volumes 50 through 100 of the original data.
- "shrink_image.R" which can be used to completely reproduce the analysis from scratch.

Source: Contains scripts sourced in the qmd for running the whole analysis.

HW_1_data_download.R downloads the data from the data folder using RNifti. HW1_data_analysis.R completes some basic data processing like creating the brain mask, selecting a seed voxel and running the seed-based connectivity analysis. HW_1_data_visualization.R contains scripts for creating the visualizations. Functions for creating the brain slice visualizations are stored in visualization_functions.R

PACKAGES REQUIRED: 
RNifti
ggnewscale
tidyverse
here

Instructions for reproducing analysis from GitHub repository:


sessionInfo() call:
R version 4.5.1 (2025-06-13 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)

Matrix products: default
  LAPACK version 3.12.1

locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: America/New_York
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ggnewscale_0.5.2 here_1.0.2       lubridate_1.9.4  forcats_1.0.1    stringr_1.5.2   
 [6] dplyr_1.1.4      purrr_1.1.0      readr_2.1.5      tidyr_1.3.1      tibble_3.3.0    
[11] ggplot2_4.0.0    tidyverse_2.0.0  RNifti_1.9.0    

loaded via a namespace (and not attached):
 [1] gtable_0.3.6       crayon_1.5.3       compiler_4.5.1     tidyselect_1.2.1   Rcpp_1.1.1        
 [6] scales_1.4.0       yaml_2.3.10        fastmap_1.2.0      R6_2.6.1           labeling_0.4.3    
[11] generics_0.1.4     knitr_1.50         rprojroot_2.1.1    pillar_1.11.1      RColorBrewer_1.1-3
[16] tzdb_0.5.0         rlang_1.1.6        stringi_1.8.7      xfun_0.53          S7_0.2.0          
[21] viridisLite_0.4.2  timechange_0.3.0   cli_3.6.5          withr_3.0.2        magrittr_2.0.4    
[26] digest_0.6.37      grid_4.5.1         rstudioapi_0.17.1  hms_1.1.3          lifecycle_1.0.4   
[31] vctrs_0.6.5        evaluate_1.0.5     glue_1.8.0         farver_2.1.2       rmarkdown_2.29    
[36] tools_4.5.1        pkgconfig_2.0.3    htmltools_0.5.8.1 
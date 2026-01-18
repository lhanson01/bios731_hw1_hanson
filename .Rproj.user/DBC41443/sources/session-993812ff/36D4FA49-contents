install.packages("here")
install.packages("RNifti")
install.packages("tidyverse")
library(tidyverse)
library(here)
library(RNifti)

anatomical_file_path <- here("data", "sub-001_T1w.nii", "sub-001_T1w.nii")
anatomical_data <- readNifti(anatomical_file_path)

functional_file_path <- here("data", "shortened_fMRI_data.nii")
functional_image_data <- readNifti(functional_file_path)

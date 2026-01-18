library(RNifti)
library(tidyverse)
library(here)
anatomical_image_path <- here("data", "sub-001_T1w.nii", "sub-011_T1w.nii")
functional_image_path <- here("data", "sub-001_ses-LSD_task-rest_run-01_bold.nii",
                              "sub-001_ses-LSD_task-rest_run-01_bold.nii")

anatomical_data <- RNifti::readNifti(anatomical_image_path)
slice_plotter(anatomical_data, "S", 45)
functional_image_data <- RNifti::readNifti(functional_image_path, volumes = 50:100)

write_to_this_path <- here("data", "shortened_fMRI_data.nii.gz")
writeNifti(functional_image_data, write_to_this_path)


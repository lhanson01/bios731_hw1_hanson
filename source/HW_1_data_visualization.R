install.packages("ggplot2")
install.packages("ggnewscale")
library(ggplot2)
library(ggnewscale)

connectivity_plotter <- function(mask, corr_frame, anat_data, seed, plane){
  #SAGITTAL
  if (plane == "S"){
    seed_slice <- anat_data[seed$x,,]
    slice_longer <- expand.grid(Y = seq_len(nrow(seed_slice)),
                                Z = seq_len(ncol(seed_slice))) %>%
      mutate(intensity = as.vector(seed_slice))

    mask_new <- as.data.frame(mask) %>% mutate(which_voxel = V4) %>% select(-V4)
    corr_intensity_frame <- inner_join(mask_new, corr_frame) %>% rename(X = dim1,
                                                                        Y = dim2,
                                                                        Z = dim3,
                                                                        correlation = value)
    slice_corr_intensity <- corr_intensity_frame %>% filter(X == seed$x) %>%
      inner_join(.,slice_longer, by=join_by(Y,Z))


    plot <- ggplot() +
      geom_tile(data = slice_corr_intensity, aes(x = Y, y = Z, fill = intensity)) +
      scale_fill_gradient(low = "black", high = "white") +
      new_scale_fill() +
      geom_tile(data=slice_corr_intensity, aes(x=Y, y=Z, fill = correlation),
                alpha = 0.6) +
      scale_fill_viridis_c(option = "magma") +
      annotate("point", x = seed$y, y = seed$z,
               color = "red", size = 3)
    coord_equal()
  }
  #CORONAL
  if (plane == "C"){
    seed_slice <- anat_data[,seed$y,]
    slice_longer <- expand.grid(X = seq_len(nrow(seed_slice)),
                                Z = seq_len(ncol(seed_slice))) %>%
      mutate(intensity = as.vector(seed_slice))

    mask_new <- as.data.frame(mask) %>% mutate(which_voxel = V4) %>% select(-V4)
    corr_intensity_frame <- inner_join(mask_new, corr_frame) %>% rename(X = dim1,
                                                                        Y = dim2,
                                                                        Z = dim3,
                                                                        correlation = value)
    slice_corr_intensity <- corr_intensity_frame %>% filter(Y == seed$y) %>%
      inner_join(.,slice_longer, by=join_by(X,Z))


    plot <- ggplot() +
      geom_tile(data = slice_corr_intensity, aes(x = X, y = Z, fill = intensity)) +
      scale_fill_gradient(low = "black", high = "white") +
      new_scale_fill() +
      geom_tile(data=slice_corr_intensity, aes(x=X, y=Z, fill = correlation),
                alpha = 0.6) +
      scale_fill_viridis_c(option = "magma") +
      annotate("point", x = seed$x, y = seed$z,
               color = "red", size = 3) +
      coord_equal()
  }
  #AXIAL
  if (plane == "A"){
    seed_slice <- anat_data[,,seed$z]
    slice_longer <- expand.grid(X = seq_len(nrow(seed_slice)),
                                Y = seq_len(ncol(seed_slice))) %>%
      mutate(intensity = as.vector(seed_slice))

    mask_new <- as.data.frame(mask) %>% mutate(which_voxel = V4) %>% select(-V4)
    corr_intensity_frame <- inner_join(mask_new, corr_frame) %>% rename(X = dim1,
                                                                        Y = dim2,
                                                                        Z = dim3,
                                                                        correlation = value)
    slice_corr_intensity <- corr_intensity_frame %>% filter(Z == seed$z) %>%
      inner_join(.,slice_longer, by=join_by(X,Y))


    plot <- ggplot() +
      geom_tile(data = slice_corr_intensity, aes(x = X, y = Y, fill = intensity)) +
      scale_fill_gradient(low = "black", high = "white") +
      new_scale_fill() +
      geom_tile(data=slice_corr_intensity, aes(x=X, y=Y, fill = correlation),
                alpha = 0.6) +
      scale_fill_viridis_c(option = "magma") +
      annotate("point", x = seed$x, y = seed$y,
               color = "red", size = 3)
    coord_equal()
  }
  return(plot)
}
long_correlation_frame <- highest_correlation_voxels %>% names()


### Plot seed time series along with  ###
ggplot(data = highest_correlation_voxels, aes(x=) )


### Plot anatomical brain with correlation based heatmap ###

plotted <- connectivity_plotter(mask = brain_mask,
                     corr_frame = corr_frame,
                     anat_data = anatomical_data,
                     seed = select_seed_voxel,
                     plane = "A")
plotted

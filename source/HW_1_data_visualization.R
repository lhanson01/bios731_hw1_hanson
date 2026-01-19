install.packages("ggplot2")
install.packages("ggnewscale")
library(ggplot2)
library(ggnewscale)

connectivity_plotter <- function(mask, corr_frame, anat_data, seed, plane){
  #SAGITTAL
  if (plane == "S"){
    plane_name <- "Sagittal"
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
      scale_fill_gradient(low = "black", high = "white") + guides(fill = "none") +
      new_scale_fill() +
      geom_tile(data=slice_corr_intensity, aes(x=Y, y=Z, fill = correlation),
                alpha = 0.6) +
      scale_fill_viridis_c(option = "magma") +
      annotate("point", x = seed$y, y = seed$z,
               color = "red", size = 3) +
      coord_equal() + labs(title = paste0("Whole-brain seed connectivity",
                                         " (", plane_name, ")"))
  }
  #CORONAL
  if (plane == "C"){
    plane_name <- "Coronal"
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
      scale_fill_gradient(low = "black", high = "white") + guides(fill = "none") +
      new_scale_fill() +
      geom_tile(data=slice_corr_intensity, aes(x=X, y=Z, fill = correlation),
                alpha = 0.6) +
      scale_fill_viridis_c(option = "magma") +
      annotate("point", x = seed$x, y = seed$z,
               color = "red", size = 3) +
      coord_equal() + labs(title = paste0("Whole-brain seed connectivity",
                                       " (", plane_name, ")"))
  }
  #AXIAL
  if (plane == "A"){
    plane_name <- "Axial"
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
      scale_fill_gradient(low = "black", high = "white") + guides(fill = "none") +
      new_scale_fill() +
      geom_tile(data=slice_corr_intensity, aes(x=X, y=Y, fill = correlation),
                alpha = 0.6) +
      scale_fill_viridis_c(option = "magma") +
      annotate("point", x = seed$x, y = seed$y,
               color = "red", size = 3) +
      coord_equal() + labs(title = paste0("Whole-brain seed connectivity",
                                         " (", plane_name, ")"))
  }
  return(plot)
}

# need a long data frame with columns for voxel #, bold value at time t,
# and correlation with seed
long_correlation_frame <- highest_correlation_voxels %>%
  setNames(corr_frame$which_mask_voxel[1:k]) %>%
  pivot_longer(cols = everything(),
               names_to = "Voxel") %>%
  mutate(Voxel = as.numeric(Voxel)) %>%
  arrange(Voxel) %>%
  left_join(corr_frame,
            by = join_by(Voxel == which_mask_voxel)) %>%
  group_by(Voxel) %>%
  mutate(time = row_number()) %>%
  rename("BOLD Intensity" = value.x,
         "Correlation" = value.y) %>%
  select(-which_voxel) %>% mutate(source = "Voxel")



### Plot seed time series along with  ###

seed_frame <- as_tibble(seed_time_series) %>% mutate(time = seq_len(nt),
                                                     source = "Seed") %>%
              rename("BOLD Intensity" = value)


ggplot(
  long_correlation_frame,
  aes(
    x = time,
    y = `BOLD Intensity`,
    group = Voxel,
    alpha = Correlation,
    color = source
  )
) +
  geom_line() +
  geom_line(
    data = seed_frame,
    aes(x = time, y = `BOLD Intensity`, color = source),
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = c("Voxel" = "red", "Seed" = "black"),
    name = NULL
  ) +
  labs(title = paste(k-1, "most correlated voxels with seed voxel"),
       x = "Scanned brain volumes across time")

### Plot anatomical brain with correlation based heatmap ###

plotted <- connectivity_plotter(mask = brain_mask,
                                corr_frame = corr_frame,
                                anat_data = anatomical_data,
                                seed = select_seed_voxel,
                                plane = "S")
plotted

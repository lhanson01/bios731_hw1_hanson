slice_plotter <- function(img, plane, slice_ind){
  dim_img <- dim(img)
  if(plane == "S"){
    if (slice_ind > dim_img[1]){
      stop("Slice index must not exceed maximum slice index for sagittal plane")
    } else {
      single_slice <- img[slice_ind,,]
      single_slice_longer <- expand.grid(X = seq_len(nrow(single_slice)),
                                         Y = seq_len(ncol(single_slice))
      )
      plottable_slice <- single_slice_longer %>% mutate(value = as.vector(single_slice))

      slice_plot <- ggplot(plottable_slice, aes(X, Y, fill = value)) +
        geom_tile() + theme_void() + scale_fill_gradient(
          low = "black",
          high = "white"
        )
      slice_plot
    }
  }else if (plane == "C"){
    if (slice_ind > dim_img[2]){
      stop("Slice index must not exceed maximum slice index for coronal plane")
    } else {
      single_slice <- img[,slice_ind,]
      single_slice_longer <- expand.grid(X = seq_len(nrow(single_slice)),
                                         Y = seq_len(ncol(single_slice))
      )
      plottable_slice <- single_slice_longer %>% mutate(value = as.vector(single_slice))

      slice_plot <- ggplot(plottable_slice, aes(X, Y, fill = value)) +
        geom_tile() + theme_void() + scale_fill_gradient(
          low = "black",
          high = "white"
        )
      slice_plot
    }
  } else if (plane == "A"){
    if (slice_ind > dim_img[3]){
      stop("Slice index must not exceed maximum slice index for axial plane")
    } else {
      single_slice <- img[,,slice_ind]
      single_slice_longer <- expand.grid(X = seq_len(nrow(single_slice)),
                                         Y = seq_len(ncol(single_slice))
      )
      plottable_slice <- single_slice_longer %>% mutate(value = as.vector(single_slice))

      slice_plot <- ggplot(plottable_slice, aes(X, Y, fill = value)) +
        geom_tile() + theme_void() + scale_fill_gradient(
          low = "black",
          high = "white"
        )
      slice_plot
    }
  } else {
    stop("Plane must be S for Sagittal images, C for coronal images, or A for Axial images")
  }
}


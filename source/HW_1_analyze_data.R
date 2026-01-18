# create rudimentary brain mask from anatomical data to reduce computation in analysis

brain_mask <- cbind(which(anatomical_data > 0, arr.ind = TRUE),
                    which(anatomical_data > 0))

# select seed voxel o be correlated with the rest of the brain. We select a voxel of
#the precuneus, a known center of the DMN (https://www.jneurosci.org/content/34/3/932.short)

brain_mask_row <- sample(1:nrow(brain_mask),1) # >= 100000 for now
select_seed_voxel <- list(x = brain_mask[brain_mask_row,1],
                          y = brain_mask[brain_mask_row,2],
                          z = brain_mask[brain_mask_row,3])
seed_time_series <- functional_image_data[select_seed_voxel$x,
                                          select_seed_voxel$y,
                                          select_seed_voxel$z,]

nt <- length(seed_time_series)

# find pearson correlation with all other voxels in mask

X <- matrix(NA, nrow = nt, ncol = nrow(brain_mask)) # make matrix t x length of mask obtained from ana

for(i in 1:nt){
  X[i,] <- functional_image_data[,,,i][brain_mask[,4]]
  #grab a whole brain at time i then only take the mask voxels from it
}

rm(functional_image_data)
gc()

corr_vector <- apply(X, MARGIN = 2, function(x){
  corr <- cor(x, seed_time_series)
})

rm(X)
gc()




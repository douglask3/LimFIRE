mask = openAllObs()[["bare"]][[1]] == 100
mask[rowFromY(mask, seq(-90, 60, by = 0.1)),] = 0.0
mask[mask == 0] = NaN

writeRaster(mask, 'data/icemask.nc', overwrite = TRUE)

# plot_raster_from_raster(mask, add = TRUE, cols = c('#FFFFFFFF', 'grey'), limits = c(-0.5, 0.5), add_legend = FALSE)
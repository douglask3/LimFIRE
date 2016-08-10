aaConvert <- function(x, lims = fire_lims, cols = fire_cols, plot = TRUE, ...) {
    x = sum(x) * 12 * 100 / nlayers(x)
    if (plot) plot_raster(x, lims, cols, ...)
    return(x)
}
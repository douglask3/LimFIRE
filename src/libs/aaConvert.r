aaConvert <- function(x, lims = fire_lims, cols = fire_cols,
                      plot = TRUE, fname = NULL,...) {
    fun <- function() sum(x) * 12 * 100 / nlayers(x)
    
    if (!is.null(fname)) x = runIfNoFile(fname, fun)
        else x = fun()
        
    if (plot) plot_raster(x, lims, cols, ...)
    return(x)
}
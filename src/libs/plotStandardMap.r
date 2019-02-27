plotStandardMap <- function(x, txt, limits, cols, ...) {
    mask = raster('data/seamask.nc')
    x[mask != 2] = NaN
    
    FUN <- function(...) {
        plot_raster_from_raster(x, y_range = c(-60, 90), limits = limits, cols = cols,
                                transpose = FALSE, srt = 0, coast.lwd = NULL, 
                                plot_loc = c(0.35,0.83,0.01,0.04),
                                quick = TRUE, ...)
    }
    
    FUN(...)
	
    addCoastlineAndIce2map()
    
    c(x, mask) := cropIndonesia(x, mask)
    
    FUN(add = TRUE, ...)
    contour(mask, add = TRUE, drawlabels = FALSE, lwd = 0.5)
    
    
    mtext(txt,side = 1, line = -3.33)
}
plotStandardMap <- function(x, txt, limits, cols, e = NULL, plot_loc = c(0.35,0.83,0.01,0.04), ...) {
    mask = raster('data/seamask.nc')
    x[mask != 2] = NaN
    if(!is.null(e)) e[mask != 2] = NaN
    
    FUN <- function(...) {
        plot_raster_from_raster(x, y_range = c(-60, 90), limits = limits, cols = cols,
                                transpose = FALSE, srt = 0, coast.lwd = NULL, 
                                plot_loc = plot_loc,
                                quick = TRUE, e = e, ...)
    }
    
    FUN(...)
    addCoastlineAndIce2map()
    
    if (!is.null(e)) c(e, nn) := cropIndonesia(e, mask)
    c(x, mask) := cropIndonesia(x, mask)
    
    FUN(add = TRUE, ...)
    contour(mask, add = TRUE, drawlabels = FALSE, lwd = 0.5)
    
    
    mtext(txt,side = 1, line = -3.33)
}
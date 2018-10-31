plotStandardMap <- function(x, txt, limits, cols, ...) {
	
    plot_raster_from_raster(x, y_range = c(-60, 90), limits = limits, cols = cols,
                            transpose = FALSE, srt = 0,
                            plot_loc = c(0.35,0.83,0.01,0.04),
                            quick = TRUE, ...)
	add_icemask()
    mtext(txt,side = 1, line = -3.33)
}
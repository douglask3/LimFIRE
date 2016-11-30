standard_legend <- function(cols = fire_cols, lims = fire_lims, dat,
                            plot_loc = c(0.35,0.75,0.65,0.78), ...) {
    add_raster_legend2(cols, lims, add = FALSE,
               plot_loc = plot_loc, dat = dat,
               transpose = FALSE,
               srt = 0, ...)
} 
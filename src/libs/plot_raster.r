plot_raster <- function(x, lims = fire_lims, cols = fire_cols, add_legend = FALSE, y_range = c(-60, 90), ...) {
    
    mask = raster('data/seamask.nc')
    x[mask ==0] = NaN
    plot_raster_from_raster(x, add_legend = add_legend,
                            limits = lims, cols = cols,# coast.lwd = NULL,
                            #x_range = c(-160, 160),
                            y_range = y_range, #projection = "mollweide",
                            ...) #projection = "azequalarea",


    addCoastlineAndIce2map()
    
    
    
}

addCoastlineAndIce2map <- function() {
    add_icemask()
    mask = raster('data/seamask.nc')
    plot_raster_from_raster(mask, add = TRUE, col = c("white", "transparent"), limits = c(0.5), quick = TRUE, interior = FALSE, coast.lwd = NULL, add_legend = FALSE)
    #
    #contour(mask, add = TRUE, drawlabels = FALSE, lwd = 0.5)  

    'ployBox <- function(x, y)
        polygon(c(x[1], x[2], x[2], x[1]), c(y[1], y[1], y[2], y[2]), col = "white")#, border = "white"
        
    ployBox(c(-180, -90), c(-60, 0))
    ployBox(c(-180, -120), c(-60, 25))
    ployBox(c(-50, -19), c(10, 25))
    ployBox(c(-50, -13.5), c(27.5, 34))
    ployBox(c(115, 125), c(-8, -7))
    ployBox(c(104, 111), c(2.5, 8))
    ployBox(c(122, 128), c(2.5, 5))'
    
    
}
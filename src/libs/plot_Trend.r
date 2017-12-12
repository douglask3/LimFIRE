plot_Trend <- function(trend, title, cols, limits, 
					   prob_lims = c(0.001, 0.01, 0.1), limits_error = prob_lims, y_range = c(-60, 90), 
					   scaling = 12 * 10, remove_probLim = c(FALSE, FALSE), 
					   prob_cols = c('blue', 'green', 'yellow', 'red'), prob_cols_rev = FALSE,
					   add_legend = FALSE, unit = '') {
	if (nlayers(trend) > 1) {
		if (remove_probLim[1]) trend[[1]][trend[[2]] > tail(prob_lims,1)] = NaN
		trend[[1]] = trend[[1]] * scaling
		x = trend[[1]]
		p = trend[[2]]
		if (nlayers(trend > 2)) e = trend[[3]] else e = NULL
	} else {
		x = trend
		p = NULL
		e = NULL
	}
	if (prob_cols_rev) prob_cols = rev(prob_cols)
	plot_raster_from_raster(x, y_range = y_range,
							cols = cols, limits = limits,
							e = e, limits_error = limits_error, 
							ePatternRes = 30, ePatternThick = 0.2,
							quick = TRUE, add_legend = FALSE)
	
	mtext.units(title, side = 3, line = -2.25, adj = 0.05)
	
	if (add_legend) {
		add_raster_legend2(cols, limits, dat = x,
						   transpose = FALSE, 
						   plot_loc = c(0.1, 0.8, 0.0, 0.04),  ylabposScling=2.5)
		mtext.units(side = 1, unit, line = -3.5, cex = 0.8)
	}
		
	if (!is.null(p)) {
		#plot.new()
		#mar0 = par("mar")
		#par(mar = c(0, 0, 3, 3))
		plot_raster_from_raster(p, y_range = y_range,
							cols = prob_cols, limits = prob_lims,
							quick = TRUE, add_legend = FALSE, interior = FALSE)
		#par(mar = mar0)
	} else plot.new()
	if (remove_probLim[2]) trend[[1]][trend[[2]] > head(prob_lims,1)] = 0.0
	
	return(trend[[1]])
}
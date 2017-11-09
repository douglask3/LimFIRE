plot_Trend <- function(trend, title, cols, limits, 
					   prob_lims = c(0.001, 0.01, 0.05), y_range = c(-60, 90), scaling = 12 * 10, remove_probLim = c(FALSE, FALSE)) {
	if (remove_probLim[1]) trend[[1]][trend[[2]] > tail(prob_lims,1)] = NaN
	trend[[1]] = trend[[1]] * scaling
	plot_raster_from_raster(trend[[1]], y_range = y_range,
							cols = cols, limits = limits,
							e = trend[[2]], limits_error = prob_lims[1:2], 
							ePatternRes = 30, ePatternThick = 0.2,
							quick = TRUE, add_legend = FALSE)
	mtext.units(title, side = 3, line = -2.25, adj = 0.05)
	if (remove_probLim[2]) trend[[1]][trend[[2]] > head(prob_lims,1)] = 0.0
	
	return(trend[[1]])
}
source("cfg.r")

breaks = c(-4, -3, -2, -1, -1E-3, 1E-3, 1, 2, 3, 4)

limFnames = c('Burnt_Area', 'Fuel', 'Moisture', 'Igntions', 'Suppression')

figName = 'figs/trendHistergrams'
nsamples = 100

barCols = c("#CC8800"   , "#6eff6e", "#0000CC" , "#480000"  , "grey"       , "white")
legLabs = c('Burnt Area', 'Fuel'   , 'Moisture', 'Ignitions', 'Suppression', 'Overall Trend')

loadData4Ecosystem_analysis()

## global

ocean_mask = is.na(trend12FF[[1]][[1]]) | is.na(biomeAssigned)

plotTrendHist <- function(trend, bmask,...) {
	mask = trend[[3]] > 1 | trend[[2]] > prob_lims
	trend = trend[[1]]
	trend[mask] = 0
	#breaks = hist(trend[[1]], plot = FALSE)$breaks
	#breaks = sort(c(breaks[breaks != 0], -1E-2, 1E-2))
	#hist = hist(trend[[1]], breaks = breaks, plot = FALSE)
	trend[bmask] = NaN
	
	find_area <- function(mn, mx) {
		mask = trend < mn | trend > mx
		trend[mask] = NaN
		areas = raster::area(trend, na.rm = TRUE)												
		return(sum.raster(areas, na.rm = TRUE))
	}
	y = mapply(find_area,  head(breaks, -1), breaks[-1])
	y = 100 * y / sum(y)
	
	#y[y < 0.001] = 0.001
	#barplot(y, width = 0.8, space = 0.25, ylim = c(0.001, 100), log = 'y',...)
	barplot(y, width = 0.8, space = 0.25, ylim = c(0.00, 70), axes = FALSE,...)
	
}
par(mfcol = c(6, 8), mar =  c(1, 1, 0, 3), oma = c(3, 3, 0, 0))
plotBiomeHist <- function(biomeN) {
	if (biomeN > 1)
		bmask = (biomeAssigned != biomeN | ocean_mask)
	else
		bmask = ocean_mask

	mapply(plotTrendHist, c(trend12FF,trendIndex), col =  barCols, MoreArgs = list(bmask = bmask))

	at = 0:length(y) + 0.125
	axis(1, at = at, labels = rep('', length(at)))

	test =  abs(breaks) <= 0.001
	point0 = mean(at[test])
	at = sort(c(at[!test], point0))

	breaks = sort(c(breaks[!test], 0))

	axis(1, at = at, labels = breaks, tick = FALSE)
	mtext('{', 1, srt = 180, las = 2, cex = 3, line = 0.25)
}
lapply(1:8, plotBiomeHist)
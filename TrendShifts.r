source("cfg.r")

limFnames = c('Burnt_Area', 'Fuel', 'Moisture', 'Igntions', 'Suppression')

figName = 'figs/trendHistergrams'
nsamples = 100

barCols = c("#CC8800"   , "#6eff6e", "#0000CC" , "#480000"  , "grey"       , "white")
legLabs = c('Burnt Area', 'Fuel'   , 'Moisture', 'Ignitions', 'Suppression', 'Overall Trend')

loadData4Ecosystem_analysis()

## global

ocean_mask = is.na(trend12FF[[1]][[1]]) | is.na(biomeAssigned)

plotTrendHist <- function(trend, bmask, breaks,...) {
	mask = trend[[3]] > 1 | trend[[2]] > prob_lims
	trend = trend[[1]]
	trend[mask] = 0
	trend[bmask] = NaN
	
	find_area <- function(mn, mx) {
		mask = trend < mn | trend > mx
		trend[mask] = NaN
		areas = raster::area(trend, na.rm = TRUE)												
		return(sum.raster(areas, na.rm = TRUE))
	}
	breaks = c(min.raster(trend, na.rm = TRUE), breaks, max.raster(trend, na.rm = TRUE))
	
	y = mapply(find_area,  head(breaks, -1), breaks[-1])
	y = 100 * y / (sum(y))
	return(y)
}

plotBiomeHist <- function(biomeN, name, breaks, density,
					      trends = trend12FF, cols = head(barCols, -1)) {
	
	if (biomeN > 1) bmask = (biomeAssigned != biomeN | ocean_mask)
		else bmask = ocean_mask
	
	ys = mapply(plotTrendHist, trends, MoreArgs = list(bmask = bmask, breaks = breaks))
	
	if (length(trends) == 1) {
		bFUN <- function(...) barplot(ys, beside = TRUE, width = 0.85, space = 0.175, xlim = c(1.5, length(ys)),xpd = FALSE, ...)
		ys[1] = NaN
	} else {
		bFUN <- function(...)  barplot(t(ys), beside = TRUE, width = 0.85 / length(trends), ...)
		density = rep(density, each = length(trends))
	}
	
	bFUN(col = cols)
	bFUN(density = density, add = TRUE)
	mtext(name, 3, adj = 0.95, line = -1, padj = 1)
	return(ys)
}

plotPlot <- function(newPlot = TRUE, breaks0, mirror_breaks = TRUE,...) {
	if (newPlot) par(mfcol = c(4, 2), mar =  c(1, 1, 0, 3), oma = c(3, 3, 0, 0))
	
	breaks = c(-1E-3, 1E-3, breaks0)
	if (mirror_breaks) breaks = c(-rev(breaks0), breaks)
	
	at = 0:(length(breaks) + 1) + 0.125
	test =  c(1, abs(breaks), 1) <= 0.001
	density = rep(0, length(breaks) + 1)
	density[which(test)[1]] = 30
	
	addXaxis <- function() {
		axis(1, at = at, labels = rep('', length(at)))
		
		point0 = mean(at[test])
		at = sort(c(at[!test], point0))
		
		breaks = sort(c(breaks[!(abs(breaks) <= 0.001)], 0))
		
		labels = c(paste('<', breaks[1]), paste('>', tail(breaks, 1)))
		labels = c(labels[1], breaks, labels[2])
		at[1] = at[1] + 0.5
		at[length(at)] = at[length(at)] - 0.5
		axis(1, at = at, labels = labels, tick = FALSE)
		
		ylim = par("usr")[3:4]
		text(x = point0, y = ylim[1] - diff(ylim)/50, '{', srt = 90, cex = 2.5, xpd = TRUE)
		return(breaks)
	}
	
	plotFun <- function(index)
		mapply(plotBiomeHist, index, names(biomes)[index],
			   MoreArgs = list(breaks, density,...), SIMPLIFY = FALSE)
	
	
	ys = plotFun(1:4)
	addXaxis()
	ys = c(ys, plotFun(5:8))
	breaks = addXaxis()
	return(list(breaks, ys))
}

graphics.off()
pdf('figs/TrendsShift.pdf', width = 10, height = 8)
plotPlot(breaks = seq(0.5, 2, 0.5))

## plot trend index only for biomes
#dev.new()
c(x, ys) := plotPlot(FALSE, breaks = seq(1, 10), mirror_breaks = FALSE,
						  trends = list(trendIndex), cols = tail(barCols, 1))
						  
c(x, ys) := plotPlot(FALSE, breaks = seq(0.2, 20, 0.2), mirror_breaks = FALSE,
						  trends = list(trendIndex), cols = tail(barCols, 1))
			  
dev.off.gitWatermark()

ys = lapply(ys, tail, -1)
ys = lapply(ys, head, -1)
maxY = max(unlist(ys))

plot(range(x), c(0.0, maxY), type = 'n')

mapply(lines, ys, col = biomesCols, MoreArgs = list(x = x))
#mapply(lines, ys, col = biomesCols, MoreArgs = list(x = x, lwd = 3))

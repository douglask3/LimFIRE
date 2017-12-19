######################
## cfg				##
######################
source("cfg.r")

nMnths = 168

nsanmples = 4000

minY = 0.001

limits = c(-50, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 50)/100
dfire_cols = c('#000033', '#0099DD', 'white', '#DD9900', '#330000')

######################
## open data		##
######################
loadData4Ecosystem_analysis()

##########################
## Extract and plot		##
##########################
mnthScaler =  nMnths/2
lmask = !is.na(trend12FF[[1]][[1]])
biome[is.na(biome)] = 0
convert2StartEnd <- function(x, biomeN = NULL, mn, tr) {
	if (!is.null(biomeN)) 
		lmask = sum(layer.apply(biomeN, '==', biome)) > 0
	mn[!lmask] = NaN
	tr[!lmask] = NaN
	start = mn - tr[[1]] * mnthScaler
	end   = mn + tr[[1]] * mnthScaler
	
	plotBound <- function(r, xi) {
		quants = quantile(r, probs = c(0.01, 0.25, 0.5, 0.75, .09), na.rm = TRUE)
		
		if (quants[1] == 0) {
			quants = quants[quants > minY]
			lines(c(xi, xi), c(quants[1], minY/10), lwd = 3)
			#arrows(xi, quants[1], xi, 0.00001)
		} else test0 = FALSE
		
		quants = rev(unique(quants))	
		
		xis = rep(xi, length(quants))
		lines(xis, quants, lwd = 3)
		points(xis, quants, pch = 16)
		
	}
	xi = x + 0.25 * c(-1, 1)
	
	
	lenMask = raster.sum(lmask)
	if (nsanmples > lenMask) nsanmplesi = lenMask else nsanmplesi = nsanmples
	index =  sample(1:lenMask, nsanmplesi, replace = FALSE)
	
	startV = start[lmask][index]
	endV   =   end[lmask][index] 
	trendV = tr[[1]][lmask][index] / mn[lmask][index]
	
	z = cut_results(trendV * 12 * 14, limits)
	cols =  make_col_vector(dfire_cols, limits = limits)
	cols = make.transparent(cols, 0.99)[z]
	
	for (i in 1:4) {
		mapply(function(st, ed, col) lines(xi, c(st, ed), col = col), 
				startV, endV, cols)
	}
	
	plotBound(start, xi[1])
	plotBound(  end, xi[2])
}

 raster.max <- function(r, na.rm = TRUE, ...) max(values(r), na.rm =  na.rm, ...)
 raster.sum <- function(r, na.rm = TRUE, ...) sum(values(r), na.rm =  na.rm, ...)

 
png('figs/ecosystemTrends.png', height = 7, width = 7, res = 300, units = 'in')
	layout(rbind(1, 2, 3, 4:5, 6:7), heights = c(1,1,0.3,0.67, 0.67))
	par(mar = c(1, 1, 0, 1), oma = c(4,2, 0,0))
	
	plot4allEcos <- function(Mean, Trend) {
		plot(c(0, 9), c(minY, 100), type = 'n', log = 'y', axes = FALSE, xlab = '', ylab = '')
		axis(2)
		mapply(convert2StartEnd, 1:8, biomes, MoreArgs = list(Mean, Trend))
		axis(1, at = 0:9, labels = rep('', 10))
	}
	
	plot4allEcos(fireMean, fireTrend)
	
	trend = trend12F[[1]][[1]] / ( 14 * 12)
	fire = mean(lims[[1]])  * 12 * 100
	plot4allEcos(fire, trend)
	axis(1, at = 1:8, labels = names(biomes), las = 2)
	plot.new()
	
	plotlims <- function(i) {
		trend = trend12F[[i]][[1]] / ( 14 * 12)
		fire = mean(lims[[i]])
		plot4allEcos(fire, trend)
	}
	lapply(2:4, plotlims)
	axis(1, at = 1:8, labels = names(biomes), las = 2)
	plotlims(5)
	axis(1, at = 1:8, labels = names(biomes), las = 2)
dev.off()
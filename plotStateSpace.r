source("cfg.r")


xrange = c(0    , 1)
yrangeLog = c(0.1, 100)
yrangeNoLog = c(0, 1)

figName = 'figs/StateSpace'
limFnames = c('Burnt_Area', 'Fuel', 'Moisture', 'Igntions', 'Suppression')

nsamples = 100


loadData4Ecosystem_analysis()
mask = !is.na(trend12FF[[1]][[1]])



plotBiome <- function(biomeN = NULL, col = 'black', midx, trdx, midy, trdy, 
				      yrange = yrangeLog, ylog = TRUE) {
	if (!is.null(biomeN)) mask = mask & any(layer.apply(biomeN, function(i) biome == i))
	
	startEnd <- function(mid, trd) {
		start = mid - trd * 84
		end   = mid + trd * 84
		return(list(start[mask], end[mask]))
	}

	c(x0, x1) := startEnd(midx, trdx)
	c(y0, y1) := startEnd(midy, trdy)
	
	testRange <- function(x, range) x > range[1] & x < range[2]
	mask = testRange(x0, xrange) & testRange(x1, xrange) &
		   testRange(y0, yrange) & testRange(y1, yrange)
	
	x0 = x0[mask]
	x1 = x1[mask]
	y0 = y0[mask]
	y1 = y1[mask]
	

	lengthX = length(x0)
	if (nsamples > lengthX) index = 1:lengthX
		else index = sample(1:lengthX, nsamples, replace = FALSE)
	
	x0 = x0[index]
	y0 = y0[index]
	x1 = x1[index]
	y1 = y1[index]
	
	#if (ylog) ydiff = (log(y1) - log(y0))/diff(log(yrange))
	#	else ydiff = (y1 - y0)/diff(yrange)
	#
	#lwd = sqrt(((x1 - x0)/diff(xrange))^2 + (ydiff)^2)
	lwd = sqrt(((y1 - y0) / y1)^2)
	
	lwd = lwd / max(lwd)
	
	colt = 1 - lwd
	colt[colt > 0.5] = 0.5
	col = make.transparent(col, colt)
	
	length =  round(lwd, 2)
	for (l in unique(length)) {
		index = length == l
		arrows(x0[index], y0[index], x1[index], y1[index], length = l * 0.2,
			   col = col[index], lwd = lwd[index]* 3)
	}
	
}
midx = varMns[['bare']]
trdx = trends[['bare']][[1]]


midy = fireMean
trdy = fireTrend[[1]]

plotLimVsFire <- function(i, lab, scale = 1, yaxis, xaxis, midy, trdy, ylog = TRUE) {
	midx = mean(lims[[i]]) * scale
	trdx = trend12F[[i]][[1]] / ( 14 * 12) * scale

	if (ylog) {
		yrange = yrangeLog
		log = 'y'
	} else {
		yrange = yrangeNoLog
		log = ''
	}
	plot(xrange, yrange, type = 'n', xlab = '', ylab = '', axes = FALSE, log = log)
	mapply(plotBiome, biomes[-1], biomesCols[-1], MoreArgs = list(midx, trdx, midy, trdy, yrange, ylog = ylog))
	if (xaxis) axis(1)
	if (yaxis) axis(2)
	lapply(1:4, axis, at = c(-9E9, 9E9))
	mtext(side = 3, lab, adj = 0.1, lines = 2)

}

plot4Lim <- function(i, fnamei, scaleMn, scaleTr, ...) {
	midy = mean(lims[[i]])  * scaleMn
	trdy = trend12F[[i]][[1]] * scaleTr / ( 14 * 12)
	
	
	labs = paste(letters[1:4], limFnames[-1], sep = ') ')

	fname = paste(figName, fnamei, '.png', sep = '-')
	png(fname, height = 5.5, width = 5.5, units = 'in', res = 300)	
		layout(rbind(1:2, 3:4, 5), heights = c(1,1,0.4))
		par(mar = rep(0.2, 4), oma = c(3, 3, 0,0) )

		mapply(plotLimVsFire, 2:5, labs, c(1,1,1, 2),
			  c(T, F, T, F), c(F, F, T, T), MoreArgs = list(midy, trdy, ...))
			  
		plot.new()
		legend(x = 'center', legend = names(biomes)[-1], lwd = 2, col = biomesCols[-1], ncol = 4, bty = 'n', xpd = TRUE)
		
		mtext(outer = TRUE, side = 1, 'limitation', line = -4)
		mtext(outer = TRUE, side = 2,       fnamei, line = 2)
	dev.off()
}



mapply(plot4Lim, 1:5, limFnames, c(12 * 100, 1, 1, 1, 2), c(1, 1, 1, 1, 2), 
	   ylog = c(T, F, F, F, F))
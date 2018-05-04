##################################################################
## cfg                                                          ##
##################################################################
source('cfg.r')
graphics.off()

fracSample    = 200
grabe_cache   = TRUE
cirlePoints   = FALSE
obsSampleFile = paste('temp/ObsSample', fracSample, '.Rd', sep = '-')
obsLimsFile   = 'temp/ObsLimsEGs2.Rd'
if (file.exists(obsSampleFile) & grabe_cache) load(obsSampleFile) else {
	if (file.exists(obsLimsFile)) load(obsLimsFile) else {
		Obs        = openAllObs()
		fuel       = ((100 - Obs[['bare']])/100) ^ param('fuel_pw')
		#fuel[fuel < 10] = 10
		
		moisture   = (Obs[['alpha']] + param('cM') * Obs[['emc']]) / (1 + param('cM'))
		
		ignitions  = Obs[['Lightn']] + param('cP') * Obs[['pas']] + 
					 param('cD1') * Obs[['popdens']]
		#ignitions[ignitions < 1] = 1
		supression = Obs[['crop']] + param('cD2') * Obs[['popdens']]

		Obs  = c(fuel, moisture, ignitions, supression, Obs[['fire']])
		save(Obs, file = obsLimsFile)
	}
	names(Obs) = c('fuel', 'moisture', 'ignitions', 'suppression', 'fire')
	grbPntInf <- function(i) {
		cell = cellFromXY(Obs[[1]], unlist(i[1:2]))
		pntObs = sapply(Obs, function(j) j[cell])
		return(pntObs)
	}

	pntObs = lapply(hlghtPnts, grbPntInf)
	
	Obs = applySampleMask(Obs, fracSample)
	
	save(Obs, pntObs, file = obsSampleFile)
}
pntObs =  lapply(pntObs, function(i) apply(i, 2, quantile, c(0.1, 0.5, 0.9)))
#pntObs[[3]][,'ignitions'] = pntObs[[3]][,'ignitions'] * 0.5
#pntObs[[1]][,'ignitions'] = pntObs[[3]][,'ignitions'] * 2.5


plotScatter <- function(name, col, FUN, dFUN, x0, k, ksc, log = '', x2pc = FALSE,...) {
	colp = make.transparent('black', 0.95)
	
	if (log == 'x') Obs[, name] = log(Obs[,name])
	
	plot(Obs[, name], Obs[, 'fire'], pch = 19, col = colp, ylim = c(0.0, 1.0), xaxt = 'n', xlab = '', ylab = '', yaxt = 'n', ...)
	if (log == 'x') {
		mnX = exp(min(Obs[,name]))
		mxX = exp(max(Obs[,name]))
		xi = x = c(1:10) * 10^find.ndig(mnX)
		xlabi = xlab = c(1, rep(NA, 9))
		while (max(x) < mxX) {
			x = x * 10
			xlab = xlab * 10
			xi = c(xi, x)
			xlabi = c(xlabi, xlab)
		}
		
		axis(1, at = log(xi), labels = xlabi)
	} else if (x2pc) {
		at = seq(0, 1, by = 0.2)
		axis(1, at = at , labels = at * 100)
	} else axis(1)
	
	
	#if (yaxis) axis(2, at = seq(0,1,by = 0.2), labels = rep('', 6), tick = -5)
	
	index = sort.int(Obs[,name], index.return= TRUE)[[2]]
	
	x = Obs[index, name]
	y = mapply(FUN, paramSample(x0), ksc * paramSample(k), MoreArgs=list(x = Obs[index, name]))
	y = apply(y, 1, quantile, c(0,0.5, 1))
	#apply(y, 2, lines, x = x, lty = 1, lwd = 2,  col = make.transparent('black', 0.98))
	apply(y, 1, lines, x = x, lty = 2)
	lines(x, y[2,])
	
	addPoints <- function(i, info, plotPnts = TRUE, ...) {
		col = info[[3]]
		colt = make.transparent(col, c(0.75, 0.95))
		
		x = seq(min(i[,name]), max(i[, name]), length.out = 1000)	
		y = c(rep(2, length(x)), rep(-2, length(x))); x = c(x, rev(x))
		polygon(x, y, border = colt[1], col = colt[2])
		
		x = i[2,name]
		y = FUN(x, param(x0), ksc * param(k))	
		g = dFUN(x, param(x0), ksc * param(k), normalise= FALSE)
		
		dx = 1
		dy = dx * g
		
		dz = sqrt((dx / diff(par("usr")[1:2]))^2) + sqrt((dy / diff(par("usr")[3:4]))^2)
		
		dx = dx * 0.25 * c(-1, 1) / dz
		dy = dy * 0.25 * c(-1, 1) / dz
		
		x = x + dx
		y = y + dy
		
		lines(x,y, lwd = 3, col = col, xpd = NA, ...)
		
		x = i[,name]; y = FUN(x, param(x0), ksc * param(k))
		if (plotPnts)
			points(x, y, col = col, pch = c(NA, 16, NA) , cex = c(2,2.5,2), lwd = 4)
		
		
		
		if (cirlePoints) {
			if (diff(range(x)) > 0) {
				if (log == 'x') x = log(x)
				if (diff(range(y)) < 0.05) {
					x0 = x; y0 = y
					x = c(x, x, x)
					ydiff = 0.05 * (y - mean(y))/diff(range(y))
					y = c(y,  y + ydiff, y - ydiff)
				}
			
				E = dataEllipse(x, y, levels=0.99, draw = FALSE)
				if (diff(range(E[,2])) > 1.02)
					E = dataEllipse(x, y, levels=sqrt(1/diff(range(E[,2]))), draw = FALSE)
				if (log == 'x') E[,1] = exp(E[,1])
				lines(E[,1], E[,2], col = col, lwd = 2)
			}
		}
		
	}
	
	mapply(addPoints, pntObs, hlghtPnts)
	mapply(addPoints, pntObs, hlghtPnts, MoreArgs = list(plotPnts = FALSE, lty = 3))
	
}

png('figs/limLines.png', width = 5.5, height = 5.5 * 1.15, units = 'in', res = 300)
layout(rbind(1:2,3:4, 5), heights = c(1,1,0.3))

par(mar = c(3,0.5,1,0), oma = c(0,3.5,1,0.5))


leg <- function(pch, col, ...) 
	legend('right', legend = legNames,
		   pch = pch, col = col, lty = 2, lwd = 2, 
		   cex = 1.33, ncol = 1, bty = 'n', seg.len	= 4, ...)



plotScatter('fuel', col = 'green', LimFIRE.fuel, dLimFIRE.fuel, 'fuel_x0', 'fuel_k', 1.0)
axis(2, at = seq(0,1,by = 0.2))
mtext('Vegetation Cover (%)', 1, line = 2.3)

plotScatter('moisture', col = 'blue', LimFIRE.moisture, dLimFIRE.moisture, 'moisture_x0', 'moisture_k', -1.0, x2pc = TRUE)
mtext('Fuel Mositure (%)', 1, line = 2.3)

plotScatter('ignitions', col = 'red', LimFIRE.ignitions, dLimFIRE.ignitions, 'igntions_x0', 'igntions_k', 1.0, xlim = c(0, 10))
mtext('No. Ignitions', 1, line = 2.3)
axis(2, at = seq(0,1,by = 0.2))

plotScatter('suppression', col = 'black', LimFIRE.supression, dLimFIRE.supression, 'suppression_x0', 'suppression_k', -1.0, xlim = c(0,100))
mtext('Suppression Index', 1, line = 2.3)
				

legNames =  names(hlghtPnts)
cols = listSelectItem(hlghtPnts, 'col')
colt = make.transparent(cols, 0.75)

leg(15, colt, pt.cex = 5)
leg(16, cols, pt.cex = 2)

mtext('Fractional Burnt Area', side = 2, line = 2, outer = TRUE)
dev.off()
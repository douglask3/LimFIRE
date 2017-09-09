##################################################################
## cfg                                                          ##
##################################################################
source('cfg.r')
graphics.off()

fracSample    = 1000
grabe_cache   = TRUE
cirlePoints   = FALSE
obsSampleFile = paste('temp/ObsSample', fracSample, '.Rd', sep = '-')
obsLimsFile   = 'temp/ObsLimsEGs.Rd'
if (file.exists(obsSampleFile) & grabe_cache) load(obsSampleFile) else {
	if (file.exists(obsLimsFile)) load(obsLimsFile) else {
		Obs        = openAllObs()
		fuel       = 100 - Obs[['bare']]
		#fuel[fuel < 10] = 10
		moisture   = (Obs[['alpha']] + param('cM') * Obs[['emc']]) / (1 + param('cM'))
		ignitions  = 1 + param('cL') * Obs[['Lightn']] + param('cP') * Obs[['pas']] + 
					 param('cD1') * Obs[['popdens']]
		ignitions[ignitions < 1] = 1
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
pntObs[[3]][,'ignitions'] = pntObs[[3]][,'ignitions'] * 0.5
pntObs[[1]][,'ignitions'] = pntObs[[3]][,'ignitions'] * 2.5

findLims <- function(x, addMid = FALSE) {
	
	if (addMid) {
		fuelMean       =  mean(x[,'fuel'])
		fuelLimm       =  1-LimFIRE.fuel(fuelMean, param('fuel_x0'), param('fuel_k'))
		fuelGrad       = dLimFIRE.fuel(fuelMean, param('fuel_x0'), param('fuel_k'))
		
		moistureMean   = mean(x[, 'moisture'])
		moistureLimm   =  1-LimFIRE.moisture(moistureMean, param('moisture_x0'), param('moisture_k'))
		moistureGrad   = dLimFIRE.moisture(moistureMean, param('moisture_x0'), -param('moisture_k'))
		
		ignitionsMean  =  mean(x[, 'moisture'])
		ignitionsLimm  =  1-LimFIRE.ignitions(ignitionsMean, param('igntions_x0'), param('igntions_k'))
		ignitionsGrad  = dLimFIRE.ignitions(ignitionsMean, param('igntions_x0'), param('igntions_k'))
		
		suppresionMean =  mean(x[, 'suppression'])
		suppresionLimm =  1-LimFIRE.supression(suppresionMean, param('suppression_x0'), param('suppression_k'))
		suppresionGrad = dLimFIRE.supression(suppresionMean, param('suppression_x0'), -param('suppression_k'))
	}
	
	x[,'fuel']     = LimFIRE.fuel(x[,'fuel'], param('fuel_x0'), param('fuel_k'))
	x[, 'moisture'] = LimFIRE.moisture(x[,'moisture'], param('moisture_x0'),  -param('moisture_k'))
	x[, 'ignitions'] = LimFIRE.ignitions(log(x[,'ignitions']), param('igntions_x0'), param('igntions_k'))
	x[, 'suppression'] = LimFIRE.ignitions(x[,'suppression'], param('suppression_x0'),  -param('suppression_k'))
	
	
	x[, 'fire'] = x[,'fuel'] * x[,'moisture'] * x[,'ignitions'] * x[,'suppression']
	
	if (addMid) {
		x = rbind(x,
				  c(fuelMean, moistureMean, ignitionsMean, suppresionMean, 0),
				  c(fuelLimm, moistureLimm, ignitionsLimm, suppresionLimm, 0),
				  c(fuelGrad, moistureGrad, ignitionsGrad, suppresionGrad, 0))		
	}
	
	return(x)
}

Sim = findLims(Obs)

pntSim = lapply(pntObs, findLims, addMid = TRUE)

plotScatter <- function(name, col, FUN, dFUN, x0, k, ksc, log = '', ...) {
	colp = make.transparent('black', 0.9)
	
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
	} else {
		axis(1)
	}
	
	axis(2, at = seq(0,1,by = 0.2), labels = rep('', 6), tick = -5)
	
	index = sort.int(Obs[,name], index.return= TRUE)[[2]]
	
	lines(Obs[index, name], Sim[index, name], lty = 2)
	
	addPoints <- function(i, j, info, plotPnts = TRUE, ...) {
		col = info[[3]]
		colt = make.transparent(col, c(0.75, 0.95))
			
		
		x = seq(min(i[,name]), max(i[, name]), length.out = 1000)
		y = FUN(x, param(x0), ksc * param(k))
		#lines(x, y, col = col, lwd = 3, ...)		
		y = c(rep(2, length(x)), rep(-2, length(x))); x = c(x, rev(x))
		polygon(x, y, border = colt[1], col = colt[2])
		
		
		x = i[2,name]
		y = FUN(x, param(x0), ksc * param(k))	
		g = dFUN(x, param(x0), ksc * param(k), normalise= FALSE)
		
		dx = 1
		dy = dx * g
		
		dz = sqrt((dx / diff(par("usr")[1:2]))^2) + sqrt((dy / diff(par("usr")[3:4]))^2)
		
		dx = dx * 0.15 * c(-1, 1) / dz
		dy = dy * 0.15 * c(-1, 1) / dz
		
		x = x + dx
		y = y + dy
		
		lines(x,y, lwd = 2, col = col, xpd = NA, ...)
		
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
	
	mapply(addPoints, pntObs, pntSim, hlghtPnts)
	mapply(addPoints, pntObs, pntSim, hlghtPnts, MoreArgs = list(plotPnts = FALSE, lty = 3))
}

png('figs/limLines.png', width = 7, height = 7 * 1.15, units = 'in', res = 300)
layout(rbind(1:2,3:4, 5), heights = c(1,1,0.3))

par(mar = c(3,0,1,0.5), oma = c(0,3.5,1,1))

plotScatter('fuel', col = 'green', LimFIRE.fuel, dLimFIRE.fuel, 'fuel_x0', 'fuel_k', 1.0)
axis(2, at = seq(0,1,by = 0.2))
mtext('Vegetation Cover (%)', 1, line = 2)
plotScatter('moisture', col = 'blue', LimFIRE.moisture, dLimFIRE.moisture, 'moisture_x0', 'moisture_k', -1.0)
mtext('Fuel Mositure (%)', 1, line = 2)
plotScatter('ignitions', col = 'red', LimFIRE.ignitions, dLimFIRE.ignitions, 'igntions_x0', 'igntions_k', 1.0, log = 'x')
mtext('No. Ignitions', 1, line = 2)
axis(2, at = seq(0,1,by = 0.2))
plotScatter('suppression', col = 'black', LimFIRE.supression, dLimFIRE.supression, 'suppression_x0', 'suppression_k', -1.0)
mtext('Suppression Index', 1, line = 2)
mtext('Fractional Burnt Area', side = 2, outer = TRUE, line = 2)

plot.new()					

legNames =  names(hlghtPnts)
cols = listSelectItem(hlghtPnts, 'col')
colt = make.transparent(cols, 0.75)



leg <- function(pch, col, ...) 
	legend('center', legend = legNames,
		   pch = pch, col = col, lty = 2, lwd = 2, 
		   cex = 1.33, horiz = TRUE, bty = 'n', seg.len	= 4, ...)

leg(15, colt, pt.cex = 5)
leg(16, cols, pt.cex = 2)

dev.off()
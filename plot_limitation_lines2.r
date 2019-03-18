##################################################################
## cfg                                                          ##
##################################################################
source('cfg.r')
graphics.off()

#polygonsNotPoints set at bootom
#ylog set at bottom
fracSample    = 50
grabe_cache   = TRUE
obsSampleFile = paste('temp/ObsSample', fracSample, '.Rd', sep = '-')
obsLimsFile   = 'temp/ObsLimsEGs-Tree-alphasMax7.Rd'
if (file.exists(obsSampleFile) & grabe_cache) load(obsSampleFile) else {
	if (file.exists(obsLimsFile)) load(obsLimsFile) else {
		Obs        = openAllObs()
		
		fuel       = ((100 - Obs[['bare']])/100) ^ param('fuel_pw') * 
					  (param('fuel_pg') * (Obs[['alphaMax']] - 1) + 1) / (1 + param('fuel_pg'))
		
		moisture   = (Obs[['alpha']] + param('cM') * Obs[['emc']] * 0.01 + 
					  param('cMT') * Obs[['tree']] / 100) / (1 + param('cM') + param('cMT'))
		
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


plotScatter <- function(name, col, yg = NULL, FUNi, dFUNi, x0, k, ksc, log = '', 
						x2pc = FALSE, plot = c(T, T, T, T),plotGrad = TRUE, xlim = c(0, 100), ...) {
		
	if (ksc < 0) {
		FUN  <- function(x, ...)  FUNi(x,...) / FUNi(0, ...)
		dFUN <- function(x, normalise = FALSE,...) dFUNi(x, normalise = normalise, ...) / FUNi(0, ...)
	} else {
		 FUN <- function(...)  FUNi(...)
		dFUN <- function(...) dFUNi(...)
	}
	
	if (log == 'x') Obs[, name] = log(Obs[,name])
	
	if (ylog) {
		log = 'y'
		if (polygonsNotPoints) ymin = 0.001 else ymin = 0.001
	} else {
		log = ''
		ymin = 0.0
	}
	
	plot(Obs[, name], Obs[, 'fire'], ylim = c(ymin, 1.0),  xlim = xlim,
	  	 xaxt = 'n', xlab = '', ylab = '', yaxt = 'n', type = 'n', log = c('','y')[ylog + 1], ...)
		
	if (polygonsNotPoints) {
		quantileDesnityPoly(Obs[, name], Obs[, 'fire'], xlim = xlim, nbins = 100)	
	} else {
		obsPoints <- function(cex, alpha, col) {
			colp = make.transparent(col, alpha)	
			points(Obs[, name], Obs[, 'fire'], pch = 19, cex = cex, col = colp)
		}
		mapply(obsPoints, seq(1.0, 0.1, -0.1), 1-(1:10)/255,  make_col_vector(c("white", "black"), ncols = 10))
	}
	
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
-       axis(1, at = at , labels = at * 100)
	} else axis(1)
	
	index = sort.int(Obs[,name], index.return= TRUE)[[2]]
	
	x = Obs[index, name]
	#y0 = y
	
	y = mapply(FUN, paramSample(x0), ksc * paramSample(k), MoreArgs=list(x = Obs[index, name]))
	y = apply(y, 1, quantile, c(0,0.5, 1)) 
	
	
	#apply(y, 2, lines, x = x, lty = 1, lwd = 2,  col = make.transparent('black', 0.98))
	apply(y, 1, lines, x = x, lty = 2)
	lines(x, y[2,])
	
	addPoints <- function(i, info, index, plot = TRUE, plotPnts = TRUE, ...) {
		
		col = info[[3]]
		colt = make.transparent(col, c(0.6, 0.9))
		
		x = seq(min(i[,name]), max(i[, name]), length.out = 1000)	
		y = c(rep(2, length(x)), rep(c(-2, 0.00001)[ylog+1], length(x))); x = c(x, rev(x))
		if (plot) 
			polygon(x, y, border = colt[1], col = colt[2])
		
		x = i[2,name]
		if (is.null(yg)) {
			y = FUN(x, param(x0), ksc * param(k))	#/ sc
			g = dFUN(x, param(x0), ksc * param(k), normalise= FALSE) #/ sc	
		} else 	{
			c(y, g) := yg[, index]
			gmax =  dFUN(param(x0), param(x0), ksc * param(k), normalise= FALSE)
			g =  g / abs(gmax * diff(par("usr")[1:2]))
		}
	
		dx = 1
		dy = dx * g
		
		dz = sqrt((dx / diff(par("usr")[1:2]))^2) + sqrt((dy / diff(par("usr")[3:4]))^2)
		
		dx = dx * 0.25 * c(-1, 1) / dz
		dy = dy * 0.25 * c(-1, 1) / dz
		
		xl = x + dx
		yl = y + dy
		
		#if (plot)
		#	if (plotGrad) lines(xl, yl, lwd = 3, col = col, xpd = NA, ...)
		
		#if (plot && plotPnts)
		#	points(x, y, col = col, pch = 16 , cex = 2.5 , lwd = 4)
		
		return(c(lim = y, sen = g))
	} 
	
	ygout = mapply(addPoints, pntObs, hlghtPnts, 1:4, plot)	
	mapply(addPoints, pntObs, hlghtPnts, 1:4, plot, MoreArgs = list(plotPnts = FALSE, lty = 3))
	return(ygout)
}

plotAll <- function(fname0 = NULL, fuel = NULL, moisture = NULL, 
				    ignitions = NULL, suppression = NULL, 
					yticks = NULL, ...) {
	if (is.null(yticks)) {
		if (ylog) yticks = 10^((-5):1)
			else yticks = seq(0,1,by = 0.2)
	}
	
	
	fname = paste('figs/limLines', fname0, ylog, polygonsNotPoints, '.png', sep = '-')
		
	axis4 = !is.null(fuel)	
		
	png(fname, width = 7.2, height = 5.5 * 1.15 * 7.2/6, units = 'in', res = 300)
	layout(rbind(1:2,3:4, 5), heights = c(1,1,0.3))

	par(mar = c(3,0.5,1,0), oma = c(0,3.5,1,3.5))

	leg <- function(pch, col, ...)  {
		legend(x=20, y = 1, pch = pch, col = c('#BBBBBB', '#999999', '#777777', '#555555', '#333333'), 
			   legend = c('99%', '95%', '90%', '75%', '50%'), bty = 'n', cex = 1.33, ...)
		legend(x = 42, y = 1, legend = legNames,
			   pch = pch, col = col,  
			   cex = 1.33, ncol = 1, bty = 'n', seg.len	= 4, ...)
	}
	fuel = plotScatter('fuel', col = 'green', fuel,
					   LimFIRE.fuel, dLimFIRE.fuel, 
					   'fuel_x0', 'fuel_k', 1.0,   x2pc = TRUE,
					   xlim = c(0, 1), ...)
					   
	axis(2, at = yticks, labels = yticks * 100)
	mtext(side = 3, 'a)', adj = 0.1, line = -1.3)
	mtext('Fuel Continuity (%)', 1, line = 2.3)

	moisture = plotScatter('moisture', col = 'blue', moisture,
						   LimFIRE.moisture, dLimFIRE.moisture, 
						   'moisture_x0', 'moisture_k', -1.0, 
						   x2pc = TRUE, xlim = c(0, 0.9), ...)
	mtext(side = 3, 'b)', adj = 0.1, line = -1.3)
	mtext('Fuel Moisture (%)', 1, line = 2.3)
	if (axis4) axis(side = 4, at = yticks, labels = 100 - yticks * 100)
	
	ignitions = plotScatter('ignitions', col = 'red', ignitions, 
							LimFIRE.ignitions, dLimFIRE.ignitions, 
							'igntions_x0', 'igntions_k', 1.0, 
							xlim = c(0, 5), ...)
	mtext(side = 3, 'c)', adj = 0.1, line = -1.3)
	mtext.units('No. Ignitions (k~m2~ ~month-1~)', 1, line = 2.3)
	axis(2, at = yticks, labels = yticks * 100)

	
	if (ylog) xlim = c(0, 90) else xlim = c(0, 80)
	suppression = plotScatter('suppression', col = 'black', suppression,
							  LimFIRE.supression, dLimFIRE.supression, 
							  'suppression_x0', 'suppression_k', -1.0, 
							  xlim = xlim, ...)
	mtext('Suppression Index', 1, line = 2.3)
	mtext(side = 3, 'd)', adj = 0.1, line = -1.3)
	if (axis4) axis(side = 4, at = yticks, labels = 100 - yticks * 100)

	mtext('Monthly Burnt Area(%)', side = 2, line = 2, outer = TRUE)
	if (axis4) 
		mtext('Limitation on Burnt Area', side = 4, line = 2, outer = TRUE)
		
	if (fname0 != "empty") {
		legNames =  names(hlghtPnts)
		cols = listSelectItem(hlghtPnts, 'col')
		colt = make.transparent(cols, 0.75)

		leg(15, colt, pt.cex = 5)
		#leg(16, cols, pt.cex = 2)		
	}
	
	dev.off()
	return(list(fuel, moisture, ignitions, suppression))
}

plotByYlog <- function(fname, ...) {
	c(fuel, moisture, ignitions, suppression) := plotAll(fname, ...)

	suppCon = 1 - LimFIRE.supression(0, param('suppression_x0'), param('suppression_k'))

	#suppression[1,] = suppression[1,] / suppCon

	lim = rbind(fuel[1,], moisture[1,], ignitions[1,], suppression[1,])
	fni <- function(index) apply(lim[-index,], 2, prod)
	fgni <- function(x, index) {
		#browser()
		#x[1, ] = 1 - fni(index) / (1 - LimFIRE.supression(0, param('suppression_x0'), param('suppression_k')))
		sc = apply(lim, 2, min)
		x[1, ] = 1 - sc/x[1,]
		
		
		x[2, ] =  x[2, ] * fni(index) / suppCon
		test = x[2,] > 0
		x[2, test] = sqrt(x[2,])
		x[2, !test] = - sqrt(-x[2,])
		
		return(x)
	}

	fuel = fgni(fuel, 1)
	moisture = fgni(moisture, 2)
	ignitions = fgni(ignitions, 3)

	suppression = fgni(suppression, 4)

	if (fname != "empty") {
		fname = paste(fname, 'shifted', sep ='-')
		plotAll(fname, fuel, moisture, ignitions, suppression, ...)
	}
}

plotAllThese <- function() {
	#plotByYlog('noGrad', plotGrad = FALSE)
	#plotByYlog('empty', plot = c(F, F, F, F), plotGrad = FALSE)
	#plotByYlog('noGrad-Desert', plot = c(T, F, F, F), plotGrad = FALSE)
	#plotByYlog('noGrad-Rainforest', plot = c(T, T, F, F), plotGrad = FALSE)
	#plotByYlog('noGrad-Savanna', plot = c(T, T, T, F), plotGrad = FALSE)


	plotByYlog('')
	#plotByYlog('Desert', plot = c(T, F, F, F))
	#plotByYlog('Rainforest', plot = c(T, T, F, F))
	#plotByYlog('Savanna', plot = c(T, T, T, F))
}

#polygonsNotPoints = FALSE
#ylog = FALSE
#plotAllThese()

#ylog = TRUE
#plotAllThese()

polygonsNotPoints = TRUE
ylog = FALSE
plotAllThese()

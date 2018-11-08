#########################################################################
## cfg                                                                 ##
#########################################################################
dontPlot = TRUE
source("plotVariableTrends.r")
grab_cache = TRUE


obs = lapply(drive_fname, stack)
vars = list('crop', 'pas', 'popdens', c('crop', 'pas', 'popdens'))
dfire_lims = list(c(-10, -5, -1,  -0.5, -0.1,  0.1, 0.5, 1, 5, 10),
				  c(-80, -60, -40, -20, -10,  -1, 1, 10, 20, 40, 60, 80),
				  c(-50, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 50))

#########################################################################
## Run model                                                           ##
#########################################################################
ens_files = open_ensembles()
ens_no <- function(file) strsplit(strsplit(file[[1]][[1]], 'outputs//ensemble_')[[1]][-1], '/')[[1]][1]
ens_nos = as.numeric(sapply(ens_files, ens_no))[1:27]

findParameterLimitation <- function(var, ens_no) {	
	dir = paste(outputs_dir, 'ensemble_noVar', ens_no, '/', sep = "")
	makeDir(dir)
	fname = paste(var, collapse = '_')
	fname = paste(dir, '/LimFIRE_fire_no_', c('', '', 'trend_'), c('', fname, fname), '-rw-fire-mean.nc', sep = '')	
	
	runLimFire <- function(...) {
		dat = runLimFIREfromstandardIns(nline = ens_no, fireOnly = TRUE, normalise = TRUE, ...)
		dat = mean(dat)
		return(dat)
	}
	
	ct = runIfNoFile(fname[1], runLimFire, test = grab_cache)		
	mn = runIfNoFile(fname[2], runLimFire, remove = var, test = grab_cache)
	
	removeTrend <- function(...)  {	
		trend = trends[var]
		
		removeTrend <- function(var, trend) 
			layer.apply(1:168, function(i) obs[[var]][[i]] - trend[[1]] * i)
			
		obs[var] = mapply(removeTrend, var, trend)
		
		dat = runLimFire(replace = obs, ...)
		return(dat)
	}
	
	mn_noTrend = runIfNoFile(fname[3], removeTrend, test = grab_cache)
	return(addLayer(ct, mn, mn_noTrend))
}

findParameterLimitationVar <- function(...) 
	lapply(vars, findParameterLimitation, ...)

mod = lapply(ens_nos, findParameterLimitationVar)


plotVar <- function(i, xlim, ylim, name, title, xlab, log = '', plotFun,...) {
	mod = lapply(mod, function(m) m[[i]])

	contr = layer.apply(mod, function(i) i[[1]])
	noVar = layer.apply(mod, function(i) i[[2]])
	trend = layer.apply(mod, function(i) i[[3]])
	diffv = contr - noVar
	norm = layer.apply(mod, function(i) max(i[[1:2]]))
	
	if (length(name) > 1) {
		var = mean(contr) * 100
		var[var < 0.001] = 0.001
		log = 'x'
	} else {
		var = mean(obs[[name]])
		log = log
	}
	
	plotMap <- function(x, limits = limits, cols = dfire_cols, add_legend = FALSE,...) {
		plotStandardMap(mean(x), '', limits = limits, cols =  cols, 
						e = sd.raster(x), ePatternRes = 30, ePatternThick = 0.2, limits_error = c(1/10, 1/2),
						add_legend = add_legend, ...)
	}

	par(mar = c(3, 0.5, 0, 0))
	if (title != "NaN") {
		plot(xlim, ylim, type = 'n', log = log, xlim = xlim, ylim = ylim, xlab = xlab)
		for (i in 1:nlayers(contr)) points(var[],100* (diffv[[i]]/norm[[i]])[]	, col = make.transparent("black", 0.99), pch = 19, cex = 0.001)
		
		mtext(title, side = 3, line = -1.3)
		mtext(xlab , side = 1, line =  1.67, cex =0.8)
		plotFun()
	} else plot.new()
	par(mar = rep(0,4))
	plotMap(diffv * 100 * 12, dfire_lims[[1]], ...)
	
	
	plotMap(layer.apply(1:nlayers(diffv), function(i) diffv[[i]]/norm[[i]]) * 100, dfire_lims[[2]],...)
	plotMap(100*(contr - trend)/norm, dfire_lims[[3]], ...)
	
	areaIncreaseDecrease <- function(x) {
		a  = raster::area(x, na.rm = TRUE)
		sa = sum.raster(a, na.rm = TRUE)
		
		FUN <- function(test) sum(a[test])/sa
		
		increase = FUN(x  > 0.01)
		same     = FUN(x  > -0.01 & x < 0.01)
		decrease = FUN(x  < -0.01)
		
		return(c(increase, same, decrease))
	}
	out = layer.apply(diffv/norm, areaIncreaseDecrease)
	out = sapply(out, function(i) i)
	
	mout  = apply(out, 1, mean)
	sdout = apply(out, 1, sd  )
	return(cbind(mout, sdout))
}
	
FUNcrop <- function() lines(c(0,100), c(0, -100), col = 'red')
FUNpas  <- function() lines(c(0,100), c(0,  100), col = 'red')
FUNpopd <- function() lines(c(0.00001, 9E9), c(0, 0), col = 'red')
	
png('figs/human_impact.png', width = 7.5 *7/5, height = 6.5, res = 300, unit = 'in')
	par(oma = c(0, 2.5, 1.5, 0))
	layout(rbind(1:4, 5:8, 9:12, 13:16, c(0, 17:19)), widths = c(1, 2, 2, 2), heights = c(1, 1, 1, 1, 0.3))
	out = mapply(plotVar, 1:4, 
		   list(c(0, 80), c(0, 80), c(0.5, 10000), c(0.001, 100)),
		   list(c(-100, 0), c(0, 100), c(-100, 100), c(-100, 100)), 
		   vars, c("cropland", "pasture", "pop. density", NaN),
		                                c('% cover', '% cover', 'pop/km2', '% Burnt Area'), c('', '', 'x', ''), 
										c(FUNcrop, FUNpas, FUNpopd), SIMPLIFY = FALSE)
		   
	addLegend <- function(limits, units) {
		add_raster_legend2(dfire_cols, limits, dat = mod[[1]][[1]][[1]], srt = 0,
						   transpose = FALSE, plot_loc = c(0.05, 0.95, 0.75, 0.9), ylabposScling=1.5, 
						   oneSideLabels = NA, add = FALSE)
		mtext(units, side = 1, line = -1)
	}
	
	
	addLegend(dfire_lims[[1]], '% change in burnt area')
	addLegend(dfire_lims[[2]], '% change in normalised burnt area')
	addLegend(dfire_lims[[3]], '% change in normalised burnt area')
	
	mapply(mtext, c("Impact on fire", "Impact on trend"), adj = c(2.2, 4.5)/5, 
		   MoreArgs = list(side = 3, outer = TRUE))
	mtext('change in burnt area (%)', side = 2, line = 1.5, cex = 0.8, outer = TRUE, adj = 0.5 * 4.3/4)
dev.off()






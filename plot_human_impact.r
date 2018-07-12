#########################################################################
## cfg                                                                 ##
#########################################################################
dontPlot = TRUE
source("plotVariableTrends.r")
grab_cache = TRUE


obs = lapply(drive_fname, stack)
vars = list('crop', 'pas', 'popdens', c('crop', 'pas', 'popdens'))
dfire_lims = c(-20, -10, -5, -2, -1, 1, 2, 5, 10, 20)

#########################################################################
## Run model                                                           ##
#########################################################################
ens_files = open_ensembles()
ens_no <- function(file) strsplit(strsplit(file[[1]][[1]], 'outputs//ensemble_')[[1]][-1], '/')[[1]][1]
ens_nos = as.numeric(sapply(ens_files, ens_no))#[1:17]

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


plotVar <- function(i, xlim, ylim, noraliseByControl, name, title, xlab,...) {
	mod = lapply(mod, function(m) m[[i]])

	contr = layer.apply(mod, function(i) i[[1]])
	noVar = layer.apply(mod, function(i) i[[2]])
	trend = layer.apply(mod, function(i) i[[3]])
	diffv = contr - noVar
	
	if (length(name) > 1) {
		var = mean(contr) * 100
		var[var < 0.001] = 0.001
		log = 'x'
	} else {
		var = mean(obs[[name]])
		log = ''
	}
	
	if (noraliseByControl) norm = contr else norm = noVar

	plotMap <- function(x, limits = dfire_lims, cols = dfire_cols, add_legend = FALSE,...) {
		plotStandardMap(mean(x), '', limits = limits, cols =  cols, 
						e = sd.raster(x), ePatternRes = 30, ePatternThick = 0.2, limits_error = c(1/10, 1/2),
						add_legend = add_legend, ...)
	}

	par(mar = c(3, 3, 0, 0))
	plot(xlim, ylim, type = 'n', log = log, xlim = xlim, ylim = ylim, xlab = xlab)
	for (i in 1:nlayers(contr)) points(var[],100* (diffv[[i]]/norm[[i]])[]	, col = make.transparent("black", 0.98), pch = 19, cex = 0.01)
	
	mtext(title, side = 2, line = 2)
	par(mar = rep(0,4))
	plotMap(diffv * 100 * 12, ...)
	plotMap(100*(1-trend/contr), ...)
}
	
png('figs/human_impact.png', width = 7.5, height = 6.5, res = 300, unit = 'in')
	par(oma = c(0, 0.5, 1.5, 0))
	layout(rbind(1:3, 4:6, 7:9, 10:12, c(0, 13,13)), widths = c(1, 2, 2), heights = c(1, 1, 1, 1, 0.3))
	mapply(plotVar, 1:4, 
		   list(c(0, 80), c(0, 80), c(0, 2000), c(0.001, 100)),
		   list(c(-100, 0), c(0, 100), c(-100, 100), c(-100, 100)), 
		   c(FALSE, TRUE, FALSE), vars, c("cropland", "pasture", "population density", "overall impact"),
		                                c('% cover', '% cover', 'pop/km2', '% Burnt Area'))
		   
	plot.new()
	add_raster_legend2(dfire_cols, dfire_lims, dat = mod[[1]][[1]][[1]], srt = 0,
					   transpose = FALSE, plot_loc = c(0.25, 0.75, 0.75, 0.9), ylabposScling=1.5, oneSideLabels = NA)
	
	mapply(mtext, c("Impact on fire", "Impact of trend"), adj = c(2, 4.5)/5, 
		   MoreArgs = list(side = 3, outer = TRUE))
dev.off()






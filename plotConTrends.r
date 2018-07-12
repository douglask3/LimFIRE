#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()

grab_cache = TRUE

fig_fname = 'figs/Trends.png'

limitTitles = c('e) Fire', 'a) Fuel', 'b) Moisture', 'c) Ignitions', 'd) Suppression')

tempF1 = 'temp/limitations4trends-Tree-alphaMax2'
tempF2 = 'temp/trendsFromLimitations-Tree-alphaMax2'
esnambleTemp <- 'temp/ensamble_12FFonly17'

dfire_lims = c(-5, -2, -1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1, 2, 5)/100
dfire_cols = c('#000033', '#0099DD', 'white', '#DD9900', '#330000')
prob_lims = c(0.001, 0.01, 0.05)

limit_cols = list(c('magenta', 'white', 'green'), c('yellow', 'white', 'blue'), c('cyan', 'white', 'red'), c('white', '#111111'))

Nensmble = 50
factor = 1

#########################################################################
## Run model                                                           ##
#########################################################################
tempFile <- function(fnames, extraName = '') {
       fnames = paste(fnames, extraName, sep = '')
       fnames = paste(fnames, c('fire', 'fuel', 'moisture', 'igntions', 'suppression'), '.nc', sep = '-')
}

findParameterTrends <- function(files, factor) {
	files = files[[2]]
	files = files[c(1, 2, 4, 3, 5)]
	lims = lapply(files, brick)
	
	ensID = strsplit(files[1], '/')[[1]]
	ensID = ensID[grepl('ensemble_', ensID)]
		
	if (factor != 1) {
		ensID = paste(ensID, '-factor', factor, sep = '')
		tempF1B = tempFile(tempF1, ensID)
		aggregateFUN <- function()  lapply(lims, raster::aggregate, factor)
		lims = runIfNoFile(tempF1B, aggregateFUN, test = grab_cache)
	}
	
	fire = lims[[1]]

	#########################################################################
	## Find  Trends                                                        ##
	#########################################################################
	findTrend <- function(lno, smoothFun = running12, 
						  trendFUN = Trend, removeFire = FALSE, ...) {
		
		if (removeFire) lims = lims[-1]
		lim  = lims[[lno]]
		lims = lims[-lno]
		if (!removeFire) lims = NULL
		return(trendFUN(lim, smoothFun, lims))
	}


	findTrends <- function(lims, ...) lapply(1:length(lims), findTrend, ...)
	findTrendNoFile <- function(FUN, trendFUN = Trend, ..., fireOnly = FALSE, trend1 = NULL) {
		if (is.null(trend1)) {
			tfiles = tempFile(...)
			if (fireOnly) {
				tfiles = tfiles[1]
				lims = lims[1]
			} 
			trends = list(runIfNoFile(tfiles, findTrends, lims[1], FUN, trendFUN, test = grab_cache))
		} else {
			trends = runIfNoFile(tempFile(...)[-1], findTrends, lims[-1], FUN, trendFUN, removeFire = TRUE, test = grab_cache)
			trends = c(trend1, trends)
		}	
		return(trends)
	}

	#########################################################################
	## Simple Trends                                                       ##
	#########################################################################	
	trend12 = findTrendNoFile(running12, removeTrend, tempF2, ensID, fireOnly = TRUE)
	trend12[[1]][[1]] = trend12[[1]][[1]] * 100 #* 14 * 12
	
	#########################################################################
	## Trend removal                                                       ##
	#########################################################################


	trend12F = findTrendNoFile(running12, removeTrend, tempF2,
							   paste('removeTrend', ensID, sep = '-'), trend1 = trend12[[1]])
	
	
	## weigted by fire
	trend12FFname = tempFile(tempF2, paste('removeTrendAndNormalise', ensID, sep = '-'))
	sfire = runIfNoFile(tempFile(tempF2, '-sfire')[1], function() sum(fire), test = FALSE)
	trend12FF = runIfNoFile(trend12FFname,
							function() lapply(trend12F, function(i) {i[[1]] = i[[1]] / sfire; return(i)}), test = FALSE)
	
	return(trend12FF)
}

if (file.exists(esnambleTemp)) {
	load (esnambleTemp)
} else {
	ens_files = open_ensembles()
	ensamble = lapply(ens_files[1:Nensmble], findParameterTrends, factor)
	save(ensamble, file = esnambleTemp)
}

extractEnsamble <- function(id, FUN)
	 summary.ens(lapply(ensamble, function(i) i[[id]]))

trend12FF = lapply(1:5, function(i) layer.apply(ensamble, function(ens) ens[[i]][[1]]))

make_trend_index_local <- function(ens, files, name = 'trendIndex1', ...) {
	tfname = head(strsplit(files[[1]][1], '/')[[1]], -1)
	tfname = paste(tfname, collapse = '/')
	tfname = paste(tfname, paste(name, '.nc', sep = ''), sep = '/')
	print(tfname)
	out = runIfNoFile(tfname, make_trend_index, ens, files, ..., test = grab_cache)
	return(out)
}

trendIndex1 = mapply(make_trend_index_local, ensamble, ens_files[1:Nensmble])
trendIndex2 = mapply(make_trend_index_local, ensamble, ens_files[1:Nensmble], MoreArgs = list('trendIndex2', absTrend = TRUE))

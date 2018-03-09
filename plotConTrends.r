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
esnambleTemp <- 'temp/ensamble4'

tempFile <- function(fnames, extraName = '') {
	fnames = paste(fnames, extraName, sep = '')
	fnames = paste(fnames, c('fire', 'fuel', 'moisture', 'igntions', 'suppression'), '.nc', sep = '-')
}

dfire_lims = c(-5, -2, -1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1, 2, 5)/100
dfire_cols = c('#000033', '#0099DD', 'white', '#DD9900', '#330000')
prob_lims = c(0.001, 0.01, 0.05)

limit_cols = list(c('magenta', 'white', 'green'), c('yellow', 'white', 'blue'), c('cyan', 'white', 'red'), c('white', '#111111'))

niterations = 21
factor = 1

#########################################################################
## Run model                                                           ##
#########################################################################
findParameterTrends <- function(line, factor, 
							    simpleTrend = FALSE, seasonTrend = FALSE) {
	print(line)
	fnameLine = paste('paramLine', line, sep = "")
	tempF1A = tempFile(tempF1, fnameLine)
	lims  = runIfNoFile(tempF1A, runLimFIREfromstandardIns, raw = TRUE, pline = line, 
					    test = grab_cache)
		
	if (factor != 1) {
		fnameLine = paste(fnameLine, '-factor', factor, sep = '')
		tempF1B = tempFile(tempF1, fnameLine)
		aggregateFUN <- function()  lapply(lims, raster::aggregate, factor = factor)
		lims = runIfNoFile(tempF1B, aggregateFUN, test = grab_cache)
	}
	
	lims[[2]] = lims[[2]] -  LimFIRE.fuel(0, param('fuel_x0'), param('fuel_k'))
	fire = lims[[1]]
	print(line)


	#########################################################################
	## Find  Trends                                                        ##
	#########################################################################
	findTrend <- function(lno, smoothFun = running12, 
						  trendFUN = Trend, removeFire = FALSE) {
		
		if (removeFire) lims = lims[-1]
		lim  = lims[[lno]]
		lims = lims[-lno]
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
	trend12 = findTrendNoFile(running12, Trend, tempF2, fnameLine, fireOnly = !simpleTrend)
	trend12[[1]][[1]] = trend12[[1]][[1]] * 100 * 14 * 12
	#########################################################################
	## Trend removal                                                       ##
	#########################################################################


	trend12F = findTrendNoFile(running12, removeTrend, tempF2,
							   paste('removeTrend', fnameLine, sep = '-'), trend1 = trend12[[1]])
	
	
	## weigted by fire
	trend12FFname = tempFile(tempF2, paste('removeTrendAndNormalise', fnameLine, sep = '-'))
	sfire = runIfNoFile(tempFile(tempF2, '-sfire')[1], function() sum(fire), test = TRUE)
	trend12FF = runIfNoFile(trend12FFname,
							function() lapply(trend12F, function(i) {i[[1]] = i[[1]] / sfire; return(i)}), test = TRUE)
	
	#########################################################################
	## During fire season                                                  ##
	######################################################################### 
	findFireMonth <- function(yr) {
		mn = (1 + (yr -1) * 12):(yr*12)
		fmax = fire[[mn]]
		return(which.max(fmax))
	}

	fireSeasonLim <- function(x, ...) {

		mask = !is.na(x[[1]])
		vx = x[mask]
		fireMonths = fireMonths[mask]
		
		xoutv = vx[,1:nyrs]
		
		for (i in 1:(dim(xoutv)[1])) for (j in 1:nyrs)  xoutv[i,j] = vx[i,fireMonths[i,j]]
		
		xout = x[[1:nyrs]]
		xout[mask] = xoutv
		return(xout)
	}
	if (seasonTrend) {
		nyrs = (nlayers(fire)/12)
		fireMonths = layer.apply(1:nyrs, findFireMonth)
		trendFS = findTrendNoFile(fireSeasonLim, Trend, tempF2,  trend1 = trend12[[1]], 'season')
	} else trendFS = NULL
	
	
	return(list(trend12, trend12F, trend12FF, trendFS, lapply(lims, mean)))
}

esnambleTemp = paste(esnambleTemp, niterations, sep = '-')
if (file.exists(esnambleTemp)) {
	load (esnambleTemp)
} else {
	ensamble = lapply(seq(0, 1, length.out = niterations), findParameterTrends, factor)
	save(ensamble, file = esnambleTemp)
}

findIndex <- function(member, id = 3) {
	member = member[[id]][-1]
	trendIndex = 100 * (prod(layer.apply(member, function(i) 1 + abs(i[[1]])/10)) - 1)
	pvals = mean(layer.apply(member, function(i) i[[2]]))
	return(addLayer(trendIndex, pvals))
}

#lims      = extractEnsamble(ensamble, 5,   brick.ens)
#trend12F  = extractEnsamble(ensamble, 2, summary.ens)
trend12FF = extractEnsamble(ensamble, 3, summary.ens)
trendIndex = lapply(ensamble, findIndex)
trendVals = layer.apply(trendIndex, function(i) i[[1]])
trendIndex = addLayer(mean(trendVals),
					  fisherPval(grabCommonField(trendIndex, 2)),
					  sd.raster(trendVals))
trendIndex[[2]][is.na(trendIndex[[2]]) & !is.na(trendIndex[[1]])] = 0.0      
prob_lims = qchisq(c(0.9, 0.95, 0.99, .999), niterations)[2]


#########################################################################
## Plot trends                                                         ##
######################################################################### 
plotHotspots <- function(trends, figName, limits = dfire_lims, fire_limits = limits, 
						 lims4way = NULL, ...) {
	
	trends[[1]] = trends[[1]]
	trends[[1]][[3]] = trends[[1]][[3]] #* 20
	#if (normFtrend) trends[[1]][[1]] = trends[[1]][[1]]  /sfire
	#browser()
	png(figName, height = 10, width = 8.5, units = 'in', res = 300)
		layout.submap(rbind(2:3, 4:5, 6, c(1,8), c(7,9), 10, 10), heights = c(1,1,0.3, 1, 0.3, 0.65, 0.65), 
					  skip = c(6, 7, 9))
		par(mar = rep(0,4))
		
		limits = c(list(fire_limits), rep(list(limits), 4))
		
		plot_Trends.local <- function(index = 1:length(trends), labs = limitTitles, plims = prob_lims) {
			mapply(plot_Trend, trends[index], labs[index], limits = limits[index],
						  MoreArgs = list(cols = dfire_cols, prob_lims = plims,
										  limits_error = c(0.1, 0.5, 1), ...))
		}
		
		hotspots = plot_Trends.local()[-1]
		
		add_raster_legend2(cols = dfire_cols, limits = limits[[2]], ylabposScling = 2,
								   transpose = FALSE, plot_loc =  c(0.2, 0.9, 0.85, 0.99), 
								   add = FALSE, nx  = 1.75)
		
		add_raster_legend2(cols = dfire_cols, limits = fire_limits, ylabposScling = 2,
								   transpose = FALSE, plot_loc =  c(0.2, 0.9, 0.85, 0.99), 
								   add = FALSE, nx  = 1.75)
		

		hotspots = lapply(hotspots, abs)	
		all = layer.apply(hotspots, function(i) i)
		if (is.null(lims4way)) lims4way = quantile(all[], seq(0.25, 0.75, 0.25), na.rm = TRUE)
		plot_4wayLimit(hotspots, '', FALSE, cols =c("FF","BB","88","44"),
					   limits = lims4way, normalise = FALSE, ePatternRes = 30, ePatternThick = 0.35)	
		mtext.units('f) Hotspots', side = 3, line = -2.25, adj = 0.05)
		plot.new()
		plot.new()
		clusters = layer.apply(trends[-1], function(i) i[[1]])
		
		
		mask = !any(is.na(clusters))
		cv = clusters[mask]
		
		mskV = apply(cv, 1, mean)
		mskV = !is.na(mskV) & !is.infinite(mskV)
		#fit  = cascadeKM(cv[mskV, ], 4, 10)$results[2,]
		#minFit = which(diff(fit) > 0)[1] + 1
		#nclusters = which.max(fit[minFit:10]) + minFit - 1 + 3
		nclusters = 6
		print(nclusters)
		vclusters = matrix(NaN, dim(cv)[1])
		vclusters[mskV] = cascadeKM(cv[mskV,], nclusters, nclusters)[[1]]
		
		
		findClustRange <- function(cl) {
			#FUN <- function(i) list(mean(i) + c(-1,1)*sd(i))
			FUN <- function(i) list(quantile(i, c(0.33, 0.67)))
			index = vclusters == cl
			if (sum(index) == 1) val = list(cv[index, ], cv[index, ])
				else val = apply(cv[index, ], 2, FUN)
			return(val)
				
		}
		cols = sapply(1:as.numeric(nclusters), findClustRange)
		
		#cols = sapply(1:as.numeric(nclusters),
		#			  function(cl) apply(cv[vclusters == cl,], 2,
		#							 function(i) list(quantile(i, c(0.1, 0.9)))))
		
		cols = cbind(cols	, list(c(Productive = '#00FF00', Arid = '#FF00FF'),
								c(Drier = '#FFFF00', Wetter = '#0000FF'),
								c("More Ignitions" = '#FF0000',"Less Ignitions" = '#00FFFF'),
								c(Wilding = 0, Suppressing = 2)))
		
		findCol <- function(x, cols = NULL) {
			
			if (is.null(cols)) {
				cols = tail(x, 1)[[1]]
				x =  unlist(head(x, -1))
				x = matrix(x, 2)
			}
			nms = names(cols)
			if (is.numeric(cols[1])) cols = seq(cols[1], cols[2], length.out = 3)
				else cols =  make_col_vector(cols, ncol = 3, whiteAt0 = FALSE)
			
			wmax = apply(x >= 0, 2, all)
			wmin = apply(x <= 0, 2, all)
			y = rep(cols[2], length(x[1,]))
			z = rep('', length(x[1,]))
			ud= rep(1, length(x[1,]))
			y[wmax] = cols[1]
			y[wmin] = cols[3]
			z[wmax] =  nms[1]
			z[wmin] =  nms[2]
			ud[wmin] = 0
			ud[wmax] = 2
			return(c(y,z, ud))
		}
		
		cols = apply(cols, 1, findCol) 
	
		fireChange <- function(clst) {
			clst = clst[1:3]
			if (all(clst == 1)) return(0)
			if (any(clst == 2) && any(clst == 0)) return(1)
			if (any(clst == 2)) return(2)
			if (any(clst == 0)) return(3)
		}
	
		direction = tail(cols, nclusters)
		direction = apply(direction, 1, fireChange)
		cols = head(cols, - nclusters)
		labs = tail(cols, nclusters)
		labs = apply(labs, 1, paste.miss.blanks, collapse = " & ")
		labs[labs == ''] = 'No Change'
		cols = head(cols, nclusters)
		
		## mean cols		
		elim = cols[,4]
        cols = apply(cols[,1:3], 1, function(i) apply(col2rgb(i), 1, mean)) /255
		cols = hex(colorspace::RGB(cols[1,], cols[2,], cols[3,]))		
		
		cols = saturate(cols, 0.2)
		clusters = clusters[[1]]
		clusters[mask] = vclusters
		clusters[!mask] = NaN
		
		er = clusters
        for (i in 1:nclusters) er[clusters == i] = as.numeric(elim[i])
		
		cols[labs == "Wetter & Less Ignitions"] = "#006FDF"
		cols[labs == "Productive & Wetter"] = "#36E868"
		cols[labs == "Arid & Drier"] = "orange"
		plotClusters <- function() {
			plot_raster_from_raster(clusters, y_range = c(-60, 90),
									cols = cols,
									limits = seq(1.5, nclusters - 0.5),
									e = er + 1, limits_error = 1:3,
									ePatternRes = 40, ePatternThick = 0.2,
									ePointPattern = c(24, 0, 25), eThick = c(1.5, 0, 1.5),
									preLeveled = TRUE,
									aggregateFun = max, 
									add_legend = FALSE, quick = TRUE)
			mtext.units('Fire drivers', side = 3, line = -0.8, adj = 0.05)	
			#addLocPoints()	
			
			findReplaceSupp <- function(elimi, name, directioni) {
				if (any(elim == elimi)) {
					labs  =  unlist(strsplit(labs, paste("&", name)))
					elim[elim == elimi] = 1
					elim = c(elim, elimi)
					labs = c(labs, name)
					cols = c(cols, "#DFDFDF")
					direction = c(direction, directioni)
				}
				return(list(labs, elim, cols, direction))
			}
			
			c(labs, elim, cols, direction) := findReplaceSupp(2, "Suppressing", 3)
			c(labs, elim, cols, direction) := findReplaceSupp(0, "Wilding", 2)
			
				
			addLegend <- function(index, txt, top = 15, left = -180) {
				text(txt, y = top + 1, x = left, adj = 0.0, font = 2)
				legend(left, top, labs[index], pch = 15, col = cols[index], pt.cex = 3, bty = "n")			
				col = make.transparent("black", 0.5)
				for (e in 1:3) {
					pch = c(24, 0, 25)[e]
					cex = rep(0, sum(index))
					cex[elim[index] == (e-1)] = c(0.42, 0, 0.42)[e]
					
					print(cex)
					for (i in left + c(-3, 0, 3)) for (j in  top + c(-3, 0, 3))
						legend(i, j, rep('', sum(index)), pch = pch,
							   col = col, pt.bg = col, pt.cex = cex, bty = "n")
				}
			}
			par(cex = 0.5)
			addLegend(direction == 1, 'Counteracting drivers', top = -40,   40)
			addLegend(direction == 2, 'Increasing burnt area', top = -40, -156)
			addLegend(direction == 3, 'Decreasing burnt area', top = -40,  -56)
			par(cex = 1.0)
		}
		
	plotClusters()
	dev.off.gitWatermark()
	
	#trendIndex = (prod(layer.apply(trends[2:5], function(i) 1 + abs(i[[1]])/10)) - 1) * 100
	#browser()
	#trendIndex = addLayer(trendIndex, mean(layer.apply(trends[-1], function(i) i[[2]])), mean(layer.apply(trends[-1], function(i) i[[3]])))
	#browser()
	writeRaster.gitInfo(trendIndex, 'outputs/TrendIndex.nc', overwrite = TRUE)
	
	png('figs/TrendMap.png', height = 7.2, width = 4.5, res = 300, units = 'in')
	layout.submap(rbind(c(1,1), 1, 2, 3, 3, 4, 5, 5), heights = c(1, 1, 0.6, 1, 1, 0.6, 1, 1))
	par(mar = rep(0,4), oma = c(0, 0, 0.5, 0))
	plot_Trends.local(1, labs = 'Normalised Trend in Burnt Area', plims = c(0.05))
	
	add_raster_legend2(cols = dfire_cols, limits = limits[[1]], ylabposScling = 1,
								   transpose = FALSE, plot_loc =  c(0.2, 0.9, 0.7, 0.85), 
								   add = FALSE, nx  = 1.25)
	plot.new()				  
	
	plot_Trend(trendIndex, 'Trend Hotspot index', 
			   limits = c(1, 2, 5, 10, 20),  prob_lims = prob_lims, 
			   cols = fire_cols, limits_error = c(0.1, 0.5, 1), scaling = 1)
		   
	add_raster_legend2(cols = fire_cols, limits = c(1, 2, 5, 10, 20), ylabposScling = 1,
								   transpose = FALSE, plot_loc =  c(0.2, 0.9, 0.7, 0.85), 
								   add = FALSE, nx  = 1.75)
	plot.new()
	plotClusters()
	dev.off()
	browser()
}

plot_raster_from_col_int <- function (rcol, rint, cols, col_limits, int_limits, ...) {
	
	zcol = cut_results(rcol * 10, col_limits )
	zint = cut_results(rint, int_limits)
	
	z = zcol + 1000 * zint
	
	limits = unique(z)
	colInt = matrix(NaN, ncol = 2, nrow = length(limits))
	for (i in 1:length(limits)) {
		index = z == limits[i]
		z[index] = i
		colInt[i, 1] = zcol[index][1]
		colInt[i, 2] = (zint[index][1] - 1)
	}
	
	col =  make_col_vector(cols, limits = col_limits)
	col = col[colInt[, 1]]	
	col = col2rgb(col)
	col = rgb2hsv(col)
	int = ( 1 - min(rgb2hsv(col2rgb(cols))[3,])) * colInt[,2] / max(colInt[,2])
	#int = colInt[,2] / max(colInt[,2])* 0.8
	col[3,] = 1-int 
	#col[2,] = (col[2,] + 1 - int) / 2
	col = hsv(col[1,], col[2,], col[3,])	
	plot_raster_from_raster(z, cols = col, 
        limits = (1:(length(limits)-1))  + 0.5, readyCut = TRUE, quick = TRUE)
        #...)

}

if (!is.True(dontPlot)) {
	#plotHotspots(trend12  , 'figs/trend12.png'  , limits = c(-1, -0.5, -0.1, -0.05, -0.01, 0.01, 0.05, 0.1, 0.5, 1))
	#plotHotspots(trend12F , 'figs/trend12F.png', #
	#			 limits = dfire_lims * 1000,
	#			 fire_limits = c(-1, -0.5, -0.1, -0.05, -0.01, 0.01, 0.05, 0.1, 0.5, 1), 
	#			 scaling = 120)
	
	plotHotspots(trend12FF, 'figs/trend12FFTest.png', limits = dfire_lims*100,
				 #fire_limits = c(-1, -0.5, -0.1, -0.05, -0.01, 0.01, 0.05, 0.1, 0.5, 1), 
				 fire_limits = c(-20, -10, -5, -2, -1,  1, 2, 5, 10, 20), 
				 lims4way = c(1, 10, 100), scaling = 10)
	#plotHotspots(trendFS  , 'figs/trendFS.png'  , limits = dfire_lims * 100)
}

	
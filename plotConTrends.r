#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()

grab_cache = TRUE

fig_fname = 'figs/Trends.png'

limitTitles = c('e) Fire', 'a) Fuel', 'b) Moisture', 'c) Ignitions', 'd) Suppression')

tempF1 = 'temp/limitations4trends'
tempF2 = 'temp/trendsFromLimitations'

tempFile <- function(fnames, extraName = '') {
	fnames = paste(fnames, extraName, sep = '')
	fnames = paste(fnames, c('fire', 'fuel', 'moisture', 'igntions', 'suppression'), '.nc', sep = '-')
}

dfire_lims = c(-5, -2, -1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1, 2, 5)/100
dfire_cols = c('#000033', '#0099DD', 'white', '#DD9900', '#330000')
prob_lims = c(0.001, 0.01, 0.05)

limit_cols = list(c('magenta', 'white', 'green'), c('yellow', 'white', 'blue'), c('cyan', 'white', 'red'), c('white', '#111111'))

#########################################################################
## Run model                                                           ##
#########################################################################
tempF1 = tempFile(tempF1)
lims  = runIfNoFile(tempF1, runLimFIREfromstandardIns, raw = TRUE, test = grab_cache)
lims[[2]] = lims[[2]] -  LimFIRE.fuel(0, param('fuel_x0'), param('fuel_k'))
fire = lims[[1]]


#########################################################################
## Find  Trends                                                        ##
#########################################################################
findTrend <- function(lno, smoothFun = running12, trendFUN = Trend) {
	lim = lims[[lno]]
	lims = lims[-lno]
	return(trendFUN(lim, smoothFun, lims))
}

findTrends <- function(lims, ...) lapply(1:length(lims), findTrend, ...)
findTrendNoFile <- function(FUN, trendFUN = Trend, ...)
	runIfNoFile(tempFile(...), findTrends, lims, FUN, trendFUN, test = grab_cache)

#########################################################################
## Simple Trends                                                       ##
#########################################################################	
trend12  = findTrendNoFile(running12, Trend, tempF2)

#########################################################################
## Trend removal                                                       ##
#########################################################################
trend12F = findTrendNoFile(running12, removeTrend, tempF2, 'removeTrend')

## weigted by fire
sfire = sum(fire)
trend12FF = lapply(trend12F, function(i) i / sfire)


#########################################################################
## During fire season                                                  ##
######################################################################### 
findFireMonth <- function(yr) {
	mn = (1 + (yr -1) * 12):(yr*12)
	fmax = fire[[mn]]
	return(which.max(fmax))
}
nyrs = (nlayers(fire)/12)
fireMonths = layer.apply(1:nyrs, findFireMonth)

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

trendFS = findTrendNoFile(fireSeasonLim, Trend, tempF2, 'season')

#########################################################################
## Plot trends                                                         ##
######################################################################### 
plotHotspots <- function(trends, figName, limits = dfire_lims, lims4way = NULL, ...) {
	
	png(figName, height = 10, width = 8.5, units = 'in', res = 300)
		layout(rbind(2:3, 4:5, 6, c(1,7), 8), heights = c(1,1,0.3, 1, 1.3))
		par(mar = rep(0,4))

		hotspots = mapply(plot_Trend, trends, limitTitles, MoreArgs = list(cols = dfire_cols, limits = limits, ...))[-1]
		
		add_raster_legend2(cols = dfire_cols, limits = limits, ylabposScling = 2,
								   transpose = FALSE, plot_loc =  c(0.2, 0.9, 0.85, 0.99), 
								   add = FALSE, nx  = 1.75)
		

		hotspots = lapply(hotspots, abs)	
		all = layer.apply(hotspots, function(i) i)
		if (is.null(lims4way)) lims4way = quantile(all[], seq(0.25, 0.75, 0.25), na.rm = TRUE)
		plot_4wayLimit(hotspots, '', FALSE, cols =c("FF","BB","88","44"),
					   limits = lims4way, normalise = FALSE, ePatternRes = 30, ePatternThick = 0.35)	
		mtext('f) Hotspots')
		
		clusters = layer.apply(trends[-1], function(i) i[[1]])
		
		
		mask = !any(is.na(clusters))
		cv = clusters[mask]

		fit = cascadeKM(cv, 1, 10)
		nclusters = which.max(fit$results[2,])
		vclusters = cascadeKM(cv, nclusters, nclusters)[[1]]
		
		cols = sapply(1:as.numeric(nclusters), function(cl) apply(cv[vclusters == cl,], 2, mean))
		cols = cbind(cols, list(c(Productive = '#00FF00', Arid = '#FF00FF'),
								c(Dry = '#FFFF00', Wet = '#0000FF'),
								c("More Ignitions" = '#FF0000',"Less Ignitions" = '#00FFFF'),
								c(Wild = "#000000", Supressed = "#FFFFFF")))
		
		findCol <- function(x, cols = NULL) {
			if (is.null(cols)) {
				cols = tail(x, 1)[[1]]
				x =  unlist(head(x, -1))
			}
			nms = names(cols)
			if (!is.numeric(cols[1])) cols =  make_col_vector(cols, ncol = 3, whiteAt0 = FALSE)
			
			y = rep(cols[2], length(x))
			z = rep('', length(x))
			y[which.max(x)] = cols[1]
			y[which.min(x)] = cols[3]
			z[which.max(x)] =  nms[1]
			z[which.min(x)] =  nms[2]
			return(c(y,z))
		}		
		
		cols = apply(cols, 1, findCol) 
		labs = tail(cols, nclusters)
		labs = apply(labs, 1, paste.miss.blanks, collapse = " & ")
		labs[labs == ''] = 'No Change'
		cols = head(cols, nclusters)
		
		## mean cols		
		cols = apply(cols, 1, function(i) apply(col2rgb(i), 1, mean)) /255
		cols = hex(colorspace::RGB(cols[1,], cols[2,], cols[3,]))
		
		cols = saturate(cols, 0.2)
		clusters = clusters[[1]]
		clusters[mask] = vclusters
		
		plot_raster_from_raster(clusters, y_range = c(-60, 90), cols = cols,
							   limits = seq(1.5, nclusters - 0.5), add_legend = FALSE, quick = TRUE)
							   
		legend("left", labs, pch = 19, col = cols)
	dev.off.gitWatermark()
}

#plotHotspots(trend12  , 'figs/trend12.png'  , limits = dfire_lims)
#plotHotspots(trend12F , 'figs/trend12F.png' , limits = dfire_lims, scaling = 1)
plotHotspots(trend12FF, 'figs/trend12FF.png', limits = dfire_lims*2000, lims4way = c(1, 10, 100), scaling = 100)
#plotHotspots(trendFS  , 'figs/trendFS.png'  , limits = dfire_lims * 100)



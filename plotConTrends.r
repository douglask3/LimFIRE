#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()

grab_cache = TRUE

fig_fname = 'figs/Trends.png'

limitTitles = c('a) Fuel', 'b) Moisture', 'c) Ignitions', 'd) Suppression')

tempF1 = 'temp/limitations4trends'
tempF2 = 'temp/trendsFromLimitations'

tempFile <- function(fnames, extraName = '') {
	fnames = paste(fnames, extraName, sep = '')
	fnames = paste(fnames, c('fire', 'fuel', 'moisture', 'igntions', 'suppression'), '.nc', sep = '-')
}

dfire_lims = c(-5, -2, -1, -0.5, -0.2, -0.1, 0, 0, 0.1, 0.2, 0.5, 1, 2, 5)/100
dfire_cols = c('#000033', '#0099DD', 'white', '#DD9900', '#330000')
prob_lims = c(0.001, 0.01, 0.05)

limit_cols = list(c('magenta', 'white', 'green'), c('yellow', 'white', 'blue'), c('cyan', 'white', 'red'), c('white', '#111111'))
#########################################################################
## Run model                                                           ##
#########################################################################
tempF1 = tempFile(tempF1)
lims  = runIfNoFile(tempF1, runLimFIREfromstandardIns, test = grab_cache)

fire = lims[[1]]
lims = lims[-1]

findTrend <- function(lno, smoothFun = running12) {
	lim = lims[[lno]]
	lims = lims[-lno]
	return(Trend(lim, smoothFun, lims))
}

findTrends <- function(lims, ...) lapply(1:length(lims), findTrend, ...)
findTrendNoFile <- function(FUN, ...)
	runIfNoFile(tempFile(...)[-1], findTrends, lims, FUN, test = grab_cache)
	
running12fire <- function(x, ys, ...) {
	ys = layer.apply(1:nlayers(ys[[1]]), function(i) ys[[1]][[i]] + ys[[2]][[i]] + ys[[3]][[i]])
	x = x * ys
	return(running12(x, ...))
}
trend12  = findTrendNoFile(running12, tempF2)
trend12F = findTrendNoFile(running12fire, tempF2, 'fireWeighted')

sfire = mean(fire)*12
trend12FF = lapply(trend12F, function(i) i / sfire)
 
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

trendFS = findTrendNoFile(fireSeasonLim, tempF2, 'season')

plotHotspots <- function(trends, figName, limits = dfire_lims) {
	
	png(figName, height = 7, width = 8.5, units = 'in', res = 300)
		layout(rbind(1:2, 3:4, 5, 6), heights = c(1,1,0.3, 1.3))
		par(mar = rep(0,4))

		hotspots = mapply(plot_Trend, trends, limitTitles, MoreArgs = list(cols = dfire_cols, limits = limits))
		
		add_raster_legend2(cols = dfire_cols, limits = limits, ylabposScling = 2,
								   transpose = FALSE, plot_loc =  c(0.2, 0.9, 0.85, 0.99), 
								   add = FALSE, nx  = 1.75)
		

		hotspots = lapply(hotspots, abs)	
		all = layer.apply(hotspots, function(i) i)
		lims4way = quantile(all[], seq(0.25, 0.75, 0.25), na.rm = TRUE)
		plot_4wayLimit(hotspots, '', FALSE, cols =c("FF","BB","88","44"),
					   limits = lims4way, normalise = FALSE, ePatternRes = 30, ePatternThick = 0.35)	
		mtext('e) Hotspots')
	dev.off.gitWatermark()
}

plotHotspots(trend12  , 'figs/trend12.png'  , limits = dfire_lims)# * 100)
plotHotspots(trend12F , 'figs/trend12F.png' , limits = dfire_lims)# * 100, limits = dfire_lims/20)
plotHotspots(trend12FF, 'figs/trend12FF.png', limits = dfire_lims * 100)#, limits = dfire_lims/20)
plotHotspots(trendFS  , 'figs/trendFS.png'  , limits = dfire_lims * 100)

######################
## cfg				##
######################
library(rgdal)
tempFile4Eco = 'temp/files4EcosystemPlot2.Rd'

nMnths = 168

ecoFile = "data/official_teow/official"

nsanmples = 10000

limits = c(-50, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 50)/100
dfire_cols = c('#000033', '#0099DD', 'white', '#DD9900', '#330000')

######################
## open data		##
######################
if (!file.exists(tempFile4Eco)) {
	dontPlot = TRUE
		source("plotConTrends.r")
		source("plotVariableTrends.r")
	dontPlot = FALSE
	fireTrend = trends[['fire']]
	fireMean  = varMns[['fire']]
	
	
	## Read in the ecoregion shapefile (located in R's current working directory)
	teow = readOGR(dsn = ecoFile, layer = "wwf_terr_ecos")

	## Set up a raster "template" to use in rasterize()
	ext  = extent (-180, 180, -90, 90)
	xy   = abs(apply(as.matrix(bbox(ext)), 1, diff))
	n    = 2
	r    = raster(ext, ncol=xy[1]*n, nrow=xy[2]*n)

	## Rasterize the shapefile
	biome = rasterize(teow, r, 'BIOME')
	
	save(lims, trend12FF, fireTrend, fireMean, biome, file = tempFile4Eco)
} else load(tempFile4Eco)
browser()
##########################
## Extract and plot		##
##########################
mnthScaler =  nMnths/2
lmask = !is.na(trend12FF[[1]][[1]])
biome[is.na(biome)] = 0
convert2StartEnd <- function(mn, tr, x, biomeN = NULL) {
	if (!is.null(biomeN)) 
		lmask = biome == biomeN	
	mn[!lmask] = NaN
	tr[!lmask] = NaN
	start = mn - tr[[1]] * mnthScaler
	end   = mn + tr[[1]] * mnthScaler
	
	plotBound <- function(r, xi) {
		quants = quantile(r, probs = c(0.1, 0.25, 0.5, 0.75, .9), na.rm = TRUE)
		
		if (quants[1] == 0) {
			quants = quants[quants > 0]
			arrows(xi, quants[1], xi, 0.003)
		} else test0 = FALSE
		
		quants = rev(unique(quants))	
		
		xis = rep(xi, length(quants))
		lines(xis, quants, lwd = 3)
		points(xis, quants, pch = 16)
		
		out = r
		#out[] = 0.0
		#out[!lmask] = NaN
		
		#for (qu in quants) out[r < qu] = out[r < qu] + 1
		return(list(out, quants))
	}
	xi = x + 0.25 * c(-1, 1)
	#c(start, qustart) := plotBound(start, xi[1])
	#c(  end, quend  ) := plotBound(  end, xi[2])
	plotBound(start, xi[1])
	plotBound(  end, xi[2])
	
	lenMask = raster.sum(lmask)
	if (nsanmples > lenMask) nsanmples = lenMask
	index =  sample(1:lenMask, nsanmples, replace = FALSE)
	
	startV = start[lmask][index]
	endV   =   end[lmask][index] 
	trendV = tr[[1]][lmask][index] / mn[lmask][index]
	
	z = cut_results(trendV * 12 * 14, limits)
	cols =  make_col_vector(dfire_cols, limits = limits)
	cols = make.transparent(cols, 0.9 * nsanmples / 10000)[z]
	
	mapply(function(st, ed, col) lines(xi, c(st, ed), col = col), 
			startV, endV, cols)
	#index = sample(
	
	return()
	browser()
	
	count = sapply(1:raster.max(start), function(i) sapply(1:raster.max(end),
				   function(j,i) raster.sum(start == i & end == j), i))
	
	nqus = dim(count)
	
	midQus <- function(qus, i) {
		if (length(qus) == nqus[i]) qustart = c(qus, 0)
		out = qus[1:(length(qus)-1)] + diff(qus)/2
		return(out)
	}
	
	qustart = midQus(qustart, 1)
	quend   = midQus(quend  , 2)
	cols = c('red', 'orange', 'green', 'blue')[1:nqus[1]]
	plotLine <- function(i, j, ...) lines(xi, c(qustart[i], quend[j]), lwd = 4 * (count[i,j]/sum(count))^0.1, ...) 
	#plotLine <- function(i, j) browser()
	lapply(1:nqus[2], function(j) mapply(plotLine, 1:nqus[1], col = cols, MoreArgs = list(j = j)))
	
	
}

 raster.max <- function(r, na.rm = TRUE, ...) max(values(r), na.rm =  na.rm, ...)
 raster.sum <- function(r, na.rm = TRUE, ...) sum(values(r), na.rm =  na.rm, ...)

plot(c(0, 10), c(0.001, 100), type = 'n', log = 'y')
for (i in 1:10) convert2StartEnd(fireMean, fireTrend, i, i)
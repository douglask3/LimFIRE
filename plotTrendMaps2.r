source("cfg.r")
source("mainDriversMap.r")
source("~/work/benchmarkmetrics/benchmarkMetrics/R/standard.round.r")
graphics.off()

limFnames = c('Burnt_Area', 'Fuel', 'Moisture', 'Igntions', 'Suppression')

figName = 'figs/TrendMaps'

trend_lims = c(-5, -2, -1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1, 2, 5) 
index_lims = list(c(-10, -5, -2, -1, 1, 2, 5, 10),
				  c(0, 1, 2, 5, 10, 20))


grab_cache = TRUE

loadData4Ecosystem_analysis()

plotControl <- function(trd, nme, sc = 1, limits = trend_lims, cols = dfire_cols, add_legend = FALSE) {
	plotStandardMap(mean(trd) * sc, '', limits = limits, cols =  cols, 
					e = sd.raster(trd), ePatternRes = 30, ePatternThick = 0.2, limits_error = c(1/10, 1/2),
					add_legend = add_legend)
	mtext(nme, adj = 0.02, line = -1.2, cex = 0.9)
}

##########################################
## Plot trends in controls              ##
##########################################
fname = paste(figName, 'controls.png', sep = '-')
png(fname, height = 4, width = 8, units = 'in', res = 300)
	layout(rbind(1:2, 3:4, c(5, 5)), heights = c(1,1, 0.3))
	
	par(mar = rep(0, 4))
	mapply(plotControl, trend12FF[-1], limFnames[-1], 1)

	plot.new()	
	add_raster_legend2(dfire_cols, trend_lims, dat = trend12FF[[2]][[1]],
					   transpose = FALSE, plot_loc = c(0.25, 0.75, 0.75, 0.9), ylabposScling=0.85)
	mtext(cex = 0.8, side = 1, '% change in area burnt', line = -1)
dev.off()

grabFirst <- function(lr) layer.apply(lr, function(r) r[[1]])
trendIndex = list(grabFirst(trendIndex1), grabFirst(trendIndex2))
trendIndex = lapply(trendIndex, function(i) {i[is.infinite(i)] = NaN; i})

##########################################
## Trends in burnt area and fire regime ##
##########################################
fname = paste(figName, 'trendIndicies.png', sep = '-')
png(fname, height = 4 * 3.3/2.3, width = 4, units = 'in', res = 300)
	
	par(mar = rep(0, 4), mfcol = c(3, 1), oma = c(10, 0, 0, 0))
	mapply(plotControl, trendIndex, c('Controls removed', 'fire regime shift'), 
						limits = index_lims, cols = list(dfire_cols, fire_cols),
						sc = 100/14, add_legend = TRUE)
	

##########################################
## Plot attribution map                 ##
##########################################
	trend = trendIndex[[2]]	
	mask  = mean(trend) > 0.5 & !is.na(trend[[1]])	
	
	controlTrendsLoc <- function(cntr) {
		cntr[[1]][!mask] = NaN
		out = cntr[[1]]
		out[] = 0
		cntr = quantile.raster(cntr, c(0.1, 0.9))
		out = sum(cntr > 0)-1
		
		return(out)
	}
	
	controls = layer.apply(trend12FF[-1], controlTrendsLoc)
	
	combinations = c()
	id = c(-1, 0, 1)
	
	cols = rbind(c('#00FF00', '#FF0000'),
		         c('#FFFF00', '#0000FF'),
		         c('#FFFFFF', '#000000'),
			     c(0        , 2))
	
	colIndex <- function(i) {
		if (i == -1) return(2)
		if (i ==  0) return(NaN)
		if (i ==  1) return(1)
	}
	
	nID = 0
	tmap = controls[[1]]
	tmap[] = 0
	for (l in id) for (i in id) for (j in  id) for (k in id)  {
		nID = nID + 1
		test = (controls[[1]] == i) + (controls[[2]] == j) + (controls[[3]] == k) + (controls[[4]] == l)
		test = test == 4
		tmap[test] = nID
		
		num = sum(area(test)[test])
		
		colsi = c(cols[1,colIndex(i)], cols[2, colIndex(j)], cols[3, colIndex(k)])
		colsi = colsi[!is.na(colsi)]
		if (length(colsi) == 0) colsi = 'grey'
		
		colsi = apply(col2rgb(colsi), 1, mean)/255
		colsi = colorspace::RGB(colsi[1], colsi[2], colsi[3])
		
		row = c(nID, i, j, k, l, num,  (hex(colsi)), cols[4, colIndex(l)])
		
		combinations = rbind(combinations, row)		
	}

	plotStandardMap(tmap, '', limits = (1:nrow(combinations)) - 0.5, cols  = c('white', combinations[,7]), add_legend = FALSE)
	
	addLegend <- function(f, m, title, x, y) {
		test = which(combinations[,2] == f & combinations[,3] == m)
		test = test[1:(length(test)/3)]
		cols = combinations[test,7]
		
		pc = combinations[test, 6]
		tot = sum(as.numeric(combinations[, 6]))
		
		title = paste(title, ' -',  round(100 * sum(as.numeric(pc)) / tot, 0), '%', sep = '')
		
		pc = as.character(standard.round(100 * as.numeric(pc) / tot, 1))
		
		pc[as.numeric(pc) < 0.1] = '0.0'
		pc[nchar(pc) == 1] = paste(pc[nchar(pc) == 1], '.0', sep = '')
		
	
		legend(x, y, c('More ignitions      ', '', 'Less ignitions      '),
			   col = cols, pch = 15, pt.cex = 2.5,
			   xpd = NA, title = title, y.intersp = 1.5)
		
		legend(x, y, c(pc), bty = 'n', adj = 1.33 * 0.67, title = ' ', cex = 0.67,
			   xpd = NA, y.intersp = 1.5/0.63,
			   pt.cex = 2, col = make.transparent("white", 1.0))
	}
	addLegend(1,  1, "Productive, Dry", -180, -60)
	addLegend(1, -1, "Productive, Wet", -80, -60)
	
	addLegend(-1, -1, "Arid, Wet", 20, -60)
	addLegend(-1,  1, "Arid, Dry", 120	, -60)
	
	
	addLegend(1, 0, "Productive", -180, -120)
	addLegend(-1, 0, "Arid", -80, -120)
	addLegend(0, 1, "Dry", 20, -120)
	addLegend(0, -1, "Wet", 120, -120)
dev.off()

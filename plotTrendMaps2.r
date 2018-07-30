source("cfg.r")
source("~/work/benchmarkmetrics/benchmarkMetrics/R/standard.round.r")
graphics.off()

limFnames = c('Burnt_Area', 'Fuel', 'Moisture', 'Igntions', 'Suppression')

figName = 'figs/TrendMaps'

trend_lims = c(-50, -40, -30, -20, -10, -5, 5, 10, 20, 30, 40, 50) 
index_lims = list(c(-10, -5, -2, -1, 1, 2, 5, 10),
				  c(0, 10, 20, 30, 40, 50, 60, 70, 80))


grab_cache = TRUE

loadData4Ecosystem_analysis()

plotControl <- function(trd, nme, sc = 1, limits = trend_lims, cols = dfire_cols, add_legend = FALSE, units = '', ...) {
	plotStandardMap(mean(trd) * sc, '', limits = limits, cols =  cols, 
					e = sd.raster(trd), ePatternRes = 30, ePatternThick = 0.2, limits_error = c(1/10, 1/2),
					add_legend = add_legend, ...)
	mtext(nme, adj = 0.02, line = -1.2, cex = 0.9)
	mtext(cex = 0.8 * 0.8, side = 1, units, line = -2.75, adj = 0.58)
}

##########################################
## Plot trends in controls              ##
##########################################
fname = paste(figName, 'controls.png', sep = '-')
png(fname, height = 4, width = 8, units = 'in', res = 300)
	layout(rbind(1:2, 3:4, c(5, 5)), heights = c(1,1, 0.3))
	
	par(mar = rep(0, 4))
	
	mapply(plotControl, trend12FF[-1], limFnames[-1], 100)

	plot.new()	
	add_raster_legend2(dfire_cols, trend_lims, dat = trend12FF[[2]][[1]], srt = 0,
					   transpose = FALSE, plot_loc = c(0.25, 0.75, 0.75, 0.9), ylabposScling=1.5, oneSideLabels = NA)
	mtext(cex = 0.8, side = 1, '% change in area burnt', line = -2.5)
dev.off()

grabFirst <- function(lr) layer.apply(lr, function(r) r[[1]])
trendIndex = list(grabFirst(trendIndex1), grabFirst(trendIndex2))
trendIndex = lapply(trendIndex, function(i) {i[is.infinite(i)] = NaN; i})
trendIndex[[2]][is.na(trendIndex[[1]][[1]])] = NaN
##########################################
## Trends in burnt area and fire regime ##
##########################################
fname = paste(figName, 'trendIndicies.png', sep = '-')
png(fname, height = 1.1 * 4 * 3.3/2.3, width = 4, units = 'in', res = 300)
	
	par(mar = rep(0, 4), mfcol = c(3, 1), oma = c(6, 0, 0, 0))
	mapply(plotControl, trendIndex, c('Normalised trend in burnt area', 'Shift in fire regime'), 
						units = c('% change in area burnt', '% shift in controls'),
						limits = index_lims, cols = list(dfire_cols, fire_cols),
						sc = c(100/14, 100), add_legend = TRUE, oneSideLabels = NA, ylabposScling=1.1)
	

##########################################
## Plot attribution map                 ##
##########################################
	trend = trendIndex[[2]]
	mask  = mean(trend) > median(trend[], na.rm = TRUE) & !is.na(trend[[1]])	
	
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
			     c(1        , -1))
	
	colIndex <- function(i) {
		if (i == -1) return(2)
		if (i ==  0) return(NaN)
		if (i ==  1) return(1)
	}
	
	nID = 0
	tmap = controls[[1:2]]
	tmap[] = 0
	for (l in id) for (i in id) for (j in  id) for (k in id)  {
		nID = nID + 1
		test = (controls[[1]] == i) + (controls[[2]] == j) + (controls[[3]] == k) + (controls[[4]] == l)
		test = test == 4
		tmap[[1]][test] = nID
		tmap[[2]][test] = as.numeric(cols[4, colIndex(l)])
		
		num = sum(area(test)[test])
		
		colsi = c(cols[1,colIndex(i)], cols[2, colIndex(j)], cols[3, colIndex(k)])
		colsi = colsi[!is.na(colsi)]
		if (length(colsi) == 0) colsi = 'grey'
		
		colsi = apply(col2rgb(colsi), 1, mean)/255
		colsi = colorspace::RGB(colsi[1], colsi[2], colsi[3])
		
		row = c(nID, i, j, k, l, num,  (hex(colsi)), cols[4, colIndex(l)])
		
		combinations = rbind(combinations, row)		
	}
	
	plotStandardMap(tmap[[1]], '', limits = (1:nrow(combinations)) - 0.5, cols  = c('white', combinations[,7]),
					e = tmap[[2]] + 2, e_lims = 1:3,  ePatternRes = 50, ePatternThick = 0.2,
								ePointPattern = c(25, 0, 24), eThick = c(1.5, 0, 1.5),
								preLeveled = TRUE, add_legend = FALSE)
	mtext('Drivers', adj = 0.02, line = -1.2, cex = 0.9)
	
	addLegend <- function(f, m, title, x, y) {
		test = which(combinations[,2] == f & combinations[,3] == m)
		test = test[1:(length(test)/3)]
		cols = combinations[test,7]
		
		pc = combinations[test, 6]
		tot = sum(as.numeric(combinations[, 6]))
		
		title = paste(title, ' (',  round(100 * sum(as.numeric(pc)) / tot, 0), '%)', sep = '')
		
		pc = as.character(standard.round(100 * as.numeric(pc) / tot, 1))
		
		pc[as.numeric(pc) < 0.1] = '0.0'
		pc[nchar(pc) == 1] = paste(pc[nchar(pc) == 1], '.0', sep = '')
		
		cexLeg = 2/3
		legend(x, y, c('More ignitions          ', '', 'Less ignitions          '),
			   col = cols, pch = 15, pt.cex = 2.5 * cexLeg, cex = cexLeg,
			   xpd = NA, title = title, y.intersp = 1.5 * cexLeg, box.col = make.transparent("black", 0.0))
		
		legend(x-0.1, y, c(pc), bty = 'n', adj = 1.2 * cexLeg, title = ' ', cex = 0.67 * cexLeg,
			   xpd = NA, y.intersp = 1.5/0.63 * cexLeg,
			   pt.cex = 2, col = make.transparent("white", 1.0))
	}
	
	legend(-173, -8, legend = c('Increased\n(less fire)', 'Decreased\n(more fire)'), pch = c(25, 24), cex = 2/3, title = 'Suppression', box.col =  make.transparent("black", 1.0), horiz = TRUE) 
	addLegend(1, -1, "Productive, Wet", -160, -44)
	addLegend(-1, 1, "Sparse, Dry",    -160, -80)
	text(-166, -80, 'Counteracting drivers', srt = 90, xpd = NA, cex = 0.8)
	
	addLegend(1,  1, "Productive, Dry", -50, -44)
	addLegend(0, 1, "Dry", 19, -44)
	addLegend(1,  0, "Productive"     , 88, -44)
	text(-56, -78, 'Increase', srt = 90, xpd = NA, adj = 0, cex = 0.8)
	
	addLegend(-1, -1, "Sparse, Wet", -50, -80)
	addLegend(-1, 0, "Sparse", 19, -80)
	addLegend(0, -1, "Wet", 88, -80)
	text(-56, -114, 'Decrease', srt = 90, xpd = NA, adj = 0, cex = 0.8)
dev.off()

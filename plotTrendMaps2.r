source("cfg.r")
graphics.off()

limFnames = c('Burnt_Area', 'a) Fuel', 'b) Moisture', 'c) Igntions', 'd) Suppression')

figName = 'figs/TrendMaps'

trend_lims = seq(10, 70, 10)/10
trend_lims = c(-rev(trend_lims), trend_lims)
index_lims = list(c(-5, -4, -3, -2, -1, 1, 2, 3, 4, 5),
				  c(5, 10, 20, 30, 40, 50)/10)


grab_cache = TRUE

loadData4Ecosystem_analysis()

plotControl <- function(trd, nme, sc = 1, limits = trend_lims, cols = dfire_cols, add_legend = FALSE, units_title = '', ...) {
	plotStandardMap(mean(trd) * sc, '', limits = limits, cols =  cols, 
					e = sd.raster(trd), ePatternRes = 40, ePatternThick = 0.6, limits_error = c(1/10, 1/2),
					add_legend = add_legend, ...)
	mtext(nme, adj = 0.02, line = -0.5, cex = 0.9,)
	mtext(cex = 0.8 * 0.8, side = 1, units_title, line = -2.9, adj = 0.58)
}
#fire_cols = c('white', '#FFDD00', '#BB0000', '#220000')
##########################################
## Plot trends in controls              ##
##########################################

fname = paste(figName, 'controls.png', sep = '-')
png(fname, height = 4 * 7.2/8, width = 7.2, units = 'in', res = 300)
	layout(rbind(1:2, 3:4, c(5, 5)), heights = c(1,1, 0.3))
	
	par(mar = rep(0, 4), oma = c(0, 0, 1, 0))
	
	mapply(plotControl, trend12FF[-1], limFnames[-1], 100/14)

	plot.new()	
	add_raster_legend2(dfire_cols, trend_lims, dat = trend12FF[[2]][[1]], srt = 0, units = '% ~yr-1~', 
					   transpose = FALSE, plot_loc = c(0.2, 0.8, 0.6, 0.75), ylabposScling=1.5, oneSideLabels = NA,
                       extend_min = TRUE, extend_max = TRUE)
	#mtext(cex = 0.8, side = 1, 'change in area burnt/mean burnt area', line = -2.5)
dev.off()

grabFirst <- function(lr) layer.apply(lr, function(r) r[[1]])
trendIndex = list(grabFirst(trendIndex1), grabFirst(trendIndex3))
#trendIndex[[2]] = layer.apply(1:50, function(i) {	
#	trendIndex[[2]][[i]][trendIndex[[1]][[i]]<0] = -trendIndex[[2]][[i]][trendIndex[[1]][[i]]]; 
#	trendIndex[[2]][[i]]})
#trendIndex = lapply(trendIndex, function(i) {i[is.infinite(i)] = NaN; i})
#trendIndex[[2]][is.na(trendIndex[[1]][[1]])] = NaN
trendIndex[[2]] = layer.apply(1:50, function(i) sqrt( trend12FF[[2]][[i]]^2 + 
												trend12FF[[3]][[i]]^2 + 
												trend12FF[[4]][[i]]^2 +
												trend12FF[[5]][[i]]^2))/2
trendIndex[[1]] =  trend12FF[[1]]
##########################################
## Trends in burnt area and fire regime ##
##########################################
fname = paste(figName, 'trendIndicies.png', sep = '-')
png(fname, height = 7.2, width = 4 * 7.2/6.3, units = 'in', res = 300)
	
	par(mar = c(0.4, 0, 0, 0), mfcol = c(3, 1), oma = c(6, 0, 0.3, 0))
	mapply(plotControl, trendIndex, c('a) Normalised trend in burnt area', 'b) Shift in fire regime'), 
						units_title = c(' ', ' '), units = c('% ~yr-1~', '% ~yr-1~'), 
                        extend_max = c(TRUE, TRUE), extend_min = c(TRUE, FALSE),
                        labelss = list(NULL, c(0, index_lims[[2]])),
						limits = index_lims, cols = list(dfire_cols, fire_cols),
						sc = c(100/14, 100/14), add_legend = TRUE, oneSideLabels = NA, ylabposScling=1.1)
	
##########################################
## Plot attribution map                 ##
##########################################
	trend = trendIndex[[2]]
	#mask  = mean(trend) > median(trend[], na.rm = TRUE) & !is.na(trend[[1]])	
	mask  = mean(trend) > sqrt(1)/2 & !is.na(trend[[1]])	
	#mask = !is.na(trend[[1]])	
	controlTrendsLoc <- function(cntr) {
        fname = paste0('temp/', filename.noPath(cntr[[1]], noExtension=TRUE), 'switch.nc')
        fun <- function() {
            cntr[[1]][!mask] = NaN
            vcntr = cntr[mask]
            out = cntr[[1]]
            out[] = 0
            cntr = quantile.raster(cntr, c(0.05, 0.95))
            out = sum(cntr > 0)-1
            return(out)
		}
        out = runIfNoFile(fname, fun)
		return(out)
	}
    
    controlRatio <- function(i, j) {
        print(i)
        print(j)
        print("===")
        if (i == j) {
            out = trend12FF[[1]][[1]][[1]]
            out[] =1
            return(out)
        }
        ci = trend12FF[-1][[i]]
        cj = trend12FF[-1][[j]]
        cr = ci/cj
        cr = abs(cr)
        qr = quantile.raster(cr, c(0.05, 0.95))
        return(qr[[1]])
    }
	
	controls_switch = layer.apply(trend12FF[-1], controlTrendsLoc)
    
    FUN <- function(j, i)
        runIfNoFile(paste0('temp/cntr_tr_ration_qr05',i, '-', j, '.nc') , controlRatio, i, j)
	control_rarios = lapply(1:4, function(i) layer.apply(1:4, FUN, i))
    control_rarios = layer.apply(control_rarios, function(i) sum(i > 0.5))
    for (i in 1:4) controls_switch[[i]][control_rarios[[i]] <2] = 0
    controls = controls_switch
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
		
		num = sum(raster::area(test)[test])
		
		colsi = c(cols[1,colIndex(i)], cols[2, colIndex(j)], cols[3, colIndex(k)])
		colsi = colsi[!is.na(colsi)]
		if (length(colsi) == 0) colsi = 'grey'
		
		colsi = apply(col2rgb(colsi), 1, mean)/255
		colsi = colorspace::RGB(colsi[1], colsi[2], colsi[3])
		
		row = c(nID, i, j, k, l, num,  (hex(colsi)), cols[4, colIndex(l)])
		
		combinations = rbind(combinations, row)		
	}
	
	plotStandardMap(tmap[[1]], '', limits = (1:nrow(combinations)) - 0.5, cols  = c('white', combinations[,7]),
					e = tmap[[2]] + 2, e_lims = 1:3,  ePatternRes = 50, ePatternThick = 0.4,
								ePointPattern = c(25, 0, 24), eThick = c(1.5, 0, 1.5),
								preLeveled = TRUE, add_legend = FALSE, interior = FALSE)
	mtext('c) Drivers', adj = 0.02, line = -1.2, cex = 0.9)
	
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
	addLegend(1, -1, "Increased Fuel \n& Moisture", -160, -44)
	addLegend(-1, 1, "Decreased Fuel \n& Moisture",    -160, -80)
	text(-166, -80, 'Counteracting drivers', srt = 90, xpd = NA, cex = 0.8)
	
	addLegend(0, 1, "Decreased Moisture", 88, -80)
	addLegend(1,  1, "Increased Fuel\n& Decreased Moisture", 19, -44)
	addLegend(1,  0, "Increased Fuel"     , 88, -44)
	text(-56, -78, 'Increase', srt = 90, xpd = NA, adj = 0, cex = 0.8)
	
	addLegend(-1, 0, "Decreased Fuel", -50, -80)
	addLegend(-1, -1, " Decreased Fuel \n& Increased Moisture", 19, -80)
	addLegend(0, -1, "Increased Moisture", -50, -44)
	text(-56, -114, 'Decrease', srt = 90, xpd = NA, adj = 0, cex = 0.8)
dev.off()

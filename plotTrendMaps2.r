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
					e = sd.raster(trd), ePatternRes = 60, ePatternThick = 0.4, limits_error = c(1/10, 1/2),
					add_legend = add_legend, ...)

        
	mtext(nme, adj = 0.02, line = -0.5, cex = 1)
	mtext(cex = 0.8 * 0.8, side = 1, units_title, line = -2.9, adj = 0.58)
}
#fire_cols = c('white', '#FFDD00', '#BB0000', '#220000')
##########################################
## Plot trends in controls              ##
##########################################

fname = paste(figName, 'controls.png', sep = '-')
"png(fname, height = 4 * 7.2/8, width = 7.2, units = 'in', res = 300)
	layout(rbind(1:2, 3:4, c(5, 5)), heights = c(1,1, 0.3))
	
	par(mar = rep(0, 4), oma = c(0, 0, 1, 0))
	
	mapply(plotControl, trend12FF[-1], limFnames[-1], 100/14)

	plot.new()	
	add_raster_legend2(dfire_cols, trend_lims, dat = trend12FF[[2]][[1]], srt = 0, units = '% ~yr-1~', 
					   transpose = FALSE, plot_loc = c(0.2, 0.8, 0.6, 0.75), ylabposScling=1.5, oneSideLabels = NA,
                       extend_min = TRUE, extend_max = TRUE)
	#mtext(cex = 0.8, side = 1, 'change in area burnt/mean burnt area', line = -2.5)
dev.off()
"
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
fname = paste(figName, 'trendIndicies.pdf', sep = '-')
heights = c(1.966, 1.866, 1.866, 1.32)
pdf(fname, height = sum(heights), width = 7.10866)#4 * 7.2/6.3)#, units = 'in', res = 600)
        layout(rbind(1:2, 3:4, 5:6, 7), heights = heights)
	par(mai = c(0.2, 0, 0, 0), oma = c(0, 0, 1, 0), xpd = NA)
        
	mapply(plotControl, trendIndex, 
                            c('a) Normalised trend in burnt area', 'b) Shift in fire regime'), 
			    units_title = c(' ', ' '), units = c('% ~yr-1~', '% ~yr-1~'), 
                            extend_max = c(TRUE, TRUE), extend_min = c(TRUE, FALSE),
                            labelss = list(NULL, c(0, index_lims[[2]])),
			    limits = index_lims, cols = list(dfire_cols, fire_cols),
			    sc = c(100/14, 100/14), 
                            add_legend = TRUE, oneSideLabels = FALSE, ylabposScling=1.7)
	
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
    control_rarios = layer.apply(control_rarios, function(i) sum(i > 0.1))
    for (i in 1:4) controls_switch[[i]][control_rarios[[i]] != 4] = 0
    controls = controls_switch
    combinations = c()
    id = c(-1, 0, 1)
	
    cols = rbind(c('#00FF00', '#FF0000'),
		 c('#FFFF00', '#0000FF'),
		 c('#000000', '#FFFFFF'),
		 c(1        , -1))

    cols = rbind(c('#00FF00', '#990099'),
		 c('#AA8800', '#0000FF'),
		 c('#000000', '#FFFFFF'),
		 c(-1        , 1))
	
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
	test = (controls[[1]] == i) + (controls[[2]] == j) + 
               (controls[[3]] == k) + (controls[[4]] == l)
		
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
    test = combinations[,7] == '#E0E0E0'
    combinations[test,7] = '#999999'
    
    test = combinations[,2] == -1 & combinations[,3] ==  1
    combinations[test & combinations[,4] == -1, 7] = "#FF9999"
    combinations[test & combinations[,4] ==  0, 7] = "#FF0000"
    combinations[test & combinations[,4] ==  1, 7] = "#990000"

    test = combinations[,2] ==  1 & combinations[,3] == -1
    combinations[test & combinations[,4] == -1, 7] = "#77CCFF"
    combinations[test & combinations[,4] ==  0, 7] = "#0099DD"
    combinations[test & combinations[,4] ==  1, 7] = "#004488"

    test = combinations[,2] ==  0 & combinations[,3] == 1
    combinations[test, 7] = lighten(combinations[test, 7], 1.6, FALSE, TRUE)

    test = combinations[,2] ==  1 & combinations[,3] == 1
    combinations[test, 7] = lighten(combinations[test, 7], 2.5, FALSE, TRUE)

    test = combinations[,2] ==  1 & combinations[,3] == 0
    combinations[test, 7] = lighten(combinations[test, 7], 1.8, FALSE, TRUE)
    
    combs4plotting = list(list(c( 1, -1), c(-1,  1)),
                          list(c( 0,  1), c( 0, -1)),
                          list(c( 1,  1), c(-1, -1)),
                          list(c( 1,  0), c(-1,  0)))
    
    combs4test = apply(combinations[,1:3], 2, as.numeric)

    par(mai = c(0.1, 0, 0, 0))
    plot_combs <- function(combs, title) {
        tmapi = tmap[[1]]
        tmapi[tmapi > 1] = length(combinations[,7]) + 1
        for (i in combs) {
            cells_cold = combs4test[combs4test[,2] == i[1] & combs4test[,3] == i[2],1]
            cells_cold = as.numeric(cells_cold)
            
            for (j in cells_cold) tmapi[tmap[[1]] == j] = j
        }
        
        plotStandardMap(tmapi, '', limits = (1:(nrow(combinations)+1)) - 0.5, 
                        cols  = c('white', combinations[,7], '#999999'), #  
		        e = tmap[[2]] + 2, e_lims = 1:3,  
                        ePatternRes = 75, ePatternThick = 0.3,
                        ePointPattern = c(25, 0, 24), eThick = c(1.5, 0, 1.5), e_alpha = 0.6,
		        preLeveled = TRUE, add_legend = FALSE, interior = FALSE)
	mtext(title, adj = 0.02, line = -0.5, cex = 1.0)
    }
    mapply(plot_combs, combs4plotting, paste0(letters[3:6],') ',  c("Counteracting drivers", "Moisture drivers", "Amplifying drivers", "Fuel drivers")))
    par(mai = c(0.0, 0, 0, 0))
    

    addLegend <- function(f, m, title, x, y) {
	test = which(combinations[,2] == f & combinations[,3] == m)
	test1 = test[1:(length(test)/3)]
	cols = rev(combinations[test1,7])
		
	pc = combinations[test, 6]
        pc = as.numeric(pc[1:3])  + as.numeric(pc[4:6]) + as.numeric(pc[7:9])
	tot = sum(as.numeric(combinations[, 6]))
		
	#title = paste(title, ' (',  round(100 * sum(as.numeric(pc)) / tot, 0), '%)', sep = '')
		
	pc = as.character(standard.round(100 * as.numeric(pc) / tot, 1))
		
	pc[as.numeric(pc) < 0.1] = '0.0'
	pc[nchar(pc) == 1] = paste(pc[nchar(pc) == 1], '.0', sep = '')
		
	cexLeg = 1#2/3
         
        select_arrow <- function(i) {
            
            if (i ==  1) x = bquote(phantom() %up%     phantom())
            else if (i == -1) x = bquote(phantom() %down%   phantom())
            else         x = bquote(phantom() %~~%     phantom())
            
            return(x)
        }
        
        legend_main = bquote(paste(.(select_arrow(f)), 'fuel,', .(select_arrow(-m)), 'moisture'))
        
        legend = c(expression(paste(phantom() %up%   phantom(), "ignitions")),
                   expression(paste(phantom() %~~%   phantom(), "ignitions")),
                   expression(paste(phantom() %down% phantom(), "ignitions")))


	legend(x, y, legend,
	       col = cols, pch = 15, pt.cex = 3 * cexLeg, cex = cexLeg,
	       xpd = NA, title = ' ', y.intersp = 1.5 * cexLeg * 2/3, bty = 'n')
        
        xwidth = 50
        ywidth = 30
        lines(x + c(-0.05, 1, 1, -0.05, -0.05) * xwidth, y + c(0.0, 0.0, -1.10, -1.10, 0.0) * ywidth, xpd = NA)
		
	legend(x-0.0, y, rev(pc), bty = 'n', adj = 0.8 * cexLeg, title = ' ', cex = 0.67 * cexLeg,
	       xpd = NA, y.intersp = 1.5/0.63 * cexLeg * 2/3, text.col = c('white', 'white', 'black'),
	       pt.cex = 2, col = make.transparent("white", 1.0))
        
        text(paste0('',  round(sum(as.numeric(pc)), 0), '%'), 
             x = x + xwidth * 0.85, y = y - ywidth * 0.985, cex = cexLeg, xpd = NA)

        title = ''
        text(legend_main, x = x + xwidth*0.1, y = y - ywidth * 0.15, cex = cexLeg, font = 2, xpd = NA, adj = 0)
        return(cols[2])
    }
    plot(c(-160, 150), c(-120, -60), type = 'n', axes = FALSE, xlab = '', ylab = '')	
    legend(-175, -45, legend = c('Increased\n(decreased\nburning)', 'Decreased\n(increased\nburning)'), 
           bg = make.transparent("black", 1.0),
           pch = c(24, 25), cex = 1, box.col =  make.transparent("black", 1.0), horiz = TRUE) 
    text(-150, -50, 'Suppression', xpd = NA, cex = 1.2, adj = c(0.5, 0))

    cii = addLegend(1, -1, "Increased fuel \n& moisture", -82, -54)
    cdd = addLegend(-1, 1, "Decreased fuel \n& moisture", -82, -88.5)
    text(-56, -50, 'Counteracting\ndrivers', srt = 0, xpd = NA, cex = 1.2, adj = c(0.5, 0))
	
    csd = addLegend(0, 1, "Decreased moisture\n", -08, -54)
    csi = addLegend(0, -1, "Increased moisture\n", -08, -88.5)
    text(17, -50, 'Moisture drivers', srt = 0, xpd = NA, cex = 1.2)

    cid = addLegend(1,  1, "Increased fuel &\ndecreased moisture", 47, -54)
    cdi = addLegend(-1, -1, " Decreased fuel &\nincreased moisture", 47, -88.5)
    text(72, -50, 'Amplifying\ndrivers', srt = 0, xpd = NA, cex = 1.2, adj = c(0.5, 0))

    cis = addLegend(1,  0, "Increased fuel\n", 102, -54)
    cds = addLegend(-1, 0, "Decreased fuel\n", 102, -88.5)
    text(127, -50, 'Fuel drivers', srt = 0, xpd = NA, cex = 1.2)

    text(-14, -69, 'Increased\nburning', srt = 90, xpd = NA, adj = c(0.5, 0), cex = 1.2)
    text(-14, -103.5, 'Decreased\nburning', srt = 90, xpd = NA, adj = c(0.5, 0), cex = 1.2)

    xi = -130; xd = -150; yi = -97.5+10; yd = -97.5-10
    ys = mean(c(yi, yd)); xs = mean(c(xi, xd)); ye = 0.2*(yi - yd); xe = 0.2*(xi - xd)
    
    xps = c(xi,  xi,  xs,  xd,  xd,  xd,  xs,  xi)
    yps = c(ys,  yi,  yi,  yi,  ys,  yd,  yd,  yd)
    pid = c('f', 'c', 'd', 'e', 'f', 'c', 'd', 'e')
    
    lines(c(xps, xi), c(yps, ys), col = '#999999', lwd = 17)
    points(xps, yps, pch = 15, cex = 3.5,
           col = c(cis, cii, csi, cdi, cds, cdd, csd, cid))
    
    #points(xs, ys, pch = 19, cex = 5, col = "white")
    xd = xd - xe; xi = xi + xe
    yd = yd - ye; yi = yi + ye 
       
    Arrows(xd, ys, xi, ys)
    Arrows(xs, yd, xs, yi)
    Arrows(xi, ys, xd, ys)
    Arrows(xs, yi, xs, yd)

    text(xps, yps, pid, col = "white")
    
    xd = xd - xe * 1.5; xi = xi + xe * 1.3
    yd = yd - ye * 1.4; yi = yi + ye * 1.4

    text(xi, ys, expression(paste(phantom() %up%   phantom(), "fuel")), adj = 0)
    text(xd, ys, expression(paste(phantom() %down%   phantom(), "fuel")), adj = 1)

    text(xs, yi, expression(paste(phantom() %up%   phantom(), "moisture")), adj = c(0.5, 0))
    text(xs, yd, expression(paste(phantom() %down%   phantom(), "moisture")), adj = c(0.5, 1))
	
dev.off()

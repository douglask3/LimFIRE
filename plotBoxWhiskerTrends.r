source("cfg.r")
graphics.off()

figName = 'figs/boxyWhistery'
limFnames = c('Burnt_Area', 'Fuel', 'Moisture', 'Ignitions', 'Suppression')

ylog = FALSE
nsamples = 100

barCols = c("#CC8800", "#CC8800", "#6eff6e", "#0000CC" , "#480000"  , "#999999"       , "purple")
legLabs = c('Burnt\nArea', 'All\nControls', ' \nFuel'   , 'Moisture\n ', ' \nIgnitions', 'Suppression\n ', 'Fire\nRegime')

loadData4Ecosystem_analysis()

mask = !is.na(trend12FF[[1]][[1]])

#maskX = trend12F[[1]][[1]] > 1.3286
'trend12F[[1]][[1]][maskX] = 0.0
trend12FF[[1]][[1]][maskX] = 0.0'
#dat = c(trend12FF[[1]][[1]] * 10, lapply(trend12FF, function(i) i[[1]] * 10))
#dat[[7]] = (prod(layer.apply(dat[3:6], function(i) 1 + abs(i)/100)) - 1)* 100#abs(dat[[3]]) + abs(dat[[4]]) + abs(dat[[5]])

dat = lapply(1:5, function(con) layer.apply(ensamble, function(i) i[[con]][[1]]))
dat = c(dat,
		layer.apply(trendIndex1, function(i) i[[1]]),
		layer.apply(trendIndex2, function(i) i[[1]]))
dat[[1]] = dat[[1]]/100

dat = dat[c(1, 6, 2:5, 7)]

addboxPlot <- function(x, y, col, mask) {
	
	y = y[mask == 1] *14/100
	
	if (ylog) {
		test = y<0
		y0 = y
		y[!test] = y[!test]^exp(-1)
		y[test] = - (-y[test])^exp(-1)
	}
	
	quants = apply(y, 2, quantile, c(0.1, 0.9), na.rm = TRUE)
	qrange = apply(quants, 2, diff)
	lwd = 0.5 + 2.5 * (qrange - min(qrange)) / (max(qrange) - min(qrange))
	for (i in seq(1, ncol(y), 1)) {
		out = boxplot(y[,i], add = TRUE, at = x, axes = FALSE, 
					  col = make.transparent(col, 0.97), pch = 19, 
					  cex = 0.2, outline = ylog, 
					  border = make.transparent('black', 1.0))[[1]][,1]
		lines(x + c(-.15, .15), rep( out[3], 2), col = make.transparent('black', 0.9))
		lines(c(x, x), quants[,i], col = make.transparent('black', 0.98), lwd = lwd[i], xpd = TRUE)
	}
}

addAllDatPlot <- function(x, biomeN) {
	if (!is.null(biomeN)) mask = mask & any(layer.apply(biomeN, function(i) biome == i))
	x = 5 * (x + seq(-0.33, 0.33, length.out = 7))
	mapply(addboxPlot, x, dat, barCols, MoreArgs = list(mask))
}


addLegend <- function() {
	plot(c(0.7, 7.3), c(-1.15, 1.15), axes = FALSE, type = 'n', xlab = '', ylab = '')
	addLegItem <- function(x, col) {
		addBoxLeg <- function(dif) {
			boxplot(c(-0.4, 0.4)+ dif, at = x, add = TRUE, border = make.transparent('black', 1.0), col = make.transparent(col, 0.97), axes = FALSE, cex = 0.2, boxwex = 0.3)
			
			lines(c(x, x), c(-0.9 - dif, 0.9 + dif), lwd = 0.5 + (0.2 + dif) * 2.5 * 1.5, col = make.transparent('black', 0.98))
			lines(x + c(-0.07, 0.07), c(0.0,0.0)+ dif * 0.25, col = make.transparent('black', 0.9))
		}
		index = runif(50, -0.2, 0.2)
		for (i in 1:1) lapply(index,addBoxLeg)
	}

	mapply(addLegItem, 1:7, col = barCols)
	text(x = 1:7, y = -1.0, legLabs, cex = 0.8)

	addAxisLab <- function(y, lab) {
		axis(2, at = y + c(-0.15, 0.15), labels = c('', ''))
		axis(2, at = y, labels = lab, tick = FALSE, cex.axis = 0.75, las = 1)
	}
	addAxisLab(-0.9, '10%')
	addAxisLab(-0.4, '25%')
	addAxisLab(0.0, 'Median')
	addAxisLab(0.4, '50%')
	addAxisLab(0.9, '90%')
}

plotAllTheBoxesAndWhiskers <- function(fname = '', ylims = c(-5.3, 3), ytextPos = ylims[1] * 8) {
	figName = paste(figName, fname, ylog, '.png', sep ='-')
	png(figName, height = 7, width = 8.5, res = 300, unit = 'in')
		layout(rbind(c(1,1), c(2,1)), heights = c(1, 0.3), widths = c(0.45, 0.55))
		
			
		par(mar = c(0, 4, 0, 1))
		
		
		if (ylog) ylims = ylims * (0.5^exp(-1))
		else ylims = ylims / 10
		plot(c(4, 8 * 5 + 1), ylims, type = 'n', axes = FALSE, 
			 xlab = '', ylab = '% change in burnt area/limitation')
		
		labels = seq(-35, 30, 5)
		at = labels / 100
		
		axis(2, at = at, labels = labels)
		
		for (y in at) lines(c(-9E9, 9E9), c(y, y), lty = 2, col = "#CCCCCC")
	
		text(x = 5 * (1:8), y = ytextPos, adj = 1.0, labels = names(biomes), srt = 90, xpd = NA)
		
		mapply(addAllDatPlot, 1:8, biomes)
		
		par(mar = c(0.5, 4, 0, 0))
		addLegend()
		#plot.new()
		#if (ylog) pos = 'center' else pos = 'bottom'
		#legend(pos, legend = legLabs, pch = 15, pt.cex = 1.2, col =  'black', horiz = TRUE, bty = 'n')
		#legend(pos, legend = legLabs, pch = 15, col =  barCols[-1], horiz = TRUE, bty = 'n')
		#legend('center', legend = legLabs, lty = 1, col =  'black', horiz = TRUE, bty = 'n')
	dev.off()
}

if (ylog) ytextPos = -(0.5^exp(-1)) else ytextPos = -0.31
plotAllTheBoxesAndWhiskers(ytextPos = ytextPos)

dat = lapply(dat, abs)
if (ylog) ytextPos = -(0.02^exp(-1)) else ytextPos = -0.03
#plotAllTheBoxesAndWhiskers('absolute', -0.17, ytextPos)


#axis(2, at = c(-0.9, -0.5, 0.0, 0.5, 0.9), labels = c('10%', '25%', 'Median', '75%', '90%'))
source("cfg.r")


figName = 'figs/boxyWhistery'
limFnames = c('Burnt_Area', 'Fuel', 'Moisture', 'Igntions', 'Suppression')

nsamples = 100

barCols = c("#CC8800", "#CC8800", "#6eff6e", "#0000CC" , "#480000"  , "grey"       , "white")
legLabs = c('Burnt Area'        , 'Fuel'   , 'Moisture', 'Ignitions', 'Suppression', 'Overall Trend')

loadData4Ecosystem_analysis()
mask = !is.na(trend12FF[[1]][[1]])

maskX = trend12F[[1]][[1]] > 1.3286
trend12F[[1]][[1]][maskX] = 0.0
trend12FF[[1]][[1]][maskX] = 0.0
dat = c(trend12F[[1]][[1]] * 10, lapply(trend12FF, function(i) i[[1]] * 10))
dat[[7]] = (prod(layer.apply(dat[3:6], function(i) 1 + abs(i)/100)) - 1)* 100#abs(dat[[3]]) + abs(dat[[4]]) + abs(dat[[5]])


addboxPlot <- function(x, y, col, mask) {
	y = y[mask == 1] / 100
	test = y<0
	y0 = y
	y[!test] = y[!test]^exp(-1)
	y[test] = - (-y[test])^exp(-1)
	boxplot(y, add = TRUE, at = x, range = 5, axes = FALSE, col = col, pch = 19, cex = 0.2)
	
}

addAllDatPlot <- function(x, biomeN) {
	if (!is.null(biomeN)) mask = mask & any(layer.apply(biomeN, function(i) biome == i))
	x = 5 * (x + seq(-0.33, 0.33, length.out = 7))
	mapply(addboxPlot, x, dat, barCols, MoreArgs = list(mask))
}

plotAllTheBoxesAndWhiskers <- function(fname = '', plotMin = -1, ytextPos = -(0.7^exp(-1))) {
	figName = paste(figName, fname, '.png', sep ='-')
	png(figName, height = 6, width = 8.5, res = 300, unit = 'in')
		layout(1:2, heights = c(1, 0.2))
		par(mar = c(0, 4, 0, 1))
		plot(c(4, 8 * 5 + 1), (0.5^exp(-1)) * c(plotMin, 1), type = 'n', axes = FALSE, xlab = '', ylab = '% change in burnt area/limitation')
		
		labels = c(0.01, 0.05, 0.1, 0.5)
		at = labels^ exp(-1)
		at = c(-rev(at), 0, at)
		labels = c(-rev(labels), 0, labels) * 100

		axis(2, at = at, labels = labels)
		for (y in at) lines(c(-9E9, 9E9), c(y, y), lty = 2, col = "#CCCCCC")

		text(x = 5 * (1:8), y = ytextPos, adj = 0.0, labels = names(biomes), srt = 90, xpd = NA)

		mapply(addAllDatPlot, 1:8, biomes)
		par(mar = c(0, 0, 0, 0))
		plot.new()
		legend('center', legend = legLabs, pch = 15, pt.cex = 1.2, col =  'black', horiz = TRUE, bty = 'n')
		legend('center', legend = legLabs, pch = 15, col =  barCols[-1], horiz = TRUE, bty = 'n')
		#legend('center', legend = legLabs, lty = 1, col =  'black', horiz = TRUE, bty = 'n')
	dev.off()
}


plotAllTheBoxesAndWhiskers()
dat = lapply(dat, abs)
plotAllTheBoxesAndWhiskers('absolute', -0.1, 0.005)
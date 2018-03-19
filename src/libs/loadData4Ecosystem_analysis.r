loadData4Ecosystem_analysis <- function() {
	library(rgdal)
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
		biomeAssigned = biome
		biomeAssigned[] = NaN
		
		for (i in 2:length(biomes)) biomeAssigned[any(layer.apply(biomes[[i]], function(j) biome == j))] = i
		
		png('figs/biome.png', height = 5, width = 10, units = 'in', res = 300)
			#layout(rbind(1, 1:2, 2), heights = c(1, 0.7, 0.0001))
			par(mar = rep(0,4))
			plot_raster_from_raster(biomeAssigned, limits = 1.5:7.5, cols = biomesCols, quick = TRUE, add_legend = FALSE, y_range = c(-60, 90))	
			#plot.new()
			legend('bottomleft', legend = names(biomes)[-1], col = biomesCols[-1], pch = 15, bty = 'n', ncol = 2, cex = 1.0, pt.cex = 2) 
		dev.off() 	
		save(lims, trend12F,  trend12FF, fireTrend, fireMean, biomeAssigned, biome, varMns, trends,prob_lims, trendIndex,
			 file = tempFile4Eco)
	} else load(tempFile4Eco, envir = .GlobalEnv)
	
}

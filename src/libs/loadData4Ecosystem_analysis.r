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
		
		save(lims, trend12F,  trend12FF, fireTrend, fireMean, biome, varMns, trends,
			 file = tempFile4Eco)
	} else load(tempFile4Eco, envir = .GlobalEnv)
	
}

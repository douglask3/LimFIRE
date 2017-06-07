library(benchmarkMetrics)
library(gitBasedProjects)
library(raster)
library(ncdf4)
library(rasterExtras)
library(rasterPlot)
library(plotrix)
library(mapdata)
library(mapplots)
data(worldHiresMapEnv)


setupProjectStructure(dirn = c("outputs", "data", "temp", "figs"))
sourceAllLibs('src/libs')
sourceAllLibs('src/LimFIRE')

years       = 2000:2014
clim_layers =  115:282

ml = c(31,28,31,30,31,30,31,31,30,31,30,31)

fire_cols = c("#FFFFFF", "#FFEE00", "#AA2200", "#330000")
fire_lims = c(0, 1, 2, 5, 10, 20, 50)

coefficants_file = 'data/params.csv'

drive_fname = c(alpha   = 'alpha',
                emc     = 'emc',
				Hr      = 'Hr',
				Tas     = 'Tas',
				Wet     = 'Wet', 
				Prc     = 'Prc',
                npp     = 'NPP',
                crop    = 'cropland',
                pas     = 'pasture',
                urban   = 'urban_area',
                popdens = 'population_density',
                Lightn  = 'lightning_ignitions',
				fire    = 'fire'
                )


nms = names(drive_fname)
drive_fname = paste(outputs_dir, drive_fname, min(years), '-', max(years), '.nc', sep = '')
names(drive_fname) = nms

#coefficants_file = paste(coefficants_file, gitVersionNumber(), '.csv', sep = '-')

try(memSafeFile.remove(), silent = TRUE)
memSafeFile.initialise('temp/tempGenerated')

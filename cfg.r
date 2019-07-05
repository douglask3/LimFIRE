library(benchmarkMetrics)
library(gitBasedProjects)
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs('../gitProjectExtras/gitBasedProjects/R/')

## uncomment on windows
#source('../gitProjectExtras/package_git2r.r')
#config(repository(), user.name="Douglas Kelley", user.email="douglas.i.kelley@gmail.com")
library(raster)
library(ncdf4)
library(rasterExtras)
library(rasterPlot)
library(plotrix)
library(mapdata)
library(mapplots)
library(ellipse)
library(vegan)
library(RcppEigen)
library(parallel)
library(snow)
library(reldist)
library(rgdal)
data(worldHiresMapEnv)


sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/')
sourceAllLibs('../rasterextrafuns/rasterExtras/R/')
setupProjectStructure(dirn = c("outputs", "data", "temp", "figs"))
sourceAllLibs('src/libs')
sourceAllLibs('src/LimFIRE')

years       = 2000:2014
clim_layers =  115:282

ml = c(31,28,31,30,31,30,31,31,30,31,30,31)

fire_cols = fire_cols = c("#FFFFFF", "#FFEE00", "#AA2200", "#330000")#c('white', '#FF9933', '#992200', '#220000')
dfire_cols = c('#000033', '#0099DD', 'white', '#DD9900', '#330000')
fire_lims = c(0, 0.1, 1, 2, 5, 10, 20, 50)

coefficants_file = 'data/params.csv'

drive_fname = c(alpha   = 'alpha',
				alphaMax= 'alpha_12monthMax',
                emc     = 'emc',
                emcMax  = 'emc_12monthMax',
				Hr      = 'Hr',
				Tas     = 'Tas',
				Wet     = 'Wet', 
				Prc     = 'Prc',
				Cld     = 'cld',
				Vap     = 'vap',
                npp     = 'NPP',
				bare    = 'bareground',
				grass   = 'nontree',
				tree    = 'treecover',
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

hlghtPnts = list("Desert"      = list( 14, 27.5, col = '#00AA66'),
                 "Rainforest"  = list(-61,   -3, col = '#0000DD'), 
				 "Savanna"     = list( 24,   10.5, col = '#660000'),
				 "Cropland"    = list( 83, 23.5, col = '#AAAA00'))

#coefficants_file = paste(coefficants_file, gitVersionNumber(), '.csv', sep = '-')

try(memSafeFile.remove(), silent = TRUE)
memSafeFile.initialise('temp/tempGenerated')

tempFile4Eco = 'temp/files4EcosystemPlot2-newEnsmebles12.Rd'
ecoFile = "data/official_teow/official"

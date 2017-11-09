################################################################################
## Load ascillary                                                             ##
################################################################################
files = list.files(dir, full.name = TRUE, recursive = TRUE)
grid  = raster(grid_file)

################################################################################
## load, process and output                                                   ##
################################################################################
make_variable <- function(var, fname_out, frac) {
    ## load
    files = files[grepl(var, files)]
    dat = stack(files)
	
    ## rerid to standard
    dat = raster::resample(dat, grid)
    dat = interpolate2monthly(dat)
	
	nlayersDat =  nlayers(dat)
	if (nlayersDat < 168) {
		extraLayers = 168 - nlayersDat
		nextraYrs =  ceiling(extraLayers/12)
		extraDat = dat[[(nlayersDat-11):nlayersDat]]
		extraDat = layer.apply(1:nextraYrs, function(i) extraDat)
		dat = addLayer(dat, extraDat[[1:extraLayers]])
	}
    ## Output
    comment[4] = var
    writeRaster.gitInfo(dat, fname_out,
                        comment = comment, overwrite = TRUE)
}

mapply(make_variable, variables, drive_fname[names(variables)])

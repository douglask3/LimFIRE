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

    ## Output
    comment[4] = var
    writeRaster.gitInfo(dat, fname_out,
                        comment = comment, overwrite = TRUE)
}

mapply(make_variable, variables, drive_fname[names(variables)])

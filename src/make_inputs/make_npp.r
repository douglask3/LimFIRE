################################################################################
## cfg                                                                        ##
################################################################################
## Libraries etc
source('cfg.r')
sourceAllLibs('src/weather/')

## paths and parameters
data_dir   = 'data/MOD17A3/'

inter_file_name = 'temp/MODISregridded.nc'

################################################################################
## make data                                                                  ##
################################################################################

files = list.files(data_dir, full.names = TRUE)

dat = layer.apply(files, raster)

bit_by_bit.aggregate <- function(...) bit_by_bit.FUN(aggregate, ...)

bit_by_bit.FUN <- function(FUN, dat, div = 10, ...) {
    dat_dims  = c(ncol(dat), nrow(dat))
    
    if (length(div) == 1) div = rep(div, 2)
    
    div_size0 = dat_dims / div
    div_size  = floor(div_size0)
    
    for (i in 1:2) while(div_size0[i] != div_size[i]) {
        div[i]       = div[i] + 1
        div_size0[i] = dat_dims[i] / div[i]
        div_size[i]  = floor(div_size0[i])
    }
    
    diffs     =  cbind(c(extent(dat)[1], diff(extent(dat)[1:2])),
                       c(extent(dat)[3], diff(extent(dat)[3:4])))
    
    div_size = div_size / dat_dims
    extent_range <- function(x, i) {
        r = c((x - 1), x) * div_size[i]
        if (r[2] > 1) r[2] = 1
        z = r * diffs[2, i] + diffs[1, i]
        return(z)
    }
    
    apply2bit <- function(i, j) {
        extent = extent(extent_range(i, 1), extent_range(j, 2))
        r = crop(dat, extent)
        r = FUN(r, ...)
        r = writeRaster(r, memSafeFile(), overwrite = TRUE)
        return(r)
    }
    
    merge_from_list <- function(ri) {
        r = ri[[1]]        
        for (i in ri) r = merge(r, i)
        r = writeRaster(r, memSafeFile(), overwrite = TRUE)
        return(r)
    }
    
    apply2y <- function(...) {
        r = lapply(1:div[1], apply2bit, ...)
        return(merge_from_list(r))
    }
    
    dat = lapply(1:div[2], apply2y)
    dat = merge_from_list(dat)
}

remove_nans_and_regrid <- function(dat) {
    #dat[dat == 65535] = 0
    dat = bit_by_bit.aggregate(dat, 9, 60)
    dat = writeRaster(dat, memSafeFile())
    return(dat)
}

dat = layer.apply(dat, remove_nans_and_regrid)
dat = writeRaster(dat, inter_file_name, overwrite = TRUE)

browser()

dat = raster::extend(dat,extent(-180, 180, -90, 90))

interpolate2monthly <- function(i) {
    print(i)
    monthly <- function(m) {
        print(m)
        w2 = 1 / m
        w1 = 1 - w2
        int = dat[[i]] *  w1 + dat[[i+1]] * w2
        int = writeRaster(int,  memSafeFile(), overwrite = TRUE)
        return(int)
    }
    layer.apply(1:12, monthly)
}

dat = layer.apply(1:(nlayers(dat)-1), interpolate2monthly)

comment = list('Data extracted from geotiff files' =
					'Source: http://www.ntsg.umt.edu/project/MOD17/')

writeRaster.gitInfo.time(dat, drive_fname['npp'],
                    comment = comment, overwrite = TRUE)

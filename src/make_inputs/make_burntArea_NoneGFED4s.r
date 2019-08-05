source("cfg.r")

files = paste0('data/', c('MODIS250_q_BA_regridded0.5.nc',
                          'MCD45.nc',
                          'meris_v2.nc', 
                          'GFED4.fBA.r0d5.1995.2013.nc'))
in_files = list.files('outputs/', full.names = TRUE)
in_files = in_files[grepl('2000-2014.nc', in_files)]
stMnth = c(7, 7, 55, -61)
newFiles = c(T, F, T, T)

make_brunt_area <- function(file, st, newFile) {
    dat = brick(file)
    ed = nlayers(dat) + st - 1
    if (ed > 168) browser()
    if (st < 0) {
        dat = dat[[-1:st]]
        st = 1
    }        

    sYr = 2000+ceiling(st/12)
    eYr = 2000+ceiling(ed/12)
    dir = paste0('outputs/yrs', sYr, '_', eYr, '/')
    makeDir(dir)

    file_end  = paste0(sYr, '-', eYr, '.nc')
    
    fire_file = gsub('data/', '', file)
    fire_file = sub('.nc', '', fire_file)
    fire_file = paste0(dir, fire_file, '-fire', file_end)
    
    writeRaster.gitInfo(dat, file = fire_file, overwrite = TRUE)

    croppedFile <- function(in_file) {
        out = brick(in_file)[[st:ed]]
        file_out = gsub('2000-2014.nc', file_end, in_file)
        file_out = paste0(dir, '/', tail(strsplit(file_out, '/')[[1]], 1))
        writeRaster.gitInfo(out, file = file_out, overwrite = TRUE)
    } 
    
    if (newFile) lapply(in_files, croppedFile)
    
}

mapply(make_brunt_area, files, stMnth, newFiles) 

################################################################################
## cfg                                                                        ##
################################################################################
## Libraries etc
source('cfg.r')
library(rhdf5)

dir = 'gfed'

files = list.files(paste(data_dir, dir, sep = '/'), full.names = TRUE)
files = files[grepl('GFED4.1s_', files)]

fileDate = c()

openHDFandConvert2Nc <- function(month, fname) {
	if (month < 10) month = paste('0', month, sep = '')
	print(fname)
	layer = paste('burned_area', month, 'burned_fraction', sep = '/')
	dat = h5read(fname, layer)
	dat = raster(t(dat))
	dat = aggregate(dat)
	extent(dat) = extent(c(-180, 180, -90, 90))
	H5close()
	
	writeRaster(dat, file = memSafeFile(), overwrite = TRUE)
	
	fDate = file.info(fname)$ctime
	fDate = as.character(fDate)
	names(fDate) = fname	
	fileDate <<- c(fileDate, fDate)
	
	return(dat)
}

dat = layer.apply(files, function(...)
				 layer.apply(1:12, openHDFandConvert2Nc, ...))
				 
names(fileDate) = paste(names(fileDate), 'obtained on')
				 
comment = list('Data from GFEDv4.1s' =
					'Raw data file list on data/gfed/file_list.txt',
				'Data obtained on' = fileDate)
dat = dat[[7:(nlayers(dat) - 6)]]
                
writeRaster.gitInfo.time(dat, drive_fname['fire'],
                    comment = comment, overwrite = TRUE)
					
memSafeFile.remove()

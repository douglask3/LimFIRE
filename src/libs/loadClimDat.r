loadClimDat <- function(dir, varns, clim_layers) {
	files = list.files(dir, full.names = TRUE)

	loadDat <- function(varn) {
	    files = files[grepl(varn, files)]
	    dat = layer.apply(files, stack)[[clim_layers]]
        dat = layer.apply(dat, convert_pacific_centric_2_regular, TRUE)
	}

	dat = lapply(varns, loadDat)

	nyears = floor(length(clim_layers)/12)

	return(list(dat, nyears))
}

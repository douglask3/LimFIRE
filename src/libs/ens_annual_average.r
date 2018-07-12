annual_average <- function(ens) {
	fname = paste(dirname(ens[[2]][[1]]), '/', filename.noPath(ens[[2]][[1]], TRUE), '-mean.nc', sep = '')
	print(fname)
	openEns <- function(ens) brick(ens[[1]][[2]][[1]])
	dat = runIfNoFile(fname, mean, openEns(ens))
	return(dat)
}



ens_annual_average <- function() {
	ens_files = open_ensembles()
	aa_mod = 12 * 100 * layer.apply(ens_files, annual_average)
	return(aa_mod)
}
	

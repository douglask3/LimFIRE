open_ensemble <- function(dir, vars = c('lm', 'rw', 'sn', 'sn-ws')) {
	files = list.files(dir, full.names = TRUE)            
	
	openSet <- function(st) {
		st = paste('-', st, '.nc', sep = '')
		files = files[grepl(st, files)]
	}
	
	mod = lapply(vars, openSet)
	return(mod)
}

open_ensembles <- function(...) {
	dirs = list.dirs(outputs_dir, full.names = TRUE)
	dirs = dirs[grep('ensemble_', dirs)]
	
	dats = lapply(dirs, open_ensemble, ...)
	
	return(dats)
}
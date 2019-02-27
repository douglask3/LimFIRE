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
    outputs_dir = 'D:/Laurens22122018/Documents/work/LimFIRE/outputs/'
	dirs = list.dirs(outputs_dir, full.names = TRUE)
	dirs = dirs[grep('ensemble_', dirs)]
	
	dats = lapply(dirs, open_ensemble, ...)
	test = sapply(dats, function(i) length(unlist(i))) > 0
	dats = dats[test]
	
	return(dats)
}
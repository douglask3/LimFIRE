#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')

grab_cache = TRUE

sensembles = 0	  
nensembles = 5200	  
niterations = 51
#########################################################################
## Run model                                                           ##
#########################################################################
findParameterLimitation <- function(line) {
	
	dir = paste(outputs_dir, 'ensemble_', line * nensembles, '/', sep = "")
	print(dir)
	makeDir(dir)
	
	mod_files = paste(dir, '/LimFIRE_',
                      c('fire', 'fuel','moisture','ignitions','supression'),
                      sep = '')	
				  
	rw_mod_files = paste0(mod_files,    '-rw')
	lm_mod_files = paste0(mod_files,    '-lm')
	sn_mod_files = paste0(mod_files,    '-sn')
	ws_mod_files = paste0(mod_files,    '-sn-ws')
        ms_mod_files = paste0(mod_files,    '-mnsures')          

	runLimFIRE <- function(fname, ...){
		fname = paste(fname,    '.nc', sep = '')
		return(runIfNoFile(fname, runLimFIREfromstandardIns, pline = line, test = grab_cache, ...))
	}

	lm_mod = runLimFIRE(lm_mod_files, normalise = TRUE, add21 = TRUE)
	rw_mod = runLimFIRE(rw_mod_files, raw = TRUE, normalise = TRUE)
	sn_mod = runLimFIRE(sn_mod_files, sensitivity = TRUE)
        md_mod = runLimFIRE(ms_mod_files, just_measures = TRUE, raw = TRUE)
        
	weightedSensitivity <- function() {
		ws <- function(sn, i) {
			lms = rw_mod[-1]
			lms = lms[-i]
			out = layer.apply(1:nlayers(sn), function(i) sn[[i]] * lms[[1]][[i]] * lms[[2]][[i]] * lms[[3]][[i]])
			return(out)
		}
		sn_mod[2:5] = mapply(ws, sn_mod[2:5], 1:4)
		return(sn_mod)
	}

	ws_mod = runIfNoFile(paste(ws_mod_files, '.nc', sep = ''), weightedSensitivity, test = grab_cache)
	
	return(c(rw_mod_files,  lm_mod_files, sn_mod_files, ws_mod_files, ms_mod_files))
	
}
#lines= seq(sensembles/nensembles, 1, length.out = nensembles + 1 - sensembles)
#lines = sample(lines, niterations, replace = FALSE)
ens_files = open_ensembles()
ens_no <- function(file) strsplit(strsplit(file[[1]][[1]], 'outputs//ensemble_')[[1]][-1], '/')[[1]][1]
ens_nos = as.numeric(sapply(ens_files, ens_no))[1:50]
lines = ens_nos/nensembles
files = mclapply(lines, findParameterLimitation, mc.cores = getOption("mc.cores", 4L))


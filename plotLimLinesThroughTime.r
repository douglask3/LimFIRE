########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()

grab_cache = TRUE

fig_fname       = 'figs/limitation_map.png'
fig_fname_indiv = 'figs/ind_limiataions'

mod_files = paste(outputs_dir, '/LimFIRE_',
                 c('fire', 'fuel','moisture','ignitions','supression'),
                  sep = '')
				  
coords = c(115, 135, -30, -15) 

x = seq(2001, 2014, by = 1/12)

cols = c('black', 'black', 'green', 'blue', 'red', 'grey')


#########################################################################
## Open                                                                ##
#########################################################################
rw_mod_files = paste(mod_files,    '-rw', sep = '')
                  

runLimFIRE <- function(fname, ...){
    fname = paste(fname,    '.nc', sep = '')
    return(runIfNoFile(fname, runLimFIREfromstandardIns, test = grab_cache, ...))
}

vars = runLimFIRE(rw_mod_files, raw = TRUE, normalise = TRUE)
vars = c(stack(drive_fname['fire']), vars)

rw_mod_files = c(paste(outputs_dir, 'GFED_fire'), rw_mod_files)
rw_mod_files = paste(rw_mod_files, paste(coords, collapse = '_'), '.csv', sep = '-')

vars = mapply(runIfNoFile, rw_mod_files, vars, MoreArgs = list(FUN = convert2Lines, coords = coords), SIMPLIFY = FALSE)
vars[[1]] = vars[[1]] * 100
vars[[2]] = vars[[2]] * 100

vars12 = lapply(vars, running12.numeric)

#########################################################################
## Plot                                                                ##
#########################################################################
par(mfrow = c(2,1))
convert2Yrange <- function(x, y, xmax = 1) y[1] + x * diff(y) / xmax

setupPlot <- function(vs, x) {
	yrange = range(vs[[1]], vs[[2]])
	plot(range(x), yrange, type = 'n', xlab = 'year', ylab = 'Burnt Area (%)')
	yrange = par("usr")[3:4]
	axis(4, at = seq(yrange[1], yrange[2], length.out = 5), labels = c(0, 0.25, 0.5, 0.75, 1))
	vs[-(1:2)] = lapply(vs[-(1:2)], convert2Yrange, yrange)
	return(vs)
}

vars12 = setupPlot(vars12, x)

mapply(lines, vars12, col = c('black', 'black', 'green', 'blue', 'red', 'grey'),
			        lty = c(1      , 2      , 1      , 1     , 1    , 1     ), 
					MoreArgs = list(x = x))
					

#lapply(vars, runIfNoFile, , convert2Lines)
#mod = lapply(sapply(mod))



annual_range <- function(var) {
	out = apply(matrix(var, nrow = 12), 1, range)
	out = cbind(out, out[,1])
	return(out)
}

varSn = lapply(vars, annual_range)
varSn = setupPlot(varSn, 0:12)

polySeasonRange <- function(var, col, ...) {
	col = make.transparent(col, 0.67)
	polygon(c(0:12, 12:0), c(var[1,], rev(var[2,])), col = col, border = NA, ...)
}

mapply(polySeasonRange, varSn, cols)
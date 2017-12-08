########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()

grab_cache = FALSE

fig_fname       = 'figs/limitation_map.png'
fig_fname_indiv = 'figs/ind_limiataions'

mod_files = paste(outputs_dir, '/LimFIRE_',
                 c('fire', 'fuel','moisture','ignitions','supression'),
                  sep = '')
				  
coords = list('Northern Australia' = c(115, 135, -30, -15),
			  'Sahel'              = c(-10, 30, 6, 10),
			  'YayAndWow'          = c(50, 70, 46, 51),
			  'Amazon'             = c(-65, -55, -12, 5))
x = seq(2001, 2014, by = 1/12)
cols = c('black', 'black', 'green', 'blue', 'red', 'grey')
ltys = c(1      , 2      , 1      , 1     , 1    , 1     )

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

mat = matrix(1:(length(coords) * 2), ncol = 2)
layout(mat)
par(mar = c(3, 2, 1, 2), oma = c(0,1,0,0))

run4regions <- function(coords, name) {
	rw_mod_files = c(paste(outputs_dir, 'GFED_fire'), rw_mod_files)
	rw_mod_files = paste(rw_mod_files, paste(coords, collapse = '_'), '.csv', sep = '-')

	vars = mapply(runIfNoFile, rw_mod_files, vars,
				  MoreArgs = list(FUN = convert2Lines, coords = coords, test = grab_cache), 
				  SIMPLIFY = FALSE)
	vars[[1]] = vars[[1]] * 100
	vars[[2]] = vars[[2]] * 100

	vars12 = lapply(vars, running12.numeric)

	#########################################################################
	## Plot                                                                ##
	#########################################################################

	convert2Yrange <- function(x, y, xmax = 1) y[1] + x * diff(y) / xmax

	setupPlot <- function(vs, x) {
		yrange = range(vs[[1]], vs[[2]])
		plot(range(x), yrange, type = 'n', xlab = '', ylab = '')
		yrange = par("usr")[3:4]
		axis(4, at = seq(yrange[1], yrange[2], length.out = 5), labels = c(0, 0.25, 0.5, 0.75, 1))
		vs[-(1:2)] = lapply(vs[-(1:2)], convert2Yrange, yrange)
		return(vs)
	}

	vars12 = setupPlot(vars12, x)
	mtext(name)
	mapply(lines, vars12, col = cols, lty = ltys, MoreArgs = list(x = x))
						

	#lapply(vars, runIfNoFile, , convert2Lines)
	#mod = lapply(sapply(mod))



	annual_range <- function(var) {
		out = apply(matrix(var, nrow = 12), 1, range)
		out = cbind(out, out[,1])
		return(out)
	}

	varSn = lapply(vars, annual_range)
	varSn = setupPlot(varSn, 0:12)

	polySeasonRange <- function(var, col, density, ...) {
		col = make.transparent(col, 0.67)
		polygon(c(0:12, 12:0), c(var[1,], rev(var[2,])),
				col = col, border = col, density = density, ...)
	}
	density = lapply(ltys, function(i) if (i == 2) return(33) else return(NULL))
	mapply(polySeasonRange, varSn, cols, density)
}

mapply(run4regions, coords, names(coords))

mtext(side = 2, 'Burnt Area (%)', outer = TRUE, cex = 0.8)
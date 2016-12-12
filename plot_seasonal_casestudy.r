source('cfg.r')
graphics.off()

fig_fnames = paste('figs/seasonal_casestudy',
				 c('Africa', 'Asia1', 'Asia2'), '.png', sep = '')

mod_files  = paste(outputs_dir, '/LimFIRE_',
                  c('fire', 'fuel','moisture','ignitions','supression'),
                   '-lm.nc',
                   sep = '')

extents    = list(Africa = c(extent( 10, 45, 5, 12),
		                     extent(-25, 45, -20, -10)),
			      Asia1  = c(extent(73,85,5, 25),
			                 extent(146,151, -45, -26)),
			      Asia2  = c(extent(73,85,5, 25),
			                 extent(146,151, -45, -26)))
							 
limIndexs  =  list(2:4,
                   2:5,
				   2:5)

limWithout =  list(NULL, NULL, 5)
				   
lm_mod = runIfNoFile(mod_files, runLimFIREfromstandardIns)


plotExtent <- function(extent, limIndex, limWithout) {
	lm_crp = lapply(lm_mod, crop, extent)

	TSmake <- function(i) apply(values(i), 2, mean, na.rm = TRUE)

	lm_ts = lapply(lm_crp, TSmake)

	seasonalTS <- function(i) {
		nm = length(i)
		
		mnthTot <- function(m) {
			index = seq(m, nm, by = 12)#((y-1)*12 + 1):(y * 12)
			return(mean(i[index]))
		}
		return(sapply(1:12, mnthTot))
	}
	
	sn_ts = sapply(lm_ts, seasonalTS)
	
	sn_ts = 1 - sn_ts
	sn_ts = sweep(sn_ts, 2, c(1, 1, 1/0.44, 1/0.67, 1), '*')
	sn_ts = 1- sn_ts

	plotstuff <- function() {
		plot(c(0,12), c(0,1), axes = FALSE, xlab = "", ylab = "", type = 'n')
		axis(1:12, side = 1, labels = month.abb)
		axis(2)

		sn_ts = matrix2list(sn_ts)
		mapply(function(y, col, lty) lines(1:12, y, col = col, lwd = 2.5, lty = lty), sn_ts, cols, ltys)
	}

	matrix2list <- function(x) split(x, rep(1:ncol(x), each = nrow(x)))

	sn_ts[, 1] = 1.0
	sn_ts = sn_ts[, c(1, limIndex)]
	for (i in 2:ncol(sn_ts)) sn_ts[, 1] = sn_ts[, 1] * (1-sn_ts[,i])
	
	if (!is.null(limWithout)) {
		fw = sn_ts[, 1]
		for (i in limWithout) fw = fw / (1-sn_ts[,i])
		sn_ts = cbind(sn_ts, fw)
		limIndex = c(limIndex, max(limIndex) + 1)
	}
	
	cols  = c('orange','green', 'blue', 'red', 'black', 'orange')[c(1, limIndex)]
	ltys  = c(1       , 1     , 1     , 1    , 1      , 2       )[c(1, limIndex)]
	sn_ts[, 4] = sn_ts[, 4] / 2

	plotstuff()
}

plotExtents <- function(fig_fname, extents, ...) {

	png(fig_fname, height = 6, width = 4, res = 300, unit = 'in')
		par(mfrow = c(2,1), mar = c(1, 2, 1, 1), oma = c(1,0,0,0))
		lapply(extents,plotExtent, ...)
	dev.off.gitWatermark()
}

mapply(plotExtents, fig_fnames, extents, limIndexs, limWithout)
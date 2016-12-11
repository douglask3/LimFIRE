#################################################################
## cfg                                                         ##
#################################################################
source('cfg.r')
graphics.off()

fig_fname = 'figs/limitation_map.png'

cols = c('orange', 'green', 'blue', 'red', 'black')

transect = rbind(c(30, -30), c(30, 30))

minv = 0.0000001

mod_files = paste(outputs_dir, '/LimFIRE_',
                 c('fire', 'fuel','moisture','ignitions','supression'),
                  sep = '')

#################################################################
## load                                                        ##
#################################################################

## limitations
lm_mod_files = paste(mod_files,    '-lm', sep = '')
aa_lm_mod_files = paste(lm_mod_files, '-aa.nc', sep = '')
lm_mod_files = paste(lm_mod_files,    '.nc', sep = '')

mod    = runIfNoFile(lm_mod_files, runLimFIREfromstandardIns)
aa_mod = runIfNoFile(aa_lm_mod_files, function(x) lapply(x, mean), lm_mod)[-5]

## transect info
c(cells, coords) := findTrasectCells(transect, mod[[1]][[1]])

#################################################################
## plot setup                                                  ##
#################################################################
#png('figs/transect2.png', units = 'in', res = 150, height = 15 *5/4, width = 22.5)


test = is.na(aa_mod[[2]])
aa_mod[[1]][test] = 0.0
aa_mod[[2]][test] = 1

#################################################################
## plot Lines                                                  ##
#################################################################
indexes = list(1:35, 1:55, 1:65, 1:121)

plot_index <- function(pltIndex, fno) {
	fname = paste('figs/transect_simple', fno, '.png')
	png(fname, units = 'in', res = 300, height = 6, width = 6)

	plot(c(0, length(cells)), c(minv*10, 1), axes = FALSE, type = 'n', xlab = '', ylab = '')#, log = 'y')
	axis(2)
	index = round(seq(1, length(cells), length.out = 5))
	axis(1, at = index, labels = rev(coords[2, index]))
	mtext.cex('longitude', side = 1, line = 2)

	aa_mod[-1] = lapply(aa_mod[-1], scale2zeropnt)
	aa_mod[[2]][tail(cells, 5)] = 1

	x = sapply(unique(cells), function(i) which(cells==i)[1])
	cells = cells[x]
	
	mapply(function(y, col, sc) lines(x[pltIndex], sc * rev(y[cells])[pltIndex], col = col, lwd = 2.5),
		  aa_mod, c("orange", "green", "blue", "red"), c(24, 1, 1, 0.5))
	dev.off.gitWatermark()
}

mapply(plot_index, indexes, 1:length(indexes))
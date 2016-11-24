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
aa_lm_mod_files = paste(lm_mod_files, '-aa.nc', sep = '')#
fs_lm_mod_files = paste(lm_mod_files, '-fs.nc', sep = '')
lm_mod_files = paste(lm_mod_files,    '.nc', sep = '')

mod    = runIfNoFile(lm_mod_files, runLimFIREfromstandardIns)
aa_mod = runIfNoFile(aa_lm_mod_files, function(x) lapply(x, mean), lm_mod)
fs_mod = runIfNoFile(fs_lm_mod_files, function(x) lapply(x, maxFireLimiation), lm_mod)

## transect info
c(cells, coords) := findTrasectCells(transect, mod[[1]][[1]])

#################################################################
## plot setup                                                  ##
#################################################################
png('figs/transect2.png', units = 'in', res = 150, height = 15 *5/4, width = 22.5)

layout(rbind(c(6, 6, 2), c(7, 7, 3), c(8, 1, 4), c(8, 1, 5), c(8, 1, 8)),
       widths = c(0.1, 0.9, 1))
par(oma = c(6, 2, 0, 0))
par(mar = rep(0,4))
aa_mod[[2]][is.na(aa_mod[[2]])] = 9E9
aa_mod[[4]] = aa_mod[[4]] / 3
aa_mod[[5]] = aa_mod[[5]] * 0.6

plot.new()
par(mar = c(0, 0, 5, 0))

projection = "perspective"
prj_parameters = 5000
orientation = c(-15,-25,-28)

plot_4way_standard(aa_mod[-1], projection = projection,
                   prj_parameters = prj_parameters, orientation = orientation,
                   spt.cex = 8)

c(x, y) := mapproject(coords[1, ],coords[2, ],
                      projection = projection,
                      orientation = orientation, parameters = prj_parameters)
lines(x, y, lwd = 5, col = 'black')	
                   
par(mar = c(1, 2, 0, 1), cex = 2)
#################################################################
## plot Lines                                                        ##
#################################################################
## Window
plot_window <- function(axs) {
    plot(c(0, length(cells)), c(minv*10, 1), axes = FALSE, type = 'n', xlab = '', ylab = '')#, log = 'y')
    axis(2)
    index = round(seq(1, length(cells), length.out = 5))
    if (axs) {
        axis(1, at = index, labels = coords[1, index])
        mtext.cex('lat', side = 1, adj = -0.05)
        axis(1, at = index, labels = coords[2, index], line = 3)
        mtext.cex('lon', side = 1, adj = -0.05, line = 3)
    } else {
        axis(1, at = index, labels = rep('', length(index)))
    }
}

## Actual plotting data
plot_lines <- function(dat, col, sc, sf, fill, rev, axs) {
    if (nplot) plot_window(axs)
    col_plg = make.transparent(col, 0.9)
    col_pnt = make.transparent(col, 0.997)
    is = layer.apply(dat, function(r) {       
            i = r[cells]
            if (fill) i[is.na(i)] = 1
            i = i * sc
            i = i - sf
            
            i[i<= minv] = NaN
            i[is.na(i)] = minv
            if (rev) i = 1 - i
            points(i, col = col_pnt, pch = 20, cex = 0.67)
            return(i)
        })
    is0 = is
    is = matrix(unlist(is), nrow = length(is[[1]]))
    mn = apply(is, 1, mean, col = col)
    qnt = apply(is, 1, quantile, c(0.1, 0.4, 0.6, 0.9))
    polygon(c(1:length(cells), length(cells):1),c(qnt[1,], rev(qnt[4,])), col = col_plg, border = NA)
    polygon(c(1:length(cells), length(cells):1),c(qnt[2,], rev(qnt[3,])), col = col_plg, border = NA)
    lines(mn, col = col, lwd = 2)   
}

for (nplot in c(T, F)) {
    mapply(plot_lines, rev(mod), rev(cols),
           c(1, 1,1,1, 12), c(0.4, 0.7, 0, 0, 0),
           fill = c(F, F, F, T, F),
           rev  = c(T, T, T, T, F),
           axs  = c(F, F, F, T, F))
    if (nplot) plot_window(TRUE)
}
mtext.cex(side = 2, 'fractional burnt area', line = 3)
dev.off.gitWatermark()
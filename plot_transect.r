#################################################################
## cfg                                                         ##
#################################################################
source('cfg.r')
graphics.off()

fig_fname = 'figs/limitation_map.png'

cols = c('orange', 'green', 'blue', 'red', 'black')

transect = rbind(c(30, -30), c(0, 35))

minv = 0.0000001

mod_files = paste(outputs_dir, '/LimFIRE_',
                 c('fire', 'fuel','moisture','ignitions','supression'),
                  sep = '')

#################################################################
## load                                                        ##
#################################################################

## limitations
lm_mod_files = paste(mod_files,    '-lm.nc', sep = '')
sn_mod_files = paste(mod_files,    '-sn.nc', sep = '')

lm_mod = runIfNoFile(lm_mod_files, runLimFIREfromstandardIns)
sn_mod = runIfNoFile(sn_mod_files, runLimFIREfromstandardIns, sensitivity = TRUE)

## transect info
c(cells, coords) := findTrasectCells(transect, mod[[1]][[1]])

#################################################################
## plot setup                                                        ##
#################################################################


#################################################################
## plot Lines                                                        ##
#################################################################
## Window
plot(c(0, length(cells)), c(minv*10, 1), axes = FALSE, type = 'n', xlab = '', ylab = '')#, log = 'y')
axis(2)

index = round(seq(1, length(cells), length.out = 5))
axis(1, at = index, labels = coords[1, index])
axis(1, at = index, labels = coords[2, index], line = 3)


mod = lm_mod



plot_lines <- function(dat, col, sc, sf, fill, rev) {
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

mapply(plot_lines, rev(mod), rev(cols),
       c(1, 1,1,1, 12), c(0.4, 0.7, 0, 0, 0),
       fill = c(F, F, F, T, F),
       rev  = c(T, T, T, T, F))
source('cfg.r')
mod_file = 'outputs/LimFIRE_fire.nc'
fig_file = 'figs/gfedComparison.png'
 
mod = runIfNoFile(mod_file, runLimFIREfromstandardIns, fireOnly = TRUE, test = FALSE)

obs = lapply(drive_fname, stack)[["fire"]]

cols = fire_cols; lims = fire_lims
 
###########################################################################
## Annual Average                                                        ##
###########################################################################
graphics.off()
png(fig_file, width = 7, height = 9, units = 'in', res = 300)
par(mar = c(0,0,0,0))
layout(rbind(c(1, 2), 
             c(3, 3),
             c(4, 6),
             c(8, 8),
             c(5, 7),
             #c(8, 8),
             c(9, 0)),
             heights = c(1, 0.2, 1, 0.2, 1, 1.3))
             
X = aaConvert(obs); Y = aaConvert(mod)
add_raster_legend2(cols, lims, add = FALSE,
               plot_loc = c(0.35,0.75,0.65,0.78), dat = X,
               transpose = FALSE,
               srt = 0)

AA = NME(X, Y)

###########################################################################
## Season                                                                ##
###########################################################################
pcols = c("red", "green", "blue", "red")
ccols = c("white", "red", "black")
clims = seq(0, 1, 0.2)
plims = 0:11

convert2Clim <- function(x) {
    base12 <- function(i) {
        i = i %% 12
        if (i == 0) i = 12
        return(i)
    }
    xi = x[[1:12]]
    for (i in 13:nlayers(x)) {
        m = base12(i)
        xi[[m]] = x[[i]]
    }
    
    PC =  PolarConcentrationAndPhase(xi)
    Phase = PC[[1]]; Conc = PC[[2]]
    Phase = Phase * 6 / pi
    
    addMask <- function(x) {
        x[mask] = NaN
        return(x)
    }
    
    plot_raster(addMask(Conc), clims, cols) 
    
    plot_raster(addMask(Phase + 5.5), plims, cols = c("red", "green", "blue", "red"), 
                smooth_image = FALSE)
    
    return(xi)
}
mask = (sum(obs) < (12/nlayers(obs)) * 0.01) +(sum(mod) < (12/nlayers(mod)) * 0.01)
mask = mask > 0
X = convert2Clim(obs); Y = convert2Clim(mod)

SeasonLegend(pcols, plims, add = TRUE,
               plot_loc = c(0.20,0.90,0.65,0.78), dat = X)
               
add_raster_legend2(cols, lims, add = FALSE,
               plot_loc = c(0.35,0.75,0.65,0.78), dat = X,
               transpose = FALSE,
               srt = 0)
SC = MPD(X ,Y)
 
###########################################################################
## IAV                                                                   ##
###########################################################################

layerTotal <- function(r) {
    r[is.na(r)] = 0
    r = sum.raster(r * area(r) / 10E5)
    return(r)
}

ma <- function(x, n = 12) {
    len = floor(length(x) / n)
    y = rep(0, len)
    for (i in 1:len) {
        index = ((i -1) * n + 1):(i * n)
        y[i] = sum(x[index])
    }
    return(y)
}

IAV <- function(x) {
    x = layer.apply(x, layerTotal)
    x = unlist(x)
    x = ma(x)
    return(x)
}

X = IAV(obs); Y = IAV(mod)

AV = NME(X, Y)
par(mar = c(4,4,3,2))
plot(range(years[-1]), range(c(X,Y)), type = 'n', xlab = 'year', ylab = 'Global Burnt Area (MHa)')
lines(years[-1], X, col = 'blue', lwd = 2)
lines(years[-1], Y, col = 'red', lwd = 2)
legend('left', c('GFED4s ', 'LimFire'), lty = 1, lwd = 2, col = c('blue', 'red'))

dev.off.gitWatermark()
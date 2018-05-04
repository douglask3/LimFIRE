##################################################################
## cfg                                                          ##
##################################################################
source('cfg.r')
graphics.off()

fig_file = 'figs/limitation_lines.png'
inter_obs_file = 'temp/samplesObs.csv'

mod = runIfNoFile(inter_obs_file, ObsRasters2DataFrame, randomSample = 1000)
mod = cbind(mod, background = 1)

plotFireVsVar <- function(i, fun, ps, 
                          col = 'green',
                          xlab = '', xlim = c(0, 2000)) {
    
    params = param(ps, return_all)
    #dev.new()
    
    if (length(i) == 1) i = mod[, i]
    else {
        index = 1:(length(i) - 1)
        
        P = apply(as.matrix(params[, index]), 2, mean)
        P = c(1, P)
        i = sweep(mod[i], 2, P, '*') / (1 + sum(P))
        i = apply(i, 1, sum)
        params = as.matrix(params[, -index])
    }
    
    x = seq(xlim[1], xlim[2], length.out = 1000)
    
    findYs <- function(j) {
        ins = list(x); for (i in params[j,]) ins = c(ins, i)
        y = do.call(fun, ins)
        return(y)   
    }
    
    ys = sapply(1:dim(params)[1], findYs)
    
    y  = apply(ys, 1, mean)
    ys = apply(ys, 1, quantile, c(0.1, 0.9))
    plot(i, mod[, 'fire'], xlim = xlim, ylim = c(0,1),
         ylab = '', xlab = '',
         pch = 16,  cex = 0.2)
    
    at = seq(par("yaxp")[1], par("yaxp")[2], length.out = par("yaxp")[3] + 1)
    axis(4, at = at, labels = 1 - at)
    
    mtext(xlab, side = 1, line = 2.5)
    mtext('burnt fraction', side = 2, line = 2.5)
    mtext('limiation', side = 4, line = 2.5)
    
    lines(x, y, col = col)
    ys = c(ys[1,], rev(ys[2,]))
    polygon(c(x, rev(x)), ys, col = make.transparent(col, 0.7), border = NA)
    
    
    pdf = hist(i, breaks = c(-9E9, x, 9E9), plot = FALSE)
    pdfY = head(pdf[[3]][-(1:2)], -1)
    pdfY = smooth.spline(pdfY)[[2]]
    pdfX = head(pdf[[4]][-(1:2)], -1)    
    pdfY = 0.5 * pdfY / max(pdfY)
    
    polygon(c(xlim[1], pdfX, xlim[2]), c(0,pdfY,0), border = NA, col = make.transparent('black', 0.9))
}

png(fig_file, height = 10, width = 6, units = 'in', res = 300)
par(mfrow = c(4, 1), mar = c(4,4,3, 4))

plotFireVsVar('npp', LimFIRE.fuel, c('w0', 'kw'), 'green', 'NPP (g/m2)', c(0, 10000))
plotFireVsVar(c('alpha', 'emc'), LimFIRE.moisture, c('M','omega0', 'komega'), 'blue', 'Moisture (fraction)', c(0, 1))
plotFireVsVar(c('Lightn', 'pas', 'popdens'), LimFIRE.ignitions, c('P','D', 'ig0', 'kig'), 'red', 'igntions (/m2)', c(0, 300))
plotFireVsVar(c('crop', 'popdens'), LimFIRE.supression, c('H','s0', 'ks'), 'purple', 'land cover (%)', c(0, 100))

dev.off.gitWatermark()

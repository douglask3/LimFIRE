source("cfg.r")

dat = brick('outputs/fire2000-2014.nc')

#aa = mean(dat)*12 * 100

TrendFun <- function(x) {
    
    fit = lm(x ~ t, data = data.frame(x = x, t = 1:length(x)))
    
    #res = try(summary(fit)[[4]][2, 3:4])
    res = try(c(coefficients(fit)[2], summary(fit)[[4]][2, 4]))
    
    if (is.na(res[1])) return(c(0, 1))  
    if (class(res) == "try-error") return(c(-999, 0.0))
    
    return(res)
}

logit <- function(r, minv = 0.00000000001) {
    r[r<minv] = minv
    r[r>(1-minv)] = (1-minv)
    log(r/(1-r))#log(r)#
}

logistic <- function(r, minv = 0.00000000001) {
    r = 1/(1+exp(-r))# = exp(r)
    r[r < minv] = 0
    r[r >=(1-minv)] = 1
    r
}


    mask = aa>0
    mdat = dat[mask]
    maa = logit(aa[mask]/100)
    mvdat = apply(mdat, 1, function(d) sapply(12:length(d),function(i) mean(d[(i-11):i])))
    mvdat = logit(mvdat)
    mtr = apply(mvdat, 2, TrendFun)    
    datT = dat[[1:2]]
    datT[] = NaN

    endV =  maa - mtr[1,]*6
    startV = maa + mtr[1,]*6
    diff = logistic( startV) - logistic(endV)

    datT[[1]][mask] = mtr[1,]
    datT[[2]][mask] = mtr[2,]
    


png("figs/newTrendMaps.png", width = 7.2, height = 0.3+7.2 * 3 * 150/360,
    res = 300, units = 'in')
par(mfrow = c(3, 1), mar = c(0, 0, 1.5, 0), oma = c(1, 0, 0, 0))

cols = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')
limits = c(0, 0.1, 1, 2, 5, 10, 20, 50)
addPlot <- function(dat, limits, cols, title, ...) {
    if (nlayers(dat)==2) {
        dat[[1]][dat[[2]] > 0.05] = NaN
        dat = dat[[1]]
    }
    plotStandardMap(dat, '', limits = limits, cols  = cols, add_legend = FALSE)
    standard_legend(cols, limits, dat, add = TRUE, plot_loc = c(0.35, 0.85, 0.05, 0.1), ...)
    mtext(title, side = 3, adj = 0.1)
}
addPlot(aa, limits, cols, 'Annual average burnt area', units = '%', extend_max = TRUE)

cols = rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))

datT[[1]][mask] = diff
limits = c(-10, -5, -2, -1, -0.1, 0.1, 1, 2, 5, 10)/10
addPlot(datT*100, limits, cols, 'Annual average trend in burnt area', units = '%', extend_max = TRUE, extend_min = TRUE)


datT[[1]][mask] = mtr[1,]
limits = c(-8, -4, -2, -1, -0.1,  0.1, 1, 2, 4, 8)/100
addPlot(datT, limits, cols, 'Trend coefficant\n(i.e areas of most concerning trends)', extend_max = TRUE, extend_min = TRUE)

dev.off.gitWatermark()


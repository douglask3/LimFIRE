#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()
fig_fname  = 'figs/human_noHuman_impact.png'

mod_file   = 'outputs/LimFIRE_fire'
mod_file   = paste(mod_file, c('', 'noCrops', 'noPopdens'), '.nc', sep ='')

xVars      = list('crop', c('pas', 'popdens'))

labs       = c('Cropland on non-crop areas', 'Population Density')
xUnits     = c('% cover', 'no. people / km2')

grab_cache = TRUE

#########################################################################
## Run Model                                                           ##
#########################################################################
control = runIfNoFile(mod_file[1], runLimFIREfromstandardIns, fireOnly = TRUE, 
                                       test = grab_cache)
control = mean(control)

plot_impact <- function(mod_filei, xVar, lab, xUnit, noneLand = FALSE, 
                        log = '', ...) {

    noVar  = runIfNoFile(mod_filei, runLimFIREfromstandardIns, fireOnly = TRUE, 
                         remove = xVar, test = grab_cache)
    noVar  = mean(noVar)
#########################################################################
## Calculate Impact                                                    ##
#########################################################################  
    
    impact = (control - noVar) 
    test = impact < 0
    impact[test] = impact[test] / noVar[test]
    test = !test
    impact[test] = impact[test] / control[test]
    
    xVar   = mean(stack(drive_fname[xVar]))

    mask   = !(is.na(impact) | is.na(xVar))
    impact = impact[mask]
    xVar   = xVar  [mask] 
    if (log == 'x') xVar[xVar < 0.01] = 0.01

    if (!noneLand) {
        sp        = smooth.spline(xVar, impact)
        f1        = predict(sp, 100)$y
        fImpact   = (impact - f1 * xVar * 0.01) / (1 - xVar * 0.01)
    } else fImpact = impact
    
#########################################################################
## plot                                                                ##
#########################################################################
    ## calculate trend line
    x      = seq(min(xVar), max(xVar), length.out = 1000)
	
    y      = predict(loess(fImpact ~ xVar), x)
	y[y < -1] = -1
    ## plot window
    yrange = quantile(fImpact, probs = c(0.001, 0.999))
    yrange = range(c(yrange, 0), na.rm = TRUE)
    xrange = range(x)
    
    plot  (range(x), 100 * yrange, type = 'n', xlab = xUnit, ylab = 'Impact (% of burnt area)', log = log)

    ## plot
    points(xVar, fImpact * 100, col =  make.transparent('black', 0.98), pch = 16, ...)
    lines (x, y * 100, lwd = 2, col = 'red')
    mtext (lab, side = 3, line = -1.2, cex = 2)
}

png(fig_fname, width = 9, height = 12, unit = 'in', res = 300)
layout(2:1)
par(cex = 1.5, mar = c(3, 4.1, 2, 1))#

mapply(plot_impact, mod_file[-1], xVars, labs, xUnits, c(FALSE, TRUE), c('', 'x'), cex = c(0.67, 0.33))

## footer
dev.off.gitWatermark()

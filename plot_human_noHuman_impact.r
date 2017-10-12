#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')
graphics.off()
fig_fname  = 'figs/human_noHuman_impact.png'

mod_file   = 'outputs/LimFIRE_fire'
mod_file   = paste(mod_file, c('', 'noCrops', 'noPopdens', 'noPas'), '.nc', sep ='')

xVars      = list('crop', 'popdens', 'pas')

labs       = c('a) Cropland', 'b) Population Density', 'c) Pasture')
xUnits     = c('% cover', 'no. people / km2', '% cover')

grab_cache = TRUE

#########################################################################
## Run Model                                                           ##
#########################################################################
control = runIfNoFile(mod_file[1], runLimFIREfromstandardIns, fireOnly = TRUE, 
                                       test = grab_cache)
control = mean(control)

plot_impact <- function(mod_filei, xVar, lab, xUnit, noneLand = FALSE, log  = 'n') {

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
	
    y      = predict(loess(fImpact ~ xVar, degree = 2), x)

    ## plot window
    yrange = quantile(fImpact, probs = c(0.001, 0.999))
    yrange = range(c(yrange, 0), na.rm = TRUE)
    plot  (range(x), 100 * yrange, type = 'n', xlab = xUnit, ylab = 'Impact (% of burnt area)', log = log)

    ## plot
    points(xVar, fImpact * 100, col =  make.transparent('black', 0.97), pch = 16)
    #lines (x, y * 100, lwd = 2, col = 'red')
    #mtext (lab, side = 3, line = -1, cex = 1.5, adj = 0.05)
}

png(fig_fname, width = 3, height = 8.5, unit = 'in', res = 300)
par(mfrow = c(3,1))

mapply(plot_impact, mod_file[-1], xVars, labs, xUnits, c(FALSE, TRUE, TRUE), c('', 'x', ''))

## footer
dev.off.gitWatermark()

#################################################################
## cfg                                                         ##
#################################################################
source('cfg.r')
graphics.off()


## output filename
mod_file = 'outputs/LimFIRE_fire_igntion_scaling'
fig_file = c('figs/IgntionScaling.png', 
             'figs/DiffIgntionScaling.png')

## limits and colours
diff_lims1 = c(0, 0.1, 1, 2, 5) 
diff_cols1 = fire_cols

diff_lims2 = c(-20, -10, -5, -2, -1, -0.1, 0.1, 1, 2, 5) 
diff_cols2 = c('#000033', '#0022AA',  '#00EEFF', 'white', '#FFEE00', '#AA2200')

cont_lims1 = c(0, 2, 5, 10, 20, 40, 60, 80)
cont_cols1 = fire_cols

cont_lims2 = cont_lims1
cont_cols2 = c("#FFFFFF", "#00EEFF", "#0022AA", "#000033") 

experiments = c(0.1, 0.5, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.5, 1.9, 2, 10)

nr = 5; nc = 3
 
#################################################################
## Open data                                                   ##
#################################################################
mod_file = paste(mod_file, experiments, '.nc', sep ='')

load_experiments <- function(file, scl)
    runIfNoFile(file, runLimFIREfromstandardIns, fireOnly = TRUE, scale_igntions = scl)
                 
exps = mapply(load_experiments, mod_file, experiments)
nplots = length(exps)              
#################################################################
## Setup plots                                                 ##
#################################################################
pngLay <- function(fname) {
    png(fname, width = nr, height = nc * 2,
        units = 'in', res = 300)
    lmat = 1:(nc * nr)
    lmat[lmat > nplots] = 0
    lmat = matrix(lmat, nrow = nr, ncol = nc)
    lmat = rbind(lmat, nplots + 1)
    layout(lmat, heights = c(rep(1, nplots), 0.3))
    
    par(mar = rep(0,4))
}

mtextStandard <- function(...) mtext(..., line = -2)

standard_legend2 <- function(...)
        standard_legend(plot_loc = c(0.2, 0.9, 0.65, 0.78), ...)
                  
mtext.burntArea <- function(txt = 'Burnt Area (%)')
    mtext(txt, cex = 0.8, line = -5)                  

#################################################################
## plot annual average                                         ##
#################################################################
pngLay(fig_file[1])

    addBeforeEx <- function(i, add)
        paste(strsplit(filename(i[[1]]), '.nc')[[1]], add, '.nc', sep = '')

    aaPlot <- function(i, j) {
        fname = addBeforeEx(i, 'aaConvert')
        i = aaConvert(i, quick = TRUE, fname = fname)
        mtextStandard(j)
       
        return(i)
    }

    exps = mapply(aaPlot, exps, experiments)#
     
dev.off.gitWatermark()

#################################################################
## Plot Differce                                               ##
#################################################################
pngLay(fig_file[2])
    control = exps[experiments == 1][[1]]

    diffPlot <- function(i, j) {
        i = i - control
        plot_raster(i, diff_lims2, diff_cols2, quick = TRUE)
        mtextStandard(j)
        return(i)
    }

    experiments = mapply(diffPlot, exps, experiments)
    standard_legend2(diff_cols2, diff_lims2, dat = experiments[[1]])
dev.off.gitWatermark()
#################################################################
## cfg                                                         ##
#################################################################
source('cfg.r')
graphics.off()


## output filename
mod_file = 'outputs/LimFIRE_fire'
fig_file = 'figs/IgntionInfo.png'

## limits and colours
diff_lims1 = c(0, 0.1, 1, 2, 5) 
diff_cols1 = fire_cols

diff_lims2 = c(-20, -10, -5, -2, -1, -0.1, 0.1, 1, 2, 5) 
diff_cols2 = c('#000033', '#0022AA',  '#00EEFF', 'white', '#FFEE00', '#AA2200')

cont_lims1 = c(0, 2, 5, 10, 20, 40, 60, 80)
cont_cols1 = fire_cols

cont_lims2 = cont_lims1
cont_cols2 = c("#FFFFFF", "#00EEFF", "#0022AA", "#000033") 

experiments =    c('fullModel' , 'HumanIgntionsOnly'     , 'LightningIngitionsOnly' , 'noHumanss'                )
remove      = list(NULL        , "Lightn"                , "pas"                    , c("pas", "crop", "popdens"))
labs1       =    c('Burnt Area', 'Human ignitions only'  , 'Lightning igntions only', 'No Humans') 
labs2       =    c(              '+ Lightning ignitions' , '+ Human igntions'       , 'Human Impact') 
 
#labs1 = paste(letters[1:length(labs1)], ') ', labs, sep = "")

#################################################################
## Open data                                                   ##
#################################################################
mod_file = paste(mod_file, experiments, '.nc', sep ='')

load_experiments <- function(file, remove)
    runIfNoFile(file, runLimFIREfromstandardIns, fireOnly = TRUE, remove = remove)
                 
experiments = mapply(load_experiments, mod_file, remove)
               
#################################################################
## Setup plot                                                  ##
#################################################################
png(fig_file, width = 12, height = 6, units = 'in', res = 300)
par(mar = c(0,0,0,0))
                 
layout(rbind(c(1,  2,  3, 4),
             rep(5, 4), 
             c(6,  8,  9, 10), 
             c(7, 11, 11, 11)),
             heights = c(1, 0.3, 1, 0.3))



mtextStandard <- function(...) mtext(..., line = -2)

standard_legend <- function(cols = fire_cols, lims = fire_lims, dat,
                            plot_loc = c(0.35,0.75,0.65,0.78), ...) {
    add_raster_legend2(cols, lims, add = FALSE,
               plot_loc = plot_loc, dat = dat,
               transpose = FALSE,
               srt = 0, ...)
} 

standard_legend2 <- function(...)
        standard_legend(plot_loc = c(0.2, 0.9, 0.65, 0.78), ...)
                  


mtext.burntArea <- function(txt = 'Burnt Area (%)')
    mtext(txt, cex = 0.8, line = -5)                  

#################################################################
## plot annual average                                         ##
#################################################################
addBeforeEx <- function(i, add)
    paste(strsplit(filename(i[[1]]), '.nc')[[1]], add, '.nc', sep = '')

aaPlot <- function(i, j) {
    fname = addBeforeEx(i, 'aaConvert')
    i = aaConvert(i, quick = TRUE, fname = fname)
    mtextStandard(j)
    return(i)
}

experiments = mapply(aaPlot, experiments, labs1) 
standard_legend(dat = experiments[[1]])
mtext.burntArea()

#################################################################
## plot no. ignitions from source                              ##
#################################################################
openMean <- function(fname_in, FUN = mean.stack, fname_ext = '-mean.nc', ...){
    fname_out = replace.str(fname_in , 'outputs/', 'temp/')
    fname_out = replace.str(fname_out, '.nc', fname_ext)
    return(runIfNoFile(fname_out, FUN, fname_in, ...))
}

# monthly mean      
param   = mean(read.csv(coefficants_file)[,'H'])           
sources = lapply(drive_fname[c('pas', 'Lightn')], openMean)
sources[[1]] = sources[[1]] * param

ratio = sources[[1]]/sources[[2]]
lims = c(1/4, 1/3, 1/2, 1/1.0001, 1.0001, 2, 3, 4)
cols = c('#004400', 'green', 'white', 'purple', '#220022')
plot_raster(ratio, lims, cols, quick = TRUE)
mtextStandard('human:lightn igntions')

standard_legend2(cols, lims, dat = experiments[[1]], 
                 labelss = c('', '1/4', '1/3', '1/2', '       1', '', '2', '3', '4'))
#################################################################
## Plot Differce                                               ##
#################################################################
diffPlot <- function(i, j) {
    i = experiments[[1]] - i
    plot_raster(i, diff_lims2, diff_cols2, quick = TRUE)
    mtextStandard(j)
    return(i)
}

experiments = mapply(diffPlot, experiments[-1], labs2)
standard_legend2(diff_cols2, diff_lims2, dat = experiments[[1]])
mtext.burntArea('Change in burnt area (%)')

#################################################################
dev.off.gitWatermark()

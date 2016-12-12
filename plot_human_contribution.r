#################################################################
## cfg                                                         ##
#################################################################
source('cfg.r')
graphics.off()


## output filename
mod_file = 'outputs/LimFIRE_fire'
fig_file = 'figs/IgntionInfo'

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
 
#################################################################
## Open data                                                   ##
#################################################################
mod_file = paste(mod_file, experiments, '.nc', sep ='')

load_experiments <- function(file, remove)
    runIfNoFile(file, runLimFIREfromstandardIns, fireOnly = TRUE, remove = remove)
                 
experiments = mapply(load_experiments, mod_file, remove)

#################################################################
## Setup plots                                                 ##
#################################################################
openPlot <- function(fname, width, height, ...) {
    fname = paste(fig_file, fname, '.png', sep = '')
    png(fname, width = width, height = height,
        units = 'in', res = 300)
    par(mar = c(0,0,0,0))
    layout(...)
}

openPlot('burntArea', 6, 4, rbind(1,2), heights = c(1, 0.3))
openPlot('SourceAdding', 8, 6, rbind(1:2,c(3,3), 4:5, c(6,6)),
         heights = c(1, 0.3, 1, 0.3))
openPlot('NoHumans    ', 5, 6, rbind(1, 2, 3, 4),
         heights = c(1, 0.3, 1, 0.3))
openPlot('sourceImportance', 6, 4, rbind(1,2), heights = c(1, 0.3))

mtextStandard <- function(...) mtext(..., line = -2)

standard_legend2 <- function(...)
        standard_legend(plot_loc = c(0.2, 0.9, 0.65, 0.78), ...)
                  


mtext.burntArea <- function(txt = 'Burnt Area (%)')
    mtext(txt, cex = 0.8, line = -5)                  

#################################################################
## plot annual average                                         ##
#################################################################
addBeforeEx <- function(i, add)
    paste(strsplit(i, '.nc')[[1]], add, '.nc', sep = '')

aaPlot <- function(i, f, j, p, L, s) {
    dev.set(p + 1)
    fname = addBeforeEx(f, 'aaConvert')
    i = aaConvert(i, quick = TRUE, fname = fname) 
	i = i * s
    mtextStandard(j)
    if (L) {
        standard_legend(dat = experiments[[1]])
        mtext.burntArea()
    }
    return(i)
}

experiments = mapply(aaPlot, experiments, names(experiments), labs1,
                     c(1, 2, 2, 3), c(T, F, T, T), c(1,1,1,1)) 


#################################################################
## plot no. ignitions from source                              ##
#################################################################
openMean <- function(fname_in, FUN = mean.stack, fname_ext = '-mean.nc', ...){
    fname_out = replace.str(fname_in , 'outputs/', 'temp/')
    fname_out = replace.str(fname_out, '.nc', fname_ext)
    return(runIfNoFile(fname_out, FUN, fname_in, ...))
}

# monthly mean      
param   = mean(read.csv(coefficants_file)[,'H']) * 6         
sources = lapply(drive_fname[c('pas', 'Lightn')], openMean)
sources[[1]] = sources[[1]] * param

ratio = sources[[1]]/sources[[2]]
lims = c(1/4, 1/3, 1/2, 1/1.0001, 1.0001, 2, 3, 4)
cols = c('#003300', 'green', 'white', 'purple', '#220022')
dev.set(4 + 1)
    plot_raster(ratio, lims, cols, quick = TRUE)
    mtextStandard('human:lightn igntions')

    standard_legend2(cols, lims, dat = experiments[[1]], 
                     labelss = c('', '4 x', '3 x', '2 x', '       equal', '', '2 x', '3 x', '4 x'))
	
	
    mtext('Human                   Natural', line = -4, cex = 0.8)
	mtext('More natural igntions     More human igntions')
#################################################################
## Plot Differce                                               ##
#################################################################
diffPlot <- function(i, j, p, L) {
    dev.set(p + 1)
    i = experiments[[1]] - i
    plot_raster(i, diff_lims2, diff_cols2, quick = TRUE)
    mtextStandard(j)
    if (L) {
        standard_legend2(diff_cols2, diff_lims2, dat = experiments[[1]])
        mtext.burntArea('Change in burnt area (%)')
    }
    return(i)
}

experiments = mapply(diffPlot, experiments[-1], labs2,
                     c(2, 2, 3), c(F, T, T))
#################################################################
graphics.off.gitWatermark()

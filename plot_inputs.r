#########################################################################
## cfg                                                                 ##
#########################################################################

source('cfg.r')
graphics.off()

grab_cache = TRUE

names = names(drive_fname)

fignames = paste('figs/', c('inputs_mean','inputs_fireSeason'), '.pdf', sep = '')

cols = list(alpha   = c('white', '#AA00AA', '#220022'),
            emc     = c('white', '#00AAAA', '#002222'),
            npp     = c('white', '#77DD00', '#004400'),
            crop    = c('white', '#AAAA00', '#222200'),
            pas     = c('white', '#CC8800', '#441100'),
            urban   = c('white', 'grey'   , 'black'  ),
            popdens = c('white', 'grey'   , 'black'  ),
            Lightn  = c('black', '#0000FF', 'yellow'  ),
            fire    = c('white', '#EE9900', '#440000'))

lims = list(alpha   = c(0.2, 0.4, 0.6, 0.8, 1.0),
            emc     = c(5, 10, 20, 40, 60, 80),
            npp     = c(0, 1000, 2000, 4000, 10000),
            crop    = c(0.1, 0.3, 1, 3, 10, 30),
            pas     = c(1, 2, 5, 10, 20, 50),
            urban   = c(0.001, 0.1, 1, 5, 10),
            popdens = c(0.01, 0.1, 1, 10, 100, 1000),
            Lightn  = c(0.01, 0.1, 0.2, 0.5, 1, 2, 3),
            fire    = c(0.001, 0.002, 0.005, 0.010, 0.020, 0.050))

#########################################################################
## open data                                                           ##
#########################################################################


openMean <- function(fname_in, FUN = mean.stack, fname_ext = '-mean.nc', ...){
    fname_out = replace.str(fname_in , 'outputs/', 'temp/')
    fname_out = replace.str(fname_out, '.nc', fname_ext)
    return(runIfNoFile(fname_out, FUN, fname_in, ..., test = grab_cache))
}

## monthly mean                    
Obs_mean = lapply(drive_fname, openMean)

## monthly mean during fire season height
fire_season = fire_season()
Obs_fire = lapply(drive_fname, openMean, fire.stack, '-season.nc', fire_season)
    
#########################################################################
## Plot                                                                ##
#########################################################################
   
plot_inputs <- function(Obs, fname, ...) {
    print(fname)
    pdf(fname, height = 9, width = 18)
    layout(matrix(1:18, nrow = 6), height = rep(c(1, 0.3), 3))

    par(mar = rep(0.5, 4))

    plot_input <- function(x, lim, col, name) {
        plot_raster(x, lim, col, quick = TRUE, ...)
        mtext(name, line = -1)
        standard_legend(col, lim, x)
    }

    mapply(plot_input, Obs, lims, cols, names)
    dev.off.gitWatermark()
}

plot_inputs(Obs_mean, fignames[1])
plot_inputs(Obs_fire, fignames[2], missCol = 'grey')
#########################################################################
## cfg                                                                 ##
#########################################################################

source('cfg.r')
graphics.off()

grab_cache = TRUE

fignames = paste('figs/',
                c('inputs_mean','inputs_fireSeason'), '.pdf', sep = '')

names_input = names(drive_fname)

cols_input = list(alpha   = c('white', '#AA00AA', '#220022'),
                  emc     = c('white', '#00AAAA', '#002222'),
                  npp     = c('white', '#77DD00', '#004400'),
                  crop    = c('white', '#AAAA00', '#222200'),
                  pas     = c('white', '#CC8800', '#441100'),
                  urban   = c('white', 'grey'   , 'black'  ),
                  popdens = c('white', 'grey'   , 'black'  ),
                  Lightn  = c('black', '#0000FF', 'yellow'  ),
                  fire    = c('white', '#EE9900', '#440000'))

lims_input = list(alpha   = c(0.2, 0.4, 0.6, 0.8, 1.0),
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
fire_season = fire_season()

## Individual inputs
openMean <- function(fname_in, FUN = mean.stack, fname_ext = '-mean.nc', ...){
    fname_out = replace.str(fname_in , 'outputs/', 'temp/')
    fname_out = replace.str(fname_out, '.nc', fname_ext)
    return(runIfNoFile(fname_out, FUN, fname_in, ..., test = grab_cache))
}

# open Obs
Obs = lapply(drive_fname, stack)

# monthly mean                    
Obs_mean = lapply(drive_fname, openMean)

# monthly mean during fire season height
Obs_fire = lapply(drive_fname, openMean, fire.stack, '-season.nc', fire_season)


#########################################################################
## Plot maps                                                           ##
#########################################################################
plot_inputs <- function(Obs, fname, names = names_input,
                        lims = lims_input, cols = cols_input, ...) {
    print(fname)
    
    plot_input <- function(x, lim, col, name) {
        plot_raster(x, lim, col, quick = TRUE, ...)
        mtext(name, line = -0.67)
        standard_legend(col, lim, x)
    }
    
    nplts = length(lims)
    nrows = ceiling(sqrt(length(lims)))
    
    pdf(fname, height = 2.5 * nrows, width = 4.5 * ceiling(nplts/nrows))
        layout(matrix(1:(2*nplts), nrow = nrows * 2), height = rep(c(1, 0.3), 3))

        par(mar = rep(0.5, 4))
        mapply(plot_input, Obs, lims, cols, names)
    dev.off.gitWatermark()
}

## Plot Individuals
plot_inputs(Obs_mean, fignames[1])
plot_inputs(Obs_fire, fignames[2])
#########################################################################
## cfg                                                                 ##
#########################################################################

source('cfg.r')
graphics.off()

grab_cache = TRUE

fignames = paste('figs/',
                c('inputs_mean','inputs_fireSeason'), sep = '')

names = names(drive_fname)

titls = list(alpha   = 'Soil Moisture',
             emc     = 'Drying rate',
             npp     = 'Net Primary Production',
             crop    = 'Cropland',
             pas     = 'Pasture land',
             urban   = 'Urban',
             popdens = 'Population Density',
             Lightn  = 'Lightning Stikes',
             fire    = 'Burnt Area')

cols =  list(alpha   = c('white', '#AA00AA', '#220022'),
             emc     = c('white', '#00AAAA', '#002222'),
             npp     = c('white', '#77DD00', '#004400'),
             crop    = c('white', '#AAAA00', '#222200'),
             pas     = c('white', '#CC8800', '#441100'),
             urban   = c('white', 'grey'   , 'black'  ),
             popdens = c('white', 'grey'   , 'black'  ),
             Lightn  = c('black', '#0000FF', 'yellow'  ),
             fire    = c('white', '#EE9900', '#440000'))

lims =  list(alpha   = c(10, 20, 40, 60, 80),
             emc     = c(10, 20, 40, 60, 80),
             npp     = c(0, 1, 2, 4, 8),
             crop    = c(0.1, 0.3, 1, 3, 10, 30),
             pas     = c(1, 2, 5, 10, 20, 50),
             urban   = c(0.001, 0.1, 1, 5, 10),
             popdens = c(0.01, 0.1, 1, 10, 100, 1000),
             Lightn  = c(0.01, 0.1, 0.2, 0.5, 1, 2, 3),
             fire    = c(1, 2, 5, 10, 20, 50))
             
scles = list(alpha   = 100/1.2,
             emc     = 1,
             npp     = 1/1000,
             crop    = 1,
             pas     = 1,
             urban   = 1,
             popdens = 1,
             Lightn  = 1,
             fire    = 1200)

units = list(alpha   = '% moisture content',
             emc     = '% equilibrium moisture content',
             npp     = expression(paste('gC ', m^2)),
             crop    = '% cover',
             pas     = '% cover',
             urban   = '% cover',
             popdens = expression(paste('people ',km^2)),
             Lightn  = expression(paste('strikes ',km^2, ' ', month^-2)),
             fire    = expression(paste('%', yr^-1)))
            
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
plot_inputs <- function(Obs, fname, ...) {
    
    plot_input <- function(x, scle, lim, col, titl, name, unit) {
        fname = paste(fname, '-', name, '.png', sep = '')
        print(fname)
        png(fname, height =     5*2/3, width = 6, units = 'in', res = 150)
            par(mar = rep(0.0, 4))
            layout(matrix(1:2, nrow = 2), height = c(1, 0.3))
            
            plot_raster(x * scle, lim, col, quick = TRUE, ...)
            mtext(titl, cex = 1.5)
            
            standard_legend(col, lim, x)
            mtext(unit, line = -0.33, adj = 0.57)
        dev.off.gitWatermark()  
    }
    mapply(plot_input, Obs, scles, lims, cols, titls, names, units)    
}

plot_inputs(Obs_mean, fignames[1])
plot_inputs(Obs_fire, fignames[2])
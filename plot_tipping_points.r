#########################################################################
## cfg                                                                 ##
#########################################################################
source('cfg.r')

grab_cache = TRUE

## output filename
fig_files = paste('figs/',c('Amazon_maps.png', 'tippingPoint.png'))

mod_file = paste('temp/tipping_', c('fire', 'fuel', 'moisture', 'ignitons', 'supression'), '.nc')

extent = extent(c(-85, -45, -15, 12.5))

limitation_lims = c(50, 70, 80, 90, 95, 99)

fire_threshold = 0.01

#########################################################################
## Run experimets                                                      ##
#########################################################################
mod = runIfNoFile(mod_file, runLimFIREfromstandardIns,
                  test = grab_cache)

obs = lapply(drive_fname, stack)[["fire"]]

apply2mod_obs <- function(FUN, ...) {
    obs = FUN(obs, ...)
    mod = lapply(mod, FUN, ...)
    return(list(obs, mod))
}

c(obs, mod) := apply2mod_obs(crop, extent)

#########################################################################
## Plot Standard stuff                                                 ##
#########################################################################
png(fig_files[1], height = 5, width = 5, units = 'in', res = 300)
    par(mfrow = c(2, 3))
    ## plot modern fire
    aaConvert_standard <- function(...) aaConvert(..., y_range = c(-15, 12.5))
    aa_obs = aaConvert_standard(obs)/100
    aa_mod = aaConvert_standard(mod[[1]])
    aa_lims = lapply(mod[-1], function(i) aaConvert_standard(i/12, lims = limitation_lims)/100)
dev.off.gitWatermark()

#########################################################################
## Find area of interest                                               ##
#########################################################################

mask = aa_lims[[2]] > aa_lims[[1]] & aa_obs < fire_threshold
mask[mask == 0] = NaN



deltaM = (fire_threshold - aa_obs) / ((1-aa_lims[[1]]) * (1-aa_lims[[3]]) * (1-aa_lims[[4]]))
deltaM[is.na(mask)] = NaN



#deltaM = deltaM + 1

#deltaM = (deltaM + 1)^ 10

png(fig_files[2], height = 5, width = 5, units = 'in', res = 300)
    cols = c('#FFFFFF','#DD9900', '#050500')
    limits = c(0, 2, 5, 10, 20, 30, 40)
    
    plot_raster_from_raster(100 * deltaM, limits = limits, cols = cols)

    add_raster_legend2(cols, limits, add = TRUE,
               plot_loc = c(0.1,0.9,0.0,0.03), dat = deltaM,
               transpose = FALSE,
               srt = 0)
    mtext('%change in fuel moisture to reach "tipping point"', side = 1, line = 2)
dev.off.gitWatermark()
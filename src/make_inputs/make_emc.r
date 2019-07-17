################################################################################
## cfg                                                                        ##
################################################################################
## Libraries etc
source('cfg.r')
sourceAllLibs('src/weather/')

## paths and parameters
dir   = 'data/cru_ts3.23/'
varns = c(wetday = 'wet',
          vap    = 'vap',
          temp   = 'tmp')

################################################################################
## load data                                                                  ##
################################################################################
clim_layers= (min(clim_layers-12):max(clim_layers))
c(dat, nyears) := loadClimDat(dir, varns, clim_layers)

################################################################################
##                                                          ##
################################################################################

make_emc <- function(i, make_hr = FALSE) {
    m = monthOfYear(i)
    Wet = dat[['wetday']][[i]] / ml[m]
    Vap = dat[[1]][[i]]
    Tas = dat[[2]][[i]]
    Hr = realtive_humidity(Vap, Tas)
    if (make_hr) return(Hr/100)
    emc = fuel_moisture_equilibrium(0, Hr, Tas)   
     
    emc = emc * (1-Wet) + 100 * Wet
    emc = emc / 100
    return(emc)
}

Hr = layer.apply(1:(12*nyears), make_emc, TRUE)
Hr = Hr[[-c(1:12)]]

emc = layer.apply(1:(12*nyears), make_emc)
emcMax = seaCy12(emc, function(...) max(...))
emc = emc[[-c(1:12)]]
################################################################################
## run and output                                                             ##
################################################################################

writeRaster.gitInfo.time(Hr, drive_fname['Hr'], overwrite = TRUE)
writeRaster.gitInfo.time(emc, drive_fname['emc'], overwrite = TRUE)
writeRaster.gitInfo.time(emcMax, drive_fname['emcMax'], overwrite = TRUE)

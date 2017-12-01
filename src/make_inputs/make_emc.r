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

make_emc <- function(i) {
    m = monthOfYear(i)
    Wet = dat[['wetday']][[i]] / ml[m]
    Vap = dat[[1]][[i]]
	Tas = dat[[2]][[i]]
	Hr = realtive_humidity(Vap, Tas)
	emc = fuel_moisture_equilibrium(0, Hr, Tas)
        
    emc = emc * (1-Wet) + 100 * Wet
        
    return(emc)
}

emc = layer.apply(1:(12*nyears), make_emc)
emcMax = seaCy12(emc, function(...) max(...))
emc = emc[[-c(1:12)]]
################################################################################
## run and output                                                             ##
################################################################################


writeRaster.gitInfo(emc, drive_fname['emc'], overwrite = TRUE)
writeRaster.gitInfo(emcMax, drive_fname['emcMax'], overwrite = TRUE)

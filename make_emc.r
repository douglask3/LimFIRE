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

clim_layers = 73:288
ml = c(31,28,31,30,31,30,31,31,30,31,30,31)
################################################################################
## load data                                                                  ##
################################################################################
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

################################################################################
## run and output                                                             ##
################################################################################


writeRaster.gitInfo(emc, drive_fname['emc'], overwrite = TRUE)

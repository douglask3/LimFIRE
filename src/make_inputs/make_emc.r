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
          temp   = 'tmp',
		  precip = 'pre')

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
    Vap = dat[['vap']][[i]]
	Tas = dat[['temp']][[i]]
	Prc = dat[["precip"]][[i]]
	
	Hr = realtive_humidity(Vap, Tas)
	emc = fuel_moisture_equilibrium(0, Hr, Tas)
    
    emc = emc * (1-Wet) + Wet
        
    return(list(emc = emc, Hr = Hr, Tas = Tas, Wet = Wet, Prc = Prc))
}

################################################################################
## run and output                                                             ##
################################################################################
outs = lapply(1:(12*nyears), make_emc)

outRaster <- function(nme) {
	out = layer.apply(outs, function(i) i[[nme]])
	print(drive_fname[nme])
	writeRaster.gitInfo(out, drive_fname[nme], overwrite = TRUE)#
}

lapply(c('emc', 'Hr', 'Tas', 'Wet', 'Prc'), outRaster)
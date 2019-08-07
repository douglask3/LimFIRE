################################################################################
## cfg                                                                        ##
################################################################################
## Libraries etc
source('cfg.r')
sourceAllLibs('src/weather/')

## paths and parameters
lightn_file   = 'data/lightn_climatology_otd_mlnha.nc'

wd_dir   = 'data/cru_ts4.03/'
wd_varn = c(wetday = 'wet' )

################################################################################
## make data                                                                  ##
################################################################################
c(dat, nyears) := loadClimDat(wd_dir, wd_varn, clim_layers)
lightn = brick(lightn_file)
lightn = convert_pacific_centric_2_regular(lightn, TRUE)

make_lightn <- function(i) {
    m = monthOfYear(i)
    Pwet = dat[['wetday']][[i]] / ml[m]
    
    L = lightn[[m]]
    L = lightningIgnitions(L, Pwet)#
    
    return(L)
}

makeVariable(make_lightn, 'Lightn', nyears)



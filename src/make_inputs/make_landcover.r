################################################################################
## cfg                                                                        ##
################################################################################
## Libraries etc
source('cfg.r')

## Paths and parameters
dir       = 'data/hyde_land/'
variables = c(crop = 'cropland', pas = 'grazing', urban = 'uopp', popdens = 'popd')

## For regridding
grid_file = 'data/cru_ts3.23/cru_ts3.23.1991.2000.cld.dat.nc'

## For output info
comment = list('data description' = 'regridded HYDE3.2',
               'data reference'   = 'Klein Goldewijk, K. , A. Beusen, M. de Vos and G. van Drecht (2011). The HYDE 3.1 spatially explicit database of human induced land use change over the past 12,000 years, Global Ecology and Biogeography20(1): 73-86. DOI: 10.1111/j.1466-8238.2010.00587.x.
                                     Klein Goldewijk, K. , A. Beusen, and P. Janssen (2010). Long term dynamic modeling of global population and built-up area in a spatially explicit way, HYDE 3 .1. The Holocene20(4):565-573. http://dx.doi.org/10.1177/0959683609356587',
               'data url'         = 'ftp://ftp.pbl.nl/hyde/hyde32/hyde32_lower/',
               'data variable'    = '')

source("src/make_inputs/make_cover.r")
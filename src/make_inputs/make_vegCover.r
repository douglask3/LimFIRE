################################################################################
## cfg                                                                        ##
################################################################################
## Libraries etc
source('cfg.r')

## Paths and parameters
dir       = 'data/vcf_0.5/'
variables = c(tree = 'treecover', grass = 'nontree', bare = 'bareground')

## For regridding
grid_file = 'data/cru_ts3.23/cru_ts3.23.1991.2000.cld.dat.nc'

## For output info
comment = list('data description' = 'regridded vcf',
               'data reference'   = '',
               'data url'         = '',
               'data variable'    = '')

source("src/make_inputs/make_cover.r")

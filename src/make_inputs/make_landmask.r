Obs        = openAllObs()

mask = layer.apply(Obs, function(i) !is.na(i[[1]]))
mask = sum(mask) > 18

writeRaster.gitInfo(mask, filename = 'data/seamask.nc')
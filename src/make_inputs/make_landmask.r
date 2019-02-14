Obs        = openAllObs()


mask = layer.apply(Obs, function(i) !is.na(i[[1]]))
mask = sum(mask) > 18
nc = ncol(mask); nr = nrow(mask)

#writeRaster.gitInfo(mask, filename = 'data/seamask.nc', overwrite = TRUE)

#mask = disaggregate(mask ,5, method = "bilinear")
#mask = sum(mask) > 18


#writeRaster.gitInfo(mask, filename = 'data/seamask-hires.nc', overwrite = TRUE)
    
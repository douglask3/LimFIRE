Obs        = openAllObs()


mask = layer.apply(Obs, function(i) !is.na(i[[1]]))
mask = sum(mask) > 18

vmask = as.matrix(mask); nc = ncol(vmask); nr = nrow(vmask)

values(mask) = vmask * 10 + vmask[c(2:nr, 1),] + vmask[c(nr, 1:(nr-1)),] + vmask[,c(2:nc,1)] + vmask[,c(nc, 1:(nc-1))]

maski = mask
mask[maski >10] = 2
mask[maski > 0 & maski < 10] = 1

writeRaster.gitInfo(mask, filename = 'data/seamask.nc', overwrite = TRUE)

#mask = disaggregate(mask ,5, method = "bilinear")
#mask = sum(mask) > 18


#writeRaster.gitInfo(mask, filename = 'data/seamask-hires.nc', overwrite = TRUE)
    
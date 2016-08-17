findTotalMask <- function(r) {
    mask = is.na(r[[1]])
    for (i in 2:nlayers(r)) mask = mask + is.na(r[[i]])
    mask = mask > 0 
    mask = writeRaster(mask, file = memSafeFile(), overwrite = TRUE)
    return(mask)
}

valuesLayerByLayer <- function(r, mask) {
    lmask = sum.raster(mask)
    v = rep(NaN, lmask * nlayers(r))
    for (i in 1:nlayers(r)) {
        index = seq((i-1) * lmask + 1, i * lmask)
        v[index] = r[[i]][mask]
    }
    return(v)
}

ObsRasters2DataFrame <- function(...) {
    Obs = lapply(drive_fname, stack)
    Obs = rasters2DataFrame(Obs, ...)
}

rasters2DataFrame <- function(x, randomSample = NULL) {
    nl = sapply(x, nlayers)
    mn = min(nl)
    if (any(nl!=mn)) {
        warning("layers different in inputs. Selecting first ", mn, " layers for each")
        x = lapply(x, function(i) i[[1:mn]])
    }
    
    mask = layer.apply(x, findTotalMask)
    mask = sum(mask) == 0
    if (!is.null(randomSample)) {
        index = which(values(mask)==1)
        index = sample(index, randomSample, replace = FALSE)
        mask[] = 0
        mask[index] = 1    
    }
    x = lapply(x,  valuesLayerByLayer, mask)
    x = data.frame(x)
    return(x)
}
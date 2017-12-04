seaCyClim <- function(r, normalise = TRUE, ...) {
	climConvert <- function(mn) {
		index = seq(mn, nlayers(r), 12)
		return(mean(r[[index]]))
	}
	r = layer.apply(1:12, climConvert)
	rClim = range(r)
	if (normaise) rClim = (rClim[[2]] - rClim[[1]]) / mean(r)
	return(rClim)
}

seaCy12 <- function(r, FUN = sd.raster, ...) {
	
	
	Cy12 <- function(mnth) {
		subr = r[[(mnth-11):mnth]]
		return(FUN(subr, ...) / mean(subr))
	}
	return(layer.apply(13:nlayers(r), Cy12))
}
sd12 <- function(r, ...) {
	 r = running12(r)
	 return(sd.raster(r, ...))
}	
IAV.raster <- function(r) {
	sumYear <- function(yr) {
		mn = 12*(yr - 1) + 1
		mn = mn:(yr*12)
		sum(r[[mn]])
	}
	yrs =  nlayers(r)/12
	IA = layer.apply(1:yrs, sumYear)
	return(sd.raster(IA))
}
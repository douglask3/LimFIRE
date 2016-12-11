scale2zeropnt <- function(x) {
	test = x == 0
	x[test] = NaN
	p = min.raster(x, na.rm = TRUE)
	print(p)
	x = 1-(1 - x)/(1 - p)       
	x[test] = 0
	return(x)
}

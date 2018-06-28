quantile.raster <- function(r, ...) {
	
	mask = is.na(sum(r)) 
	rv = r[!mask]
	qnt = apply(rv, 1, quantile, ...)
	
	r = r[[1]]
	r = apply(qnt, 1, function(i) {r[!mask] = i; r})
	r = layer.apply(r, function(i) i)
	
	return(r)
}
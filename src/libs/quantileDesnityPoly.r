quantileDesnityPoly <- function(x, y, xlim = range(x), nbins = 40, 
								quantiles = c(0.5, 0.9, 0.95, 0.99, 0.999),
								ymin = 0.000001) {#seq(0.01, 0.99, 0.01)
	x0 = x; y0 = y
	bins = seq(xlim[1], xlim[2], length.out = nbins + 1)
	
	findBinRanges <- function(x1, x2)
		quantile(y[x < x2 & x > x1], quantiles)
	
	y = mapply(findBinRanges, head(bins, -1), bins[-1])
	x = bins[-1] - diff(bins)/2
	mask = !(apply(is.na(y), 2, any) | (apply(y, 2, sum) == 0))
	y[y < ymin] = ymin
	y = y[, mask]
	x = x[mask]
	
	ploygonQs <- function(qy) 
		polygon(c(x, rev(x)), c(qy, rep(ymin, length(x))), border = NA, col = make.transparent('black', 0.67))
	apply(y, 1, ploygonQs)
}
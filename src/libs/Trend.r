findRasterTrend <- function(r) {
	mask = !is.na(r[[1]])
	vr = r[mask]

	lmFUN <- function(y) {
		fit = lm(as.vector(y) ~ c(1:length(y)))
		p = summary(fit)[[4]][,4][2]
		dy = coefficients(lm(as.vector(y) ~ c(1:length(y))))[2]
		return(c(gradient = dy, p = p))
	}

	res = apply(vr, 1, lmFUN)
	trend = r[[1:2]]

	trend[[1]][mask] = res[1,]
	trend[[2]][mask] = res[2,]
	
	return(trend)	
}

Trend <- function(r, smoothFun = running12, ...) {
	if (!is.null(smoothFun)) r = smoothFun(r, ...)
	mask = !is.na(r[[1]])
	vr = r[mask]

	lmFUN <- function(y) {
		fit = lm(as.vector(y) ~ c(1:length(y)))
		p = summary(fit)[[4]][,4][2]
		dy = coefficients(lm(as.vector(y) ~ c(1:length(y))))[2]
		return(c(gradient = dy, p = p))
	}

	res = apply(vr, 1, lmFUN)
	trend = r[[1:2]]

	trend[[1]][mask] = res[1,]
	trend[[2]][mask] = res[2,]
	
	return(trend)	
}


removeTrend <- function(r, smoothFun = running12, rs, ...) {
	fireUnControl = rs[[2]] * rs[[3]] * rs[[4]]
	fireControl = r * fireUnControl
	r0 = r
	trend = findRasterTrend(r)
	
	r = layer.apply(1:nlayers(r), function(i) r[[i]] - trend[[1]] * i)
	
	fireTrendRm = r * fireUnControl
	fireTrendRm = fireControl - fireTrendRm
	trend[[1]] = sum(fireTrendRm)
	
	return(trend)
}
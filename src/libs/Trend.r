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